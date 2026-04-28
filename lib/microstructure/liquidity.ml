open Core

type kyle_result =
  | Lambda of float
  | Settlement_contamination of { hours_to_expiry : float }
  | Insufficient_data of int
[@@deriving sexp]

type amihud_result =
  | Illiquidity of float
  | All_zero_volume of { days_observed : int }
  | Settlement_contamination of { hours_to_expiry : float }
  | Insufficient_data of int
[@@deriving sexp]

let mean xs =
  match Array.length xs with
  | 0 -> Float.nan
  | n -> Array.fold xs ~init:0. ~f:( +. ) /. Float.of_int n

let covariance xs ys =
  let n = Array.length xs in
    match n <> Array.length ys with
    | true -> Float.nan
    | false ->
      (match n < 2 with
       | true -> Float.nan
       | false ->
         let mx = mean xs in
         let my = mean ys in
         let acc = ref 0. in
           for i = 0 to n - 1 do
             acc := !acc +. ((xs.(i) -. mx) *. (ys.(i) -. my))
           done;
           !acc /. Float.of_int (n - 1))

let variance xs =
  let n = Array.length xs in
    match n < 2 with
    | true -> Float.nan
    | false ->
      let m = mean xs in
      let acc = ref 0. in
        for i = 0 to n - 1 do
          acc := !acc +. Float.square (xs.(i) -. m)
        done;
        !acc /. Float.of_int (n - 1)

let hours_to_expiry ~expiry ~as_of =
  let span = Time_float_unix.diff expiry as_of in
    Time_float.Span.to_hr span

let check_settlement_contamination ~expiry_time ~min_hours_to_expiry ~as_of =
  match expiry_time with
  | None -> None
  | Some expiry ->
    let now =
      match as_of with
      | Some t -> t
      | None -> Time_float_unix.now ()
    in
    let h = hours_to_expiry ~expiry ~as_of:now in
      match Float.( < ) h min_hours_to_expiry with
      | true -> Some h
      | false -> None

let kyle_lambda
    ~returns
    ~signed_volumes
    ?expiry_time
    ?(min_hours_to_expiry = 1.0)
    ?as_of
    () : kyle_result
  =
  let n = Array.length returns in
    match n < 2 || n <> Array.length signed_volumes with
    | true -> Insufficient_data n
    | false ->
      (match
         check_settlement_contamination
           ~expiry_time
           ~min_hours_to_expiry
           ~as_of
       with
       | Some h -> Settlement_contamination { hours_to_expiry = h }
       | None ->
         let cov = covariance returns signed_volumes in
         let var = variance signed_volumes in
           match Float.( = ) var 0. with
           | true -> Insufficient_data n
           | false -> Lambda (cov /. var))

let amihud_illiquidity
    ~daily_returns
    ~daily_volumes
    ?(min_volume = 0.)
    ?expiry_time
    ?(min_hours_to_expiry = 24.0)
    ?as_of
    () : amihud_result
  =
  let n = Array.length daily_returns in
    match n < 1 || n <> Array.length daily_volumes with
    | true -> Insufficient_data n
    | false ->
      (match
         check_settlement_contamination
           ~expiry_time
           ~min_hours_to_expiry
           ~as_of
       with
       | Some h -> Settlement_contamination { hours_to_expiry = h }
       | None ->
         let valid_days = ref [] in
         let zero_days = ref 0 in
           for i = 0 to n - 1 do
             let v = daily_volumes.(i) in
               match Float.( <= ) v min_volume with
               | true -> incr zero_days
               | false ->
                 valid_days :=
                   (Float.abs daily_returns.(i) /. v) :: !valid_days
           done;
           match !valid_days with
           | [] -> All_zero_volume { days_observed = n }
           | xs ->
             let arr = Array.of_list xs in
               Illiquidity (mean arr))

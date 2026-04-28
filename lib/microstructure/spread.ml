open Core

type result =
  | Spread of float
  | Discrete_tick_warning of
      { estimated_spread : float
      ; tick_size : float
      }
  | Insufficient_data of int
[@@deriving sexp]

(** Detect if all prices are multiples of a common tick. Returns
    [Some tick] if detected, [None] otherwise. *)
let detect_tick prices =
  let n = Array.length prices in
    match n with
    | 0 -> None
    | _ ->
      (* Try common PM tick sizes: 1.0 (e.g. Kalshi pre-cents), 0.01
         (cents), 0.001 (sub-cent). Prediction markets typically work
         in 0.01 (Kalshi 0-100 cents = 0-1 prob with 0.01 tick). *)
      let candidates = [ 1.0; 0.01; 0.001 ] in
        List.find candidates ~f:(fun tick ->
          Array.for_all prices ~f:(fun p ->
            let scaled = p /. tick in
            let rounded = Float.round_nearest scaled in
              Float.( <= ) (Float.abs (scaled -. rounded)) 1e-6))

let pairwise_diffs prices =
  let n = Array.length prices in
    Array.init (n - 1) ~f:(fun i -> prices.(i + 1) -. prices.(i))

let lag1_autocovariance xs =
  let n = Array.length xs in
    match n < 2 with
    | true -> 0.
    | false ->
      let mean = Array.fold xs ~init:0. ~f:( +. ) /. Float.of_int n in
      let acc = ref 0. in
        for i = 1 to n - 1 do
          acc := !acc +. ((xs.(i) -. mean) *. (xs.(i - 1) -. mean))
        done;
        !acc /. Float.of_int (n - 1)

let roll ~prices ?tick_size () =
  let n = Array.length prices in
    match n < 3 with
    | true -> Insufficient_data n
    | false ->
      let diffs = pairwise_diffs prices in
      let cov = lag1_autocovariance diffs in
      let estimated_spread =
        match Float.( < ) cov 0. with
        | true -> 2. *. Float.sqrt (Float.neg cov)
        | false -> 0.
      in
      let detected_tick =
        match tick_size with
        | Some t when Float.( <= ) t 0. ->
          (* Caller asserted continuous price — skip detection *)
          None
        | Some t ->
          (* Caller-supplied tick — verify *)
          let all_multiples =
            Array.for_all prices ~f:(fun p ->
              let scaled = p /. t in
              let rounded = Float.round_nearest scaled in
                Float.( <= ) (Float.abs (scaled -. rounded)) 1e-6)
          in
            (match all_multiples with
             | true -> Some t
             | false -> None)
        | None -> detect_tick prices
      in
        (match detected_tick with
         | Some t -> Discrete_tick_warning { estimated_spread; tick_size = t }
         | None -> Spread estimated_spread)

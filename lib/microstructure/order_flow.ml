open Core

type book_snapshot =
  { timestamp : Time_float_unix.t
  ; bids : (float * float) array
  ; asks : (float * float) array
  }
[@@deriving sexp]

type imbalance_result =
  | Imbalance of float
  | Sampling_aliased of
      { observed_dt : float
      ; threshold : float
      }
  | Insufficient_data of int
[@@deriving sexp]

(** Mean inter-snapshot gap in seconds. Returns 0 for streams of length < 2. *)
let mean_inter_sample_seconds (snapshots : book_snapshot array) =
  let n = Array.length snapshots in
    match n < 2 with
    | true -> 0.
    | false ->
      let total =
        ref (Time_float_unix.diff
               snapshots.(n - 1).timestamp
               snapshots.(0).timestamp)
      in
      let secs = Time_float.Span.to_sec !total in
        secs /. Float.of_int (n - 1)

let snapshot_imbalance ?(levels = 5) (snap : book_snapshot) =
  let take_n arr =
    let len = Int.min levels (Array.length arr) in
      Array.sub arr ~pos:0 ~len
  in
  let bids = take_n snap.bids in
  let asks = take_n snap.asks in
  let bid_size = Array.fold bids ~init:0. ~f:(fun acc (_, s) -> acc +. s) in
  let ask_size = Array.fold asks ~init:0. ~f:(fun acc (_, s) -> acc +. s) in
  let total = bid_size +. ask_size in
    match Float.( = ) total 0. with
    | true -> 0.
    | false -> (bid_size -. ask_size) /. total

let check_sampling
    ~snapshots
    ~max_inter_sample_seconds : [ `Ok | `Aliased of float * float ]
  =
  match Float.is_inf max_inter_sample_seconds with
  | true -> `Ok
  | false ->
    let dt = mean_inter_sample_seconds snapshots in
      match Float.( > ) dt max_inter_sample_seconds with
      | true -> `Aliased (dt, max_inter_sample_seconds)
      | false -> `Ok

let orderbook_imbalance
    ~snapshots
    ?(levels = 5)
    ?(max_inter_sample_seconds = Float.infinity)
    ()
  =
  let n = Array.length snapshots in
    match n < 1 with
    | true -> Insufficient_data n
    | false ->
      (match check_sampling ~snapshots ~max_inter_sample_seconds with
       | `Aliased (observed_dt, threshold) ->
         Sampling_aliased { observed_dt; threshold }
       | `Ok ->
         let sum =
           Array.fold snapshots ~init:0. ~f:(fun acc s ->
             acc +. snapshot_imbalance ~levels s)
         in
           Imbalance (sum /. Float.of_int n))

type imbalance_series_result =
  | Series of float array
  | Series_sampling_aliased of
      { observed_dt : float
      ; threshold : float
      }
  | Series_insufficient_data of int
[@@deriving sexp]

let imbalance_series
    ~snapshots
    ?(levels = 5)
    ?(max_inter_sample_seconds = Float.infinity)
    ()
  =
  let n = Array.length snapshots in
    match n < 1 with
    | true -> Series_insufficient_data n
    | false ->
      (match check_sampling ~snapshots ~max_inter_sample_seconds with
       | `Aliased (observed_dt, threshold) ->
         Series_sampling_aliased { observed_dt; threshold }
       | `Ok ->
         Series (Array.map snapshots ~f:(snapshot_imbalance ~levels)))

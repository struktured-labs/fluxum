open Core

let brier_score ~pred ~outcome = Float.square (pred -. outcome)

let assert_same_len ~preds ~outcomes name =
  match Array.length preds <> Array.length outcomes with
  | true ->
    invalid_argf
      "%s: preds/outcomes length mismatch (%d vs %d)"
      name
      (Array.length preds)
      (Array.length outcomes)
      ()
  | false -> ()

let brier_score_avg ~preds ~outcomes =
  assert_same_len ~preds ~outcomes "brier_score_avg";
  let n = Array.length preds in
    match n with
    | 0 -> Float.nan
    | _ ->
      let sum =
        Array.foldi preds ~init:0. ~f:(fun i acc p ->
          acc +. brier_score ~pred:p ~outcome:outcomes.(i))
      in
        sum /. Float.of_int n

let log_loss ?(eps = 1e-15) ~pred ~outcome () =
  let p = Float.max eps (Float.min (1. -. eps) pred) in
    -.((outcome *. Float.log p) +. ((1. -. outcome) *. Float.log (1. -. p)))

let log_loss_avg ?eps ~preds ~outcomes () =
  assert_same_len ~preds ~outcomes "log_loss_avg";
  let n = Array.length preds in
    match n with
    | 0 -> Float.nan
    | _ ->
      let sum =
        Array.foldi preds ~init:0. ~f:(fun i acc p ->
          acc +. log_loss ?eps ~pred:p ~outcome:outcomes.(i) ())
      in
        sum /. Float.of_int n

type bin =
  { min_prob : float
  ; max_prob : float
  ; pred_avg : float
  ; outcome_rate : float
  ; count : int
  }
[@@deriving sexp]

let calibration_bins ~preds ~outcomes ~n_bins =
  assert_same_len ~preds ~outcomes "calibration_bins";
  match n_bins <= 0 with
  | true -> []
  | false ->
    let width = 1. /. Float.of_int n_bins in
    let buckets = Array.create ~len:n_bins ([] : (float * float) list) in
    Array.iteri preds ~f:(fun i p ->
      let p_clamped = Float.max 0. (Float.min 1. p) in
      let idx =
        let raw = Float.to_int (p_clamped /. width) in
          Int.min (n_bins - 1) (Int.max 0 raw)
      in
        buckets.(idx) <- (p_clamped, outcomes.(i)) :: buckets.(idx));
    Array.to_list
      (Array.mapi buckets ~f:(fun i pairs ->
         let min_prob = Float.of_int i *. width in
         let max_prob = Float.of_int (i + 1) *. width in
         let count = List.length pairs in
           match count with
           | 0 ->
             { min_prob
             ; max_prob
             ; pred_avg = (min_prob +. max_prob) /. 2.
             ; outcome_rate = Float.nan
             ; count = 0
             }
           | _ ->
             let n = Float.of_int count in
             let p_sum = List.fold pairs ~init:0. ~f:(fun acc (p, _) -> acc +. p) in
             let o_sum = List.fold pairs ~init:0. ~f:(fun acc (_, o) -> acc +. o) in
               { min_prob
               ; max_prob
               ; pred_avg = p_sum /. n
               ; outcome_rate = o_sum /. n
               ; count
               }))

let expected_calibration_error ~bins =
  let total_count =
    List.fold bins ~init:0 ~f:(fun acc (b : bin) -> acc + b.count)
  in
    match total_count with
    | 0 -> Float.nan
    | _ ->
      let weighted_dev =
        List.fold bins ~init:0. ~f:(fun acc (b : bin) ->
          match b.count with
          | 0 -> acc
          | _ ->
            let dev = Float.abs (b.pred_avg -. b.outcome_rate) in
              acc +. (Float.of_int b.count *. dev))
      in
        weighted_dev /. Float.of_int total_count

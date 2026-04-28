open Core

type quote =
  { venue : string
  ; side : [ `Yes | `No ]
  ; price : float
  ; size : float
  }
[@@deriving sexp]

type opportunity =
  { yes_quote : quote
  ; no_quote : quote
  ; cost : float
  ; profit_per_unit : float
  ; max_size : float
  }
[@@deriving sexp]

let pair_to_opportunity (yes_q : quote) (no_q : quote) : opportunity =
  let cost = yes_q.price +. no_q.price in
  { yes_quote = yes_q
  ; no_quote = no_q
  ; cost
  ; profit_per_unit = 1. -. cost
  ; max_size = Float.min yes_q.size no_q.size
  }

let candidate_pairs ~quotes =
  let yes_quotes =
    List.filter quotes ~f:(fun q ->
      match q.side with
      | `Yes -> true
      | `No -> false)
  in
  let no_quotes =
    List.filter quotes ~f:(fun q ->
      match q.side with
      | `No -> true
      | `Yes -> false)
  in
    List.concat_map yes_quotes ~f:(fun y ->
      List.filter_map no_quotes ~f:(fun n ->
        match String.equal y.venue n.venue with
        | true -> None
        | false -> Some (pair_to_opportunity y n)))

let all_two_leg ~quotes =
  candidate_pairs ~quotes
  |> List.filter ~f:(fun (op : opportunity) -> Float.( > ) op.profit_per_unit 0.)
  |> List.sort ~compare:(fun (a : opportunity) b ->
    Float.compare b.profit_per_unit a.profit_per_unit)

let best_two_leg ~quotes =
  match all_two_leg ~quotes with
  | [] -> None
  | top :: _ -> Some top

type categorical_result =
  | Arb of float
  | Insufficient_coverage of
      { observed_count : int
      ; expected_count : int
      ; observed_sum : float
      }
  | No_arb of float
[@@deriving sexp]

let categorical_check
    ~outcome_prices
    ~expected_outcomes
    ?(min_coverage_fraction = 1.0)
    ()
  =
  let observed_count = Array.length outcome_prices in
  let observed_sum = Array.fold outcome_prices ~init:0. ~f:( +. ) in
  let coverage =
    match expected_outcomes with
    | 0 -> 0.
    | n -> Float.of_int observed_count /. Float.of_int n
  in
    match Float.( < ) coverage min_coverage_fraction with
    | true ->
      Insufficient_coverage
        { observed_count; expected_count = expected_outcomes; observed_sum }
    | false ->
      (match Float.( < ) observed_sum 1.0 with
       | true -> Arb (1.0 -. observed_sum)
       | false -> No_arb observed_sum)

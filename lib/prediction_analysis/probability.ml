open Core

let clamp01 x = Float.max 0. (Float.min 1. x)

let from_kalshi_cents c =
  match c < 0 || c > 100 with
  | true -> Float.nan
  | false -> Float.of_int c /. 100.

let to_kalshi_cents p =
  match Float.is_nan p with
  | true -> 0
  | false -> Float.to_int (Float.round_nearest (clamp01 p *. 100.))

let from_decimal_odds o =
  match Float.( <= ) o 1. with
  | true -> Float.nan
  | false -> 1. /. o

let to_decimal_odds p =
  match Float.( <= ) p 0. || Float.( >= ) p 1. with
  | true -> Float.nan
  | false -> 1. /. p

let from_fractional_odds ~num ~denom =
  match num <= 0 || denom <= 0 with
  | true -> Float.nan
  | false ->
    let n = Float.of_int num in
    let d = Float.of_int denom in
      d /. (n +. d)

let from_american_odds o =
  match o = 0 with
  | true -> Float.nan
  | false ->
    (match o > 0 with
     | true ->
       (* Positive (underdog): +200 means $100 stake → $200 profit;
          implied prob = 100 / (odds + 100) *)
       100. /. (Float.of_int o +. 100.)
     | false ->
       (* Negative (favorite): -150 means $150 stake → $100 profit;
          implied prob = |odds| / (|odds| + 100) *)
       let m = Float.of_int (-o) in
         m /. (m +. 100.))

let to_american_odds p =
  match Float.( <= ) p 0. || Float.( >= ) p 1. with
  | true -> 0
  | false ->
    (match Float.( >= ) p 0.5 with
     | true ->
       (* Favorite: -|odds| where |odds| = 100 * p / (1 - p) *)
       Float.to_int (Float.round_nearest (-.(100. *. p /. (1. -. p))))
     | false ->
       (* Underdog: +odds where odds = 100 * (1 - p) / p *)
       Float.to_int (Float.round_nearest (100. *. (1. -. p) /. p)))

let implied_overround ~probs =
  Array.fold probs ~init:0. ~f:( +. ) -. 1.

let remove_overround ~probs =
  let total = Array.fold probs ~init:0. ~f:( +. ) in
    match Float.( = ) total 0. with
    | true -> Array.copy probs
    | false -> Array.map probs ~f:(fun p -> p /. total)

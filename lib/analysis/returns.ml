open Core

let simple ~prices =
  let n = Array.length prices in
    match n < 2 with
    | true -> [||]
    | false ->
      Array.init (n - 1) ~f:(fun i ->
        let p0 = prices.(i) in
        let p1 = prices.(i + 1) in
          match Float.( = ) p0 0. with
          | true -> 0.
          | false -> (p1 -. p0) /. p0)

let log ~prices =
  let n = Array.length prices in
    match n < 2 with
    | true -> [||]
    | false ->
      Array.init (n - 1) ~f:(fun i ->
        let p0 = prices.(i) in
        let p1 = prices.(i + 1) in
          match Float.( <= ) p0 0. || Float.( <= ) p1 0. with
          | true -> 0.
          | false -> Float.log (p1 /. p0))

let cumulative ~returns =
  let n = Array.length returns in
  let acc = ref 1. in
    Array.init n ~f:(fun i ->
      acc := !acc *. (1. +. returns.(i));
      !acc -. 1.)

let cumulative_log ~log_returns =
  let n = Array.length log_returns in
  let acc = ref 0. in
    Array.init n ~f:(fun i ->
      acc := !acc +. log_returns.(i);
      !acc)

let to_log r = Float.log (1. +. r)

let of_log r = Float.exp r -. 1.

let annualize ~periods_per_year r =
  ((1. +. r) ** periods_per_year) -. 1.

let annualize_log ~periods_per_year r = r *. periods_per_year

open Core

let mean xs =
  match Array.length xs with
  | 0 -> Float.nan
  | n -> Array.fold xs ~init:0. ~f:( +. ) /. Float.of_int n

let variance ?(biased = false) xs =
  let n = Array.length xs in
    match n with
    | 0 | 1 -> Float.nan
    | _ ->
      let m = mean xs in
      let sum_sq =
        Array.fold xs ~init:0. ~f:(fun acc x -> acc +. Float.square (x -. m))
      in
      let denom =
        match biased with
        | true -> Float.of_int n
        | false -> Float.of_int (n - 1)
      in
        sum_sq /. denom

let std ?biased xs = Float.sqrt (variance ?biased xs)

let percentile xs ~p =
  let n = Array.length xs in
    match n with
    | 0 -> Float.nan
    | 1 -> xs.(0)
    | _ ->
      let sorted = Array.copy xs in
      Array.sort sorted ~compare:Float.compare;
      let p_clamped = Float.max 0. (Float.min 100. p) in
      let rank = p_clamped /. 100. *. Float.of_int (n - 1) in
      let lo = Float.to_int (Float.round_down rank) in
      let hi = Float.to_int (Float.round_up rank) in
        match lo = hi with
        | true -> sorted.(lo)
        | false ->
          let frac = rank -. Float.of_int lo in
            sorted.(lo) +. (frac *. (sorted.(hi) -. sorted.(lo)))

let median xs = percentile xs ~p:50.

let min xs =
  Array.fold xs ~init:Float.infinity ~f:(fun acc x ->
    match Float.is_nan x with
    | true -> acc
    | false -> Float.min acc x)

let max xs =
  Array.fold xs ~init:Float.neg_infinity ~f:(fun acc x ->
    match Float.is_nan x with
    | true -> acc
    | false -> Float.max acc x)

let covariance ?(biased = false) xs ys =
  let n = Array.length xs in
    match n <> Array.length ys with
    | true ->
      invalid_argf
        "covariance: array length mismatch (%d vs %d)"
        n
        (Array.length ys)
        ()
    | false ->
      (match n with
       | 0 | 1 -> Float.nan
       | _ ->
         let mx = mean xs in
         let my = mean ys in
         let acc = ref 0. in
           for i = 0 to n - 1 do
             acc := !acc +. ((xs.(i) -. mx) *. (ys.(i) -. my))
           done;
           let denom =
             match biased with
             | true -> Float.of_int n
             | false -> Float.of_int (n - 1)
           in
             !acc /. denom)

let correlation xs ys =
  let cov = covariance xs ys in
  let sx = std xs in
  let sy = std ys in
    match Float.( = ) sx 0. || Float.( = ) sy 0. with
    | true -> Float.nan
    | false -> cov /. (sx *. sy)

let z_score xs =
  let n = Array.length xs in
    match n with
    | 0 -> [||]
    | _ ->
      let m = mean xs in
      let s = std xs in
        match Float.( = ) s 0. with
        | true -> Array.copy xs
        | false -> Array.map xs ~f:(fun x -> (x -. m) /. s)

let rolling_sum ~window xs =
  let n = Array.length xs in
    match window <= 0 || n = 0 with
    | true -> [||]
    | false ->
      let result = Array.create ~len:n Float.nan in
      let acc = ref 0. in
        for i = 0 to n - 1 do
          acc := !acc +. xs.(i);
          (match i >= window with
           | true -> acc := !acc -. xs.(i - window)
           | false -> ());
          (match i >= window - 1 with
           | true -> result.(i) <- !acc
           | false -> ())
        done;
        result

let rolling_mean ~window xs =
  let sums = rolling_sum ~window xs in
  let w = Float.of_int window in
    Array.map sums ~f:(fun s ->
      match Float.is_nan s with
      | true -> Float.nan
      | false -> s /. w)

let rolling_std ?(biased = false) ~window xs =
  let n = Array.length xs in
    match window <= 1 || n < window with
    | true -> Array.create ~len:n Float.nan
    | false ->
      let result = Array.create ~len:n Float.nan in
        for i = window - 1 to n - 1 do
          let slice = Array.sub xs ~pos:(i - window + 1) ~len:window in
            result.(i) <- std ~biased slice
        done;
        result

let ewma ~alpha xs =
  let n = Array.length xs in
    match n with
    | 0 -> [||]
    | _ ->
      let result = Array.create ~len:n 0. in
      result.(0) <- xs.(0);
      for i = 1 to n - 1 do
        result.(i) <- (alpha *. xs.(i)) +. ((1. -. alpha) *. result.(i - 1))
      done;
      result

let ewmstd ~alpha xs =
  let n = Array.length xs in
    match n with
    | 0 -> [||]
    | _ ->
      let m = ewma ~alpha xs in
      let result = Array.create ~len:n 0. in
      let var = Array.create ~len:n 0. in
        var.(0) <- 0.;
        for i = 1 to n - 1 do
          let dev = xs.(i) -. m.(i - 1) in
          var.(i) <- ((1. -. alpha) *. (var.(i - 1) +. (alpha *. Float.square dev)))
        done;
        Array.iteri var ~f:(fun i v -> result.(i) <- Float.sqrt v);
        result

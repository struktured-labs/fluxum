let floor_cents x = Float.round_down (x *. 100.) /. 100.

module Gemini = struct
  type config =
    { max_spread_cents : float
    ; min_size : float
    ; size_cap : float
    ; two_sided_multiplier : float
    ; uptime_threshold : float
    ; min_payout_usd : float
    }

  let default_config =
    { max_spread_cents = 10.
    ; min_size = 10.
    ; size_cap = 250.
    ; two_sided_multiplier = 1.5
    ; uptime_threshold = 0.5
    ; min_payout_usd = 1.
    }

  type quote =
    { price : float
    ; size : float
    }

  let spread_weight c ~distance_cents =
    let d = Float.abs distance_cents in
    if Float.( > ) d c.max_spread_cents
    then 0.
    else (
      let x = (c.max_spread_cents -. d) /. c.max_spread_cents in
      x *. x)

  let side_score c ~reference quotes =
    List.sum (module Float) quotes ~f:(fun q ->
      if Float.( < ) q.size c.min_size
      then 0.
      else
        spread_weight c ~distance_cents:((q.price -. reference) *. 100.)
        *. Float.min q.size c.size_cap)

  let snapshot_score c ~reference ~bids ~asks =
    let b = side_score c ~reference bids
    and a = side_score c ~reference asks in
    let mult =
      if Float.( > ) b 0. && Float.( > ) a 0. then c.two_sided_multiplier else 1.
    in
    (b +. a) *. mult

  let daily_payout c ~pool_usd ~own_score ~total_score ~uptime =
    if Float.( < ) uptime c.uptime_threshold
       || Float.( <= ) total_score 0.
       || Float.( <= ) own_score 0.
    then 0.
    else (
      let raw = pool_usd *. own_score /. total_score in
      if Float.( < ) raw c.min_payout_usd then 0. else floor_cents raw)
end

module Polymarket = struct
  let order_score ~max_spread ~spread ~multiplier =
    let s = Float.abs spread in
    if Float.( > ) s max_spread || Float.( <= ) max_spread 0.
    then 0.
    else (
      let x = (max_spread -. s) /. max_spread in
      x *. x *. multiplier)

  let side_score ~max_spread ~min_size ~multiplier orders =
    List.sum (module Float) orders ~f:(fun (spread, size) ->
      if Float.( < ) size min_size
      then 0.
      else order_score ~max_spread ~spread ~multiplier *. size)

  let q_min ~midpoint ~c ~q_one ~q_two =
    if Float.( >= ) midpoint 0.10 && Float.( <= ) midpoint 0.90
    then Float.max (Float.min q_one q_two) (Float.max (q_one /. c) (q_two /. c))
    else Float.min q_one q_two

  let epoch_score ~sample_q_mins ~uptime =
    List.sum (module Float) sample_q_mins ~f:Fn.id *. uptime

  let payout ~reward ~own_epoch ~total_epoch =
    if Float.( <= ) total_epoch 0. then 0. else reward *. own_epoch /. total_epoch
end

module Kalshi = struct
  type order =
    { price_ticks : int
    ; size : float
    ; mine : bool
    }

  type side_result =
    { reference_ticks : int
    ; qualifying : order list
    }

  let qualify_side ~target_size ~max_price_ticks orders =
    let levels =
      List.sort orders ~compare:(fun a b -> Int.compare b.price_ticks a.price_ticks)
      |> List.group ~break:(fun a b -> a.price_ticks <> b.price_ticks)
    in
    match levels with
    | [] -> None
    | (best :: _) :: _ when best.price_ticks >= max_price_ticks -> None
    | _ ->
      let rec walk levels ~cum ~reference ~acc =
        match levels with
        | [] -> None
        | level :: rest ->
          let price = (List.hd_exn level).price_ticks in
          let cum = cum +. List.sum (module Float) level ~f:(fun o -> o.size) in
          let acc = level @ acc in
          let reference =
            match reference with
            | Some _ -> reference
            | None when Float.( >= ) cum (target_size /. 5.) -> Some price
            | None -> None
          in
          if Float.( >= ) cum target_size
          then
            Option.map reference ~f:(fun reference_ticks ->
              { reference_ticks; qualifying = List.rev acc })
          else walk rest ~cum ~reference ~acc
      in
      walk levels ~cum:0. ~reference:None ~acc:[]

  let order_score ~discount_factor ~reference_ticks o =
    let n = Int.max 0 (reference_ticks - o.price_ticks) in
    o.size *. (discount_factor ** Float.of_int n)

  let side_share ~discount_factor r =
    let score o = order_score ~discount_factor ~reference_ticks:r.reference_ticks o in
    let total = List.sum (module Float) r.qualifying ~f:score in
    let mine =
      List.sum (module Float) r.qualifying ~f:(fun o -> if o.mine then score o else 0.)
    in
    if Float.( <= ) total 0. then 0. else mine /. total

  let snapshot_score ~target_size ~max_price_ticks ~discount_factor ~yes_bids ~no_bids =
    match
      ( qualify_side ~target_size ~max_price_ticks yes_bids
      , qualify_side ~target_size ~max_price_ticks no_bids )
    with
    | Some y, Some n -> side_share ~discount_factor y +. side_share ~discount_factor n
    | _ -> 0.

  let payout ~reward ~own ~total ~min_payout =
    if Float.( <= ) total 0.
    then 0.
    else (
      let raw = reward *. own /. total in
      if Float.( < ) raw min_payout then 0. else floor_cents raw)
end

open Core

type error =
  | Invalid_config of string
  | Invalid_book of string
  | Invalid_input of string
[@@deriving sexp, equal]

let is_finite x = not (Float.is_nan x || Float.is_inf x)
let positive_finite x = is_finite x && Float.(x > 0.)
let nonnegative_finite x = is_finite x && Float.(x >= 0.)

module Top_of_book = struct
  type t =
    { bid_price : float
    ; bid_qty : float
    ; ask_price : float
    ; ask_qty : float
    }
  [@@deriving sexp, equal]

  let create ~bid_price ~bid_qty ~ask_price ~ask_qty =
    {bid_price; bid_qty; ask_price; ask_qty}

  let validate t =
    if not (positive_finite t.bid_price && positive_finite t.ask_price)
    then Error (Invalid_book "bid and ask prices must be positive and finite")
    else if not (nonnegative_finite t.bid_qty && nonnegative_finite t.ask_qty)
    then Error (Invalid_book "bid and ask quantities must be non-negative and finite")
    else if Float.(t.bid_price >= t.ask_price)
    then Error (Invalid_book "top of book must have bid_price < ask_price")
    else Ok ()

  let midpoint t = t.bid_price +. ((t.ask_price -. t.bid_price) /. 2.)

  let microprice t =
    if Float.(t.bid_qty <= 0. && t.ask_qty <= 0.)
    then midpoint t
    else
      (* Compute bid_qty / (bid_qty + ask_qty) without overflowing the
         denominator or the price/quantity products. *)
      let ask_weight =
        if Float.(t.bid_qty >= t.ask_qty)
        then 1. /. (1. +. (t.ask_qty /. t.bid_qty))
        else
          let ratio = t.bid_qty /. t.ask_qty in
            ratio /. (1. +. ratio)
      in
      t.bid_price +. (ask_weight *. (t.ask_price -. t.bid_price))
end

type fair_value =
  [ `Midpoint
  | `Microprice
  ]
[@@deriving sexp, equal]

module Config = struct
  type t =
    { tick_size : float
    ; lot_size : float
    ; min_order_qty : float
    ; min_notional : float
    ; base_order_qty : float
    ; levels : int
    ; base_half_spread_bps : float
    ; level_spacing_bps : float
    ; maker_fee_bps : float
    ; target_edge_bps : float
    ; volatility_multiplier : float
    ; inventory_skew_bps : float
    ; max_position : float
    ; fair_value : fair_value
    ; price_bounds : (float * float) option
    }
  [@@deriving sexp, equal]

  let default =
    { tick_size = 0.01
    ; lot_size = 0.0001
    ; min_order_qty = 0.
    ; min_notional = 0.
    ; base_order_qty = 0.01
    ; levels = 1
    ; base_half_spread_bps = 5.
    ; level_spacing_bps = 5.
    ; maker_fee_bps = 0.
    ; target_edge_bps = 0.
    ; volatility_multiplier = 0.
    ; inventory_skew_bps = 10.
    ; max_position = 1.
    ; fair_value = `Microprice
    ; price_bounds = None
    }

  let validate t =
    let invalid field = Error (Invalid_config field) in
    if not (positive_finite t.tick_size) then invalid "tick_size must be positive and finite"
    else if not (positive_finite t.lot_size) then invalid "lot_size must be positive and finite"
    else if not (nonnegative_finite t.min_order_qty)
    then invalid "min_order_qty must be non-negative and finite"
    else if not (nonnegative_finite t.min_notional)
    then invalid "min_notional must be non-negative and finite"
    else if not (positive_finite t.base_order_qty)
    then invalid "base_order_qty must be positive and finite"
    else if t.levels < 1 then invalid "levels must be at least one"
    else if not (nonnegative_finite t.base_half_spread_bps)
    then invalid "base_half_spread_bps must be non-negative and finite"
    else if not (nonnegative_finite t.level_spacing_bps)
    then invalid "level_spacing_bps must be non-negative and finite"
    else if not (is_finite t.maker_fee_bps)
    then invalid "maker_fee_bps must be finite"
    else if not (nonnegative_finite t.target_edge_bps)
    then invalid "target_edge_bps must be non-negative and finite"
    else if not (nonnegative_finite t.volatility_multiplier)
    then invalid "volatility_multiplier must be non-negative and finite"
    else if not (nonnegative_finite t.inventory_skew_bps)
    then invalid "inventory_skew_bps must be non-negative and finite"
    else if Float.(t.inventory_skew_bps >= 10_000.)
    then invalid "inventory_skew_bps must be less than 10000"
    else if not (positive_finite t.max_position)
    then invalid "max_position must be positive and finite"
    else
      match t.price_bounds with
      | None -> Ok ()
      | Some (lower, upper) ->
        if not (nonnegative_finite lower && positive_finite upper && Float.(lower < upper))
        then invalid "price_bounds must be finite and satisfy 0 <= lower < upper"
        else Ok ()
end

module Quote = struct
  type t =
    { side : Types.Side.t
    ; level : int
    ; price : float
    ; qty : float
    }
  [@@deriving sexp, equal]
end

type quote_set =
  { fair_price : float
  ; reservation_price : float
  ; half_spread_bps : float
  ; inventory_ratio : float
  ; bids : Quote.t list
  ; asks : Quote.t list
  }
[@@deriving sexp, equal]

let round_down ~step value =
  Float.round_down ((value /. step) +. 1e-12) *. step

let round_up ~step value =
  Float.round_up ((value /. step) -. 1e-12) *. step

let round_qty_down ~step value =
  (* Quantity rounding must never exceed the remaining risk capacity. *)
  Float.round_down (value /. step) *. step

let clamp_unit x = Float.max (-1.) (Float.min 1. x)

let price_within_bounds bounds price =
  is_finite price
  && match bounds with
     | None -> Float.(price > 0.)
     | Some (lower, upper) -> Float.(price >= lower && price <= upper && price > 0.)

let generate ~config ~book ~inventory ?(volatility_bps = 0.) () =
  match Config.validate config with
  | Error _ as error -> error
  | Ok () ->
    (match Top_of_book.validate book with
     | Error _ as error -> error
     | Ok () ->
       if not (is_finite inventory)
       then Error (Invalid_input "inventory must be finite")
       else if not (nonnegative_finite volatility_bps)
       then Error (Invalid_input "volatility_bps must be non-negative and finite")
       else
         let fair_price =
           match config.fair_value with
           | `Midpoint -> Top_of_book.midpoint book
           | `Microprice -> Top_of_book.microprice book
         in
         let inventory_ratio = clamp_unit (inventory /. config.max_position) in
         let reservation_price =
           fair_price *. (1. -. (inventory_ratio *. config.inventory_skew_bps /. 10_000.))
         in
         let fee_edge_floor = Float.max 0. (config.maker_fee_bps +. config.target_edge_bps) in
         let half_spread_bps =
           Float.max config.base_half_spread_bps fee_edge_floor
           +. (config.volatility_multiplier *. volatility_bps)
         in
         if not (positive_finite fair_price
                 && positive_finite reservation_price
                 && nonnegative_finite half_spread_bps)
         then Error (Invalid_input "quote calculation overflowed")
         else
         let bid_qty_multiplier = Float.max 0. (1. -. inventory_ratio) in
         let ask_qty_multiplier = Float.max 0. (1. +. inventory_ratio) in
         let bid_capacity = ref (Float.max 0. (config.max_position -. inventory)) in
         let ask_capacity = ref (Float.max 0. (config.max_position +. inventory)) in
         let quote_for_level side level =
           let distance_bps =
             half_spread_bps +. (Float.of_int level *. config.level_spacing_bps)
           in
           let raw_price =
             match side with
             | Types.Side.Buy -> reservation_price *. (1. -. (distance_bps /. 10_000.))
             | Sell -> reservation_price *. (1. +. (distance_bps /. 10_000.))
           in
           let price =
             match side with
             | Types.Side.Buy ->
               Float.min raw_price (book.ask_price -. config.tick_size)
               |> round_down ~step:config.tick_size
             | Sell ->
               Float.max raw_price (book.bid_price +. config.tick_size)
               |> round_up ~step:config.tick_size
           in
           let multiplier, capacity =
             match side with
             | Types.Side.Buy -> (bid_qty_multiplier, bid_capacity)
             | Sell -> (ask_qty_multiplier, ask_capacity)
           in
           let unrounded_qty = Float.min (config.base_order_qty *. multiplier) !capacity in
           let qty = round_qty_down ~step:config.lot_size unrounded_qty in
           if (not (price_within_bounds config.price_bounds price))
              || not (positive_finite qty)
              || Float.(qty < config.min_order_qty)
              || Float.((price *. qty) < config.min_notional)
           then None
           else (
             capacity := Float.max 0. (!capacity -. qty);
             Some {Quote.side; level; price; qty})
         in
         let build_side side =
           List.init config.levels ~f:(fun level -> quote_for_level side level)
           |> List.filter_opt
         in
         let bids = build_side Types.Side.Buy in
         let asks = build_side Types.Side.Sell in
         Ok {fair_price; reservation_price; half_spread_bps; inventory_ratio; bids; asks})

let quotes t = t.bids @ t.asks

module Working_order = struct
  type t =
    { order_id : string
    ; side : Types.Side.t
    ; level : int
    ; price : float
    ; qty : float
    }
  [@@deriving sexp, equal]
end

type reconciliation =
  { cancel : Working_order.t list
  ; keep : (Working_order.t * Quote.t) list
  ; place : Quote.t list
  }
[@@deriving sexp, equal]

let same_key (working : Working_order.t) (desired : Quote.t) =
  Types.Side.equal working.side desired.side && Int.equal working.level desired.level

let should_replace
      ~tick_size
      ~price_tolerance_ticks
      ~qty_tolerance_ratio
      (working : Working_order.t)
      (desired : Quote.t)
  =
  let tick_size = if positive_finite tick_size then tick_size else 0. in
  let qty_tolerance_ratio =
    if nonnegative_finite qty_tolerance_ratio then qty_tolerance_ratio else 0.
  in
  let price_tolerance =
    Float.of_int (Int.max 0 price_tolerance_ticks) *. tick_size
  in
  let price_changed = Float.(abs (working.price -. desired.price) > price_tolerance +. 1e-12) in
  let qty_scale = Float.max (Float.abs working.qty) (Float.abs desired.qty) in
  let qty_change = Float.abs (working.qty -. desired.qty) in
  let qty_changed =
    Float.(qty_scale > 0. && (qty_change /. qty_scale) > qty_tolerance_ratio)
  in
  price_changed || qty_changed

let reconcile
      ~tick_size
      ?(price_tolerance_ticks = 0)
      ?(qty_tolerance_ratio = 0.)
      ~existing
      ~desired
      ()
  =
  let compare_distance (desired_quote : Quote.t) (a : Working_order.t) (b : Working_order.t) =
    match Float.compare (Float.abs (a.price -. desired_quote.Quote.price))
            (Float.abs (b.price -. desired_quote.price)) with
    | 0 -> Float.compare (Float.abs (a.qty -. desired_quote.qty))
             (Float.abs (b.qty -. desired_quote.qty))
    | comparison -> comparison
  in
  let should_replace =
    should_replace ~tick_size ~price_tolerance_ticks ~qty_tolerance_ratio
  in
  let remaining, cancel, keep, place =
    List.fold desired ~init:(existing, [], [], [])
      ~f:(fun (remaining, cancel, keep, place) desired_quote ->
        let matching, remaining =
          List.partition_tf remaining ~f:(fun working -> same_key working desired_quote)
        in
        let keepable, replaceable =
          List.partition_tf matching ~f:(fun working ->
            not (should_replace working desired_quote))
        in
        match List.sort keepable ~compare:(compare_distance desired_quote) with
        | working :: duplicates ->
          let cancel = List.rev_append duplicates (List.rev_append replaceable cancel) in
          (remaining, cancel, (working, desired_quote) :: keep, place)
        | [] ->
          (match List.sort replaceable ~compare:(compare_distance desired_quote) with
           | [] -> (remaining, cancel, keep, desired_quote :: place)
           | working :: duplicates ->
             let cancel = List.rev_append duplicates (working :: cancel) in
             (remaining, cancel, keep, desired_quote :: place)))
  in
  { cancel = List.rev_append remaining (List.rev cancel)
  ; keep = List.rev keep
  ; place = List.rev place
  }

(** Order fill simulation for backtesting *)

open Core

(** Fill configuration *)
module Config = struct
  type t =
    { slippage_pct   : float  (** Slippage as percentage (e.g., 0.001 = 0.1%) *)
    ; commission_pct : float  (** Commission as percentage of trade value *)
    ; fill_on        : [ `Open | `Close | `Midpoint ]  (** When to fill market orders *)
    }
  [@@deriving sexp, compare, equal]

  let default =
    { slippage_pct = 0.001   (* 0.1% slippage *)
    ; commission_pct = 0.001 (* 0.1% commission *)
    ; fill_on = `Close
    }

  let no_costs =
    { slippage_pct = 0.
    ; commission_pct = 0.
    ; fill_on = `Close
    }

  let create ?(slippage_pct = 0.001) ?(commission_pct = 0.001) ?(fill_on = `Close) () =
    { slippage_pct; commission_pct; fill_on }
end

(** Fill result *)
module Fill = struct
  type t =
    { price      : float   (** Actual fill price (including slippage) *)
    ; qty        : float   (** Filled quantity *)
    ; side       : [ `Buy | `Sell ]
    ; commission : float   (** Commission charged *)
    ; timestamp  : Time_ns_unix.t
    }
  [@@deriving sexp, fields]

  (** Total cost/proceeds of the fill *)
  let total t =
    match t.side with
    | `Buy -> (t.price *. t.qty) +. t.commission
    | `Sell -> (t.price *. t.qty) -. t.commission
end

(** Fill result type *)
type fill_result =
  [ `Filled of Fill.t
  | `Rejected of string
  | `Pending  (** Limit order not yet filled *)
  ]

(** Get base fill price from candle based on configuration *)
let base_price ~config candle =
  match config.Config.fill_on with
  | `Open -> candle.Candle.open_
  | `Close -> candle.Candle.close
  | `Midpoint -> (candle.Candle.high +. candle.Candle.low) /. 2.

(** Apply slippage to price *)
let apply_slippage ~config ~side price =
  let slip = price *. config.Config.slippage_pct in
  match side with
  | `Buy -> price +. slip   (* Buy at higher price *)
  | `Sell -> price -. slip  (* Sell at lower price *)

(** Calculate commission *)
let calculate_commission ~config ~price ~qty =
  price *. qty *. config.Config.commission_pct

(** Check if limit order would fill within candle's range *)
let limit_would_fill ~side ~limit candle =
  match side with
  | `Buy ->
    (* Buy limit fills if low <= limit *)
    Float.(candle.Candle.low <= limit)
  | `Sell ->
    (* Sell limit fills if high >= limit *)
    Float.(candle.Candle.high >= limit)

(** Simulate filling a market order *)
let fill_market ~config ~side ~qty candle =
  let base = base_price ~config candle in
  let price = apply_slippage ~config ~side base in
  let commission = calculate_commission ~config ~price ~qty in
  `Filled
    { Fill.price
    ; qty
    ; side
    ; commission
    ; timestamp = candle.Candle.timestamp
    }

(** Simulate filling a limit order *)
let fill_limit ~config ~side ~qty ~limit candle =
  match limit_would_fill ~side ~limit candle with
  | false -> `Pending
  | true ->
    (* For limit orders, fill at the limit price (assuming good execution) *)
    let price = apply_slippage ~config ~side limit in
    let commission = calculate_commission ~config ~price ~qty in
    `Filled
      { Fill.price
      ; qty
      ; side
      ; commission
      ; timestamp = candle.Candle.timestamp
      }

(** Simulate filling a signal *)
let simulate_fill ~config ~signal candle =
  let open Strategy_intf.Signal in
  match signal with
  | Hold -> `Pending

  | Buy { qty; limit = None } ->
    fill_market ~config ~side:`Buy ~qty candle

  | Buy { qty; limit = Some limit } ->
    fill_limit ~config ~side:`Buy ~qty ~limit candle

  | Sell { qty; limit = None } ->
    fill_market ~config ~side:`Sell ~qty candle

  | Sell { qty; limit = Some limit } ->
    fill_limit ~config ~side:`Sell ~qty ~limit candle

  | Close_long _ | Close_short _ ->
    (* These are handled by the engine which knows the position *)
    `Pending

(** Simulate closing a position *)
let simulate_close ~config ~side ~qty candle =
  match side with
  | `Long ->
    (* Closing long = selling *)
    fill_market ~config ~side:`Sell ~qty candle
  | `Short ->
    (* Closing short = buying *)
    fill_market ~config ~side:`Buy ~qty candle

(** Check if we have sufficient balance for a buy *)
let can_afford ~balance ~price ~qty ~config =
  let cost = price *. qty in
  let commission = calculate_commission ~config ~price ~qty in
  Float.(balance >= cost +. commission)

(** Calculate maximum quantity we can buy with given balance *)
let max_buy_qty ~balance ~price ~config =
  (* qty * price * (1 + commission_pct) <= balance *)
  (* qty <= balance / (price * (1 + commission_pct)) *)
  let effective_price = price *. (1. +. config.Config.commission_pct) in
  match Float.(effective_price > 0.) with
  | true -> balance /. effective_price
  | false -> 0.

(** Venue-agnostic market-making quote generation and reconciliation.

    This module is deliberately pure: it computes desired post-only quotes and
    the order actions needed to reach them, but never talks to an exchange. *)

open Core

type error =
  | Invalid_config of string
  | Invalid_book of string
  | Invalid_input of string
[@@deriving sexp, equal]

module Top_of_book : sig
  type t =
    { bid_price : float
    ; bid_qty : float
    ; ask_price : float
    ; ask_qty : float
    }
  [@@deriving sexp, equal]

  val create
    :  bid_price:float
    -> bid_qty:float
    -> ask_price:float
    -> ask_qty:float
    -> t

  val validate : t -> (unit, error) Result.t
  val midpoint : t -> float

  (** Size-weighted top-of-book fair value. A larger bid queue moves the
      microprice toward the ask, and a larger ask queue moves it toward the
      bid. Falls back to the midpoint when both displayed quantities are zero. *)
  val microprice : t -> float
end

type fair_value =
  [ `Midpoint
  | `Microprice
  ]
[@@deriving sexp, equal]

module Config : sig
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

  val default : t
  val validate : t -> (unit, error) Result.t
end

module Quote : sig
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

(** Generate post-only quote levels.

    [inventory] is signed base-asset inventory (positive is long).
    [volatility_bps] is an optional non-negative spread add-on input. Total
    live quantity on either side is capped so that a complete fill cannot move
    inventory beyond [max_position]. [inventory_skew_bps] must be below 10,000
    so the reservation price remains positive at the position limit. *)
val generate
  :  config:Config.t
  -> book:Top_of_book.t
  -> inventory:float
  -> ?volatility_bps:float
  -> unit
  -> (quote_set, error) Result.t

val quotes : quote_set -> Quote.t list

module Working_order : sig
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

(** Compare working orders with desired quotes by [(side, level)]. Prices are
    retained within [price_tolerance_ticks], and quantities within
    [qty_tolerance_ratio]. Duplicate working orders for a level are canceled. *)
val reconcile
  :  tick_size:float
  -> ?price_tolerance_ticks:int
  -> ?qty_tolerance_ratio:float
  -> existing:Working_order.t list
  -> desired:Quote.t list
  -> unit
  -> reconciliation

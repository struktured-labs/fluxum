(** Ledger Interface - Extracted from Gemini's proven P&L tracking implementation *)

open Async

(** Source of ledger update *)
module Update_source = struct
  type t =
    [ `Market_data
    | `Trade
    | `External_trade
    ]
  [@@deriving sexp, equal, compare, enumerate]
end

(** Single ledger entry tracking P&L and position for one symbol *)
module type ENTRY = sig
  type t =
    { symbol : Types.Symbol.t;
      (* P&L fields *)
      pnl : float; [@default 0.0]
      position : float; [@default 0.0]
      spot : float; [@default 0.0]
      pnl_spot : float; [@default 0.0]
      notional : float; [@default 0.0]
      (* Execution tracking *)
      avg_buy_price : float; [@default 0.0]
      avg_sell_price : float; [@default 0.0]
      avg_price : float; [@default 0.0]
      total_buy_qty : float; [@default 0.0]
      total_sell_qty : float; [@default 0.0]
      buy_notional : float; [@default 0.0]
      sell_notional : float; [@default 0.0]
      (* Order tracking *)
      total_original : float; [@default 0.0]
      total_executed : float; [@default 0.0]
      total_remaining : float; [@default 0.0]
      (* Cost basis accounting *)
      cost_basis : float; [@default 0.0]
      running_price : float; [@default 0.0]
      running_qty : float; [@default 0.0]
      (* Metadata *)
      update_time : Time_float_unix.t;
      update_source : Update_source.t; [@default `Market_data]
      (* Latest quote *)
      price : Types.Price.Option.t; [@default None]
      side : Types.Side.Option.t; [@default None]
      qty : float option; [@default None]
      package_price : Types.Price.Option.t; [@default None]
    }
  [@@deriving sexp, compare, equal, fields]

  (** Create new entry *)
  val create :
    ?update_time:Time_float_unix.t ->
    symbol:Types.Symbol.t ->
    ?pnl:float ->
    ?position:float ->
    ?spot:float ->
    ?pnl_spot:float ->
    ?notional:float ->
    ?avg_buy_price:float ->
    ?avg_sell_price:float ->
    ?avg_price:float ->
    ?update_source:Update_source.t ->
    ?total_buy_qty:float ->
    ?total_sell_qty:float ->
    ?price:Types.Price.Option.t ->
    ?side:Types.Side.Option.t ->
    ?qty:float option ->
    ?package_price:Types.Price.Option.t ->
    ?buy_notional:float ->
    ?sell_notional:float ->
    ?total_original:float ->
    ?total_executed:float ->
    ?total_remaining:float ->
    ?cost_basis:float ->
    ?running_price:float ->
    ?running_qty:float ->
    unit ->
    t

  (** Update from trade execution *)
  val on_trade :
    ?update_source:Update_source.t ->
    ?timestamp:Time_float_unix.t ->
    ?avg_trade_price:float ->
    ?fee_usd:float ->
    t ->
    price:float ->
    side:Types.Side.t ->
    qty:float ->
    t

  (** Update spot price from market data *)
  val update_spot : ?timestamp:Time_float_unix.t -> t -> float -> t

  (** Update from order book (for market price estimation) *)
  val update_from_book : t -> 'book -> t

  (** Real-time entry pipe combining order book + trade events *)
  val pipe :
    init:t ->
    ?num_values:int ->
    ?behavior:[ `Alternate | `Priority | `Random ] ->
    'book Pipe.Reader.t ->
    'trade_events Pipe.Reader.t ->
    t Pipe.Reader.t Deferred.t
end

(** Multi-symbol ledger (map of symbol → entry) *)
module type S = sig
  module Entry : ENTRY

  type t [@@deriving sexp, compare, equal]

  (** Bootstrap from account balances *)
  val from_balances :
    ?notional_currency:Types.Currency.t ->
    'balance list ->
    t

  (** Bootstrap from historical trades *)
  val from_trades :
    ?init:t ->
    ?avg_trade_prices:float Types.Symbol.Map.t ->
    'trade list ->
    t * Entry.t Pipe.Reader.t Types.Symbol.Map.t

  (** Update from order books (unrealized P&L) *)
  val update_from_books : t -> books:'books -> t
  val update_from_book : t -> book:'book -> t

  (** Update spot prices *)
  val update_spots :
    ?timestamp:Time_float_unix.t ->
    t ->
    float Types.Symbol.Map.t ->
    t

  (** Trade updates *)
  val on_trade :
    ?update_source:Update_source.t ->
    ?timestamp:Time_float_unix.t ->
    ?avg_trade_price:float ->
    ?fee_usd:float ->
    t ->
    symbol:Types.Symbol.t ->
    price:Types.Price.t ->
    side:Types.Side.t ->
    qty:float ->
    t

  (** Order event updates *)
  val on_order_events : t -> 'order_event list -> t
  val on_order_event_response : t -> 'order_event_response -> t

  (** Multi-symbol real-time pipes *)
  val pipe :
    ?num_values:int ->
    ?behavior:[ `Alternate | `Priority | `Random ] ->
    ?how:Async.Monad_sequence.how ->
    init:Entry.t Types.Symbol.Map.t ->
    'book Pipe.Reader.t Types.Symbol.Map.t ->
    'order_events Pipe.Reader.t ->
    Entry.t Pipe.Reader.t Types.Symbol.Map.t Deferred.t

  (** CLI command *)
  val command : string * Command.t
end

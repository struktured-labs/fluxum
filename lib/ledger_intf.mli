(** Ledger Interface - Real-Time P&L Tracking

    Extracted from Gemini's production P&L tracking implementation.
    Provides comprehensive position and profit/loss accounting.

    {b Key Features:}
    - Real-time P&L calculations (realized and unrealized)
    - Cost basis tracking with FIFO accounting
    - Multi-symbol portfolio management
    - Integration with order books and trade streams
    - 28-field comprehensive accounting

    {b Accounting Method:} FIFO (First In, First Out)
    - Tracks running cost basis as position changes
    - Calculates realized P&L on position reductions
    - Updates unrealized P&L from market prices

    {b Design:} Based on Gemini's proven ledger implementation.
    Used in production for real-time portfolio tracking.
*)

open Async

(** {1 Update Sources} *)

module Update_source : sig
  (** Source that triggered a ledger update

      Used for tracking why and how ledger entries change.
      Helps distinguish mark-to-market vs actual execution updates.
  *)
  type t =
    [ `Market_data      (** Price update from order book/ticker *)
    | `Trade            (** Actual trade execution *)
    | `External_trade   (** Trade from external source (manual import) *)
    ]
  [@@deriving sexp, equal, compare, enumerate]
end

(** {1 Single Symbol Ledger Entry} *)

module type ENTRY = sig
  (** Ledger entry for a single trading pair

      Tracks complete P&L state including:
      - Current position (long/short/flat)
      - Realized P&L (from closed trades)
      - Unrealized P&L (mark-to-market)
      - Cost basis and execution history
      - Order tracking metrics

      All 28 fields default to 0.0 except timestamps and optionals.
  *)

  type t =
    { symbol : Types.Symbol.t

      (** {2 P&L Fields} *)

      ; pnl : float
      (** Realized P&L: Profit/loss from closed positions (default: 0.0) *)

      ; position : float
      (** Current position: positive = long, negative = short, 0 = flat (default: 0.0) *)

      ; spot : float
      (** Current market price (from order book/ticker) (default: 0.0) *)

      ; pnl_spot : float
      (** Unrealized P&L: (position × spot) - cost_basis (default: 0.0) *)

      ; notional : float
      (** Position notional value: abs(position × spot) (default: 0.0) *)

      (** {2 Execution Tracking} *)

      ; avg_buy_price : float
      (** Average price of all buy executions (default: 0.0) *)

      ; avg_sell_price : float
      (** Average price of all sell executions (default: 0.0) *)

      ; avg_price : float
      (** Volume-weighted average execution price (default: 0.0) *)

      ; total_buy_qty : float
      (** Cumulative quantity bought (default: 0.0) *)

      ; total_sell_qty : float
      (** Cumulative quantity sold (default: 0.0) *)

      ; buy_notional : float
      (** Total notional value of buys: sum(qty × price) (default: 0.0) *)

      ; sell_notional : float
      (** Total notional value of sells: sum(qty × price) (default: 0.0) *)

      (** {2 Order Tracking} *)

      ; total_original : float
      (** Total order quantity submitted (default: 0.0) *)

      ; total_executed : float
      (** Total quantity executed (should equal buy_qty + sell_qty) (default: 0.0) *)

      ; total_remaining : float
      (** Total quantity still in open orders (default: 0.0) *)

      (** {2 Cost Basis Accounting (FIFO)} *)

      ; cost_basis : float
      (** Cost basis of current position using FIFO (default: 0.0)
          - Long position: total cost paid for position
          - Short position: total proceeds from shorting
      *)

      ; running_price : float
      (** Running average price of current position (default: 0.0) *)

      ; running_qty : float
      (** Running quantity tracker (should match position) (default: 0.0) *)

      (** {2 Metadata} *)

      ; update_time : Time_float_unix.t
      (** Last update timestamp *)

      ; update_source : Update_source.t
      (** What triggered this update (default: `Market_data) *)

      (** {2 Latest Quote} *)

      ; price : Types.Price.Option.t
      (** Latest price from update (default: None) *)

      ; side : Types.Side.Option.t
      (** Latest side (if from trade) (default: None) *)

      ; qty : float option
      (** Latest quantity (if from trade) (default: None) *)

      ; package_price : Types.Price.Option.t
      (** Package/bundle price if applicable (default: None) *)
    }
  [@@deriving sexp, compare, equal, fields]

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
  (** Create new ledger entry

      All fields default to 0.0 except timestamp (now) and source (Market_data).
      Typically start with just symbol and optionally position/spot.
  *)

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
  (** Update entry from trade execution

      Updates:
      - Position: +qty (buy) or -qty (sell)
      - Realized P&L: if position reduces (closing trades)
      - Cost basis: FIFO accounting
      - Execution statistics: avg prices, totals

      @param update_source Defaults to `Trade
      @param timestamp Defaults to now
      @param avg_trade_price Override average trade price
      @param fee_usd Trading fee in USD (subtracted from P&L)
      @param price Execution price
      @param side Buy or Sell
      @param qty Quantity executed (always positive)
  *)

  val update_spot : ?timestamp:Time_float_unix.t -> t -> float -> t
  (** Update spot price from market data

      Recalculates:
      - pnl_spot: Unrealized P&L at new price
      - notional: Position value at new price

      Does not affect cost basis or realized P&L.

      @param timestamp Defaults to now
      @return Updated entry with new spot price and unrealized P&L
  *)

  val update_from_book : t -> 'book -> t
  (** Update from order book (polymorphic over book type)

      Extracts mid-price or best bid/ask from book and calls update_spot.
      Book type is polymorphic - works with any exchange's book type.
  *)

  val pipe :
    init:t ->
    ?num_values:int ->
    ?behavior:[ `Alternate | `Priority | `Random ] ->
    'book Pipe.Reader.t ->
    'trade_events Pipe.Reader.t ->
    t Pipe.Reader.t Deferred.t
  (** Create real-time ledger entry pipe

      Combines order book (for mark-to-market) and trade events (for execution).
      Emits updated entry on every event.

      @param init Initial ledger state
      @param num_values Buffer size for merging pipes
      @param behavior How to merge book and trade streams
      @param 'book Book update pipe (polymorphic type)
      @param 'trade_events Trade event pipe (polymorphic type)
      @return Pipe emitting updated ledger entries
  *)
end

(** {1 Multi-Symbol Ledger} *)

module type S = sig
  (** Portfolio-level ledger managing multiple symbols

      Aggregates individual symbol ledgers into a portfolio view.
      Provides bulk operations and multi-symbol pipes.
  *)

  module Entry : ENTRY

  type t [@@deriving sexp, compare, equal]
  (** Map from symbol to ledger entry *)

  val from_balances :
    ?notional_currency:Types.Currency.t ->
    'balance list ->
    t
  (** Bootstrap ledger from account balances

      Creates flat positions (position = balance, cost_basis = 0).
      Use for initializing from current holdings.

      @param notional_currency Currency for notional calculations
      @param 'balance Balance list (polymorphic type)
  *)

  val from_trades :
    ?init:t ->
    ?avg_trade_prices:float Types.Symbol.Map.t ->
    'trade list ->
    t * Entry.t Pipe.Reader.t Types.Symbol.Map.t
  (** Bootstrap ledger from historical trades

      Replays trades to reconstruct cost basis and P&L.
      Returns final ledger state + per-symbol entry pipes.

      @param init Initial ledger (defaults to empty)
      @param avg_trade_prices Override average prices per symbol
      @param 'trade Trade list (polymorphic type)
      @return (final_ledger, per_symbol_pipes)
  *)

  val update_from_books : t -> books:'books -> t
  (** Update all entries from multi-symbol order books

      Calls update_spot on each symbol with book's mid-price.
  *)

  val update_from_book : t -> book:'book -> t
  (** Update single symbol from order book *)

  val update_spots :
    ?timestamp:Time_float_unix.t ->
    t ->
    float Types.Symbol.Map.t ->
    t
  (** Bulk update spot prices from map

      More efficient than updating one-by-one.
  *)

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
  (** Update ledger from trade execution

      Creates entry if symbol doesn't exist yet.
      See Entry.on_trade for update semantics.
  *)

  val on_order_events : t -> 'order_event list -> t
  (** Update from list of order events

      Extracts fills/executions and updates ledger accordingly.
  *)

  val on_order_event_response : t -> 'order_event_response -> t
  (** Update from single order event response *)

  val pipe :
    ?num_values:int ->
    ?behavior:[ `Alternate | `Priority | `Random ] ->
    ?how:Async.Monad_sequence.how ->
    init:Entry.t Types.Symbol.Map.t ->
    'book Pipe.Reader.t Types.Symbol.Map.t ->
    'order_events Pipe.Reader.t ->
    Entry.t Pipe.Reader.t Types.Symbol.Map.t Deferred.t
  (** Create multi-symbol real-time ledger pipes

      @param init Initial entries per symbol
      @param 'book Order book pipes per symbol
      @param 'order_events Order event pipe (all symbols)
      @return Map from symbol to entry pipe
  *)

  val command : string * Command.t
  (** CLI command for ledger operations

      Provides command-line interface for ledger queries and display.
  *)
end

open Async

open Types

module type S = sig
  type t

  module Venue : sig
    val t : Venue.t
  end

  module Native : sig
    module Order : sig
      type id
      type request
      type response
      type status
    end

    module Trade : sig
      type t
    end

    module Balance : sig
      type t
    end

    module Book : sig
      type update
      type snapshot  (** Order book depth snapshot *)
    end

    module Symbol_info : sig
      type t
    end

    module Ticker : sig
      type t  (** 24hr ticker statistics *)
    end

    module Public_trade : sig
      type t  (** Public market trade *)
    end

    module Error : sig
      type t
    end
  end

  val place_order
    :  t
    -> Native.Order.request
    -> (Native.Order.response, Native.Error.t) Deferred.Result.t

  val cancel_order
    :  t
    -> Native.Order.id
    -> (unit, Native.Error.t) Deferred.Result.t

  val balances
    :  t
    -> (Native.Balance.t list, Native.Error.t) Deferred.Result.t

  (** Get status of a specific order by ID *)
  val get_order_status
    :  t
    -> Native.Order.id
    -> (Native.Order.status, Native.Error.t) Deferred.Result.t

  (** Get all open orders, optionally filtered by symbol *)
  val get_open_orders
    :  t
    -> ?symbol:Symbol.t
    -> unit
    -> (Native.Order.status list, Native.Error.t) Deferred.Result.t

  (** Get closed/historical orders *)
  val get_order_history
    :  t
    -> ?symbol:Symbol.t
    -> ?limit:int
    -> unit
    -> (Native.Order.status list, Native.Error.t) Deferred.Result.t

  (** Get user's trade history for a symbol *)
  val get_my_trades
    :  t
    -> symbol:Symbol.t
    -> ?limit:int
    -> unit
    -> (Native.Trade.t list, Native.Error.t) Deferred.Result.t

  (** Get available trading symbols/pairs *)
  val get_symbols
    :  t
    -> unit
    -> (Native.Symbol_info.t list, Native.Error.t) Deferred.Result.t

  (** Get 24hr ticker statistics for a symbol *)
  val get_ticker
    :  t
    -> symbol:Symbol.t
    -> unit
    -> (Native.Ticker.t, Native.Error.t) Deferred.Result.t

  (** Get order book depth snapshot *)
  val get_order_book
    :  t
    -> symbol:Symbol.t
    -> ?limit:int
    -> unit
    -> (Native.Book.snapshot, Native.Error.t) Deferred.Result.t

  (** Get recent public trades for a symbol *)
  val get_recent_trades
    :  t
    -> symbol:Symbol.t
    -> ?limit:int
    -> unit
    -> (Native.Public_trade.t list, Native.Error.t) Deferred.Result.t

  (** Cancel all open orders, optionally filtered by symbol. Returns count of canceled orders. *)
  val cancel_all_orders
    :  t
    -> ?symbol:Symbol.t
    -> unit
    -> (int, Native.Error.t) Deferred.Result.t

  module Streams : sig
    val trades : t -> Native.Trade.t Pipe.Reader.t Deferred.t
    val book_updates : t -> Native.Book.update Pipe.Reader.t Deferred.t
  end

  module Normalize : sig
    val order_response   : Native.Order.response -> (Order.t, string) Result.t
    val order_status     : Native.Order.status   -> Order_status.t
    val order_from_status : Native.Order.status  -> (Order.t, string) Result.t  (** Full order from status query *)
    val trade            : Native.Trade.t        -> (Trade.t, string) Result.t
    val balance          : Native.Balance.t      -> (Balance.t, string) Result.t
    val book_update      : Native.Book.update    -> Book_update.t
    val symbol_info      : Native.Symbol_info.t  -> Symbol_info.t
    val ticker           : Native.Ticker.t       -> (Ticker.t, string) Result.t
    val order_book       : Native.Book.snapshot  -> (Order_book.t, string) Result.t
    val public_trade     : Native.Public_trade.t -> (Public_trade.t, string) Result.t
    val error            : Native.Error.t        -> Error.t
  end
end

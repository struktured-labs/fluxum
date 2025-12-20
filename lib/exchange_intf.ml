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

  module Streams : sig
    val trades : t -> Native.Trade.t Pipe.Reader.t Deferred.t
    val book_updates : t -> Native.Book.update Pipe.Reader.t Deferred.t
  end

  module Normalize : sig
    val order_response : Native.Order.response -> Order.t
    val order_status   : Native.Order.status   -> Order_status.t
    val trade          : Native.Trade.t        -> Trade.t
    val balance        : Native.Balance.t      -> Balance.t
    val book_update    : Native.Book.update    -> Book_update.t
    val error          : Native.Error.t        -> Error.t
  end
end

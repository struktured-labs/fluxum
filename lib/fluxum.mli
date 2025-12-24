open Async
[@@@warning "-67"]

module Types = Types
module Exchange_intf = Exchange_intf
module Json = Json
module Cli_args = Cli_args

module type BUILDER = sig
  module E : Exchange_intf.S

  val make_order_request
    :  symbol:Types.Symbol.t
    -> side:Types.Side.t
    -> kind:Types.Order_kind.t
    -> qty:Types.Qty.t
    -> E.Native.Order.request
end

module Make (E : Exchange_intf.S) (Builder : BUILDER with module E := E) : sig
  type t = E.t

  module Venue : sig
    val t : Types.Venue.t
  end

  module Native : sig
    module Order : sig
      type id = E.Native.Order.id
      type request = E.Native.Order.request
      type response = E.Native.Order.response
      type status = E.Native.Order.status
    end

    module Trade : sig
      type t = E.Native.Trade.t
    end

    module Balance : sig
      type t = E.Native.Balance.t
    end

    module Book : sig
      type update = E.Native.Book.update
    end

    module Error : sig
      type t = E.Native.Error.t
    end
  end

  val place_order
    :  t
    -> Native.Order.request
    -> (Types.Order.t, Types.Error.t) Deferred.Result.t

  val place_order_norm
    :  t
    -> symbol:Types.Symbol.t
    -> side:Types.Side.t
    -> kind:Types.Order_kind.t
    -> qty:Types.Qty.t
    -> (Types.Order.t, Types.Error.t) Deferred.Result.t

  val cancel_order
    :  t
    -> Native.Order.id
    -> (unit, Types.Error.t) Deferred.Result.t

  val balances
    :  t
    -> (Types.Balance.t list, Types.Error.t) Deferred.Result.t

  module Streams : sig
    val trades : t -> Types.Trade.t Pipe.Reader.t Deferred.t
    val book_updates : t -> Types.Book_update.t Pipe.Reader.t Deferred.t
  end
end

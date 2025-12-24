open Core
open Async

module Types = Types
module Exchange_intf = Exchange_intf
module Json = Json

module type BUILDER = sig
  module E : Exchange_intf.S

  val make_order_request
    :  symbol:Types.Symbol.t
    -> side:Types.Side.t
    -> kind:Types.Order_kind.t
    -> qty:Types.Qty.t
    -> E.Native.Order.request
end

module Make (E : Exchange_intf.S) (Builder : BUILDER with module E := E) = struct
  type t = E.t

  module Venue = struct
    let t = E.Venue.t
  end

  module Native = struct
    module Order = struct
      type id = E.Native.Order.id
      type request = E.Native.Order.request
      type response = E.Native.Order.response
      type status = E.Native.Order.status
    end

    module Trade = struct
      type t = E.Native.Trade.t
    end

    module Balance = struct
      type t = E.Native.Balance.t
    end

    module Book = struct
      type update = E.Native.Book.update
    end

    module Error = struct
      type t = E.Native.Error.t
    end
  end

  let map_error (e : E.Native.Error.t) : Types.Error.t = E.Normalize.error e

  let place_order t req =
    E.place_order t req
    >>| Result.map ~f:E.Normalize.order_response
    >>| Result.map_error ~f:map_error

  let place_order_norm t ~symbol ~side ~kind ~qty =
    let req = Builder.make_order_request ~symbol ~side ~kind ~qty in
    place_order t req

  let cancel_order t id =
    E.cancel_order t id >>| Result.map_error ~f:map_error

  let balances t =
    E.balances t
    >>| Result.map ~f:(List.map ~f:E.Normalize.balance)
    >>| Result.map_error ~f:map_error

  module Streams = struct
    let trades t =
      E.Streams.trades t >>| Pipe.map ~f:E.Normalize.trade

    let book_updates t =
      E.Streams.book_updates t >>| Pipe.map ~f:E.Normalize.book_update
  end
end

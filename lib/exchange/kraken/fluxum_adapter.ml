open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module Adapter : Exchange_intf.S = struct
  type t = unit

  module Venue = struct
    let t = Types.Venue.Kraken
  end

  module Native = struct
    module Order = struct
      type id = string
      type request = unit
      type response = unit
      type status = unit
    end

    module Trade = struct
      type t = unit
    end

    module Balance = struct
      type t = unit
    end

    module Book = struct
      type update = unit
    end

    module Error = struct
      type t = [ `Not_implemented of string ]
    end
  end

  let place_order (_ : t) (_ : Native.Order.request) =
    Deferred.Result.fail (`Not_implemented "kraken.place_order")

  let cancel_order (_ : t) (_ : Native.Order.id) =
    Deferred.Result.fail (`Not_implemented "kraken.cancel_order")

  let balances (_ : t) =
    Deferred.Result.fail (`Not_implemented "kraken.balances")

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (_ : Native.Order.response) : Types.Order.t =
      { venue = Venue.t
      ; id = ""
      ; symbol = ""
      ; side = Types.Side.Buy
      ; kind = Types.Order_kind.Market
      ; qty = 0.
      ; filled = 0.
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let order_status (_ : Native.Order.status) : Types.Order_status.t =
      Types.Order_status.New

    let trade (_ : Native.Trade.t) : Types.Trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Side.Buy
      ; price = 0.
      ; qty = 0.
      ; fee = None
      ; trade_id = None
      ; ts = None
      }

    let balance (_ : Native.Balance.t) : Types.Balance.t =
      { venue = Venue.t
      ; currency = ""
      ; total = 0.
      ; available = 0.
      ; locked = 0.
      }

    let book_update (_ : Native.Book.update) : Types.Book_update.t =
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels = []
      ; ts = None
      ; is_snapshot = false
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Not_implemented msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "not-impl"; message = msg }
  end
end

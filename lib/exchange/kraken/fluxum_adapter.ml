open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = V1
module Common = Common
module Ws = Ws

module Adapter : Exchange_intf.S = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  module Venue = struct
    let t = Types.Venue.Kraken
  end

  module Native = struct
    module Order = struct
      type id = string  (* Kraken txid *)
      type request = V1.Add_order.T.request
      type response = V1.Add_order.T.response
      type status = V1.Open_orders.Order.t
    end

    module Trade = struct
      type t = V1.Trades_history.Trade.t
    end

    module Balance = struct
      type t = string * string  (* currency, amount *)
    end

    module Book = struct
      type update = Ws.Book_update.t
    end

    module Symbol_info = struct
      type t = string * V1.Asset_pairs.Pair_info.t  (* pair name, info *)
    end

    module Error = struct
      type t = Rest.Error.post
    end
  end

  let place_order (t : t) (req : Native.Order.request) =
    let (module Cfg) = t.cfg in
    V1.Add_order.post (module Cfg) req
    >>| function
    | `Ok r -> Ok r
    | (#Rest.Error.post as e) -> Error e

  let cancel_order (t : t) (txid : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Cancel_order.post (module Cfg) { txid }
    >>| function
    | `Ok _ -> Ok ()
    | (#Rest.Error.post as e) -> Error e

  let balances (t : t) =
    let (module Cfg) = t.cfg in
    V1.Balances.post (module Cfg) ()
    >>| function
    | `Ok bal_list -> Ok bal_list
    | (#Rest.Error.post as e) -> Error e

  let get_order_status (t : t) (txid : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Query_orders.post (module Cfg) { txids = [txid]; trades = false }
    >>| function
    | `Ok orders ->
      (match List.hd orders with
       | Some (_, order) -> Ok order
       | None -> Error (`Bad_request "Order not found"))
    | (#Rest.Error.post as e) -> Error e

  let get_open_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Open_orders.post (module Cfg) { trades = false }
    >>| function
    | `Ok { open_; _ } -> Ok (List.map open_ ~f:snd)
    | (#Rest.Error.post as e) -> Error e

  let get_order_history (t : t) ?symbol:_ ?limit:_ () =
    let (module Cfg) = t.cfg in
    V1.Closed_orders.post (module Cfg)
      { trades = false; userref = None; start = None; end_ = None; ofs = None; closetime = None }
    >>| function
    | `Ok { closed; _ } -> Ok (List.map closed ~f:snd)
    | (#Rest.Error.post as e) -> Error e

  let get_my_trades (t : t) ~symbol:_ ?limit:_ () =
    let (module Cfg) = t.cfg in
    V1.Trades_history.post (module Cfg)
      { type_ = None; trades = false; start = None; end_ = None; ofs = None }
    >>| function
    | `Ok { trades; _ } -> Ok (List.map trades ~f:snd)
    | (#Rest.Error.post as e) -> Error e

  let get_symbols (t : t) () =
    let (module Cfg) = t.cfg in
    V1.Asset_pairs.post (module Cfg) { pair = None; info = None }
    >>| function
    | `Ok pairs -> Ok pairs
    | (#Rest.Error.post as e) -> Error e

  module Streams = struct
    let trades (_ : t) =
      (* TODO: Implement WebSocket trade stream *)
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (t : t) =
      let symbol = List.hd t.symbols |> Option.value ~default:"XBT/USD" in
      Ws.subscribe ~symbols:[symbol] ()
      >>| Pipe.filter_map ~f:(function
        | `Ok update -> Some update
        | _ -> None)
  end

  module Normalize = struct
    let side_of_string s =
      match s with
      | "buy" | "b" -> Types.Side.Buy
      | "sell" | "s" -> Types.Side.Sell
      | _ -> Types.Side.Buy

    let order_type_of_string s =
      match s with
      | "market" -> Types.Order_kind.Market
      | "limit" -> Types.Order_kind.Limit 0.0  (* price not available in status *)
      | _ -> Types.Order_kind.Market

    let status_of_string s =
      match s with
      | "pending" | "open" -> Types.Order_status.New
      | "closed" -> Types.Order_status.Filled
      | "canceled" | "cancelled" -> Types.Order_status.Canceled
      | "expired" -> Types.Order_status.Canceled
      | _ -> Types.Order_status.New

    let order_response (r : Native.Order.response) : Types.Order.t =
      let txid = List.hd r.txid |> Option.value ~default:"" in
      { venue = Venue.t
      ; id = txid
      ; symbol = ""  (* Not available in response *)
      ; side = Types.Side.Buy  (* Not available in response *)
      ; kind = Types.Order_kind.Market
      ; qty = 0.
      ; filled = 0.
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let order_status (o : Native.Order.status) : Types.Order_status.t =
      status_of_string o.status

    let order_from_status (o : Native.Order.status) : Types.Order.t =
      let side = side_of_string o.descr.type_ in
      let kind = order_type_of_string o.descr.ordertype in
      let qty = Float.of_string o.vol in
      let filled = Float.of_string o.vol_exec in
      let status = status_of_string o.status in
      let created_at =
        Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec o.opentm))
      in
      { venue = Venue.t
      ; id = ""  (* txid is the key, not in the order struct *)
      ; symbol = o.descr.pair
      ; side
      ; kind
      ; qty
      ; filled
      ; status
      ; created_at
      ; updated_at = None
      }

    let trade (tr : Native.Trade.t) : Types.Trade.t =
      let side = side_of_string tr.type_ in
      let price = Float.of_string tr.price in
      let qty = Float.of_string tr.vol in
      let fee = Float.of_string tr.fee in
      let ts = Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec tr.time)) in
      { venue = Venue.t
      ; symbol = tr.pair
      ; side
      ; price
      ; qty
      ; fee = Some fee
      ; trade_id = Some tr.ordertxid
      ; ts
      }

    let balance ((currency, amount) : Native.Balance.t) : Types.Balance.t =
      let total = Float.of_string amount in
      { venue = Venue.t
      ; currency
      ; total
      ; available = total  (* Kraken doesn't separate available in Balances endpoint *)
      ; locked = 0.
      }

    let book_update (u : Native.Book.update) : Types.Book_update.t =
      let side = match u.side with
        | `Bid -> Types.Book_update.Side.Bid
        | `Ask -> Types.Book_update.Side.Ask
      in
      let levels = List.map u.levels ~f:(fun (price, qty) ->
        { Types.Book_update.price; qty })
      in
      { venue = Venue.t
      ; symbol = u.symbol
      ; side
      ; levels
      ; ts = Some u.timestamp
      ; is_snapshot = u.is_snapshot
      }

    let symbol_info ((name, info) : Native.Symbol_info.t) : Types.Symbol_info.t =
      let min_order_size = match info.ordermin with
        | Some s -> Float.of_string s
        | None -> 0.0
      in
      { venue = Venue.t
      ; symbol = name
      ; base_currency = info.base
      ; quote_currency = info.quote
      ; status = info.status
      ; min_order_size
      ; tick_size = None  (* Kraken uses pair_decimals instead *)
      ; quote_increment = None
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Bad_request msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "400"; message = msg }
      | `Not_found -> Types.Error.Exchange_specific { venue = Venue.t; code = "404"; message = "not_found" }
      | `Service_unavailable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "503"; message = msg }
      | `Unauthorized _ -> Types.Error.Auth_failed
      | `Too_many_requests _ -> Types.Error.Rate_limited
      | `Api_error { errors } ->
        let msg = String.concat ~sep:"; " errors in
        Types.Error.Exchange_specific { venue = Venue.t; code = "api"; message = msg }
      | `Json_parse_error { message; body = _ } ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message }
  end
end

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol ~side ~kind ~qty =
    let type_ = match side with
      | Types.Side.Buy -> Common.Side.Buy
      | Types.Side.Sell -> Common.Side.Sell
    in
    let ordertype, price = match kind with
      | Types.Order_kind.Market -> Common.Order_type.Market, None
      | Types.Order_kind.Limit p -> Common.Order_type.Limit, Some (Float.to_string p)
      | Types.Order_kind.Post_only_limit p -> Common.Order_type.Limit, Some (Float.to_string p)
    in
    let oflags = match kind with
      | Types.Order_kind.Post_only_limit _ -> Some "post"
      | _ -> None
    in
    V1.Add_order.T.
      { pair = symbol
      ; type_
      ; ordertype
      ; volume = Float.to_string qty
      ; price
      ; price2 = None
      ; leverage = None
      ; oflags
      ; starttm = None
      ; expiretm = None
      ; timeinforce = None
      }
end

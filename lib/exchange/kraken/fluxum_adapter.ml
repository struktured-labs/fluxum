open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = V1
module Common = Common
module Ws = Ws

(* Suppress unused field and value warnings - these may be used by other code *)
[@@@warning "-32-69"]

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

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
      type update = Ws.Public.Book_update.t
      type snapshot = (string * V1.Depth.depth_data) list  (* pair -> depth data *)
    end

    module Ticker = struct
      type t = string * V1.Ticker.Ticker_data.t  (* pair name, ticker data *)
    end

    module Public_trade = struct
      type t = V1.Recent_trades.trade
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

  let get_ticker (t : t) ~symbol () =
    let (module Cfg) = t.cfg in
    V1.Ticker.get (module Cfg) { pair = symbol }
    >>| function
    | `Ok tickers ->
      (match List.hd tickers with
       | Some ticker -> Ok ticker
       | None -> Error (`Bad_request "Ticker not found"))
    | (#Rest.Error.post as e) -> Error e

  let get_order_book (t : t) ~symbol ?limit () =
    let (module Cfg) = t.cfg in
    V1.Depth.get (module Cfg) { pair = symbol; count = limit }
    >>| function
    | `Ok depth -> Ok depth
    | (#Rest.Error.post as e) -> Error e

  let get_recent_trades (t : t) ~symbol ?limit () =
    let (module Cfg) = t.cfg in
    V1.Recent_trades.get (module Cfg) { pair = symbol; since = None; count = limit }
    >>| function
    | `Ok (trades, _last) ->
      (* Flatten the pair -> trades list to just trades *)
      let all_trades = List.concat_map trades ~f:snd in
      Ok all_trades
    | (#Rest.Error.post as e) -> Error e

  let cancel_all_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Cancel_all.post (module Cfg) ()
    >>| function
    | `Ok { count } -> Ok count
    | (#Rest.Error.post as e) -> Error e

  module Streams = struct
    (** Private trade stream requires authenticated WebSocket connection.
        For user trades, use the Order_book module's Session interface instead. *)
    let trades (_ : t) =
      (* Private trades require authenticated WebSocket - return empty pipe *)
      let r, _w = Pipe.create () in
      Deferred.return r

    (** Subscribe to order book updates via public WebSocket *)
    let book_updates (t : t) =
      let symbols = match t.symbols with
        | [] -> ["XBT/USD"]  (* Default to BTC/USD *)
        | s -> s
      in
      let open Deferred.Let_syntax in
      let%bind md_result = Market_data.connect
        ~subscriptions:[{ channel = "book"; pairs = symbols; interval = None; depth = Some 10 }]
        ()
      in
      match md_result with
      | Error _ ->
        let r, _w = Pipe.create () in
        return r
      | Ok md ->
        let messages = Market_data.messages md in
        let book_reader, book_writer = Pipe.create () in
        don't_wait_for (
          Pipe.iter messages ~f:(fun msg_str ->
            match Ws.parse_message msg_str with
            | Ok (Ws.Public (Ws.Public.Book book_data)) ->
              (* Convert Book_data to Book_update for each side *)
              let now = Time_float_unix.now () in
              let symbol = book_data.pair in
              let bids = List.map book_data.update.bids ~f:(fun l ->
                (Float.of_string l.Ws.Public.Price_level.price,
                 Float.of_string l.volume))
              in
              let asks = List.map book_data.update.asks ~f:(fun l ->
                (Float.of_string l.Ws.Public.Price_level.price,
                 Float.of_string l.volume))
              in
              let%bind () =
                match bids with
                | [] -> Deferred.unit
                | _ ->
                  let update : Ws.Public.Book_update.t =
                    { side = `Bid; levels = bids; symbol; timestamp = now; is_snapshot = false }
                  in
                  Pipe.write book_writer update
              in
              (match asks with
               | [] -> Deferred.unit
               | _ ->
                 let update : Ws.Public.Book_update.t =
                   { side = `Ask; levels = asks; symbol; timestamp = now; is_snapshot = false }
                 in
                 Pipe.write book_writer update)
            | Ok _ -> Deferred.unit  (* Other messages *)
            | Error _ -> Deferred.unit
          )
          >>| fun () -> Pipe.close book_writer
        );
        return book_reader
  end

  module Normalize = struct
    let side_of_string s =
      match s with
      | "buy" | "b" -> Types.Side.Buy
      | "sell" | "s" -> Types.Side.Sell
      | _ -> Types.Side.Buy

    let side_of_common (s : Common.Side.t) : Types.Side.t =
      match s with
      | `Buy -> Types.Side.Buy
      | `Sell -> Types.Side.Sell

    let order_type_of_string s =
      match s with
      | "market" -> Types.Order_kind.Market
      | "limit" -> Types.Order_kind.Limit 0.0  (* price not available in status *)
      | _ -> Types.Order_kind.Market

    let order_type_of_common (o : Common.Order_type.t) : Types.Order_kind.t =
      match o with
      | `Market -> Types.Order_kind.Market
      | `Limit -> Types.Order_kind.Limit 0.0  (* price not available in status *)
      | _ -> Types.Order_kind.Market

    let status_of_string s =
      match s with
      | "pending" | "open" -> Types.Order_status.New
      | "closed" -> Types.Order_status.Filled
      | "canceled" | "cancelled" -> Types.Order_status.Canceled
      | "expired" -> Types.Order_status.Canceled
      | _ -> Types.Order_status.New

    let order_response (r : Native.Order.response) : (Types.Order.t, string) Result.t =
      try
        let txid = List.hd r.txid |> Option.value ~default:"" in
        Ok { venue = Venue.t
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
      with
      | exn ->
        Error (sprintf "Order response conversion error: %s" (Exn.to_string exn))

    let order_status (o : Native.Order.status) : Types.Order_status.t =
      status_of_string o.status

    let order_from_status (o : Native.Order.status) : (Types.Order.t, string) Result.t =
      try
        let side = side_of_common o.descr.type_ in
        let kind = order_type_of_common o.descr.ordertype in
        let qty = Float.of_string o.vol in
        let filled = Float.of_string o.vol_exec in
        let status = status_of_string o.status in
        let created_at =
          Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec o.opentm))
        in
        Ok { venue = Venue.t
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
      with
      | Failure msg ->
        Error (sprintf "Order conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Order unexpected error: %s" (Exn.to_string exn))

    let trade (tr : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      try
        let side = side_of_string tr.type_ in
        let price = Float.of_string tr.price in
        let qty = Float.of_string tr.vol in
        let fee = Float.of_string tr.fee in
        let ts = Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec tr.time)) in
        Ok { venue = Venue.t
        ; symbol = tr.pair
        ; side
        ; price
        ; qty
        ; fee = Some fee
        ; trade_id = Some tr.ordertxid
        ; ts
        }
      with
      | Failure msg ->
        Error (sprintf "Trade conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Trade unexpected error: %s" (Exn.to_string exn))

    let balance ((currency, amount) : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      try
        let total = Float.of_string amount in
        Ok { venue = Venue.t
        ; currency
        ; total
        ; available = total  (* Kraken doesn't separate available in Balances endpoint *)
        ; locked = 0.
        }
      with
      | Failure msg ->
        Error (sprintf "Balance conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Balance unexpected error: %s" (Exn.to_string exn))

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

    let ticker ((pair, data) : Native.Ticker.t) : Types.Ticker.t =
      let get_first lst = List.hd lst |> Option.value ~default:"0" |> Float.of_string in
      let get_second lst = List.nth lst 1 |> Option.value ~default:"0" |> Float.of_string in
      { venue = Venue.t
      ; symbol = pair
      ; last_price = get_first data.c  (* c[0] = last trade price *)
      ; bid_price = get_first data.b   (* b[0] = best bid price *)
      ; ask_price = get_first data.a   (* a[0] = best ask price *)
      ; high_24h = get_second data.h   (* h[1] = 24hr high *)
      ; low_24h = get_second data.l    (* l[1] = 24hr low *)
      ; volume_24h = get_second data.v (* v[1] = 24hr volume *)
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      }

    let order_book (depth_list : Native.Book.snapshot) : Types.Order_book.t =
      (* Take first pair's depth data *)
      match List.hd depth_list with
      | Some (pair, depth) ->
        let bids = List.map depth.bids ~f:(fun (price, vol, _ts) ->
          { Types.Order_book.Price_level.price = Float.of_string price
          ; volume = Float.of_string vol
          })
        in
        let asks = List.map depth.asks ~f:(fun (price, vol, _ts) ->
          { Types.Order_book.Price_level.price = Float.of_string price
          ; volume = Float.of_string vol
          })
        in
        { venue = Venue.t
        ; symbol = pair
        ; bids
        ; asks
        ; ts = None
        ; epoch = 0
        }
      | None ->
        { venue = Venue.t
        ; symbol = ""
        ; bids = []
        ; asks = []
        ; ts = None
        ; epoch = 0
        }

    let public_trade (tr : Native.Public_trade.t) : Types.Public_trade.t =
      let side = match tr.side with
        | "b" -> Some Types.Side.Buy
        | "s" -> Some Types.Side.Sell
        | _ -> None
      in
      { venue = Venue.t
      ; symbol = ""  (* Not included in trade data *)
      ; price = Float.of_string tr.price
      ; qty = Float.of_string tr.volume
      ; side
      ; trade_id = Some (Int.to_string tr.trade_id)
      ; ts = Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec tr.time))
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Bad_request msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "400"; message = msg }
      | `Not_found -> Types.Error.Exchange_specific { venue = Venue.t; code = "404"; message = "not_found" }
      | `Not_acceptable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "406"; message = msg }
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
    let type_ : Common.Side.t = match side with
      | Types.Side.Buy -> `Buy
      | Types.Side.Sell -> `Sell
    in
    let ordertype, price = match kind with
      | Types.Order_kind.Market -> (`Market : Common.Order_type.t), None
      | Types.Order_kind.Limit p -> (`Limit : Common.Order_type.t), Some (Float.to_string p)
      | Types.Order_kind.Post_only_limit p -> (`Limit : Common.Order_type.t), Some (Float.to_string p)
    in
    let oflags = match kind with
      | Types.Order_kind.Post_only_limit _ -> Some "post"
      | _ -> None
    in
    V1.Add_order.T.
      { pair = symbol
      ; type_
      ; ordertype
      ; volume = qty
      ; price
      ; price2 = None
      ; leverage = None
      ; oflags
      ; starttm = None
      ; expiretm = None
      ; timeinforce = None
      }
end

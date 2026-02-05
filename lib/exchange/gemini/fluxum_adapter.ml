(** Gemini Exchange Adapter

    Reference implementation of Exchange_intf.S for Gemini exchange.

    {b Features:}
    - ✅ REST trading (spot only, no derivatives)
    - ✅ WebSocket market data (trades, order book, L2 updates)
    - ✅ Order book tracking (Order_book_intf.S)
    - ✅ P&L ledger (Ledger_intf.ENTRY)
    - ✅ Session management with auto-reconnect

    {b Authentication:}
    - API key/secret via environment variables (GEMINI_API_KEY, GEMINI_SECRET)
    - Nonce management with file-based tracking to prevent replay attacks
    - Payload signing with HMAC-SHA384

    {b Rate Limits:}
    - Public endpoints: 120 requests/minute
    - Private endpoints: 600 requests/minute
    - WebSocket: No documented limit

    {b Symbol Format:}
    - Lowercase, no separator: ["btcusd"], ["ethusd"]
    - Use normalize functions to convert to/from Fluxum types

    {b Known Limitations:}
    - Spot trading only (no margin, futures, or perpetuals)
    - No batch order placement
    - Limited order types (market, limit, IOC, maker-or-cancel)
    - WebSocket reconnection requires full re-subscription

    @see <https://docs.gemini.com/rest-api/> Gemini REST API Documentation
    @see <https://docs.gemini.com/websocket-api/> Gemini WebSocket API Documentation
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = V1
module Common = Common

module Adapter = struct
  type t =
     { cfg   : (module Cfg.S)
     ; nonce : Nonce.reader
    ; symbols : string list (* used for public market-data subscription *)
    }

  let create ~cfg ~nonce ?(symbols = []) () =
    { cfg; nonce; symbols }

  module Venue = struct
    let t = Types.Venue.Gemini
  end

  module Native = struct
    module Order = struct
      type id = Common.Int_string.t
      type request = V1.Order.New.request
      type response = V1.Order.Status.response
      type status = V1.Order.Status.response
    end

    module Trade = struct
      (* WebSocket trades come from order events, REST trades from mytrades *)
      type t =
        | Ws of V1.Order_events.Order_event.t
        | Rest of V1.Mytrades.trade
    end

    module Balance = struct
      type t = V1.Balances.balance
    end

    module Book = struct
      type update = V1.Market_data.Update.t
      type snapshot = Yojson.Safe.t  (* Raw JSON from /v1/book/{symbol} *)
    end

    module Ticker = struct
      type t = Yojson.Safe.t  (* Raw JSON from /v1/pubticker/{symbol} *)
    end

    module Public_trade = struct
      type t = Yojson.Safe.t  (* Raw JSON from /v1/trades/{symbol} *)
    end

    module Candle = struct
      type t = unit  (* Gemini does not have public REST candle/klines endpoint *)
    end

    module Symbol_info = struct
      type t = V1.Symbol_details.T.response
    end

    module Error = struct
      type t = Rest.Error.post
    end
  end

  let place_order (t : t) (req : Native.Order.request) =
    let (module Cfg) = t.cfg in
    V1.Order.New.post (module Cfg) t.nonce req
    >>| function
    | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e

  let cancel_order (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Order.Cancel.By_order_id.post (module Cfg) t.nonce { order_id = id }
    >>| function
    | `Ok _ -> Ok ()
      | (#Rest.Error.post as e) -> Error e

  let balances (t : t) =
    let (module Cfg) = t.cfg in
    V1.Balances.post (module Cfg) t.nonce ()
    >>| function
    | `Ok r -> Ok r
    | (#Rest.Error.post as e) -> Error e

  let get_order_status (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Order.Status.post (module Cfg) t.nonce { order_id = id }
    >>| function
    | `Ok r -> Ok r
    | (#Rest.Error.post as e) -> Error e

  let get_open_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Orders.post (module Cfg) t.nonce ()
    >>| function
    | `Ok orders -> Ok orders
    | (#Rest.Error.post as e) -> Error e

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    (* Gemini doesn't have a closed orders endpoint *)
    Deferred.return (Error (`Bad_request "Gemini does not support order history endpoint"))

  let get_my_trades (t : t) ~symbol ?limit () =
    let (module Cfg) = t.cfg in
    V1.Mytrades.post (module Cfg) t.nonce
      { symbol = V1.Symbol.of_string symbol
      ; limit_trades = limit
      ; timestamp = None
      }
    >>| function
    | `Ok trades -> Ok (List.map trades ~f:(fun t -> Native.Trade.Rest t))
    | (#Rest.Error.post as e) -> Error e

  let get_symbols (t : t) () =
    let (module Cfg) = t.cfg in
    (* Fetch details for all known symbols *)
    Deferred.List.filter_map V1.Symbol.all ~how:`Parallel ~f:(fun sym ->
      V1.Symbol_details.get (module Cfg) t.nonce ~uri_args:sym ()
      >>| function
      | `Ok r -> Some r
      | #Rest.Error.get -> None)
    >>| fun results -> Ok results

  let get_ticker (_ : t) ~symbol () =
    (* Use Gemini public ticker endpoint *)
    let symbol_lower = String.lowercase symbol in
    let uri = Uri.of_string (sprintf "https://api.gemini.com/v1/pubticker/%s" symbol_lower) in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.get uri >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
    ) >>| function
    | Error _ -> Error (`Network_error "Failed to fetch ticker")
    | Ok body ->
      match Yojson.Safe.from_string body with
      | exception _ -> Error (`Json_parse "Failed to parse ticker response")
      | json -> Ok json

  let get_order_book (_ : t) ~symbol ?limit () =
    (* Use Gemini public order book endpoint *)
    let symbol_lower = String.lowercase symbol in
    let limit_str = match limit with Some n -> sprintf "?limit_bids=%d&limit_asks=%d" n n | None -> "" in
    let uri = Uri.of_string (sprintf "https://api.gemini.com/v1/book/%s%s" symbol_lower limit_str) in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.get uri >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
    ) >>| function
    | Error _ -> Error (`Network_error "Failed to fetch order book")
    | Ok body ->
      match Yojson.Safe.from_string body with
      | exception _ -> Error (`Json_parse "Failed to parse order book response")
      | json -> Ok json

  let get_recent_trades (_ : t) ~symbol ?limit () =
    (* Use Gemini public trades endpoint *)
    let symbol_lower = String.lowercase symbol in
    let limit_str = match limit with Some n -> sprintf "?limit_trades=%d" n | None -> "" in
    let uri = Uri.of_string (sprintf "https://api.gemini.com/v1/trades/%s%s" symbol_lower limit_str) in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.get uri >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
    ) >>| function
    | Error _ -> Error (`Network_error "Failed to fetch recent trades")
    | Ok body ->
      match Yojson.Safe.from_string body with
      | exception _ -> Error (`Json_parse "Failed to parse trades response")
      | json ->
        match json with
        | `List trades -> Ok trades
        | _ -> Error (`Json_parse "Expected array of trades")

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    (* Gemini does not have a public REST candle/OHLCV endpoint *)
    Deferred.return (Error (`Bad_request "Gemini does not support REST candle/klines endpoint"))

  let cancel_all_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Order.Cancel.All.post (module Cfg) t.nonce ()
    >>| function
    | `Ok { details } -> Ok (List.length details.cancelled_orders)
    | (#Rest.Error.post as e) -> Error e

  module Streams = struct
    let trades (t : t) =
      let (module Cfg) = t.cfg in
      let open V1.Order_events in
      let query =
        [ `Event_type_filter `Fill ]
        |> List.map ~f:sexp_of_query
      in
      client (module Cfg) ~nonce:t.nonce ~query ()
      >>| Pipe.filter_map ~f:(function
        | `Ok (`Order_event ev) -> Some (Native.Trade.Ws ev)
        | `Ok (`Order_events evs) -> Option.map (List.hd evs) ~f:(fun ev -> Native.Trade.Ws ev)
        | `Ok (`Heartbeat _) -> None
        | `Ok (`Subscription_ack _) -> None
        | #Ws.Error.t -> None)
      |> Deferred.map ~f:Fn.id

    let book_updates (t : t) =
      let (module Cfg) = t.cfg in
      let symbol_opt = List.hd t.symbols in
      V1.Market_data.client (module Cfg)
        ?uri_args:(Option.map symbol_opt ~f:V1.Symbol.of_string)
        ()
      >>| Pipe.filter_map ~f:(function
        | `Ok (r : V1.Market_data.response) -> (
            match r.message with
            | `Update u -> Some u
            | `Heartbeat _ -> None)
        | #Ws.Error.t -> None )
      |> Deferred.map ~f:Fn.id
  end

  module Normalize = struct
    let time_of_ts_opt (ts : Common.Timestamp.t option) : Time_float_unix.t option =
      Option.map ts ~f:Fn.id

    let order_kind (type_ : Common.Order_type.t) (price : Common.Decimal_string.t)
        (options : Common.Order_execution_option.t list) : (Types.Order_kind.t, string) Result.t =
      match type_ with
      | `Exchange_limit ->
        let open Result.Let_syntax in
        let%bind p = Fluxum.Normalize_common.Float_conv.price_of_string
          (Common.Decimal_string.to_string price) in
        Ok (match List.exists options ~f:(fun o -> Poly.equal o `Maker_or_cancel) with
         | true -> Types.Order_kind.post_only p
         | false -> Types.Order_kind.limit p)
      | _ -> Ok Types.Order_kind.market

    let side (s : Common.Side.t) : Types.Side.t =
      match s with
      | `Buy -> Types.Side.Buy
      | `Sell -> Types.Side.Sell

    let symbol_to_string (s : Common.Symbol.Enum_or_string.t) : string =
      Common.Symbol.Enum_or_string.to_string s

    let order_response (r : Native.Order.response) : (Types.Order.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind qty_orig = Fluxum.Normalize_common.Float_conv.qty_of_string
        (Common.Decimal_string.to_string r.original_amount) in
      let%bind qty_exec = Fluxum.Normalize_common.Float_conv.qty_of_string
        (Common.Decimal_string.to_string r.executed_amount) in
      let%bind qty_rem = Fluxum.Normalize_common.Float_conv.qty_of_string
        (Common.Decimal_string.to_string r.remaining_amount) in
      let%bind kind = order_kind r.type_ r.price r.options in
      let reason_to_status = function
        | Some `Invalid_quantity -> Types.Order_status.Rejected "invalid_quantity"
        | Some `Insufficient_funds -> Types.Order_status.Rejected "insufficient_funds"
        | Some `Self_cross_prevented -> Types.Order_status.Rejected "self_cross_prevented"
        | Some `Immediate_or_cancel_would_post -> Types.Order_status.Rejected "ioc_would_post"
        | Some `Maker_or_cancel_would_take -> Types.Order_status.Rejected "moc_would_take"
        | Some `Requested -> Types.Order_status.New
        | None -> Types.Order_status.New
      in
      let status : Types.Order_status.t =
        match r.is_cancelled with
        | true -> Types.Order_status.Canceled
        | false ->
          match r.is_live with
          | true ->
            (match Float.(qty_exec = 0.) with
             | true -> Types.Order_status.New
             | false ->
               match Float.(qty_exec > 0.) with
               | true -> Types.Order_status.Partially_filled
               | false ->
                 match Float.(qty_rem = 0.) with
                 | true -> Types.Order_status.Filled
                 | false -> reason_to_status r.reason)
          | false ->
            match Float.(qty_rem = 0.) with
            | true -> Types.Order_status.Filled
            | false -> reason_to_status r.reason
      in
      Ok ({ Types.Order.venue = Venue.t
      ; id = Common.Int_string.to_string r.order_id
      ; symbol = symbol_to_string r.symbol
      ; side = side r.side
      ; kind
      ; time_in_force = Types.Time_in_force.GTC
      ; qty = qty_orig
      ; filled = qty_exec
      ; status
      ; created_at = time_of_ts_opt (Some r.timestamp)
      ; updated_at = time_of_ts_opt (Some r.timestamp)
      } : Types.Order.t)

    let order_status (r : Native.Order.status) : Types.Order_status.t =
      match order_response r with
      | Ok order -> order.status
      | Error _ -> Types.Order_status.Rejected "conversion_error"

    let order_from_status (r : Native.Order.status) : (Types.Order.t, string) Result.t =
      order_response r

    let trade (t : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let open Result.Let_syntax in
      match t with
      | Ws ev ->
        (match ev.fill with
         | None ->
           (* Parse optional avg_execution_price and executed_amount *)
           let price_result = Option.value_map ev.avg_execution_price
             ~default:(Ok 0.0)
             ~f:(fun d -> Fluxum.Normalize_common.Float_conv.price_of_string
               (Common.Decimal_string.to_string d)) in
           let qty_result = Option.value_map ev.executed_amount
             ~default:(Ok 0.0)
             ~f:(fun d -> Fluxum.Normalize_common.Float_conv.qty_of_string
               (Common.Decimal_string.to_string d)) in
           let%bind price = price_result in
           let%bind qty = qty_result in
           Ok ({ venue = Venue.t
           ; symbol = Common.Symbol.Enum_or_string.to_string ev.symbol
           ; side = side ev.side
           ; price
           ; qty
           ; fee = None
           ; trade_id = None
           ; ts = time_of_ts_opt (Some ev.timestamp)
           } : Types.Trade.t)
         | Some f ->
           let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string
             (Common.Decimal_string.to_string f.price) in
           let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string
             (Common.Decimal_string.to_string f.amount) in
           let%bind fee = Fluxum.Normalize_common.Float_conv.qty_of_string
             (Common.Decimal_string.to_string f.fee) in
           Ok ({ venue = Venue.t
           ; symbol = Common.Symbol.Enum_or_string.to_string ev.symbol
           ; side = side ev.side
           ; price
           ; qty
           ; fee = Some fee
           ; trade_id = Some (Common.Int_string.to_string f.trade_id)
           ; ts = time_of_ts_opt (Some ev.timestamp)
           } : Types.Trade.t))
      | Rest tr ->
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string
          (Common.Decimal_string.to_string tr.price) in
        let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string
          (Common.Decimal_string.to_string tr.amount) in
        let%bind fee = Fluxum.Normalize_common.Float_conv.qty_of_string
          (Common.Decimal_string.to_string tr.fee_amount) in
        Ok ({ venue = Venue.t
        ; symbol = Common.Symbol.Enum_or_string.to_string tr.symbol
        ; side = side tr.type_
        ; price
        ; qty
        ; fee = Some fee
        ; trade_id = Some (Common.Int_number.to_string tr.tid)
        ; ts = Some tr.timestampms
        } : Types.Trade.t)

    let balance (b : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind total = Fluxum.Normalize_common.Float_conv.qty_of_string (Common.Decimal_string.to_string b.amount) in
      let%bind available = Fluxum.Normalize_common.Float_conv.qty_of_string (Common.Decimal_string.to_string b.available) in
      Ok ({ venue = Venue.t
      ; currency = Common.Currency.Enum_or_string.to_string b.currency
      ; total
      ; available
      ; locked = total -. available
      } : Types.Balance.t)

    let book_update (u : Native.Book.update) : Types.Book_update.t =
      let ts = Option.first_some u.timestamp u.timestampms in
      let ts = time_of_ts_opt ts in
      let levels_of_event = function
        | `Change (c : V1.Market_data.Change_event.t) ->
          (match (
            let open Result.Let_syntax in
            let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string
              (Common.Decimal_string.to_string c.price) in
            let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string
              (Common.Decimal_string.to_string c.remaining) in
            Ok (price, qty)
          ) with
          | Ok (price, qty) ->
            let side =
              match c.side with
              | `Bid -> Types.Book_update.Side.Bid
              | `Ask -> Types.Book_update.Side.Ask
            in
            Some (side, [{ Types.Book_update.price; qty }])
          | Error err ->
            Log.Global.error "Gemini: Failed to parse book update level: %s" err;
            None)
        | `Trade _ -> None
        | `Auction _ | `Auction_open _ | `Block_trade _ -> None
      in
      (* Gemini batches multiple events; emit the first change as a minimal update *)
      let side_levels =
        Array.to_list u.events |> List.filter_map ~f:levels_of_event |> List.hd
      in
      match side_levels with
      | Some (side, levels) ->
        { venue = Venue.t
        ; symbol = "" (* market data update doesn't include symbol in Update *)
        ; side
        ; levels
        ; ts
        ; is_snapshot = false
        }
      | None ->
        { venue = Venue.t
        ; symbol = ""
        ; side = Types.Book_update.Side.Bid
        ; levels = []
        ; ts
        ; is_snapshot = false
        }

    let symbol_info (s : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind min_order_size = Fluxum.Normalize_common.Float_conv.qty_of_string
        (Common.Decimal_string.to_string s.min_order_size) in
      Ok ({ venue = Venue.t
      ; symbol = Common.Symbol.Enum_or_string.to_string s.symbol
      ; base_currency = Common.Currency.Enum_or_string.to_string s.base_currency
      ; quote_currency = Common.Currency.Enum_or_string.to_string s.quote_currency
      ; status = s.status
      ; min_order_size
      ; tick_size = Some (Common.Decimal_number.to_float s.tick_size)
      ; quote_increment = Some (Common.Decimal_number.to_float s.quote_increment)
      } : Types.Symbol_info.t)

    let ticker (json : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      try
        let open Yojson.Safe.Util in
        let open Result.Let_syntax in
        let bid_str = json |> member "bid" |> to_string in
        let ask_str = json |> member "ask" |> to_string in
        let last_str = json |> member "last" |> to_string in
        let%bind bid = Fluxum.Normalize_common.Float_conv.price_of_string bid_str in
        let%bind ask = Fluxum.Normalize_common.Float_conv.price_of_string ask_str in
        let%bind last = Fluxum.Normalize_common.Float_conv.price_of_string last_str in
        let volume = json |> member "volume" in
        let%bind symbol_vol = match volume |> to_assoc |> List.hd with
          | Some (symbol, vol) ->
            let vol_str = vol |> to_string in
            let%bind vol_float = Fluxum.Normalize_common.Float_conv.qty_of_string vol_str in
            Ok (symbol, vol_float)
          | None -> Ok ("", 0.)
        in
        Ok ({ venue = Venue.t
        ; symbol = String.uppercase (fst symbol_vol)
        ; last_price = last
        ; bid_price = bid
        ; ask_price = ask
        ; high_24h = last  (* Gemini doesn't provide high/low in ticker *)
        ; low_24h = last
        ; volume_24h = snd symbol_vol
        ; quote_volume = None
        ; price_change = None
        ; price_change_pct = None
        ; ts = None
        } : Types.Ticker.t)
      with
      | Yojson.Safe.Util.Type_error (msg, _) ->
        Error (sprintf "Ticker JSON type error: %s" msg)
      | Failure msg ->
        Error (sprintf "Ticker conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Ticker unexpected error: %s" (Exn.to_string exn))

    let order_book (json : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      try
        let open Yojson.Safe.Util in
        let open Result.Let_syntax in
        let parse_level level =
          let price_str = level |> member "price" |> to_string in
          let volume_str = level |> member "amount" |> to_string in
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
          let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string volume_str in
          Ok { Types.Order_book.Price_level.price; volume }
        in
        let parse_levels levels_json =
          levels_json |> to_list
          |> List.map ~f:parse_level
          |> Fluxum.Normalize_common.Result_util.transpose
        in
        let%bind bids = json |> member "bids" |> parse_levels in
        let%bind asks = json |> member "asks" |> parse_levels in
        Ok ({ venue = Venue.t
        ; symbol = ""  (* Symbol not in response, caller must track *)
        ; bids
        ; asks
        ; ts = None
        ; epoch = 0
        } : Types.Order_book.t)
      with
      | Yojson.Safe.Util.Type_error (msg, _) ->
        Error (sprintf "Order book JSON type error: %s" msg)
      | Failure msg ->
        Error (sprintf "Order book conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Order book unexpected error: %s" (Exn.to_string exn))

    let public_trade (json : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      try
        let open Yojson.Safe.Util in
        let open Result.Let_syntax in
        let timestamp = json |> member "timestampms" |> to_int in
        let ts = Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_int timestamp)) in
        let price_str = json |> member "price" |> to_string in
        let qty_str = json |> member "amount" |> to_string in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        let side_str = json |> member "type" |> to_string in
        let side =
          match Fluxum.Normalize_common.Side.of_string side_str with
          | Ok s -> Some s
          | Error _ -> Some Types.Side.Buy  (* Default for unknown *)
        in
        Ok ({ venue = Venue.t
        ; symbol = ""  (* Symbol not in response, caller must track *)
        ; price
        ; qty
        ; side
        ; trade_id = Some (json |> member "tid" |> to_int |> Int.to_string)
        ; ts = Some ts
        } : Types.Public_trade.t)
      with
      | Yojson.Safe.Util.Type_error (msg, _) ->
        Error (sprintf "Public trade JSON type error: %s" msg)
      | Failure msg ->
        Error (sprintf "Public trade conversion failed: %s" msg)
      | exn ->
        Error (sprintf "Public trade unexpected error: %s" (Exn.to_string exn))

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "Gemini does not support candle/klines data"

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Bad_request msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "400"; message = msg }
      | `Not_found -> Types.Error.Exchange_specific { venue = Venue.t; code = "404"; message = "not_found" }
      | `Service_unavailable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "503"; message = msg }
      | `Not_acceptable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "406"; message = msg }
      | `Unauthorized _ -> Types.Error.Auth_failed
      | `Bad_gateway msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "502"; message = msg }
      | `Gateway_timeout msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "504"; message = msg }
      | `Json_parse_error { message; body = _ } -> Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message }
      | `Error { reason; message } -> Types.Error.Exchange_specific { venue = Venue.t; code = reason; message }
    end
end

(* Adapter constructor is defined inside the Adapter module *)

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol ~side ~kind ~qty =
    let open Common in
    let amount = Decimal_string.of_string (Float.to_string qty) in
    let price_str_of p = Decimal_string.of_string (Float.to_string p) in
    let side' = match side with | Types.Side.Buy -> `Buy | Types.Side.Sell -> `Sell in
    let type_ = `Exchange_limit in
    let price, options =
      match kind with
      | Types.Order_kind.Basic Market -> (price_str_of 0.0, [])
      | Types.Order_kind.Basic (Limit p) -> (price_str_of p, [])
      | Types.Order_kind.Basic (Post_only p) -> (price_str_of p, [ `Maker_or_cancel ])
      | Types.Order_kind.Conditional _ -> (price_str_of 0.0, [])  (* Gemini doesn't support stop orders via this endpoint *)
    in
    V1.Order.New.
      { client_order_id = Client_order_id.of_string (sprintf "fluxum-%0.0f" (Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_ms))
      ; symbol = V1.Symbol.of_string symbol
      ; amount
      ; price
      ; side = side'
      ; type_
      ; options
      }
end

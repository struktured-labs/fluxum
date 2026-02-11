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
    ; rate_limiter : Exchange_common.Rate_limiter.t
    }

  let create ~cfg ~nonce ?(symbols = []) () =
    { cfg
    ; nonce
    ; symbols
    ; rate_limiter = Exchange_common.Rate_limiter.create
        ~config:Exchange_common.Rate_limiter.Configs.gemini ()
    }

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

    (** Account operations - deposits/withdrawals *)
    module Deposit_address = struct
      type t = V1.Deposit_address.response
    end

    module Deposit = struct
      type t = V1.Transfers.transfer
    end

    module Withdrawal = struct
      type t =
        | Withdraw_response of V1.Withdraw.response
        | Transfer of V1.Transfers.transfer
    end
  end

  let place_order (t : t) (req : Native.Order.request) =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Order.New.post (module Cfg) t.nonce req
      >>| function
      | `Ok r -> Ok r
        | (#Rest.Error.post as e) -> Error e)

  let cancel_order (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Order.Cancel.By_order_id.post (module Cfg) t.nonce { order_id = id }
      >>| function
      | `Ok _ -> Ok ()
        | (#Rest.Error.post as e) -> Error e)

  let balances (t : t) =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Balances.post (module Cfg) t.nonce ()
      >>| function
      | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e)

  let get_order_status (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Order.Status.post (module Cfg) t.nonce { order_id = id }
      >>| function
      | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e)

  let get_open_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Orders.post (module Cfg) t.nonce ()
      >>| function
      | `Ok orders -> Ok orders
      | (#Rest.Error.post as e) -> Error e)

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    (* Gemini doesn't have a closed orders endpoint *)
    Deferred.return (Error (`Bad_request "Gemini does not support order history endpoint"))

  let get_my_trades (t : t) ~symbol ?limit () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Mytrades.post (module Cfg) t.nonce
        { symbol = V1.Symbol.of_string symbol
        ; limit_trades = limit
        ; timestamp = None
        }
      >>| function
      | `Ok trades -> Ok (List.map trades ~f:(fun t -> Native.Trade.Rest t))
      | (#Rest.Error.post as e) -> Error e)

  let get_symbols (t : t) () =
    let (module Cfg) = t.cfg in
    (* Fetch details for all known symbols - rate limiting applied per request *)
    Deferred.List.filter_map V1.Symbol.all ~how:`Sequential ~f:(fun sym ->
      Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
        V1.Symbol_details.get (module Cfg) t.nonce ~uri_args:sym ()
        >>| function
        | `Ok r -> Ok (Some r)
        | #Rest.Error.get -> Ok None)
      >>| function
      | Ok r -> r
      | Error _ -> None)
    >>| fun results -> Ok results

  let get_ticker (t : t) ~symbol () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
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
        | json -> Ok json)

  let get_order_book (t : t) ~symbol ?limit () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
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
        | json -> Ok json)

  let get_recent_trades (t : t) ~symbol ?limit () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
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
          | _ -> Error (`Json_parse "Expected array of trades"))

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    (* Gemini does not have a public REST candle/OHLCV endpoint *)
    Deferred.return (Error (`Bad_request "Gemini does not support REST candle/klines endpoint"))

  let cancel_all_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Order.Cancel.All.post (module Cfg) t.nonce ()
      >>| function
      | `Ok { details } -> Ok (List.length details.cancelled_orders)
      | (#Rest.Error.post as e) -> Error e)

  (** {2 Account Operations - Deposits/Withdrawals} *)

  let get_deposit_address (t : t) ~currency ?network:_ () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Deposit_address.post (module Cfg) t.nonce { currency; label = None }
      >>| function
      | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e)

  let withdraw (t : t) ~currency ~amount ~address ?tag ?network:_ () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Withdraw.post (module Cfg) t.nonce
        { currency
        ; address
        ; amount = Common.Decimal_string.of_string (Float.to_string amount)
        ; memo = tag
        }
      >>| function
      | `Ok r -> Ok (Native.Withdrawal.Withdraw_response r)
      | (#Rest.Error.post as e) -> Error e)

  let get_deposits (t : t) ?currency ?limit () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Transfers.post (module Cfg) t.nonce
        { currency; timestamp = None; limit_transfers = limit }
      >>| function
      | `Ok transfers ->
        (* Filter to only deposits *)
        let deposits = List.filter transfers ~f:(fun tr ->
          match tr.V1.Transfers.type_ with
          | `Deposit -> true
          | `Withdrawal -> false)
        in
        Ok deposits
      | (#Rest.Error.post as e) -> Error e)

  let get_withdrawals (t : t) ?currency ?limit () =
    let (module Cfg) = t.cfg in
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      V1.Transfers.post (module Cfg) t.nonce
        { currency; timestamp = None; limit_transfers = limit }
      >>| function
      | `Ok transfers ->
        (* Filter to only withdrawals and wrap them *)
        let withdrawals = List.filter_map transfers ~f:(fun tr ->
          match tr.V1.Transfers.type_ with
          | `Withdrawal -> Some (Native.Withdrawal.Transfer tr)
          | `Deposit -> None)
        in
        Ok withdrawals
      | (#Rest.Error.post as e) -> Error e)

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

    (** {2 Prediction Market Normalization} *)

    let prediction_outcome (o : Prediction_markets.Outcome.t) : Types.Prediction_outcome.t =
      match o with
      | `Yes -> Types.Prediction_outcome.Yes
      | `No -> Types.Prediction_outcome.No

    let prediction_contract (c : Prediction_markets.Contract.t)
        : (Types.Prediction_contract.t, string) Result.t =
      let open Result.Let_syntax in
      let last_price = Option.bind c.prices ~f:(fun p ->
        Option.bind p.last_trade_price ~f:Float.of_string_opt) in
      let best_bid = Option.bind c.prices ~f:(fun p ->
        Option.bind p.best_bid ~f:Float.of_string_opt) in
      let best_ask = Option.bind c.prices ~f:(fun p ->
        Option.bind p.best_ask ~f:Float.of_string_opt) in
      let%bind total_shares = Fluxum.Normalize_common.Float_conv.qty_of_string c.total_shares in
      Ok ({ instrument_symbol = c.instrument_symbol
          ; label = c.label
          ; ticker = c.ticker
          ; last_price
          ; best_bid
          ; best_ask
          ; total_shares
          ; status = c.status
          } : Types.Prediction_contract.t)

    let prediction_event (e : Prediction_markets.Event.t)
        : (Types.Prediction_event.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string e.volume in
      let%bind liquidity = Fluxum.Normalize_common.Float_conv.qty_of_string e.liquidity in
      let%bind contracts = List.map e.contracts ~f:prediction_contract
        |> Result.all in
      Ok ({ venue = Venue.t
          ; id = e.id
          ; title = e.title
          ; description = e.description
          ; category = e.category
          ; ticker = e.ticker
          ; status = Prediction_markets.Event_status.to_string e.status
          ; volume
          ; liquidity
          ; contracts
          ; is_live = e.is_live
          } : Types.Prediction_event.t)

    let prediction_order (r : Prediction_markets.Place_order.response)
        : (Types.Prediction_order.t, string) Result.t =
      let open Result.Let_syntax in
      let side' = match r.side with `Buy -> Types.Side.Buy | `Sell -> Types.Side.Sell in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string r.quantity in
      let%bind filled = Fluxum.Normalize_common.Float_conv.qty_of_string r.filled_quantity in
      let%bind remaining = Fluxum.Normalize_common.Float_conv.qty_of_string r.remaining_quantity in
      let%bind price = Fluxum.Normalize_common.Float_conv.of_string r.price in
      let%bind avg_execution_price = match r.avg_execution_price with
        | None -> Ok None
        | Some s -> Fluxum.Normalize_common.Float_conv.of_string s |> Result.map ~f:Option.some
      in
      Ok ({ venue = Venue.t
          ; id = Int64.to_string r.order_id
          ; symbol = r.symbol
          ; side = side'
          ; outcome = prediction_outcome r.outcome
          ; qty
          ; filled
          ; remaining
          ; price
          ; avg_execution_price
          ; status = r.status
          ; event_ticker = Option.bind r.contract_metadata ~f:(fun m -> m.event_ticker)
          ; created_at = None
          ; updated_at = None
          } : Types.Prediction_order.t)

    let prediction_position (p : Prediction_markets.Positions.position)
        : (Types.Prediction_position.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string p.total_quantity in
      let%bind avg_price = Fluxum.Normalize_common.Float_conv.of_string p.avg_price in
      Ok ({ venue = Venue.t
          ; symbol = p.symbol
          ; outcome = prediction_outcome p.outcome
          ; qty
          ; avg_price
          ; event_ticker = Option.bind p.contract_metadata ~f:(fun m -> m.event_ticker)
          ; contract_name = Option.bind p.contract_metadata ~f:(fun m -> m.contract_name)
          } : Types.Prediction_position.t)

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

    (** Account operations normalization *)

    let deposit_address (r : Native.Deposit_address.t) : (Types.Deposit_address.t, string) Result.t =
      Ok ({ venue = Venue.t
          ; currency = r.currency
          ; address = r.address
          ; tag = None  (* Gemini doesn't return tag in this response *)
          ; network = r.network
          } : Types.Deposit_address.t)

    let deposit (tr : Native.Deposit.t) : (Types.Deposit.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind amount = Fluxum.Normalize_common.Float_conv.amount_of_string
        (Common.Decimal_string.to_string tr.amount) in
      let status = match String.lowercase tr.status with
        | "complete" | "completed" -> Types.Transfer_status.Completed
        | "pending" -> Types.Transfer_status.Pending
        | "processing" | "advanced" -> Types.Transfer_status.Processing
        | "failed" -> Types.Transfer_status.Failed
        | "cancelled" | "canceled" -> Types.Transfer_status.Cancelled
        | _ -> Types.Transfer_status.Pending
      in
      Ok ({ venue = Venue.t
          ; id = Int64.to_string tr.eid
          ; currency = Common.Currency.Enum_or_string.to_string tr.currency
          ; amount
          ; status
          ; address = tr.destination
          ; tx_id = tr.txHash
          ; created_at = Some tr.timestampms
          ; updated_at = Some tr.timestampms
          } : Types.Deposit.t)

    let withdrawal (w : Native.Withdrawal.t) : (Types.Withdrawal.t, string) Result.t =
      let open Result.Let_syntax in
      match w with
      | Native.Withdrawal.Withdraw_response r ->
        let%bind amount = Fluxum.Normalize_common.Float_conv.amount_of_string
          (Common.Decimal_string.to_string r.amount) in
        Ok ({ venue = Venue.t
            ; id = r.withdrawalId
            ; currency = ""  (* Not in response, would need to be passed from request *)
            ; amount
            ; fee = None
            ; status = Types.Transfer_status.Processing  (* Just submitted *)
            ; address = r.address
            ; tag = None
            ; tx_id = r.txHash
            ; created_at = Some (Time_float_unix.now ())
            ; updated_at = Some (Time_float_unix.now ())
            } : Types.Withdrawal.t)
      | Native.Withdrawal.Transfer tr ->
        let%bind amount = Fluxum.Normalize_common.Float_conv.amount_of_string
          (Common.Decimal_string.to_string tr.V1.Transfers.amount) in
        let%bind fee = match tr.V1.Transfers.feeAmount with
          | Some fa -> Fluxum.Normalize_common.Float_conv.amount_of_string
              (Common.Decimal_string.to_string fa) |> Result.map ~f:Option.some
          | None -> Ok None
        in
        let status = match String.lowercase tr.V1.Transfers.status with
          | "complete" | "completed" -> Types.Transfer_status.Completed
          | "pending" -> Types.Transfer_status.Pending
          | "processing" | "advanced" -> Types.Transfer_status.Processing
          | "failed" -> Types.Transfer_status.Failed
          | "cancelled" | "canceled" -> Types.Transfer_status.Cancelled
          | _ -> Types.Transfer_status.Pending
        in
        Ok ({ venue = Venue.t
            ; id = Int64.to_string tr.V1.Transfers.eid
            ; currency = Common.Currency.Enum_or_string.to_string tr.V1.Transfers.currency
            ; amount
            ; fee
            ; status
            ; address = Option.value tr.V1.Transfers.destination ~default:""
            ; tag = None
            ; tx_id = tr.V1.Transfers.txHash
            ; created_at = Some tr.V1.Transfers.timestampms
            ; updated_at = Some tr.V1.Transfers.timestampms
            } : Types.Withdrawal.t)
    end

  (** {2 Prediction Market Operations} *)

  let prediction_list_events (t : t) ?status ?category ?search ?limit ?offset () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.List_events.get t.cfg ?status ?category ?search ?limit ?offset ()
      >>| function
      | `Ok events ->
        let normalized = List.filter_map events ~f:(fun e ->
          match Normalize.prediction_event e with
          | Ok n -> Some n
          | Error err ->
            Log.Global.error "prediction event normalize error (ticker=%s): %s" e.ticker err;
            None) in
        Ok normalized
      | (#Rest.Error.get as e) -> Error e)

  let prediction_get_event (t : t) ~event_ticker () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_event.get t.cfg ~event_ticker ()
      >>| function
      | `Ok event ->
        (match Normalize.prediction_event event with
         | Ok n -> Ok n
         | Error err -> Error (`Normalize_error err))
      | (#Rest.Error.get as e) -> Error e)

  let prediction_place_order (t : t) request =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Place_order.post t.cfg t.nonce request
      >>| function
      | `Ok r ->
        (match Normalize.prediction_order r with
         | Ok n -> Ok n
         | Error err -> Error (`Normalize_error err))
      | (#Rest.Error.post as e) -> Error e)

  let prediction_cancel_order (t : t) ~order_id =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Cancel_order.post t.cfg t.nonce { order_id }
      >>| function
      | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e)

  let prediction_active_orders (t : t) ?limit ?offset ?symbol () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Active_orders.get ?limit ?offset ?symbol t.cfg t.nonce ()
      >>| function
      | `Ok orders ->
        let normalized = List.filter_map orders ~f:(fun o ->
          match Normalize.prediction_order o with
          | Ok n -> Some n
          | Error err ->
            Log.Global.error "prediction order normalize error (id=%Ld): %s" o.order_id err;
            None) in
        Ok normalized
      | (#Rest.Error.post as e) -> Error e)

  let prediction_order_history (t : t) ?limit ?offset ?symbol () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Order_history.get ?limit ?offset ?symbol t.cfg t.nonce ()
      >>| function
      | `Ok orders ->
        let normalized = List.filter_map orders ~f:(fun o ->
          match Normalize.prediction_order o with
          | Ok n -> Some n
          | Error err ->
            Log.Global.error "prediction order normalize error (id=%Ld): %s" o.order_id err;
            None) in
        Ok normalized
      | (#Rest.Error.post as e) -> Error e)

  let prediction_positions (t : t) () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Positions.post t.cfg t.nonce ()
      >>| function
      | `Ok positions ->
        let normalized = List.filter_map positions ~f:(fun p ->
          match Normalize.prediction_position p with
          | Ok n -> Some n
          | Error err ->
            Log.Global.error "prediction position normalize error (symbol=%s): %s" p.symbol err;
            None) in
        Ok normalized
      | (#Rest.Error.post as e) -> Error e)
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

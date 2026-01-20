(** OKX Exchange Adapter

    Complete implementation of Exchange_intf.S for OKX Global exchange.

    {b Status:} BETA - Core functionality complete, needs testing

    {b Features:}
    - ✅ REST trading (spot, futures, swap, option via V5 API)
    - ✅ WebSocket market data (books, trades, tickers)
    - ✅ Fallible normalization (Phase 1 complete)
    - ✅ Unified V5 API across all products

    {b Authentication:}
    - API key/secret/passphrase via environment variables (OKX_PRODUCTION_API_KEY, etc.)
    - HMAC-SHA256 signature with Base64 encoding
    - ISO timestamp format (2020-12-08T09:08:57.715Z)
    - OK-ACCESS-* headers

    {b Rate Limits:}
    - Public endpoints: Varies by endpoint (typically 20 req/s)
    - Private endpoints: Varies by endpoint (typically 60 req/s for trading)
    - WebSocket: 3 subscribe requests/second, 480 requests/hour per connection

    {b Symbol Format:}
    - Hyphenated: ["BTC-USDT"], ["ETH-USDT"]
    - Case-sensitive: uppercase required
    - Product type specified via instType parameter

    {b Known Limitations:}
    - Some advanced order types require specific instTypes
    - Margin trading requires tdMode (cross/isolated) configuration
    - WebSocket auto-reconnect requires re-subscription

    {b V5 API Product Types:}
    - SPOT: Spot trading
    - FUTURES: Delivery futures
    - SWAP: Perpetual swaps
    - OPTION: Options trading

    @see <https://www.okx.com/docs-v5/en/> OKX V5 API Documentation
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    ; inst_type : V5.InstType.t
    ; td_mode : V5.TdMode.t
    }

  let create ~cfg ?(symbols = []) ?(inst_type = `Spot) ?(td_mode = `Cash) () =
    { cfg; symbols; inst_type; td_mode }

  module Venue = struct
    let t = Types.Venue.Okx
  end

  module Native = struct
    module Order = struct
      type id = string
      type request = V5.Place_order.request
      type response = V5.Place_order.order_data
      type status = V5.Order_details.order
    end

    module Trade = struct
      type t = V5.Recent_trades.trade
    end

    module Balance = struct
      type t = V5.Account_balance.balance_detail
    end

    module Book = struct
      type update = V5.Orderbook.book_data
      type snapshot = V5.Orderbook.book_data
    end

    module Ticker = struct
      type t = V5.Market_tickers.ticker
    end

    module Public_trade = struct
      type t = V5.Recent_trades.trade
    end

    module Symbol_info = struct
      type t = V5.Instruments.instrument
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  (** Trading operations *)

  let place_order t (req : Native.Order.request) =
    let%bind result = V5.Place_order.request t.cfg req in
    match result with
    | `Ok resp ->
      (match resp.data with
       | order :: _ -> return (Ok order)
       | [] -> return (Error (`Bad_request "No order data returned")))
    | #Rest.Error.t as e -> return (Error e)

  let cancel_order t ~symbol ~order_id =
    let req : V5.Cancel_order.request =
      { instId = symbol
      ; ordId = Some order_id
      ; clOrdId = None
      }
    in
    let%bind result = V5.Cancel_order.request t.cfg req in
    match result with
    | `Ok resp ->
      (match resp.data with
       | cancel :: _ -> return (Ok cancel.ordId)
       | [] -> return (Error (`Bad_request "No cancel data returned")))
    | #Rest.Error.t as e -> return (Error e)

  let get_order_status t ~symbol ~order_id =
    let req : V5.Order_details.request =
      { instId = symbol
      ; ordId = Some order_id
      ; clOrdId = None
      }
    in
    let%bind result = V5.Order_details.request t.cfg req in
    match result with
    | `Ok resp ->
      (match resp.data with
       | order :: _ -> return (Ok order)
       | [] -> return (Error (`Not_found)))
    | #Rest.Error.t as e -> return (Error e)

  let get_order_history t ~symbol =
    let req : V5.Orders_history.request =
      { instType = t.inst_type
      ; instId = Some symbol
      ; limit = Some 100
      }
    in
    let%bind result = V5.Orders_history.request t.cfg req in
    match result with
    | `Ok resp -> return (Ok resp.data)
    | #Rest.Error.t as e -> return (Error e)

  (** Market data operations *)

  let get_ticker t ~symbol =
    ignore symbol;
    let req : V5.Market_tickers.request =
      { instType = t.inst_type
      ; uly = None
      ; instFamily = None
      }
    in
    let%bind result = V5.Market_tickers.request t.cfg req in
    match result with
    | `Ok resp ->
      (match List.find resp.data ~f:(fun ticker -> String.equal ticker.instId symbol) with
       | Some ticker -> return (Ok ticker)
       | None -> return (Error (`Not_found)))
    | #Rest.Error.t as e -> return (Error e)

  let get_order_book t ~symbol =
    let req : V5.Orderbook.request =
      { instId = symbol
      ; sz = Some 100
      }
    in
    let%bind result = V5.Orderbook.request t.cfg req in
    match result with
    | `Ok resp ->
      (match resp.data with
       | book :: _ -> return (Ok book)
       | [] -> return (Error (`Not_found)))
    | #Rest.Error.t as e -> return (Error e)

  let get_recent_trades t ~symbol =
    let req : V5.Recent_trades.request =
      { instId = symbol
      ; limit = Some 100
      }
    in
    let%bind result = V5.Recent_trades.request t.cfg req in
    match result with
    | `Ok resp -> return (Ok resp.data)
    | #Rest.Error.t as e -> return (Error e)

  let get_symbols t =
    let req : V5.Instruments.request =
      { instType = t.inst_type
      ; instId = None
      }
    in
    let%bind result = V5.Instruments.request t.cfg req in
    match result with
    | `Ok resp -> return (Ok resp.data)
    | #Rest.Error.t as e -> return (Error e)

  (** Account operations *)

  let get_balances t =
    let req : V5.Account_balance.request = { ccy = None } in
    let%bind result = V5.Account_balance.request t.cfg req in
    match result with
    | `Ok resp ->
      let balances =
        List.concat_map resp.data ~f:(fun account -> account.details)
      in
      return (Ok balances)
    | #Rest.Error.t as e -> return (Error e)

  (** Normalization module *)

  module Normalize = struct
    let order_response (resp : Native.Order.response) : (Types.Order.t, string) Result.t =
      (* OKX returns sCode/sMsg for order placement results *)
      match resp.sCode with
      | "0" ->
        Ok ({ venue = Venue.t
        ; id = resp.ordId
        ; symbol = ""  (* Not provided in response *)
        ; side = Types.Side.Buy  (* Not provided - will be overridden *)
        ; kind = Types.Order_kind.Market  (* Not provided - will be overridden *)
        ; qty = 0.0  (* Not provided in place response *)
        ; filled = 0.0
        ; status = Types.Order_status.New
        ; created_at = None
        ; updated_at = None
        } : Types.Order.t)
      | code ->
        Error (sprintf "Order placement failed: code=%s msg=%s" code resp.sMsg)

    let order_status (status : Native.Order.status) : (Types.Order_status.t, string) Result.t =
      (* OKX statuses: live, partially_filled, filled, canceled *)
      match String.lowercase status.state with
      | "live" -> Ok Types.Order_status.New
      | "partially_filled" -> Ok Types.Order_status.Partially_filled
      | "filled" -> Ok Types.Order_status.Filled
      | "canceled" -> Ok Types.Order_status.Canceled
      | s -> Error (sprintf "Unknown OKX order status: %s" s)

    let order_from_status (status : Native.Order.status) : (Types.Order.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind side = Fluxum.Normalize_common.Side.of_string status.side in
      let%bind kind =
        match String.lowercase status.ordType with
        | "market" -> Ok Types.Order_kind.Market
        | "limit" ->
          let%map price = Fluxum.Normalize_common.Float_conv.price_of_string status.px in
          Types.Order_kind.Limit price
        | "post_only" ->
          let%map price = Fluxum.Normalize_common.Float_conv.price_of_string status.px in
          Types.Order_kind.Post_only_limit price
        | t -> Error (sprintf "Unknown order type: %s" t)
      in
      let%bind order_status = order_status status in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string status.sz in
      let%bind filled = Fluxum.Normalize_common.Float_conv.qty_of_string status.accFillSz in
      Ok ({ venue = Venue.t
      ; id = status.ordId
      ; symbol = status.instId
      ; side
      ; kind
      ; qty
      ; filled
      ; status = order_status
      ; created_at = Some (Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_string status.cTime)))
      ; updated_at = Some (Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_string status.uTime)))
      } : Types.Order.t)

    let trade (t : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind side = Fluxum.Normalize_common.Side.of_string t.side in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string t.px in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string t.sz in
      Ok ({ venue = Venue.t
      ; symbol = t.instId
      ; side
      ; price
      ; qty
      ; fee = None
      ; trade_id = Some t.tradeId
      ; ts = Some (Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_string t.ts)))
      } : Types.Trade.t)

    let balance (b : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind available = Fluxum.Normalize_common.Float_conv.of_string b.availBal in
      let%bind cash = Fluxum.Normalize_common.Float_conv.of_string b.cashBal in
      let%bind frozen = Fluxum.Normalize_common.Float_conv.of_string b.frozenBal in
      let total = cash in  (* cashBal represents total balance *)
      Ok ({ venue = Venue.t
      ; currency = b.ccy
      ; total
      ; available
      ; locked = frozen
      } : Types.Balance.t)

    let ticker (t : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind last_price = Fluxum.Normalize_common.Float_conv.price_of_string t.last in
      let%bind bid_price = Fluxum.Normalize_common.Float_conv.price_of_string t.bidPx in
      let%bind ask_price = Fluxum.Normalize_common.Float_conv.price_of_string t.askPx in
      let%bind high_24h = Fluxum.Normalize_common.Float_conv.of_string t.high24h in
      let%bind low_24h = Fluxum.Normalize_common.Float_conv.of_string t.low24h in
      let%bind volume_24h = Fluxum.Normalize_common.Float_conv.qty_of_string t.vol24h in
      let%bind quote_volume = Fluxum.Normalize_common.Float_conv.of_string t.volCcy24h in
      Ok ({ venue = Venue.t
      ; symbol = t.instId
      ; last_price
      ; bid_price
      ; ask_price
      ; high_24h
      ; low_24h
      ; volume_24h
      ; quote_volume = Some quote_volume
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      } : Types.Ticker.t)

    let order_book ~symbol (book : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind bids =
        List.map book.bids ~f:(fun (price, qty, _, _) ->
          let%bind price_f = Fluxum.Normalize_common.Float_conv.price_of_string price in
          let%bind qty_f = Fluxum.Normalize_common.Float_conv.qty_of_string qty in
          Ok { Types.Order_book.Price_level.price = price_f; volume = qty_f })
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      let%bind asks =
        List.map book.asks ~f:(fun (price, qty, _, _) ->
          let%bind price_f = Fluxum.Normalize_common.Float_conv.price_of_string price in
          let%bind qty_f = Fluxum.Normalize_common.Float_conv.qty_of_string qty in
          Ok { Types.Order_book.Price_level.price = price_f; volume = qty_f })
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      Ok ({ venue = Venue.t
      ; symbol
      ; bids
      ; asks
      ; ts = Some (Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_string book.ts)))
      ; epoch = 0  (* OKX doesn't provide update ID in same format *)
      } : Types.Order_book.t)

    let public_trade (trade : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind side = Fluxum.Normalize_common.Side.of_string trade.side in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string trade.px in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string trade.sz in
      Ok ({ venue = Venue.t
      ; symbol = trade.instId
      ; side = Some side
      ; price
      ; qty
      ; trade_id = Some trade.tradeId
      ; ts = Some (Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_string trade.ts)))
      } : Types.Public_trade.t)

    let symbol_info (info : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind min_size = Fluxum.Normalize_common.Float_conv.qty_of_string info.minSz in
      let%bind tick_size = Fluxum.Normalize_common.Float_conv.of_string info.tickSz in
      Ok (Types.Symbol_info.create
        ~venue:Venue.t
        ~symbol:info.instId
        ~base_currency:info.baseCcy
        ~quote_currency:info.quoteCcy
        ~status:info.state
        ~min_order_size:min_size
        ~tick_size
        ())

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Bad_request msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "bad_request"; message = msg }
      | `Not_found ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "not_found"; message = "Resource not found" }
      | `Unauthorized _ ->
        Types.Error.Auth_failed
      | `Forbidden _ ->
        Types.Error.Auth_failed
      | `Too_many_requests _ ->
        Types.Error.Rate_limited
      | `Service_unavailable msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "service_unavailable"; message = msg }
      | `Json_parse_error { message; body } ->
        Types.Error.Normalization_error (sprintf "JSON parse error: %s (body: %s)" message body)
      | `Api_error { code; msg } ->
        Types.Error.Exchange_specific { venue = Venue.t; code; message = msg }
  end

  (** Order builder module *)

  module Builder = struct
    let market_order ~symbol ~side ~qty =
      let side_str = match side with
        | Types.Side.Buy -> `Buy
        | Types.Side.Sell -> `Sell
      in
      ({ instId = symbol
      ; tdMode = `Cash  (* Default to cash mode for spot *)
      ; side = side_str
      ; ordType = "market"
      ; sz = Float.to_string qty
      ; px = None
      ; clOrdId = None
      } : V5.Place_order.request)

    let limit_order ~symbol ~side ~qty ~price =
      let side_str = match side with
        | Types.Side.Buy -> `Buy
        | Types.Side.Sell -> `Sell
      in
      ({ instId = symbol
      ; tdMode = `Cash
      ; side = side_str
      ; ordType = "limit"
      ; sz = Float.to_string qty
      ; px = Some (Float.to_string price)
      ; clOrdId = None
      } : V5.Place_order.request)

    let post_only_order ~symbol ~side ~qty ~price =
      let side_str = match side with
        | Types.Side.Buy -> `Buy
        | Types.Side.Sell -> `Sell
      in
      ({ instId = symbol
      ; tdMode = `Cash
      ; side = side_str
      ; ordType = "post_only"
      ; sz = Float.to_string qty
      ; px = Some (Float.to_string price)
      ; clOrdId = None
      } : V5.Place_order.request)
  end
end

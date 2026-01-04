(** Binance Unified Adapter - Implements Exchange_intf.S *)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Binance
  end

  module Native = struct
    module Order = struct
      type id = int64
      type request = V3.New_order.T.request
      type response = V3.New_order.T.response
      type status = V3.Open_orders.T.order
    end

    module Trade = struct
      type t = V3.My_trades.T.trade
    end

    module Balance = struct
      type t = V3.Account.T.balance
    end

    module Book = struct
      type update = V3.Depth.T.response
      type snapshot = V3.Depth.T.response
    end

    module Ticker = struct
      type t = V3.Ticker_24hr.T.response
    end

    module Public_trade = struct
      type t = V3.Recent_trades.T.trade
    end

    module Symbol_info = struct
      type t = V3.Exchange_info.T.symbol_info
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  let place_order t (req : Native.Order.request) =
    V3.New_order.request t.cfg req >>| function
    | `Ok resp -> Ok resp
    | #Rest.Error.t as e -> Error e

  let cancel_order t (order_id : Native.Order.id) =
    match t.symbols with
    | symbol :: _ ->
      V3.Cancel_order.request t.cfg
        { symbol
        ; orderId = Some order_id
        ; origClientOrderId = None
        ; newClientOrderId = None
        ; recvWindow = None
        }
      >>| (function
      | `Ok _ -> Ok ()
      | #Rest.Error.t as e -> Error e)
    | [] ->
      Deferred.return
        (Error (`Api_error Rest.Error.{ code = -1; msg = "No symbol configured" }))

  let balances t =
    V3.Account.request t.cfg () >>| function
    | `Ok resp -> Ok resp.balances
    | #Rest.Error.t as e -> Error e

  let get_order_status t (order_id : Native.Order.id) =
    match t.symbols with
    | symbol :: _ ->
      V3.Query_order.request t.cfg
        { symbol; orderId = Some order_id; origClientOrderId = None; recvWindow = None }
      >>| (function
        | `Ok resp ->
          let order : V3.Open_orders.T.order =
            { symbol = resp.symbol
            ; orderId = resp.orderId
            ; orderListId = resp.orderListId
            ; clientOrderId = resp.clientOrderId
            ; price = resp.price
            ; origQty = resp.origQty
            ; executedQty = resp.executedQty
            ; cummulativeQuoteQty = resp.cummulativeQuoteQty
            ; status = resp.status
            ; timeInForce = resp.timeInForce
            ; type_ = resp.type_
            ; side = resp.side
            ; stopPrice = resp.stopPrice
            ; icebergQty = resp.icebergQty
            ; time = resp.time
            ; updateTime = resp.updateTime
            ; isWorking = resp.isWorking
            ; origQuoteOrderQty = resp.origQuoteOrderQty
            }
          in
          Ok order
        | #Rest.Error.t as e -> Error e)
    | [] ->
      Deferred.return
        (Error (`Api_error Rest.Error.{ code = -1; msg = "No symbol configured" }))

  let get_open_orders t ?symbol () =
    let symbol = match symbol with Some s -> Some s | None -> List.hd t.symbols in
    V3.Open_orders.request t.cfg { symbol; recvWindow = None }
    >>| function
    | `Ok orders -> Ok orders
    | #Rest.Error.t as e -> Error e

  let get_order_history t ?symbol ?limit () =
    let symbol = match symbol with Some s -> s | None -> List.hd_exn t.symbols in
    V3.All_orders.request t.cfg
      { symbol; orderId = None; startTime = None; endTime = None; limit; recvWindow = None }
    >>| function
    | `Ok orders ->
      (* Convert All_orders.order to Open_orders.order *)
      let convert (o : V3.All_orders.T.order) : V3.Open_orders.T.order =
        { symbol = o.symbol
        ; orderId = o.orderId
        ; orderListId = o.orderListId
        ; clientOrderId = o.clientOrderId
        ; price = o.price
        ; origQty = o.origQty
        ; executedQty = o.executedQty
        ; cummulativeQuoteQty = o.cummulativeQuoteQty
        ; status = o.status
        ; timeInForce = o.timeInForce
        ; type_ = o.type_
        ; side = o.side
        ; stopPrice = o.stopPrice
        ; icebergQty = o.icebergQty
        ; time = o.time
        ; updateTime = o.updateTime
        ; isWorking = o.isWorking
        ; origQuoteOrderQty = o.origQuoteOrderQty
        }
      in
      Ok (List.map orders ~f:convert)
    | #Rest.Error.t as e -> Error e

  let get_my_trades t ~symbol ?limit () =
    V3.My_trades.request t.cfg
      { symbol; orderId = None; startTime = None; endTime = None; fromId = None; limit; recvWindow = None }
    >>| function
    | `Ok trades -> Ok trades
    | #Rest.Error.t as e -> Error e

  let get_symbols t () =
    V3.Exchange_info.request t.cfg { symbol = None }
    >>| function
    | `Ok info -> Ok info.symbols
    | #Rest.Error.t as e -> Error e

  let get_ticker t ~symbol () =
    V3.Ticker_24hr.request t.cfg { symbol }
    >>| function
    | `Ok ticker -> Ok ticker
    | #Rest.Error.t as e -> Error e

  let get_order_book t ~symbol ?limit () =
    V3.Depth.request t.cfg { symbol; limit }
    >>| function
    | `Ok depth -> Ok depth
    | #Rest.Error.t as e -> Error e

  let get_recent_trades t ~symbol ?limit () =
    V3.Recent_trades.request t.cfg { symbol; limit }
    >>| function
    | `Ok trades -> Ok trades
    | #Rest.Error.t as e -> Error e

  let cancel_all_orders t ?symbol () =
    match symbol with
    | Some sym ->
      V3.Cancel_all_orders.request t.cfg { symbol = sym; recvWindow = None }
      >>| (function
        | `Ok orders -> Ok (List.length orders)
        | #Rest.Error.t as e -> Error e)
    | None ->
      match t.symbols with
      | sym :: _ ->
        V3.Cancel_all_orders.request t.cfg { symbol = sym; recvWindow = None }
        >>| (function
          | `Ok orders -> Ok (List.length orders)
          | #Rest.Error.t as e -> Error e)
      | [] ->
        Deferred.return
          (Error (`Api_error Rest.Error.{ code = -1; msg = "No symbol configured" }))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (resp : Native.Order.response) : Types.Order.t =
      let side =
        match resp.side with
        | "BUY" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let kind =
        match resp.type_ with
        | "LIMIT" -> Types.Order_kind.Limit (Float.of_string resp.price)
        | "MARKET" -> Types.Order_kind.Market
        | _ -> Types.Order_kind.Market
      in
      let status =
        match resp.status with
        | "FILLED" -> Types.Order_status.Filled
        | "PARTIALLY_FILLED" -> Types.Order_status.Partially_filled
        | "CANCELED" -> Types.Order_status.Canceled
        | "REJECTED" -> Types.Order_status.Rejected "Order rejected"
        | _ -> Types.Order_status.New
      in
      { venue = Venue.t
      ; id = Int64.to_string resp.orderId
      ; symbol = resp.symbol
      ; side
      ; kind
      ; qty = Float.of_string resp.origQty
      ; filled = Float.of_string resp.executedQty
      ; status
      ; created_at =
          (match Int64.(resp.transactTime > 0L) with
          | true ->
            Some
              (Time_float_unix.of_span_since_epoch
                 (Time_float_unix.Span.of_ms (Int64.to_float resp.transactTime)))
          | false -> None)
      ; updated_at = None
      }

    let order_status (status : Native.Order.status) : Types.Order_status.t =
      match status.status with
      | "FILLED" -> Types.Order_status.Filled
      | "PARTIALLY_FILLED" -> Types.Order_status.Partially_filled
      | "CANCELED" -> Types.Order_status.Canceled
      | "REJECTED" -> Types.Order_status.Rejected "Order rejected"
      | _ -> Types.Order_status.New

    let order_from_status (status : Native.Order.status) : Types.Order.t =
      let side =
        match status.side with
        | "BUY" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let kind =
        match status.type_ with
        | "LIMIT" -> Types.Order_kind.Limit (Float.of_string status.price)
        | "MARKET" -> Types.Order_kind.Market
        | _ -> Types.Order_kind.Market
      in
      let order_status =
        match status.status with
        | "FILLED" -> Types.Order_status.Filled
        | "PARTIALLY_FILLED" -> Types.Order_status.Partially_filled
        | "CANCELED" -> Types.Order_status.Canceled
        | "REJECTED" -> Types.Order_status.Rejected "Order rejected"
        | _ -> Types.Order_status.New
      in
      { venue = Venue.t
      ; id = Int64.to_string status.orderId
      ; symbol = status.symbol
      ; side
      ; kind
      ; qty = Float.of_string status.origQty
      ; filled = Float.of_string status.executedQty
      ; status = order_status
      ; created_at =
          (match Int64.(status.time > 0L) with
          | true ->
            Some
              (Time_float_unix.of_span_since_epoch
                 (Time_float_unix.Span.of_ms (Int64.to_float status.time)))
          | false -> None)
      ; updated_at =
          (match Int64.(status.updateTime > 0L) with
          | true ->
            Some
              (Time_float_unix.of_span_since_epoch
                 (Time_float_unix.Span.of_ms (Int64.to_float status.updateTime)))
          | false -> None)
      }

    let trade (t : Native.Trade.t) : Types.Trade.t =
      let side = match t.isBuyer with true -> Types.Side.Buy | false -> Types.Side.Sell in
      { venue = Venue.t
      ; symbol = t.symbol
      ; side
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.qty
      ; fee = Some (Float.of_string t.commission)
      ; trade_id = Some (Int64.to_string t.id)
      ; ts =
          Some
            (Time_float_unix.of_span_since_epoch
               (Time_float_unix.Span.of_ms (Int64.to_float t.time)))
      }

    let balance (b : Native.Balance.t) : Types.Balance.t =
      let free = Float.of_string b.free in
      let locked = Float.of_string b.locked in
      { venue = Venue.t
      ; currency = b.asset
      ; total = free +. locked
      ; available = free
      ; locked
      }

    let book_update (depth : Native.Book.update) : Types.Book_update.t =
      let levels =
        List.map depth.bids ~f:(fun (price, qty) ->
          { Types.Book_update.price = Float.of_string price
          ; qty = Float.of_string qty
          })
      in
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info (s : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = s.symbol
      ; base_currency = s.baseAsset
      ; quote_currency = s.quoteAsset
      ; status = s.status
      ; min_order_size = 0.0
      ; tick_size = None
      ; quote_increment = None
      }

    let ticker (t : Native.Ticker.t) : Types.Ticker.t =
      { venue = Venue.t
      ; symbol = t.symbol
      ; last_price = Float.of_string t.lastPrice
      ; bid_price = Float.of_string t.bidPrice
      ; ask_price = Float.of_string t.askPrice
      ; high_24h = Float.of_string t.highPrice
      ; low_24h = Float.of_string t.lowPrice
      ; volume_24h = Float.of_string t.volume
      ; quote_volume = Some (Float.of_string t.quoteVolume)
      ; price_change = Some (Float.of_string t.priceChange)
      ; price_change_pct = Some (Float.of_string t.priceChangePercent)
      ; ts = Some (Time_float_unix.of_span_since_epoch
                    (Time_float_unix.Span.of_ms (Int64.to_float t.closeTime)))
      }

    let order_book (depth : Native.Book.snapshot) : Types.Order_book.t =
      let bids = List.map depth.bids ~f:(fun (price, qty) ->
        { Types.Order_book.Price_level.price = Float.of_string price
        ; volume = Float.of_string qty
        })
      in
      let asks = List.map depth.asks ~f:(fun (price, qty) ->
        { Types.Order_book.Price_level.price = Float.of_string price
        ; volume = Float.of_string qty
        })
      in
      { venue = Venue.t
      ; symbol = ""
      ; bids
      ; asks
      ; ts = None
      ; epoch = Int64.to_int_trunc depth.lastUpdateId
      }

    let public_trade (t : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.qty
      ; side = Some (match t.isBuyerMaker with
                    | true -> Types.Side.Sell
                    | false -> Types.Side.Buy)
      ; trade_id = Some (Int64.to_string t.id)
      ; ts = Some (Time_float_unix.of_span_since_epoch
                    (Time_float_unix.Span.of_ms (Int64.to_float t.time)))
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Api_error { code; msg } ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Unauthorized _ -> Types.Error.Auth_failed
      | `Too_many_requests _ -> Types.Error.Rate_limited
      | `Json_parse_error { message; _ } ->
        Types.Error.Transport (Failure message)
      | `Bad_request msg -> Types.Error.Transport (Failure msg)
      | `Not_found -> Types.Error.Transport (Failure "Not found")
      | `Service_unavailable msg -> Types.Error.Transport (Failure msg)
      | `Forbidden msg -> Types.Error.Transport (Failure msg)
  end
end

(** Helper to create order request from normalized types *)
let make_order_request
    ~symbol
    ~(side : Types.Side.t)
    ~(kind : Types.Order_kind.t)
    ~(qty : Types.Qty.t)
  : V3.New_order.T.request =
  let binance_side : Common.Side.t =
    match side with
    | Types.Side.Buy -> `BUY
    | Types.Side.Sell -> `SELL
  in
  let order_type, price =
    match kind with
    | Types.Order_kind.Market -> (`MARKET : Common.Order_type.t), None
    | Types.Order_kind.Limit p ->
      (`LIMIT : Common.Order_type.t), Some (Float.to_string p)
    | Types.Order_kind.Post_only_limit p ->
      (`LIMIT_MAKER : Common.Order_type.t), Some (Float.to_string p)
  in
  V3.New_order.T.
    { symbol
    ; side = binance_side
    ; order_type
    ; quantity = Some (Float.to_string qty)
    ; quoteOrderQty = None
    ; price
    ; newClientOrderId = None
    ; timeInForce =
        (match kind with
        | Types.Order_kind.Market -> None
        | _ -> Some (`GTC : Common.Time_in_force.t))
    ; stopPrice = None
    ; icebergQty = None
    ; newOrderRespType = None
    ; recvWindow = None
    }

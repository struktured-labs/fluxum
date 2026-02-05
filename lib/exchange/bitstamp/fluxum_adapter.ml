(** Bitstamp Exchange Adapter - Exchange_intf.S implementation *)

open Core
open Async

module Native_types = Types
module Types = Fluxum.Types

module Adapter = struct
  type t =
    { cfg : Cfg.t
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Bitstamp
  end

  module Native = struct
    module Order = struct
      type id = string
      type request = [ `Buy_limit | `Sell_limit | `Buy_market | `Sell_market ]
        * string * float * float option  (* side_type, pair, amount, price option *)
      type response = Native_types.order_response
      type status = Native_types.open_order
    end

    module Trade = struct
      type t = Native_types.trade
    end

    module Balance = struct
      type t = Native_types.balance
    end

    module Book = struct
      type update = Native_types.order_book
      type snapshot = Native_types.order_book
    end

    module Ticker = struct
      type t = Native_types.ticker
    end

    module Public_trade = struct
      type t = Native_types.trade
    end

    module Candle = struct
      type t = unit  (* Bitstamp OHLC - TODO: implement *)
    end

    module Symbol_info = struct
      type t = Native_types.trading_pair_info
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  let place_order t (req : Native.Order.request) =
    let (side_type, pair, amount, price) = req in
    match side_type, price with
    | `Buy_limit, Some price ->
      Rest.buy_limit_order ~cfg:t.cfg ~pair ~amount ~price ()
    | `Sell_limit, Some price ->
      Rest.sell_limit_order ~cfg:t.cfg ~pair ~amount ~price ()
    | `Buy_market, _ ->
      Rest.buy_market_order ~cfg:t.cfg ~pair ~amount
    | `Sell_market, _ ->
      Rest.sell_market_order ~cfg:t.cfg ~pair ~amount
    | `Buy_limit, None ->
      Deferred.return (Error (`Api_error "Limit buy requires price"))
    | `Sell_limit, None ->
      Deferred.return (Error (`Api_error "Limit sell requires price"))

  let cancel_order t ~symbol:_ ~order_id =
    Rest.cancel_order ~cfg:t.cfg ~order_id >>| function
    | Ok _id -> Ok ()
    | Error e -> Error e

  let balances t =
    Rest.balance ~cfg:t.cfg >>| function
    | Ok bal -> Ok [bal]
    | Error e -> Error e

  let get_order_status _t ~symbol:_ ~order_id:_ =
    Deferred.return (Error (`Api_error "Bitstamp: individual order status not supported"))

  let get_open_orders t ?symbol () =
    match symbol with
    | Some pair ->
      Rest.open_orders ~cfg:t.cfg ~pair >>| (function
      | Ok orders -> Ok orders
      | Error e -> Error e)
    | None ->
      Rest.open_orders_all ~cfg:t.cfg >>| (function
      | Ok orders -> Ok orders
      | Error e -> Error e)

  let get_order_history _t ?symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Bitstamp: use user_transactions for order history"))

  let get_my_trades t ~symbol ?limit () =
    let limit = Option.value limit ~default:100 in
    Rest.user_transactions ~cfg:t.cfg ~pair:symbol ~limit () >>| function
    | Ok txs ->
      (* Filter to only trade transactions (type_ = 2) *)
      Ok (List.filter txs ~f:(fun tx -> tx.Native_types.type_ = 2))
    | Error e -> Error e

  let get_symbols t () =
    Rest.trading_pairs_info ~cfg:t.cfg

  let get_ticker t ~symbol () =
    Rest.ticker ~cfg:t.cfg ~pair:symbol

  let get_order_book t ~symbol ?limit () =
    let group = match limit with Some _ -> 1 | None -> 1 in
    Rest.order_book ~cfg:t.cfg ~pair:symbol ~group ()

  let get_recent_trades t ~symbol ?limit () =
    ignore limit;
    Rest.transactions ~cfg:t.cfg ~pair:symbol ()

  let cancel_all_orders t ?symbol:_ () =
    Rest.cancel_all_orders ~cfg:t.cfg >>| function
    | Ok _ -> Ok 0
    | Error e -> Error e

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    (* TODO: Implement using Bitstamp OHLC endpoint *)
    Deferred.return (Error (`Api_error "Bitstamp candles not yet implemented"))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    open Fluxum.Normalize_common

    let trade (t : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Float_conv.price_of_string t.price in
      let%bind qty = Float_conv.qty_of_string t.amount in
      let side = match t.type_ with
        | 0 -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let ts = Some (Time_float_unix.of_span_since_epoch
        (Time_float_unix.Span.of_sec (Float.of_string t.date))) in
      Ok ({ venue = Venue.t
          ; symbol = ""
          ; side
          ; price
          ; qty
          ; fee = None
          ; trade_id = Some (Int.to_string t.tid)
          ; ts
          } : Types.Trade.t)

    let balance (b : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      (* Bitstamp returns per-currency fields; extract BTC as example *)
      let extract_currency name available_opt balance_opt reserved_opt =
        let avail = Option.value_map available_opt ~default:0.0 ~f:(fun s ->
          Option.value ~default:0.0 (Float.of_string_opt s)) in
        let total = Option.value_map balance_opt ~default:0.0 ~f:(fun s ->
          Option.value ~default:0.0 (Float.of_string_opt s)) in
        let locked = Option.value_map reserved_opt ~default:0.0 ~f:(fun s ->
          Option.value ~default:0.0 (Float.of_string_opt s)) in
        { Types.Balance.venue = Venue.t; currency = name;
          total; available = avail; locked }
      in
      (* Return first non-zero balance found *)
      let balances = [
        extract_currency "USD" b.usd_available b.usd_balance b.usd_reserved;
        extract_currency "BTC" b.btc_available b.btc_balance b.btc_reserved;
        extract_currency "EUR" b.eur_available b.eur_balance b.eur_reserved;
        extract_currency "ETH" b.eth_available b.eth_balance b.eth_reserved;
      ] in
      match List.find balances ~f:(fun b -> Float.(b.total > 0.0)) with
      | Some bal -> Ok bal
      | None -> Ok (List.hd_exn balances)

    let order_book (book : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let bids = List.map book.bids ~f:(fun (price, volume) ->
        ({ Types.Order_book.Price_level.price; volume } : Types.Order_book.Price_level.t)
      ) in
      let asks = List.map book.asks ~f:(fun (price, volume) ->
        ({ Types.Order_book.Price_level.price; volume } : Types.Order_book.Price_level.t)
      ) in
      Ok ({ venue = Venue.t; symbol = ""; bids; asks; ts = None; epoch = 0 } : Types.Order_book.t)

    let symbol_info (s : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      let parts = String.split s.name ~on:'/' in
      let base_currency = match parts with x :: _ -> x | [] -> "" in
      let quote_currency = match parts with _ :: y :: _ -> y | _ -> "" in
      Ok ({ venue = Venue.t
          ; symbol = s.url_symbol
          ; base_currency
          ; quote_currency
          ; status = s.trading
          ; min_order_size = Option.value ~default:0.0 (Float.of_string_opt s.minimum_order)
          ; tick_size = None
          ; quote_increment = None
          } : Types.Symbol_info.t)

    let ticker (t : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind last_price = Float_conv.price_of_string t.last in
      let%bind bid_price = Float_conv.price_of_string t.bid in
      let%bind ask_price = Float_conv.price_of_string t.ask in
      let%bind high_24h = Float_conv.price_of_string t.high in
      let%bind low_24h = Float_conv.price_of_string t.low in
      let%bind volume_24h = Float_conv.qty_of_string t.volume in
      let%bind vwap = Float_conv.of_string t.vwap in
      let ts = Some (Time_float_unix.of_span_since_epoch
        (Time_float_unix.Span.of_sec (Float.of_string t.timestamp))) in
      Ok ({ venue = Venue.t
          ; symbol = ""
          ; last_price
          ; bid_price
          ; ask_price
          ; high_24h
          ; low_24h
          ; volume_24h
          ; quote_volume = Some (volume_24h *. vwap)
          ; price_change = None
          ; price_change_pct = None
          ; ts
          } : Types.Ticker.t)

    let public_trade (t : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Float_conv.price_of_string t.price in
      let%bind qty = Float_conv.qty_of_string t.amount in
      let side = Some (match t.type_ with
        | 0 -> Types.Side.Buy
        | _ -> Types.Side.Sell)
      in
      let ts = Some (Time_float_unix.of_span_since_epoch
        (Time_float_unix.Span.of_sec (Float.of_string t.date))) in
      Ok ({ venue = Venue.t
          ; symbol = ""
          ; price
          ; qty
          ; side
          ; trade_id = Some (Int.to_string t.tid)
          ; ts
          } : Types.Public_trade.t)

    let order_from_status (o : Native.Order.status) : (Types.Order.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Float_conv.price_of_string o.price in
      let%bind qty = Float_conv.qty_of_string o.amount in
      let side = match o.type_ with
        | 0 -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      Ok ({ venue = Venue.t
          ; id = o.id
          ; symbol = o.currency_pair
          ; side
          ; kind = Types.Order_kind.limit price
          ; time_in_force = Types.Time_in_force.GTC
          ; qty
          ; filled = 0.0
          ; status = Types.Order_status.New
          ; created_at = None
          ; updated_at = None
          } : Types.Order.t)

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "Bitstamp candle normalization not yet implemented"

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Transport (Failure msg)
      | `Api_error msg ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = "API"; message = msg }
      | `Network msg ->
        Types.Error.Transport (Failure msg)
  end
end

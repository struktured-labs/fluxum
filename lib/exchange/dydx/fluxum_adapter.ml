(** dYdX v4 Fluxum Adapter

    Implements the unified Exchange_intf.S interface for dYdX v4.
    Note: dYdX is a decentralized exchange - trading operations require
    wallet signatures via the Validator Client, not REST API keys.
    Public market data endpoints are fully supported.
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = Rest
module Common = Common
module Ws = Ws

(* Suppress unused field and value warnings *)
[@@@warning "-32-69"]

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Dydx
  end

  module Native = struct
    module Order = struct
      type id = string  (* dYdX order id *)
      type request = unit  (* Trading not implemented via REST *)
      type response = unit
      type status = unit
    end

    module Trade = struct
      type t = Rest.Types.trade
    end

    module Balance = struct
      type t = unit  (* Requires subaccount query *)
    end

    module Book = struct
      type update = Ws.orderbook_contents
      type snapshot = Rest.Types.orderbook
    end

    module Ticker = struct
      type t = string * Rest.Types.perpetual_market  (* ticker, market info *)
    end

    module Public_trade = struct
      type t = Rest.Types.trade
    end

    module Symbol_info = struct
      type t = string * Rest.Types.perpetual_market  (* ticker, market info *)
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  (* Trading operations - not supported via REST (requires wallet signing) *)
  let place_order (_ : t) (_ : Native.Order.request) =
    Deferred.return (Error (`Api_error "Trading requires wallet signing - use dYdX TypeScript/Python SDK"))

  let cancel_order (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "Trading requires wallet signing - use dYdX TypeScript/Python SDK"))

  let balances (_ : t) =
    Deferred.return (Error (`Api_error "Balances require subaccount query with address"))

  let get_order_status (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "Order status requires subaccount address"))

  let get_open_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "Open orders require subaccount address"))

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Order history requires subaccount address"))

  let get_my_trades (_ : t) ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "User trades require subaccount address"))

  let cancel_all_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "Trading requires wallet signing - use dYdX TypeScript/Python SDK"))

  (* Public market data operations - fully supported *)
  let get_symbols (t : t) () =
    let cfg = t.cfg in
    Rest.markets cfg () >>| function
    | Error _ as err -> err
    | Ok resp -> Ok resp.markets

  let get_ticker (t : t) ~symbol () =
    let cfg = t.cfg in
    Rest.ticker cfg ~market:symbol >>| function
    | Error _ as err -> err
    | Ok (Some ticker) -> Ok ticker
    | Ok None -> Error `Not_found

  let get_order_book (t : t) ~symbol ?limit:_ () =
    let cfg = t.cfg in
    Rest.orderbook cfg ~market:symbol >>| function
    | Error _ as err -> err
    | Ok book -> Ok book

  let get_recent_trades (t : t) ~symbol ?limit () =
    let cfg = t.cfg in
    Rest.trades cfg ~market:symbol ?limit () >>| function
    | Error _ as err -> err
    | Ok trades -> Ok trades

  module Streams = struct
    let trades (t : t) =
      (* Use WebSocket trade stream *)
      let (module Cfg) = t.cfg in
      let%bind conn = Ws.connect ~url:Cfg.ws_url in
      let%bind message_pipe = Ws.create_message_pipe ~conn in
      (* Subscribe to all configured symbols *)
      let%bind () = Deferred.List.iter t.symbols ~how:`Sequential ~f:(fun symbol ->
        Ws.Trades.subscribe ~conn ~market:symbol ()
      ) in
      (* Filter and transform messages to trades *)
      let trade_pipe = Pipe.filter_map message_pipe ~f:(fun json ->
        match Ws.Trades.parse_message json with
        | Ok (_, contents) ->
          (* Return first trade from batch *)
          List.hd contents.trades
        | Error _ -> None
      ) in
      return trade_pipe

    let book_updates (t : t) =
      let (module Cfg) = t.cfg in
      let%bind conn = Ws.connect ~url:Cfg.ws_url in
      let%bind message_pipe = Ws.create_message_pipe ~conn in
      let%bind () = Deferred.List.iter t.symbols ~how:`Sequential ~f:(fun symbol ->
        Ws.Orderbook.subscribe ~conn ~market:symbol ()
      ) in
      let book_pipe = Pipe.filter_map message_pipe ~f:(fun json ->
        match Ws.Orderbook.parse_message json with
        | Ok (_, contents) -> Some contents
        | Error _ -> None
      ) in
      return book_pipe
  end

  module Normalize = struct
    let side_of_string s =
      match String.uppercase s with
      | "BUY" -> Types.Side.Buy
      | "SELL" -> Types.Side.Sell
      | _ -> Types.Side.Buy

    let order_response (() : Native.Order.response) : Types.Order.t =
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

    let order_status (() : Native.Order.status) : Types.Order_status.t =
      Types.Order_status.New

    let order_from_status (() : Native.Order.status) : Types.Order.t =
      order_response ()

    let trade (tr : Native.Trade.t) : Types.Trade.t =
      let side = side_of_string tr.side in
      let price = Float.of_string tr.price in
      let qty = Float.of_string tr.size in
      let ts =
        try
          Some (Time_float_unix.of_string tr.createdAt)
        with _ -> None
      in
      { venue = Venue.t
      ; symbol = ""  (* market is not in trade struct *)
      ; side
      ; price
      ; qty
      ; fee = None
      ; trade_id = Some tr.id
      ; ts
      }

    let balance (() : Native.Balance.t) : Types.Balance.t =
      { venue = Venue.t
      ; currency = ""
      ; total = 0.
      ; available = 0.
      ; locked = 0.
      }

    let book_update (u : Native.Book.update) : Types.Book_update.t =
      (* Convert orderbook contents to book update *)
      let bid_levels = List.map u.bids ~f:(fun level ->
        { Types.Book_update.price = Float.of_string level.price
        ; qty = Float.of_string level.size
        })
      in
      let ask_levels = List.map u.asks ~f:(fun level ->
        { Types.Book_update.price = Float.of_string level.price
        ; qty = Float.of_string level.size
        })
      in
      (* Return bids as the update - full book is in snapshot *)
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels = bid_levels @ List.map ask_levels ~f:(fun l ->
          { Types.Book_update.price = l.price; qty = l.qty })
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info ((ticker, market) : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = ticker
      ; base_currency = Rest.Types.base_asset_of_ticker ticker
      ; quote_currency = Rest.Types.quote_asset_of_ticker ticker
      ; status = market.status
      ; min_order_size = Float.of_string market.stepSize
      ; tick_size = Some (Float.of_string market.tickSize)
      ; quote_increment = None
      }

    let ticker ((tick, market) : Native.Ticker.t) : Types.Ticker.t =
      let oracle_price =
        Option.value_map market.oraclePrice ~default:0.0 ~f:Float.of_string
      in
      let volume_24h =
        Option.value_map market.volume24H ~default:0.0 ~f:Float.of_string
      in
      let price_change =
        Option.map market.priceChange24H ~f:Float.of_string
      in
      { venue = Venue.t
      ; symbol = tick
      ; last_price = oracle_price  (* Use oracle price as proxy *)
      ; bid_price = 0.0  (* Not available in market info *)
      ; ask_price = 0.0
      ; high_24h = 0.0
      ; low_24h = 0.0
      ; volume_24h
      ; quote_volume = None
      ; price_change = price_change
      ; price_change_pct = None
      ; ts = None
      }

    let order_book (book : Native.Book.snapshot) : Types.Order_book.t =
      let bids = List.map book.bids ~f:(fun level ->
        { Types.Order_book.Price_level.price = Float.of_string level.price
        ; volume = Float.of_string level.size
        })
      in
      let asks = List.map book.asks ~f:(fun level ->
        { Types.Order_book.Price_level.price = Float.of_string level.price
        ; volume = Float.of_string level.size
        })
      in
      { venue = Venue.t
      ; symbol = ""
      ; bids
      ; asks
      ; ts = None
      ; epoch = 0
      }

    let public_trade (tr : Native.Public_trade.t) : Types.Public_trade.t =
      let side = match String.uppercase tr.side with
        | "BUY" -> Some Types.Side.Buy
        | "SELL" -> Some Types.Side.Sell
        | _ -> None
      in
      let ts =
        try
          Some (Time_float_unix.of_string tr.createdAt)
        with _ -> None
      in
      { venue = Venue.t
      ; symbol = ""
      ; price = Float.of_string tr.price
      ; qty = Float.of_string tr.size
      ; side
      ; trade_id = Some tr.id
      ; ts
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message = msg }
      | `Api_error msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "api"; message = msg }
      | `Not_found ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "404"; message = "not_found" }
  end
end

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol:_ ~side:_ ~kind:_ ~qty:_ =
    (* dYdX trading requires wallet signing, not REST API *)
    ()
end

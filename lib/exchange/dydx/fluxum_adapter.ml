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

    module Candle = struct
      type t = unit  (* dYdX candles - TODO: implement *)
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

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "dYdX candles not yet implemented"))

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
    (* Use shared normalize function *)
    let side_of_string s = Fluxum.Normalize_common.Side.of_string_exn s

    let order_response (_ : Native.Order.response) : (Types.Order.t, string) Result.t =
      Error "dYdX order response normalization requires wallet signing - not implemented"

    let order_status (_ : Native.Order.status) : (Types.Order_status.t, string) Result.t =
      Error "dYdX order status normalization requires wallet signing - not implemented"

    let order_from_status (_ : Native.Order.status) : (Types.Order.t, string) Result.t =
      Error "dYdX order normalization requires wallet signing - not implemented"

    let trade (tr : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string tr.price in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string tr.size in
      let side = side_of_string tr.side in
      let ts =
        try
          Some (Time_float_unix.of_string tr.createdAt)
        with _ -> None
      in
      Ok ({ venue = Venue.t
      ; symbol = ""  (* market is not in trade struct *)
      ; side
      ; price
      ; qty
      ; fee = None
      ; trade_id = Some tr.id
      ; ts
      } : Types.Trade.t)

    let balance (_ : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      Error "dYdX balance normalization requires wallet signing - not implemented"

    let book_update (u : Native.Book.update) : (Types.Book_update.t, string) Result.t =
      let open Result.Let_syntax in
      (* Convert orderbook contents to book update *)
      let parse_level (lvl : Ws.price_level) =
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string lvl.price in
        let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string lvl.size in
        Ok { Types.Book_update.price; qty }
      in
      let%bind bid_levels =
        u.bids
        |> List.map ~f:parse_level
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      let%bind ask_levels =
        u.asks
        |> List.map ~f:parse_level
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      (* Return bids as the update - full book is in snapshot *)
      Ok ({ venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels = bid_levels @ ask_levels
      ; ts = None
      ; is_snapshot = true
      } : Types.Book_update.t)

    let symbol_info ((ticker, market) : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind min_order_size = Fluxum.Normalize_common.Float_conv.qty_of_string market.stepSize in
      let%bind tick_size = Fluxum.Normalize_common.Float_conv.price_of_string market.tickSize in
      Ok ({ venue = Venue.t
      ; symbol = ticker
      ; base_currency = Rest.Types.base_asset_of_ticker ticker
      ; quote_currency = Rest.Types.quote_asset_of_ticker ticker
      ; status = market.status
      ; min_order_size
      ; tick_size = Some tick_size
      ; quote_increment = None
      } : Types.Symbol_info.t)

    let ticker ((tick, market) : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let parse_opt_price opt_str =
        match opt_str with
        | None -> Ok 0.0
        | Some s -> Fluxum.Normalize_common.Float_conv.price_of_string s
      in
      let%bind oracle_price = parse_opt_price market.oraclePrice in
      let%bind volume_24h =
        match market.volume24H with
        | None -> Ok 0.0
        | Some s -> Fluxum.Normalize_common.Float_conv.qty_of_string s
      in
      let%bind price_change =
        match market.priceChange24H with
        | None -> Ok None
        | Some s ->
          let%bind p = Fluxum.Normalize_common.Float_conv.of_string s in
          Ok (Some p)
      in
      Ok ({ venue = Venue.t
      ; symbol = tick
      ; last_price = oracle_price  (* Use oracle price as proxy *)
      ; bid_price = 0.0  (* Not available in market info *)
      ; ask_price = 0.0
      ; high_24h = 0.0
      ; low_24h = 0.0
      ; volume_24h
      ; quote_volume = None
      ; price_change
      ; price_change_pct = None
      ; ts = None
      } : Types.Ticker.t)

    let order_book (book : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let open Result.Let_syntax in
      let parse_level (lvl : Rest.Types.price_level) =
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string lvl.price in
        let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string lvl.size in
        Ok { Types.Order_book.Price_level.price; volume }
      in
      let%bind bids =
        book.bids
        |> List.map ~f:parse_level
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      let%bind asks =
        book.asks
        |> List.map ~f:parse_level
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      Ok ({ venue = Venue.t
      ; symbol = ""
      ; bids
      ; asks
      ; ts = None
      ; epoch = 0
      } : Types.Order_book.t)

    let public_trade (tr : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string tr.price in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string tr.size in
      let side =
        match Fluxum.Normalize_common.Side.of_string tr.side with
        | Ok s -> Some s
        | Error _ -> None
      in
      let ts =
        try
          Some (Time_float_unix.of_string tr.createdAt)
        with _ -> None
      in
      Ok ({ venue = Venue.t
      ; symbol = ""
      ; price
      ; qty
      ; side
      ; trade_id = Some tr.id
      ; ts
      } : Types.Public_trade.t)

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "dYdX candle normalization not yet implemented"

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

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module Rest = Rest
module Ws = Ws
module Order_book = Order_book

(** Hyperliquid Exchange Adapter

    Note: Hyperliquid is a decentralized perpetual futures exchange.
    Trading operations (place/cancel orders) require blockchain transactions
    and are not yet implemented. This adapter provides:

    - Market data (order books, trades, tickers)
    - Account queries (positions, balances, open orders, fills)
    - WebSocket streams
*)

module Adapter = struct
  type t =
    { cfg : Cfg.t
    ; user : string option  (* Hyperliquid wallet address for account queries *)
    ; symbols : string list
    }

  let create ~cfg ?user ?(symbols = []) () =
    { cfg; user; symbols }

  module Venue = struct
    let t = Types.Venue.Hyperliquid
  end

  module Native = struct
    module Order = struct
      type id = int64  (* oid - order ID *)
      type request = unit  (* Not implemented - requires blockchain tx *)
      type response = Rest.Types.open_order
      type status = Rest.Types.open_order
    end

    module Trade = struct
      type t = Rest.Types.user_fill
    end

    module Balance = struct
      (* Hyperliquid returns account state with positions *)
      type t = Rest.Types.clearinghouse_state
    end

    module Book = struct
      type update =
        { coin : string
        ; side : [`Bid | `Ask]
        ; levels : (float * float) list  (* price, size *)
        ; time : int64
        }
      type snapshot = Rest.Types.l2_book
    end

    module Ticker = struct
      (* Hyperliquid doesn't have 24hr ticker, use asset_ctx from meta *)
      type t = string * Rest.Types.asset_ctx  (* coin, ctx *)
    end

    module Public_trade = struct
      type t = Rest.Types.trade
    end

    module Symbol_info = struct
      type t = Rest.Types.universe_item
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  (* ============================================================ *)
  (* Trading Operations - Not Implemented (requires blockchain) *)
  (* ============================================================ *)

  let place_order (_ : t) (_ : Native.Order.request) =
    Deferred.return (Error (`Api_error
      "Hyperliquid order placement not implemented - requires blockchain transaction"))

  let cancel_order (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error
      "Hyperliquid order cancellation not implemented - requires blockchain transaction"))

  let cancel_all_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error
      "Hyperliquid cancel_all not implemented - requires blockchain transaction"))

  (* ============================================================ *)
  (* Account/Position Queries *)
  (* ============================================================ *)

  let balances (t : t) =
    match t.user with
    | None -> Deferred.return (Error (`Api_error "User address required for balance queries"))
    | Some user ->
      Rest.clearinghouse_state t.cfg ~user
      >>| function
      | Ok state -> Ok [state]  (* Wrap in list for consistency *)
      | Error e -> Error e

  let get_order_status (t : t) (oid : Native.Order.id) =
    match t.user with
    | None -> Deferred.return (Error (`Api_error "User address required"))
    | Some user ->
      Rest.open_orders t.cfg ~user
      >>| function
      | Ok orders ->
        (match List.find orders ~f:(fun o -> Int64.equal o.Rest.Types.oid oid) with
         | Some order -> Ok order
         | None -> Error (`Api_error (sprintf "Order %Ld not found" oid)))
      | Error e -> Error e

  let get_open_orders (t : t) ?symbol:_ () =
    match t.user with
    | None -> Deferred.return (Error (`Api_error "User address required"))
    | Some user ->
      Rest.open_orders t.cfg ~user
      >>| function
      | Ok orders -> Ok orders
      | Error e -> Error e

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    (* Hyperliquid doesn't have separate order history - only fills *)
    Deferred.return (Error (`Api_error
      "Hyperliquid order history not available - use get_my_trades for fills"))

  let get_my_trades (t : t) ~symbol:_ ?limit () =
    match t.user with
    | None -> Deferred.return (Error (`Api_error "User address required"))
    | Some user ->
      Rest.user_fills t.cfg ~user
      >>| function
      | Ok fills ->
        let limited = match limit with
          | Some n -> List.take fills n
          | None -> fills
        in
        Ok limited
      | Error e -> Error e

  (* ============================================================ *)
  (* Public Market Data *)
  (* ============================================================ *)

  let get_symbols (t : t) () =
    Rest.meta t.cfg
    >>| function
    | Ok meta -> Ok meta.universe
    | Error e -> Error e

  let get_ticker (t : t) ~symbol () =
    Rest.meta_and_asset_ctxs t.cfg
    >>| function
    | Ok (meta, ctxs) ->
      (* Find the index of the symbol in universe *)
      (match List.findi meta.universe ~f:(fun _ item ->
         String.equal item.Rest.Types.name symbol) with
       | Some (idx, _) ->
         (match List.nth ctxs idx with
          | Some ctx -> Ok (symbol, ctx)
          | None -> Error (`Api_error (sprintf "Context for %s not found" symbol)))
       | None -> Error (`Api_error (sprintf "Symbol %s not found" symbol)))
    | Error e -> Error e

  let get_order_book (t : t) ~symbol ?limit () =
    let _ = limit in  (* Hyperliquid doesn't support limit, ignore *)
    Rest.l2_book t.cfg ~coin:symbol
    >>| function
    | Ok book -> Ok book
    | Error e -> Error e

  let get_recent_trades (t : t) ~symbol ?limit () =
    Rest.recent_trades t.cfg ~coin:symbol
    >>| function
    | Ok trades ->
      let limited = match limit with
        | Some n -> List.take trades n
        | None -> trades
      in
      Ok limited
    | Error e -> Error e

  (* ============================================================ *)
  (* WebSocket Streams *)
  (* ============================================================ *)

  module Streams = struct
    let trades (t : t) =
      let symbols = match t.symbols with
        | [] -> ["BTC"]  (* Default *)
        | s -> s
      in
      let streams = List.map symbols ~f:(fun coin ->
        Ws.Stream.Trades coin) in
      Ws.connect ~streams ()
      >>| function
      | Error _ ->
        let r, _w = Pipe.create () in
        r
      | Ok ws ->
        let messages = Ws.messages ws in
        let reader, writer = Pipe.create () in
        don't_wait_for (
          Pipe.iter messages ~f:(fun msg_str ->
            match Ws.parse_message msg_str with
            | Ws.Message.Trades trades ->
              (* Convert each public trade to user_fill format and write *)
              Deferred.List.iter trades ~how:`Sequential ~f:(fun t ->
                let fill = { Rest.Types.
                  coin = t.Ws.Message.coin
                ; px = t.px
                ; sz = t.sz
                ; side = t.side
                ; time = t.time
                ; startPosition = "0"
                ; dir = t.side
                ; closedPnl = "0"
                ; hash = t.hash
                ; oid = Option.value t.tid ~default:0L
                ; crossed = false
                ; fee = "0"
                ; tid = Option.value t.tid ~default:0L
                ; feeToken = None
                } in
                Pipe.write writer fill)
            | _ -> Deferred.return ())
          >>| fun () -> Pipe.close writer);
        reader

    let book_updates (t : t) =
      let symbols = match t.symbols with
        | [] -> ["BTC"]
        | s -> s
      in
      let streams = List.map symbols ~f:(fun coin ->
        Ws.Stream.L2Book { coin; n_sig_figs = None; mantissa = None }) in
      Ws.connect ~streams ()
      >>| function
      | Error _ ->
        let r, _w = Pipe.create () in
        r
      | Ok ws ->
        let messages = Ws.messages ws in
        let reader, writer = Pipe.create () in
        don't_wait_for (
          Pipe.iter messages ~f:(fun msg_str ->
            match Ws.parse_message msg_str with
            | Ws.Message.L2Book book ->
              (* Split into separate updates for bids and asks *)
              (match book.levels with
               | [bids; asks] ->
                 let bid_levels = List.map bids ~f:(fun level ->
                   (Float.of_string level.Ws.Message.px, Float.of_string level.sz)) in
                 let ask_levels = List.map asks ~f:(fun level ->
                   (Float.of_string level.Ws.Message.px, Float.of_string level.sz)) in
                 let bid_update = { Native.Book.coin = book.coin; side = `Bid; levels = bid_levels; time = book.time } in
                 let ask_update = { Native.Book.coin = book.coin; side = `Ask; levels = ask_levels; time = book.time } in
                 Pipe.write writer bid_update >>= fun () ->
                 Pipe.write writer ask_update
               | _ -> Deferred.return ())
            | _ -> Deferred.return ())
          >>| fun () -> Pipe.close writer);
        reader
  end

  (* ============================================================ *)
  (* Normalization to Fluxum.Types *)
  (* ============================================================ *)

  module Normalize = struct
    let time_of_ms (ms : int64) : Time_float_unix.t =
      Time_float_unix.of_span_since_epoch
        (Time_float_unix.Span.of_ms (Int64.to_float ms))

    let side_of_string (s : string) : Types.Side.t =
      match String.lowercase s with
      | "buy" | "b" -> Types.Side.Buy
      | "sell" | "s" -> Types.Side.Sell
      | _ -> Types.Side.Buy  (* Default *)

    let order_response (_r : Native.Order.response) : Types.Order.t =
      (* Not implemented - Hyperliquid doesn't support trading yet *)
      { venue = Venue.t
      ; id = "0"
      ; symbol = ""
      ; side = Types.Side.Buy
      ; kind = Types.Order_kind.Market
      ; qty = 0.
      ; filled = 0.
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let order_status (_o : Native.Order.status) : Types.Order_status.t =
      (* Open orders are considered New *)
      Types.Order_status.New

    let order_from_status (o : Native.Order.status) : Types.Order.t =
      { venue = Venue.t
      ; id = Int64.to_string o.oid
      ; symbol = o.coin
      ; side = side_of_string o.side
      ; kind = Types.Order_kind.Limit (Float.of_string o.limitPx)
      ; qty = Float.of_string o.sz
      ; filled = 0.  (* Open orders have no filled qty *)
      ; status = Types.Order_status.New
      ; created_at = Some (time_of_ms o.timestamp)
      ; updated_at = Some (time_of_ms o.timestamp)
      }

    let trade (f : Native.Trade.t) : Types.Trade.t =
      { venue = Venue.t
      ; symbol = f.coin
      ; side = side_of_string f.side
      ; price = Float.of_string f.px
      ; qty = Float.of_string f.sz
      ; fee = Some (Float.of_string f.fee)
      ; trade_id = Some (Int64.to_string f.tid)
      ; ts = Some (time_of_ms f.time)
      }

    let balance (state : Native.Balance.t) : Types.Balance.t =
      { venue = Venue.t
      ; currency = "USD"  (* Hyperliquid uses USD-settled perps *)
      ; total = Float.of_string state.marginSummary.accountValue
      ; available = Float.of_string state.withdrawable
      ; locked = Float.of_string state.marginSummary.totalMarginUsed
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
      ; symbol = u.coin
      ; side
      ; levels
      ; ts = Some (time_of_ms u.time)
      ; is_snapshot = true  (* Hyperliquid L2 updates are snapshots *)
      }

    let symbol_info (item : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = item.name
      ; base_currency = item.name  (* Hyperliquid perps use coin as base *)
      ; quote_currency = "USD"
      ; status = "ACTIVE"  (* All in universe are active *)
      ; min_order_size = 0.0  (* Unknown, using 0 *)
      ; tick_size = None
      ; quote_increment =
          (* Size decimals determines increment *)
          Some (Float.of_string (sprintf "0.%s1"
            (String.make item.szDecimals '0')))
      }

    let ticker ((coin, ctx) : Native.Ticker.t) : Types.Ticker.t =
      { venue = Venue.t
      ; symbol = coin
      ; last_price = Float.of_string ctx.markPx
      ; bid_price = Float.of_string ctx.markPx  (* Use mark price as approx *)
      ; ask_price = Float.of_string ctx.markPx
      ; high_24h = Float.of_string ctx.prevDayPx  (* Best approx available *)
      ; low_24h = Float.of_string ctx.prevDayPx
      ; volume_24h = Float.of_string ctx.dayNtlVlm
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = Some (Time_float_unix.now ())
      }

    let order_book (book : Native.Book.snapshot) : Types.Order_book.t =
      let parse_levels (levels : Rest.Types.level list) =
        List.map levels ~f:(fun (level : Rest.Types.level) ->
          { Types.Order_book.Price_level.
            price = Float.of_string level.px
          ; volume = Float.of_string level.sz
          })
      in
      let bid_levels, ask_levels = match book.levels with
        | [bid_list; ask_list] -> (parse_levels bid_list, parse_levels ask_list)
        | _ -> ([], [])
      in
      { venue = Venue.t
      ; symbol = book.coin
      ; bids = bid_levels
      ; asks = ask_levels
      ; ts = Some (time_of_ms book.time)
      ; epoch = 0  (* No sequence number from Hyperliquid *)
      }

    let public_trade (t : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = t.coin
      ; price = Float.of_string t.px
      ; qty = Float.of_string t.sz
      ; side = Some (side_of_string t.side)
      ; trade_id = Some (Int64.to_string t.tid)
      ; ts = Some (time_of_ms t.time)
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue = Venue.t
          ; code = Int.to_string code
          ; message = msg
          }
      | `Json_parse msg ->
        Types.Error.Exchange_specific
          { venue = Venue.t
          ; code = "JSON_PARSE_ERROR"
          ; message = msg
          }
      | `Api_error msg ->
        Types.Error.Exchange_specific
          { venue = Venue.t
          ; code = "API_ERROR"
          ; message = msg
          }
  end
end

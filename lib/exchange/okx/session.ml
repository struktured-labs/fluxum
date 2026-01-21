(** OKX Session - Implements unified Session_intf with auto-reconnecting streams

    Complete session management for OKX exchange with unified V5 API WebSocket support.
    Supports market data, trades, depth updates, and order book tracking.
*)

open Core
open Async

(** Re-export State from interface *)
module State = Fluxum.Session_intf.State

(** Auto-restart utility (from Session_intf) *)
module Auto_restart = struct
  (** Create a pipe that automatically reconnects on EOF *)
  let pipe ~name ~create_pipe () =
    let reader, writer = Pipe.create () in
    let rec restart_loop () =
      Log.Global.info "auto_restart_pipe[%s]: connecting..." name;
      create_pipe () >>= fun source_pipe ->
      Log.Global.info "auto_restart_pipe[%s]: connected, relaying" name;
      Pipe.transfer source_pipe writer ~f:Fn.id >>= fun () ->
      Log.Global.info "auto_restart_pipe[%s]: EOF detected, restarting in 1s" name;
      after (Time_float_unix.Span.of_sec 1.0) >>= fun () ->
      restart_loop ()
    in
    don't_wait_for (restart_loop ());
    reader
end

(** Multi-stream event container *)
module Events = struct
  (** Type aliases for OKX-specific types *)
  type balance = V5.Account_balance.T.balance_detail
  type trade = Ws.Message.trade_data
  type market_event = string  (* Raw WebSocket message *)
  type book = Order_book.Book.t
  type ledger_entry = Ledger.Entry.t
  type order_event = V5.Place_order.T.response  (* Order execution updates *)
  type order_id = string

  type t =
    { symbols : Fluxum.Types.Symbol.t list;
      balance_pipe : balance Pipe.Reader.t;
      trades : trade Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      market_data : market_event Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      order_books : (book, string) Result.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      ledger : ledger_entry Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      order_events_pipe : order_event Pipe.Reader.t;
    }

  let symbols t = t.symbols
  let balance t = t.balance_pipe
  let trades t = t.trades
  let market_data t = t.market_data
  let order_books t = t.order_books
  let ledger t = t.ledger
  let order_events t = t.order_events_pipe

  (** Create session events *)
  let create
      ?(symbols : Fluxum.Types.Symbol.t list = [])
      ?order_ids:_
      ()
    : t Deferred.t =
    Log.Global.info "OKX Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - placeholder for now *)
    let balance_reader, _balance_writer = Pipe.create () in
    let balance_pipe = balance_reader in

    (* Market data pipes - one per symbol with auto-restart *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "okx_market_data[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating OKX WebSocket for %s" symbol;
            let%bind ws_result = Ws.connect
              ~streams:[{ channel = Ws.Stream.Tickers; instId = symbol }]
              ()
            in
            match ws_result with
            | Ok ws -> return (Ws.messages ws)
            | Error err ->
              Log.Global.error "OKX WS connection failed: %s" (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              return reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Order book pipes - one per symbol *)
    let%bind order_books =
      Deferred.List.map ~how:`Parallel symbols ~f:(fun symbol ->
        let%bind pipe = Order_book.Book.pipe ~symbol () in
        return (symbol, pipe)
      )
      >>| Fluxum.Types.Symbol.Map.of_alist_exn
    in

    (* Trades pipes - one per symbol with auto-restart *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "okx_trades[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating OKX trade stream for %s" symbol;
            let%bind ws_result = Ws.connect
              ~streams:[{ channel = Ws.Stream.Trades; instId = symbol }]
              ()
            in
            match ws_result with
            | Ok ws ->
              (* Filter for trade messages only *)
              let trade_pipe = Pipe.filter_map (Ws.messages ws) ~f:(fun msg ->
                match Ws.parse_message msg with
                | Ws.Message.Trades trades -> List.hd trades  (* Safe: returns None if empty *)
                | _ -> None
              ) in
              return trade_pipe
            | Error err ->
              Log.Global.error "OKX trade WS failed: %s" (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              return reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Ledger pipes - one per symbol *)
    let ledger =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let init = Ledger.Entry.create ~symbol () in

          (* Get order book and trade pipes for this symbol *)
          let book_pipe = match Map.find order_books symbol with
            | Some pipe -> pipe
            | None ->
              let reader, _writer = Pipe.create () in
              reader
          in

          let trade_pipe = match Map.find trades symbol with
            | Some pipe -> pipe
            | None ->
              let reader, _writer = Pipe.create () in
              reader
          in

          (* Create ledger entry pipe *)
          let ledger_reader, ledger_writer = Pipe.create () in

          (* Combine book and trade updates *)
          let entry_ref = ref init in

          don't_wait_for (
            (* Update from book prices *)
            Pipe.iter book_pipe ~f:(fun book_result ->
              match book_result with
              | Ok book ->
                entry_ref := Ledger.Entry.on_market_data !entry_ref ~book;
                Pipe.write ledger_writer !entry_ref
              | Error err ->
                Log.Global.error "OKX order book error for %s: %s" symbol err;
                Deferred.unit
            )
          );

          don't_wait_for (
            (* Update from trades *)
            Pipe.iter trade_pipe ~f:(fun trade ->
              match (
                let open Result.Let_syntax in
                let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string trade.px in
                let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string trade.sz in
                let%bind side = Fluxum.Normalize_common.Side.of_string trade.side in
                Ok (price, qty, side)
              ) with
              | Ok (price, qty, side) ->
                entry_ref := Ledger.Entry.on_trade !entry_ref ~price ~side ~qty;
                Pipe.write ledger_writer !entry_ref
              | Error err ->
                Log.Global.error "OKX trade parse error: %s" err;
                Deferred.unit
            )
          );

          (symbol, ledger_reader)
        )
      )
    in

    (* Order events pipe - placeholder (WebSocket private channel needed) *)
    let order_events_reader, _order_events_writer = Pipe.create () in
    let order_events_pipe = order_events_reader in

    return {
      symbols;
      balance_pipe;
      trades;
      market_data;
      order_books;
      ledger;
      order_events_pipe;
    }
end

(** Session state management *)
type t =
  { events : Events.t;
    mutable state : State.t;
    state_changes_reader : State.t Pipe.Reader.t;
    state_changes_writer : State.t Pipe.Writer.t;
  }

(** Create session *)
let create
    ?(symbols : Fluxum.Types.Symbol.t list = [])
    ?order_ids
    ()
  : t Deferred.t =
  Log.Global.info "OKX Session.create: symbols=%d" (List.length symbols);

  (* Create state change pipe *)
  let state_changes_reader, state_changes_writer = Pipe.create () in

  (* Start in Connecting state and emit *)
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);

  (* Create events *)
  let%bind events = Events.create ~symbols ?order_ids () in

  (* Update state to Ready *)
  let state = State.Ready in
  don't_wait_for (Pipe.write state_changes_writer state);

  return {
    events;
    state;
    state_changes_reader;
    state_changes_writer;
  }

(** Get events *)
let events t = t.events

(** Get current state *)
let state t = t.state

(** Get state change notifications *)
let state_changes t = t.state_changes_reader

(** Close session *)
let close t =
  Log.Global.info "OKX Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

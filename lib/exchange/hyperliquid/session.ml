(** Hyperliquid Session - Implements unified Session_intf with auto-reconnecting streams *)

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
  (** Type aliases for Hyperliquid-specific types *)
  type balance = unit
  type trade = Yojson.Safe.t
  type market_event = string
  type book = (Order_book.Book.t, string) Result.t
  type ledger_entry = Ledger.Entry.t
  type order_event = unit
  type order_id = string

  type t =
    { symbols : Fluxum.Types.Symbol.t list;
      balance_pipe : balance Pipe.Reader.t;
      trades : trade list Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      market_data : market_event Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
      order_books : book Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
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
    Log.Global.info "Hyperliquid Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe *)
    let balance_reader, _balance_writer = Pipe.create () in
    let balance_pipe = balance_reader in

    (* Market data pipes *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let reader, _writer = Pipe.create () in
          (symbol, reader)
        )
      )
    in

    (* Order book pipes with auto-restart *)
    let order_books =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "order_book[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating order book for %s" symbol;
            Order_book.Book.pipe ~symbol ~depth:10 ()
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Trades pipes *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let reader, _writer = Pipe.create () in
          (symbol, reader)
        )
      )
    in

    (* Ledger pipes *)
    let ledger =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let init = Ledger.Entry.create ~symbol () in
          let ledger_reader, ledger_writer = Pipe.create () in
          let entry_ref = ref init in

          (* Get order book pipe *)
          let book_pipe = match Map.find order_books symbol with
            | Some pipe -> pipe
            | None ->
              let reader, _writer = Pipe.create () in
              reader
          in

          don't_wait_for (
            Pipe.iter book_pipe ~f:(fun book_result ->
              match book_result with
              | Ok book ->
                let spot = (Order_book.Book.best_bid book).price in
                (match Float.(spot > 0.) with
                 | true ->
                   entry_ref := Ledger.Entry.update_spot !entry_ref spot;
                   Pipe.write ledger_writer !entry_ref
                 | false -> Deferred.unit)
              | Error _ -> Deferred.unit
            )
          );

          (symbol, ledger_reader)
        )
      )
    in

    (* Order events pipe *)
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
  Log.Global.info "Hyperliquid Session.create: symbols=%d" (List.length symbols);

  let state_changes_reader, state_changes_writer = Pipe.create () in
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);

  let%bind events = Events.create ~symbols ?order_ids () in

  let state = State.Ready in
  don't_wait_for (Pipe.write state_changes_writer state);

  return {
    events;
    state;
    state_changes_reader;
    state_changes_writer;
  }

let events t = t.events
let state t = t.state
let state_changes t = t.state_changes_reader

let close t =
  Log.Global.info "Hyperliquid Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

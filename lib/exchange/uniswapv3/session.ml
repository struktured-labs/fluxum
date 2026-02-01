(** Uniswap V3 Session - Polling-based session for subgraph data

    The Graph subgraph does not support WebSocket subscriptions for custom
    queries, so this session polls REST endpoints at configurable intervals.
*)

open Core
open Async

(** Re-export State from interface *)
module State = Fluxum.Session_intf.State

module Auto_restart = Exchange_common.Auto_restart

(** Multi-stream event container *)
module Events = struct
  type balance = Fluxum.Types.Balance.t
  type trade = Types.swap
  type market_event = Yojson.Safe.t
  type book = (Order_book.Book.t, string) Result.t
  type ledger_entry = Ledger.Entry.t
  type order_event = unit
  type order_id = string

  type t =
    { symbols : Fluxum.Types.Symbol.t list;
      balance_pipe : balance Pipe.Reader.t;
      trades : trade Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
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

  (** Create session events.
      Each "symbol" here is a pool_id for Uniswap V3. *)
  let create ~cfg ?(symbols = []) () : t Deferred.t =
    Log.Global.info "Uniswap V3 Events.create: start pools=%d" (List.length symbols);

    (* Balance pipe - placeholder (Phase 3 wires ERC-20 balance polling) *)
    let balance_reader, _balance_writer = Pipe.create () in
    let balance_pipe = balance_reader in

    (* Market data pipes - poll pool_by_id per pool *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pool_id ->
          let name = sprintf "uniswapv3_market[%s]" pool_id in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            let reader, writer = Pipe.create () in
            let rec poll_loop () =
              Rest.pool_by_id ~cfg ~pool_id >>= function
              | Ok pool ->
                let json = Types.pool_to_yojson pool in
                Pipe.write writer json >>= fun () ->
                after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
              | Error err ->
                Log.Global.error "Uniswap V3 market data poll failed for %s: %s"
                  pool_id (Sexp.to_string (Rest.sexp_of_error err));
                after (Time_float_unix.Span.of_sec 15.0) >>= poll_loop
            in
            don't_wait_for (poll_loop ());
            return reader
          ) () in
          (pool_id, pipe)
        )
      )
    in

    (* Order book pipes - poll tick data per pool *)
    let order_books =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pool_id ->
          let pipe = Order_book.Book.pipe ~cfg ~pool_id () in
          (pool_id, pipe)
        )
      )
    in

    (* Trade pipes - poll recent_swaps per pool *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pool_id ->
          let name = sprintf "uniswapv3_trades[%s]" pool_id in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            let reader, writer = Pipe.create () in
            let last_seen = ref "" in
            let rec poll_loop () =
              Rest.recent_swaps ~cfg ~pool_id ~first:20 () >>= function
              | Ok swaps ->
                (* Only emit new swaps since last poll *)
                let new_swaps = match String.is_empty !last_seen with
                  | true -> swaps
                  | false ->
                    List.take_while swaps ~f:(fun s ->
                      not (String.equal s.Types.id !last_seen))
                in
                (match new_swaps with
                 | [] -> ()
                 | hd :: _ -> last_seen := hd.id);
                Deferred.List.iter ~how:`Sequential new_swaps ~f:(fun swap ->
                  Pipe.write writer swap)
                >>= fun () ->
                after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
              | Error err ->
                Log.Global.error "Uniswap V3 trade poll failed for %s: %s"
                  pool_id (Sexp.to_string (Rest.sexp_of_error err));
                after (Time_float_unix.Span.of_sec 15.0) >>= poll_loop
            in
            don't_wait_for (poll_loop ());
            return reader
          ) () in
          (pool_id, pipe)
        )
      )
    in

    (* Ledger pipes - combine book + trade updates *)
    let ledger =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pool_id ->
          let init = Ledger.Entry.create ~symbol:pool_id () in
          let book_pipe = match Map.find order_books pool_id with
            | Some pipe -> pipe
            | None ->
              let reader, _writer = Pipe.create () in
              reader
          in
          let ledger_reader, ledger_writer = Pipe.create () in
          let entry_ref = ref init in

          don't_wait_for (
            Pipe.iter book_pipe ~f:(fun book_result ->
              match book_result with
              | Ok book ->
                let best = Order_book.Book.best_bid book in
                let spot = best.price in
                (match Float.(spot > 0.) with
                 | true ->
                   entry_ref := Ledger.Entry.update_spot !entry_ref spot;
                   Pipe.write ledger_writer !entry_ref
                 | false -> Deferred.unit)
              | Error err ->
                Log.Global.error "Uniswap V3 order book error for %s: %s" pool_id err;
                Deferred.unit
            )
          );

          (pool_id, ledger_reader)
        )
      )
    in

    (* Order events - placeholder (swaps are atomic, no open orders) *)
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

let create ~cfg ?(symbols = []) () : t Deferred.t =
  Log.Global.info "Uniswap V3 Session.create: pools=%d" (List.length symbols);
  let state_changes_reader, state_changes_writer = Pipe.create () in
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);
  let%bind events = Events.create ~cfg ~symbols () in
  let state = State.Ready in
  don't_wait_for (Pipe.write state_changes_writer state);
  return { events; state; state_changes_reader; state_changes_writer }

let events t = t.events
let state t = t.state
let state_changes t = t.state_changes_reader

let close t =
  Log.Global.info "Uniswap V3 Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

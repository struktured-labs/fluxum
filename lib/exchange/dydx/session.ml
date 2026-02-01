(** dYdX Session - Implements unified Session_intf with auto-reconnecting streams *)

open Core
open Async

module State = Fluxum.Session_intf.State

module Auto_restart = Exchange_common.Auto_restart

module Events = struct
  type balance = unit
  type trade = Ws.trade list
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

  let create ~cfg:(module Cfg : Cfg.S) ?(symbols : Fluxum.Types.Symbol.t list = []) ?order_ids:_ () : t Deferred.t =
    Log.Global.info "dYdX Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - dYdX v4 is permissionless, no balance query without auth *)
    let balance_reader, _balance_writer = Pipe.create () in
    let balance_pipe = balance_reader in

    (* Market data pipes - subscribe to v4_markets channel *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "dydx_market[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "dYdX: connecting market data WS for %s" symbol;
            let%bind conn = Ws.connect ~url:Cfg.ws_url in
            let%bind msg_pipe = Ws.create_message_pipe ~conn in
            let%bind () = Ws.Markets.subscribe ~conn () in
            return msg_pipe
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Order book pipes - use Order_book.Book.pipe with auto-restart *)
    let order_books =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "dydx_orderbook[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "dYdX: creating order book for %s" symbol;
            let%bind book_pipe = Order_book.Book.pipe (module Cfg) ~symbol () in
            let result_pipe = Pipe.map book_pipe ~f:(fun book -> Ok book) in
            return result_pipe
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Trade pipes - subscribe to v4_trades channel per market *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "dydx_trades[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "dYdX: connecting trades WS for %s" symbol;
            let%bind conn = Ws.connect ~url:Cfg.ws_url in
            let%bind msg_pipe = Ws.create_message_pipe ~conn in
            let%bind () = Ws.Trades.subscribe ~conn ~market:symbol () in
            let trade_reader, trade_writer = Pipe.create () in
            don't_wait_for (
              Pipe.iter msg_pipe ~f:(fun json ->
                match Ws.Trades.parse_message json with
                | Ok (_data, contents) ->
                  Pipe.write trade_writer contents.trades
                | Error _ -> Deferred.unit
              )
              >>| fun () -> Pipe.close trade_writer
            );
            return trade_reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Ledger pipes - combine book + trade updates *)
    let ledger =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let init = Ledger.Entry.create ~symbol () in
          let ledger_reader, ledger_writer = Pipe.create () in
          let entry_ref = ref init in

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

    (* Order events - dYdX v4 is permissionless chain, no private order events *)
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

type t =
  { events : Events.t;
    mutable state : State.t;
    state_changes_reader : State.t Pipe.Reader.t;
    state_changes_writer : State.t Pipe.Writer.t;
  }

let create ~cfg ?(symbols : Fluxum.Types.Symbol.t list = []) ?order_ids () : t Deferred.t =
  Log.Global.info "dYdX Session.create: symbols=%d" (List.length symbols);

  let state_changes_reader, state_changes_writer = Pipe.create () in
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);

  let%bind events = Events.create ~cfg ~symbols ?order_ids () in

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
  Log.Global.info "dYdX Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

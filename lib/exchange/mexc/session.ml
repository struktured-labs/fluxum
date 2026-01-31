(** MEXC Session - Implements unified Session_intf with auto-reconnecting streams

    Complete session management for MEXC Global exchange with Binance-compatible WebSocket API.
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
  (** Type aliases for MEXC-specific types *)
  type balance = V1.Account.balance
  type trade = V1.My_trades.trade
  type market_event = string  (* Raw WebSocket message *)
  type book = Order_book.Book.t
  type ledger_entry = Ledger.Entry.t
  type order_event = V1.New_order.response  (* Order execution updates *)
  type order_id = int64

  type t =
    { symbols : Fluxum.Types.Symbol.t list;
      balance_pipe : balance Pipe.Reader.t;
      trades : trade list Pipe.Reader.t Fluxum.Types.Symbol.Map.t;
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
      ~cfg:(module Cfg : Cfg.S)
      ?(symbols : Fluxum.Types.Symbol.t list = [])
      ?order_ids:_
      ()
    : t Deferred.t =
    Log.Global.info "Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - REST polling every 5s *)
    let balance_pipe =
      Auto_restart.pipe ~name:"mexc_balance_poll" ~create_pipe:(fun () ->
        let reader, writer = Pipe.create () in
        let rec poll_loop () =
          V1.Account.request (module Cfg) () >>= function
          | `Ok resp ->
            Deferred.List.iter ~how:`Sequential resp.balances ~f:(fun balance ->
              Pipe.write writer balance)
            >>= fun () ->
            after (Time_float_unix.Span.of_sec 5.0) >>= poll_loop
          | #Rest.Error.t as err ->
            Log.Global.error "MEXC balance poll failed: %s"
              (Sexp.to_string (Rest.Error.sexp_of_t err));
            after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
        in
        don't_wait_for (poll_loop ());
        return reader
      ) ()
    in

    (* Market data pipes - one per symbol using WebSocket *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "market_data[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating market data connection for %s" symbol;
            let streams = [
              Ws.Stream.BookTicker symbol;
              Ws.Stream.AggreDeals { symbol; frequency = Ws.Stream.Ms100 }
            ] in
            Ws.connect ~streams () >>| function
            | Ok ws -> Ws.messages ws
            | Error err ->
              Log.Global.error "Failed to connect market data for %s: %s" symbol (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Order book pipes - one per symbol with auto-restart *)
    let order_books =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "order_book[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating order book for %s" symbol;
            let streams = [
              Ws.Stream.LimitDepths { symbol; levels = 20 };
              Ws.Stream.AggreDepths { symbol; frequency = Ws.Stream.Ms100 }
            ] in
            Ws.connect ~streams () >>| function
            | Ok ws ->
              let book_reader, book_writer = Pipe.create () in
              let book_ref = ref (Order_book.Book.empty symbol) in
              don't_wait_for (
                Pipe.iter (Ws.messages ws) ~f:(fun msg ->
                  match Ws.parse_message msg with
                  | Ws.Message.Data wrapper ->
                    (match wrapper.body with
                     | Ws.Message.LimitDepth depth ->
                       (match Ws.apply_limit_depth_to_book !book_ref depth with
                        | Ok book ->
                          book_ref := book;
                          Pipe.write book_writer (Ok book)
                        | Error err ->
                          Pipe.write book_writer (Error err))
                     | Ws.Message.AggreDepth depth ->
                       (match Ws.apply_depth_to_book !book_ref depth with
                        | Ok book ->
                          book_ref := book;
                          Pipe.write book_writer (Ok book)
                        | Error err ->
                          Pipe.write book_writer (Error err))
                     | _ -> Deferred.unit)
                  | _ -> Deferred.unit
                )
              );
              book_reader
            | Error err ->
              Log.Global.error "Failed to connect order book for %s: %s" symbol (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Trades pipes - one per symbol using WebSocket *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "trades[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating trade stream for %s" symbol;
            let streams = [Ws.Stream.AggreDeals { symbol; frequency = Ws.Stream.Ms100 }] in
            Ws.connect ~streams () >>| function
            | Ok ws ->
              let trade_reader, trade_writer = Pipe.create () in
              don't_wait_for (
                Pipe.iter (Ws.messages ws) ~f:(fun msg ->
                  match Ws.parse_message msg with
                  | Ws.Message.Data wrapper ->
                    (match wrapper.body with
                     | Ws.Message.AggreDeals deals_msg ->
                       (* Convert deal items to V1.My_trades.trade format *)
                       let trades = List.filter_map deals_msg.deals ~f:(fun deal ->
                         let trade : V1.My_trades.trade = {
                           symbol;
                           id = deal.time;  (* Use timestamp as ID *)
                           orderId = "";    (* Not available from public stream *)
                           price = deal.price;
                           qty = deal.quantity;
                           quoteQty = "0";
                           commission = "0";
                           commissionAsset = "";
                           time = deal.time;
                           isBuyer = (deal.trade_type = 1);
                           isMaker = false;
                           isBestMatch = true;
                         } in
                         Some trade
                       ) in
                       (match List.is_empty trades with
                        | true -> Deferred.unit
                        | false -> Pipe.write trade_writer trades)
                     | Ws.Message.AggreDepth _ -> Deferred.unit
                     | Ws.Message.LimitDepth _ -> Deferred.unit
                     | Ws.Message.BookTicker _ -> Deferred.unit
                     | Ws.Message.Unknown _ -> Deferred.unit)
                  | _ -> Deferred.unit
                )
              );
              trade_reader
            | Error err ->
              Log.Global.error "Failed to connect trade stream for %s: %s" symbol (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              reader
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

          let _trade_pipe = match Map.find trades symbol with
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

    (* Order events pipe - placeholder (not supported) *)
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
    ~cfg:(module Cfg : Cfg.S)
    ?(symbols : Fluxum.Types.Symbol.t list = [])
    ?order_ids
    ()
  : t Deferred.t =
  Log.Global.info "Session.create: symbols=%d" (List.length symbols);

  (* Create state change pipe *)
  let state_changes_reader, state_changes_writer = Pipe.create () in

  (* Start in Connecting state and emit *)
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);

  (* Create events *)
  let%bind events = Events.create ~cfg:(module Cfg) ~symbols ?order_ids () in

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
  Log.Global.info "Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

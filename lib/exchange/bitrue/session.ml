(** Bitrue Session - Implements unified Session_intf with auto-reconnecting streams

    Complete session management for Bitrue exchange with WebSocket market data,
    order book tracking, trade streams, and REST balance polling.
*)

open Core
open Async

(** Re-export State from interface *)
module State = Fluxum.Session_intf.State

module Auto_restart = Exchange_common.Auto_restart

(** Multi-stream event container *)
module Events = struct
  (** Type aliases for Bitrue-specific types *)
  type balance = Rest.Account.balance
  type trade = Ws.Message.trade
  type market_event = string  (* Raw WebSocket message *)
  type book = Order_book.Book.t
  type ledger_entry = Ledger.Entry.t
  type order_event = Rest.Order.order_status_response
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
      ~cfg
      ?(symbols : Fluxum.Types.Symbol.t list = [])
      ?order_ids:_
      ()
    : t Deferred.t =
    Log.Global.info "Bitrue Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - REST polling every 5s *)
    let balance_pipe =
      Auto_restart.pipe ~name:"bitrue_balance_poll" ~create_pipe:(fun () ->
        let reader, writer = Pipe.create () in
        let rec poll_loop () =
          Rest.account cfg >>= function
          | Ok resp ->
            Deferred.List.iter ~how:`Sequential resp.balances ~f:(fun balance ->
              Pipe.write writer balance)
            >>= fun () ->
            after (Time_float_unix.Span.of_sec 5.0) >>= poll_loop
          | Error err ->
            Log.Global.error "Bitrue balance poll failed: %s"
              (Sexp.to_string (Rest.Error.sexp_of_t err));
            after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
        in
        don't_wait_for (poll_loop ());
        return reader
      ) ()
    in

    (* Market data pipes - one per symbol with auto-restart *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "bitrue_market_data[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating Bitrue market data WS for %s" symbol;
            let streams = [Ws.Stream.Ticker symbol] in
            Market_data_curl.connect ~streams () >>| function
            | Ok md -> Market_data_curl.messages md
            | Error err ->
              Log.Global.error "Bitrue market data WS failed for %s: %s" symbol err;
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
          let name = sprintf "bitrue_order_book[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating Bitrue order book for %s" symbol;
            Order_book.Book.pipe ~symbol () >>| function
            | Ok reader -> reader
            | Error err ->
              Log.Global.error "Bitrue order book failed for %s: %s"
                symbol (Error.to_string_hum err);
              let reader, _writer = Pipe.create () in
              reader
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Trade pipes - one per symbol with auto-restart *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "bitrue_trades[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating Bitrue trade stream for %s" symbol;
            let streams = [Ws.Stream.Trade symbol] in
            Market_data_curl.connect ~streams () >>| function
            | Ok md ->
              Pipe.filter_map (Market_data_curl.messages md) ~f:(fun msg ->
                match Ws.parse_message msg with
                | Ws.Message.Trade trades_msg -> Some trades_msg.tick
                | _ -> None
              )
            | Error err ->
              Log.Global.error "Bitrue trade WS failed for %s: %s" symbol err;
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

          let ledger_reader, ledger_writer = Pipe.create () in
          let entry_ref = ref init in

          don't_wait_for (
            (* Update from book prices *)
            Pipe.iter book_pipe ~f:(fun book_result ->
              match book_result with
              | Ok book ->
                let best = Order_book.Book.best_bid book in
                let spot = best.price in
                (match Float.(spot > 0.) with
                 | true ->
                   entry_ref := Ledger.Entry.recalculate_pnl !entry_ref ~current_price:spot;
                   Pipe.write ledger_writer !entry_ref
                 | false -> Deferred.unit)
              | Error err ->
                Log.Global.error "Bitrue order book error for %s: %s" symbol err;
                Deferred.unit
            )
          );

          don't_wait_for (
            (* Update from trades *)
            Pipe.iter trade_pipe ~f:(fun (trade : Ws.Message.trade) ->
              match Fluxum.Normalize_common.Float_conv.price_of_string trade.price with
              | Ok price ->
                let side = match String.equal trade.ds "buy" with
                  | true -> Fluxum.Types.Side.Buy
                  | false -> Fluxum.Types.Side.Sell
                in
                let qty = match Fluxum.Normalize_common.Float_conv.qty_of_string trade.vol with
                  | Ok v -> v
                  | Error _ -> 0.0
                in
                entry_ref := Ledger.Entry.with_market_data !entry_ref ~price ~qty ~side;
                Pipe.write ledger_writer !entry_ref
              | Error err ->
                Log.Global.error "Bitrue trade parse error: %s" err;
                Deferred.unit
            )
          );

          (symbol, ledger_reader)
        )
      )
    in

    (* Order events pipe - placeholder (no private WS available) *)
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
    ~cfg
    ?(symbols : Fluxum.Types.Symbol.t list = [])
    ?order_ids
    ()
  : t Deferred.t =
  Log.Global.info "Bitrue Session.create: symbols=%d" (List.length symbols);

  (* Create state change pipe *)
  let state_changes_reader, state_changes_writer = Pipe.create () in

  (* Start in Connecting state and emit *)
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);

  (* Create events *)
  let%bind events = Events.create ~cfg ~symbols ?order_ids () in

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
  Log.Global.info "Bitrue Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

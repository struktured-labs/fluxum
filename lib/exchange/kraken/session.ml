(** Kraken Session - Implements unified Session_intf with auto-reconnecting streams *)

open Core
open Async

(** Re-export State from interface *)
module State = Fluxum.Session_intf.State

module Auto_restart = Exchange_common.Auto_restart

(** Multi-stream event container *)
module Events = struct
  (** Type aliases for Kraken-specific types *)
  type balance = string * string  (** Currency and amount pair *)
  type trade = Ws.Public.Trade_item.t
  type market_event = string  (* Raw WebSocket message *)
  type book = (Order_book.Book.t, string) Result.t
  type ledger_entry = Ledger.Entry.t
  type order_event = V1.Open_orders.Order.t  (** Order execution updates *)
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
      ~cfg:(module Cfg : Cfg.S)
      ?(symbols : Fluxum.Types.Symbol.t list = [])
      ?order_ids:_
      ()
    : t Deferred.t =
    Log.Global.info "Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - REST polling every 5s *)
    let balance_pipe =
      Auto_restart.pipe ~name:"kraken_balance_poll" ~create_pipe:(fun () ->
        let reader, writer = Pipe.create () in
        let rec poll_loop () =
          V1.Balances.post (module Cfg) () >>= function
          | `Ok balances ->
            Deferred.List.iter ~how:`Sequential balances ~f:(fun balance ->
              Pipe.write writer balance)
            >>= fun () ->
            after (Time_float_unix.Span.of_sec 5.0) >>= poll_loop
          | #Rest.Error.post as err ->
            Log.Global.error "Kraken balance poll failed: %s"
              (Sexp.to_string (Rest.Error.sexp_of_post err));
            after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
        in
        don't_wait_for (poll_loop ());
        return reader
      ) ()
    in

    (* Market data pipes - one per symbol *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "market_data[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating market data connection for %s" symbol;
            let%bind md_result = Market_data.connect
              ~subscriptions:[{
                channel = "ticker";
                pairs = [symbol];
                interval = None;
                depth = None;
              }]
              ()
            in
            match md_result with
            | Error err ->
              Log.Global.error "Market data connection failed: %s"
                (Sexp.to_string (Ws.Error.sexp_of_t err));
              let reader, _writer = Pipe.create () in
              return reader
            | Ok md ->
              return (Market_data.messages md)
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
            Order_book.Book.pipe ~symbol ~depth:10 ()
          ) () in
          (symbol, pipe)
        )
      )
    in

    (* Trades pipes - one per symbol *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun symbol ->
          let name = sprintf "trades[%s]" symbol in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Creating trade stream for %s" symbol;
            let%bind md_result = Market_data.connect
              ~subscriptions:[{
                channel = "trade";
                pairs = [symbol];
                interval = None;
                depth = None;
              }]
              ()
            in
            match md_result with
            | Error _err ->
              let reader, _writer = Pipe.create () in
              return reader
            | Ok md ->
              let messages = Market_data.messages md in
              let trade_reader, trade_writer = Pipe.create () in
              don't_wait_for (
                Pipe.iter messages ~f:(fun msg ->
                  match Ws.parse_message msg with
                  | Ok (Ws.Public (Ws.Public.Trade { trades; _ })) ->
                    Pipe.write trade_writer trades
                  | _ -> Deferred.unit
                )
                >>| fun () -> Pipe.close trade_writer
              );
              return trade_reader
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

    (* Order events pipe - private WebSocket with auto-restart *)
    let order_events_pipe =
      Auto_restart.pipe ~name:"kraken_order_events" ~create_pipe:(fun () ->
        Private_ws.connect ~cfg:(module Cfg) () >>| function
        | Ok msg_pipe ->
          let order_reader, order_writer = Pipe.create () in
          don't_wait_for (
            Pipe.iter msg_pipe ~f:(fun msg ->
              match msg with
              | Ws.Private.Own_orders orders ->
                Deferred.List.iter ~how:`Sequential orders ~f:(fun (order_id, update) ->
                  (* Convert Private.Order_update to V1.Open_orders.Order *)
                  let order : V1.Open_orders.Order.t =
                    { refid = None
                    ; userref = Option.value update.userref ~default:0
                    ; status = Common.Order_status.to_string update.status
                    ; reason = None
                    ; opentm = Option.value update.lastupdated ~default:0.0
                    ; closetm = 0.0
                    ; starttm = 0.0
                    ; expiretm = 0.0
                    ; descr = { pair = update.descr.pair
                               ; type_ = update.descr.type_
                               ; ordertype = update.descr.ordertype
                               ; price = update.descr.price
                               ; price2 = update.descr.price2
                               ; leverage = update.descr.leverage
                               ; order = update.descr.order
                               ; close = update.descr.close
                               }
                    ; vol = update.vol
                    ; vol_exec = update.vol_exec
                    ; cost = update.cost
                    ; fee = update.fee
                    ; price = update.avg_price
                    ; stopprice = update.stop_price
                    ; limitprice = update.limit_price
                    ; misc = update.misc
                    ; oflags = update.oflags
                    }
                  in
                  let _ = order_id in
                  Pipe.write order_writer order)
              | Ws.Private.Own_trades _trades ->
                Deferred.unit
              | Ws.Private.Pong ->
                Deferred.unit)
            >>| fun () -> Pipe.close order_writer);
          order_reader
        | Error _err ->
          Log.Global.error "Kraken private WS connection failed, order events unavailable";
          let reader, _writer = Pipe.create () in
          reader
      ) ()
    in

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

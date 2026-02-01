(** Bitstamp Session - WebSocket session with auto-reconnecting streams *)

open Core
open Async

module State = Fluxum.Session_intf.State

module Auto_restart = Exchange_common.Auto_restart

module Events = struct
  type balance = Types.balance
  type trade = Types.trade
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

  let create ~cfg ?(symbols : Fluxum.Types.Symbol.t list = []) ?order_ids:_ () : t Deferred.t =
    Log.Global.info "Bitstamp Events.create: start symbols=%d" (List.length symbols);

    (* Balance pipe - REST polling every 10s (requires auth) *)
    let balance_pipe =
      match cfg.Cfg.api_key with
      | None ->
        let reader, _writer = Pipe.create () in
        reader
      | Some _ ->
        Auto_restart.pipe ~name:"bitstamp_balance_poll" ~create_pipe:(fun () ->
          let reader, writer = Pipe.create () in
          let rec poll_loop () =
            Rest.balance ~cfg >>= function
            | Ok bal ->
              Pipe.write writer bal >>= fun () ->
              after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
            | Error err ->
              Log.Global.error "Bitstamp balance poll failed: %s"
                (Sexp.to_string (Rest.Error.sexp_of_t err));
              after (Time_float_unix.Span.of_sec 15.0) >>= poll_loop
          in
          don't_wait_for (poll_loop ());
          return reader
        ) ()
    in

    (* Market data pipes - ticker via REST polling *)
    let market_data =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pair ->
          let name = sprintf "bitstamp_market[%s]" pair in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            let reader, writer = Pipe.create () in
            let rec poll_loop () =
              Rest.ticker ~cfg ~pair >>= function
              | Ok ticker ->
                let json = Types.ticker_to_yojson ticker in
                Pipe.write writer json >>= fun () ->
                after (Time_float_unix.Span.of_sec 5.0) >>= poll_loop
              | Error err ->
                Log.Global.error "Bitstamp ticker poll failed for %s: %s"
                  pair (Sexp.to_string (Rest.Error.sexp_of_t err));
                after (Time_float_unix.Span.of_sec 10.0) >>= poll_loop
            in
            don't_wait_for (poll_loop ());
            return reader
          ) () in
          (pair, pipe)
        )
      )
    in

    (* Order book pipes - WebSocket diff_order_book per pair *)
    let order_books =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pair ->
          let name = sprintf "bitstamp_orderbook[%s]" pair in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Bitstamp: creating order book for %s" pair;
            Order_book.Book.pipe ~cfg ~pair ()
          ) () in
          (pair, pipe)
        )
      )
    in

    (* Trade pipes - WebSocket live_trades per pair *)
    let trades =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pair ->
          let name = sprintf "bitstamp_trades[%s]" pair in
          let pipe = Auto_restart.pipe ~name ~create_pipe:(fun () ->
            Log.Global.info "Bitstamp: connecting trades WS for %s" pair;
            let channels = [Ws.Channel.Live_trades pair] in
            let%bind conn_result = Ws.connect ~cfg ~channels () in
            match conn_result with
            | Error err ->
              Log.Global.error "Bitstamp trades WS failed: %s" err;
              let reader, _writer = Pipe.create () in
              return reader
            | Ok conn ->
              let trade_reader, trade_writer = Pipe.create () in
              don't_wait_for (
                let rec read_loop () =
                  Ws.receive conn >>= function
                  | None ->
                    Pipe.close trade_writer;
                    return ()
                  | Some msg_str ->
                    (match Ws.parse_message msg_str with
                     | Ws.Message.Trade trade ->
                       Pipe.write trade_writer trade >>= read_loop
                     | _ -> read_loop ())
                in
                read_loop ()
              );
              return trade_reader
          ) () in
          (pair, pipe)
        )
      )
    in

    (* Ledger pipes - combine book + trade updates *)
    let ledger =
      Fluxum.Types.Symbol.Map.of_alist_exn (
        List.map symbols ~f:(fun pair ->
          let init = Ledger.Entry.create ~symbol:pair () in
          let book_pipe = match Map.find order_books pair with
            | Some pipe -> pipe
            | None ->
              let reader, _writer = Pipe.create () in
              reader
          in
          let trade_pipe = match Map.find trades pair with
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
                let spot = (Order_book.Book.best_bid book).price in
                (match Float.(spot > 0.) with
                 | true ->
                   entry_ref := Ledger.Entry.update_spot !entry_ref spot;
                   Pipe.write ledger_writer !entry_ref
                 | false -> Deferred.unit)
              | Error err ->
                Log.Global.error "Bitstamp order book error for %s: %s" pair err;
                Deferred.unit
            )
          );

          don't_wait_for (
            Pipe.iter trade_pipe ~f:(fun (trade : Types.trade) ->
              match Fluxum.Normalize_common.Float_conv.price_of_string trade.price with
              | Ok price ->
                let side = match trade.type_ with
                  | 0 -> Fluxum.Types.Side.Buy
                  | _ -> Fluxum.Types.Side.Sell
                in
                let qty = match Fluxum.Normalize_common.Float_conv.qty_of_string trade.amount with
                  | Ok v -> v | Error _ -> 0.0
                in
                entry_ref := Ledger.Entry.with_market_data !entry_ref ~price ~qty ~side;
                Pipe.write ledger_writer !entry_ref
              | Error err ->
                Log.Global.error "Bitstamp trade parse error: %s" err;
                Deferred.unit
            )
          );

          (pair, ledger_reader)
        )
      )
    in

    (* Order events - placeholder (Bitstamp WS doesn't support private order events) *)
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
  Log.Global.info "Bitstamp Session.create: symbols=%d" (List.length symbols);
  let state_changes_reader, state_changes_writer = Pipe.create () in
  don't_wait_for (Pipe.write state_changes_writer State.Connecting);
  let%bind events = Events.create ~cfg ~symbols ?order_ids () in
  let state = State.Ready in
  don't_wait_for (Pipe.write state_changes_writer state);
  return { events; state; state_changes_reader; state_changes_writer }

let events t = t.events
let state t = t.state
let state_changes t = t.state_changes_reader

let close t =
  Log.Global.info "Bitstamp Session.close: closing session";
  t.state <- State.Disconnected;
  don't_wait_for (Pipe.write t.state_changes_writer State.Disconnected);
  Pipe.close t.state_changes_writer;
  Deferred.unit

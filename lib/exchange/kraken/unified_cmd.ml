(** Kraken unified adapter commands - orderbook, ledger, session *)

open Core
open Async

let name = "unified"

(** Order book command - streams live order book updates *)
module Orderbook = struct
  let name = "orderbook"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Stream live order book for a symbol"
      (Command.Param.(
        let symbol = flag "--symbol"
          (required string)
          ~doc:"STRING trading symbol (e.g., BTC/USD, ETH/USD)"
        and depth = flag "--depth"
          (optional_with_default 10 int)
          ~doc:"INT order book depth (default 10)"
        and limit = flag "--limit"
          (optional_with_default 20 int)
          ~doc:"INT number of updates to show (default 20, 0 for unlimited)"
        in
        return (fun symbol depth limit () ->
          printf "Streaming order book for %s (depth: %d)...\n\n" symbol depth;

          Order_book.Book.pipe ~symbol ~depth ()
          >>= fun book_pipe ->

          let update_count = ref 0 in
          Pipe.iter book_pipe ~f:(fun book_result ->
            incr update_count;

            match book_result with
            | Ok book ->
              (* Clear screen and print updated book *)
              printf "\027[2J\027[H";  (* ANSI clear screen *)
              printf "=== Order Book Update #%d - %s ===\n\n" !update_count symbol;

              (* Display best bid/ask and spread *)
              let best_bid = Order_book.Book.best_bid book in
              let best_ask = Order_book.Book.best_ask book in
              let spread = Order_book.Book.spread book in
              printf "Best Bid: %.8f (vol: %.8f)\n" best_bid.price best_bid.volume;
              printf "Best Ask: %.8f (vol: %.8f)\n" best_ask.price best_ask.volume;
              printf "Spread:   %.8f\n\n" spread;

              (* Display top 5 levels *)
              let top_bids = Order_book.Book.best_n_bids book ~n:5 () in
              let top_asks = Order_book.Book.best_n_asks book ~n:5 () in
              printf "%-20s | %-20s\n" "BIDS" "ASKS";
              printf "%-20s | %-20s\n" "--------------------" "--------------------";
              List.iter2_exn
                (List.take (top_bids @ List.init 5 ~f:(fun _ -> Order_book.Price_level.empty)) 5)
                (List.take (top_asks @ List.init 5 ~f:(fun _ -> Order_book.Price_level.empty)) 5)
                ~f:(fun bid ask ->
                  printf "%.4f @ %.4f | %.4f @ %.4f\n"
                    bid.volume bid.price ask.price ask.volume);
              printf "\nBook updated (epoch: %d)\n" (Order_book.Book.epoch book);
              printf "\n";

              (match limit > 0 && !update_count >= limit with
               | true -> Pipe.close_read book_pipe
               | false -> ());

              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" err;
              Deferred.unit
          )
          >>= fun () ->
          printf "\nOrder book streaming complete.\n";
          Deferred.unit)
        <*> symbol
        <*> depth
        <*> limit
      )))
end

(** Ledger command - tracks P&L for multiple symbols *)
module Ledger = struct
  let name = "ledger"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Track P&L ledger for symbols"
      (Command.Param.(
        let symbols = flag "--symbols"
          (required string)
          ~doc:"STRING comma-separated trading symbols (e.g., BTC/USD,ETH/USD)"
        in
        return (fun symbols_str () ->
          let symbol_list = String.split symbols_str ~on:',' |> List.map ~f:String.strip in
          printf "Starting ledger tracking for: %s\n\n" (String.concat ~sep:", " symbol_list);

          (* Create ledger entries for each symbol *)
          let ledgers = List.map symbol_list ~f:(fun symbol ->
            let entry = Ledger.Entry.create ~symbol () in
            (symbol, ref entry)
          ) in

          (* Create order book pipes for spot price updates *)
          let%bind book_pipes = Deferred.List.map symbol_list ~how:`Sequential ~f:(fun symbol ->
            let%map pipe = Order_book.Book.pipe ~symbol ~depth:1 () in
            (symbol, pipe)
          ) in

          (* Update ledgers from book prices *)
          Deferred.List.iter book_pipes ~how:`Sequential ~f:(fun (symbol, book_pipe) ->
            let ledger_ref = List.Assoc.find_exn ledgers ~equal:String.equal symbol in

            Pipe.iter book_pipe ~f:(fun book_result ->
              match book_result with
              | Ok book ->
                let spot = (Order_book.Book.best_bid book).price in
                (match Float.(spot > 0.) with
                 | true ->
                   ledger_ref := Ledger.Entry.update_spot !ledger_ref spot;

                   (* Print updated ledger *)
                   printf "\n=== Ledger Update - %s ===\n" symbol;
                   printf "Spot: $%.2f\n" spot;
                   printf "Position: %.8f\n" (!ledger_ref).position;
                   printf "Notional: $%.2f\n" (!ledger_ref).notional;
                   printf "Cost Basis: $%.2f\n" (!ledger_ref).cost_basis;
                   printf "PnL: $%.2f\n" (!ledger_ref).pnl;
                   printf "\n";
                 | false -> ());
                Deferred.unit
              | Error _ -> Deferred.unit
            )
          )
          >>= fun () ->
          printf "Ledger tracking complete.\n";
          Deferred.unit)
        <*> symbols
      )))
end

(** Session command - creates full session with all event streams *)
module Session = struct
  let name = "session"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Create session with all event streams"
      (Command.Param.(
        let symbols = flag "--symbols"
          (required string)
          ~doc:"STRING comma-separated trading symbols (e.g., BTC/USD,ETH/USD)"
        and limit = flag "--limit"
          (optional_with_default 50 int)
          ~doc:"INT number of events to show per stream (default 50)"
        in
        return (fun symbols_str limit () ->
          let symbol_list = String.split symbols_str ~on:',' |> List.map ~f:String.strip in
          printf "Creating session for: %s\n\n" (String.concat ~sep:", " symbol_list);

          Session.create ~symbols:symbol_list ()
          >>= fun session ->

          printf "Session state: %s\n"
            (Sexp.to_string_hum (Fluxum.Session_intf.State.sexp_of_t (Session.state session)));
          printf "Session created, streaming events...\n\n";

          let events = Session.events session in

          (* Monitor state changes *)
          don't_wait_for (
            Pipe.iter (Session.state_changes session) ~f:(fun state ->
              printf "\n>>> State changed to: %s\n\n"
                (Sexp.to_string_hum (Fluxum.Session_intf.State.sexp_of_t state));
              Deferred.unit
            )
          );

          (* Monitor order books for each symbol *)
          let book_monitors = List.map symbol_list ~f:(fun symbol ->
            match Map.find (Session.Events.order_books events) symbol with
            | Some book_pipe ->
              let count = ref 0 in
              Pipe.iter book_pipe ~f:(fun book_result ->
                incr count;
                match book_result with
                | Ok book ->
                  printf "[%s] Order book update #%d - Best bid: $%.2f, Best ask: $%.2f\n"
                    symbol !count
                    (Order_book.Book.best_bid book).price
                    (Order_book.Book.best_ask book).price;
                  (match !count >= limit with
                   | true -> Pipe.close_read book_pipe
                   | false -> ());
                  Deferred.unit
                | Error err ->
                  eprintf "[%s] Order book error: %s\n" symbol err;
                  Deferred.unit
              )
            | None ->
              printf "No order book pipe for %s\n" symbol;
              Deferred.unit
          ) in

          (* Monitor ledger updates for each symbol *)
          let ledger_monitors = List.map symbol_list ~f:(fun symbol ->
            match Map.find (Session.Events.ledger events) symbol with
            | Some ledger_pipe ->
              let count = ref 0 in
              Pipe.iter ledger_pipe ~f:(fun entry ->
                incr count;
                printf "[%s] Ledger update #%d - Spot: $%.2f, PnL: $%.2f\n"
                  symbol !count entry.spot entry.pnl;
                (match !count >= limit with
                 | true -> Pipe.close_read ledger_pipe
                 | false -> ());
                Deferred.unit
              )
            | None ->
              printf "No ledger pipe for %s\n" symbol;
              Deferred.unit
          ) in

          (* Wait for all monitors to complete *)
          Deferred.all_unit (book_monitors @ ledger_monitors)
          >>= fun () ->
          Session.close session
          >>= fun () ->
          printf "\nSession closed.\n";
          Deferred.unit)
        <*> symbols
        <*> limit
      )))
end

let command : string * Command.t =
  (name, Command.group
    ~summary:"Kraken unified adapter commands (orderbook, ledger, session)"
    [ Orderbook.command; Ledger.command; Session.command ])

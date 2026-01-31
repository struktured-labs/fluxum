(** Bitrue unified adapter commands - orderbook, ledger, session *)

open Core
open Async

let name = "unified"

(** Order book command - streams live order book updates *)
module Orderbook = struct
  let name = "orderbook"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Stream live order book for a symbol"
      (let%map_open.Command symbol = flag "--symbol"
          (required string)
          ~doc:"STRING trading pair (e.g. BTCUSDT)"
       and limit = flag "--limit"
          (optional_with_default 20 int)
          ~doc:"INT number of updates to show (default 20, 0 for unlimited)"
       in
       fun () ->
         printf "Streaming order book for %s...\n\n" symbol;

         Order_book.Book.pipe ~symbol ()
         >>= function
         | Error err ->
           eprintf "Failed to create order book: %s\n" (Error.to_string_hum err);
           Deferred.unit
         | Ok book_pipe ->
           let update_count = ref 0 in
           Pipe.iter book_pipe ~f:(fun book_result ->
             incr update_count;

             match book_result with
             | Ok book ->
               printf "\027[2J\027[H";
               printf "=== Order Book Update #%d - %s ===\n\n" !update_count symbol;

               let best_bid = Order_book.Book.best_bid book in
               let best_ask = Order_book.Book.best_ask book in
               let spread = Order_book.Book.spread book in
               printf "Best Bid: %.8f (vol: %.8f)\n" best_bid.price best_bid.volume;
               printf "Best Ask: %.8f (vol: %.8f)\n" best_ask.price best_ask.volume;
               printf "Spread:   %.8f\n\n" spread;

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
           Deferred.unit
      ))
end

(** Session command - creates full session with all event streams *)
module Session_cmd = struct
  let name = "session"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Create session with all event streams"
      (let%map_open.Command symbols_str = flag "--symbols"
          (required string)
          ~doc:"STRING comma-separated trading pairs (e.g. BTCUSDT,ETHUSDT)"
       and limit = flag "--limit"
          (optional_with_default 50 int)
          ~doc:"INT number of events to show per stream (default 50)"
       in
       fun () ->
         let cfg = (module Cfg.Production : Cfg.S) in
         let symbol_list = String.split symbols_str ~on:',' |> List.map ~f:String.strip in
         printf "Creating session for: %s\n\n" (String.concat ~sep:", " symbol_list);

         Session.create ~cfg ~symbols:symbol_list ()
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
               printf "[%s] Ledger update #%d - PnL: $%.2f, Position: %.8f\n"
                 symbol !count entry.Ledger.Entry.pnl entry.Ledger.Entry.position;
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
         Deferred.unit
      ))
end

let command : string * Command.t =
  (name, Command.group
    ~summary:"Bitrue unified adapter commands (orderbook, session)"
    [ Orderbook.command; Session_cmd.command ])

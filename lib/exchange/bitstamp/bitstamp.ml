(** Bitstamp Exchange Module

    Bitstamp is a European cryptocurrency exchange founded in 2011,
    known for reliability and regulatory compliance.

    @see <https://www.bitstamp.net/api/>
*)

open Core
open Async

module Cfg = Cfg
module Types = Types
module Bitstamp_types = Types
module Rest = Rest
module Ws = Ws
module Pool_adapter = Pool_adapter
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter

let cfg = Cfg.production

(** CLI Commands *)

let ticker_command : string * Command.t =
  ("ticker", Command.async
    ~summary:"Get ticker for a trading pair"
    (let%map_open.Command pair = flag "--pair" (required string)
        ~doc:"STRING Trading pair (e.g. btcusd)"
     in
     fun () ->
       Rest.ticker ~cfg ~pair >>| function
       | Ok t ->
         printf "Ticker: %s\n" pair;
         printf "  Last:    %s\n" t.last;
         printf "  Bid:     %s\n" t.bid;
         printf "  Ask:     %s\n" t.ask;
         printf "  High:    %s\n" t.high;
         printf "  Low:     %s\n" t.low;
         printf "  Volume:  %s\n" t.volume;
         printf "  VWAP:    %s\n" t.vwap
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e)))

let depth_command : string * Command.t =
  ("depth", Command.async
    ~summary:"Get order book depth"
    (let%map_open.Command pair = flag "--pair" (required string)
        ~doc:"STRING Trading pair (e.g. btcusd)"
     in
     fun () ->
       Rest.order_book ~cfg ~pair () >>| function
       | Ok book ->
         printf "Order Book: %s (ts: %s)\n\n" pair book.timestamp;
         printf "%-20s  %-20s\n" "BIDS" "ASKS";
         printf "%s\n" (String.make 45 '-');
         let max_len = min 20 (max (List.length book.bids) (List.length book.asks)) in
         let bids = Array.of_list book.bids in
         let asks = Array.of_list book.asks in
         for i = 0 to max_len - 1 do
           let bid_str = match i < Array.length bids with
             | true -> let (p, q) = bids.(i) in sprintf "%.2f x %.8f" p q
             | false -> ""
           in
           let ask_str = match i < Array.length asks with
             | true -> let (p, q) = asks.(i) in sprintf "%.2f x %.8f" p q
             | false -> ""
           in
           printf "%-20s  %s\n" bid_str ask_str
         done
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e)))

let trades_command : string * Command.t =
  ("recent-trades", Command.async
    ~summary:"Get recent trades"
    (let%map_open.Command pair = flag "--pair" (required string)
        ~doc:"STRING Trading pair (e.g. btcusd)"
     in
     fun () ->
       Rest.transactions ~cfg ~pair () >>| function
       | Ok trades ->
         printf "Recent Trades: %s (%d)\n\n" pair (List.length trades);
         List.iter trades ~f:(fun t ->
           let side = match t.Types.type_ with 0 -> "BUY " | _ -> "SELL" in
           printf "  %s  price=%s  amount=%s  tid=%d\n" side t.price t.amount t.tid)
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e)))

let pairs_command : string * Command.t =
  ("pairs", Command.async
    ~summary:"List available trading pairs"
    (Command.Param.return (fun () ->
       Rest.trading_pairs_info ~cfg >>| function
       | Ok pairs ->
         printf "Trading Pairs (%d):\n" (List.length pairs);
         List.iter pairs ~f:(fun p ->
           printf "  %-12s  %s  min=%s  %s\n"
             p.url_symbol p.name p.minimum_order p.trading)
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e))))

let balances_command : string * Command.t =
  ("balances", Command.async
    ~summary:"Get account balances (authenticated)"
    (Command.Param.return (fun () ->
       Rest.balance ~cfg >>| function
       | Ok b ->
         printf "Balances:\n";
         let print_bal name avail reserved total =
           match avail, reserved, total with
           | Some a, Some r, Some t ->
             (match Float.(of_string a + of_string r > 0.) with
              | true -> printf "  %-6s  available=%-14s  reserved=%-14s  total=%s\n"
                  name a r t
              | false -> ())
           | _ -> ()
         in
         print_bal "USD" b.usd_available b.usd_reserved b.usd_balance;
         print_bal "BTC" b.btc_available b.btc_reserved b.btc_balance;
         print_bal "EUR" b.eur_available b.eur_reserved b.eur_balance;
         print_bal "ETH" b.eth_available b.eth_reserved b.eth_balance
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e))))

let open_orders_command : string * Command.t =
  ("open-orders", Command.async
    ~summary:"Get open orders (authenticated)"
    (let%map_open.Command pair = flag "--pair" (optional string)
        ~doc:"STRING Trading pair (omit for all)"
     in
     fun () ->
       (match pair with
        | Some p -> Rest.open_orders ~cfg ~pair:p
        | None -> Rest.open_orders_all ~cfg)
       >>| function
       | Ok orders ->
         printf "Open Orders (%d):\n" (List.length orders);
         List.iter orders ~f:(fun o ->
           let side = match o.Types.type_ with 0 -> "BUY " | _ -> "SELL" in
           printf "  %s  %s  price=%s  amount=%s  pair=%s\n"
             o.id side o.price o.amount o.currency_pair)
       | Error e ->
         eprintf "Error: %s\n" (Rest.Error.to_string e)))

let orderbook_ws_command : string * Command.t =
  ("orderbook", Command.async
    ~summary:"Stream live order book via WebSocket"
    (let%map_open.Command pair = flag "--pair" (required string)
        ~doc:"STRING Trading pair (e.g. btcusd)"
     and limit = flag "--limit"
        (optional_with_default 20 int)
        ~doc:"INT number of updates (default 20, 0 for unlimited)"
     in
     fun () ->
       printf "Streaming order book for %s...\n\n" pair;
       Order_book.Book.pipe ~cfg ~pair ()
       >>= fun book_pipe ->
       let update_count = ref 0 in
       Pipe.iter book_pipe ~f:(fun book_result ->
         incr update_count;
         match book_result with
         | Ok book ->
           printf "\027[2J\027[H";
           printf "=== Order Book Update #%d - %s ===\n\n" !update_count pair;
           let best_bid = Order_book.Book.best_bid book in
           let best_ask = Order_book.Book.best_ask book in
           let spread = Order_book.Book.spread book in
           printf "Best Bid: %.2f (vol: %.8f)\n" best_bid.price best_bid.volume;
           printf "Best Ask: %.2f (vol: %.8f)\n" best_ask.price best_ask.volume;
           printf "Spread:   %.2f\n\n" spread;
           let top_bids = Order_book.Book.best_n_bids book ~n:5 () in
           let top_asks = Order_book.Book.best_n_asks book ~n:5 () in
           printf "%-20s | %-20s\n" "BIDS" "ASKS";
           printf "%-20s | %-20s\n" "--------------------" "--------------------";
           List.iter2_exn
             (List.take (top_bids @ List.init 5 ~f:(fun _ -> Order_book.Price_level.empty)) 5)
             (List.take (top_asks @ List.init 5 ~f:(fun _ -> Order_book.Price_level.empty)) 5)
             ~f:(fun bid ask ->
               printf "%.4f @ %.2f | %.2f @ %.4f\n"
                 bid.volume bid.price ask.price ask.volume);
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

let session_command : string * Command.t =
  ("session", Command.async
    ~summary:"Create full session with all event streams"
    (let%map_open.Command pairs_str = flag "--pairs" (required string)
        ~doc:"STRING comma-separated pairs (e.g. btcusd,ethusd)"
     and limit = flag "--limit"
        (optional_with_default 50 int)
        ~doc:"INT number of events per stream (default 50)"
     in
     fun () ->
       let pairs = String.split pairs_str ~on:',' |> List.map ~f:String.strip in
       printf "Creating session for: %s\n\n" (String.concat ~sep:", " pairs);
       Session.create ~cfg ~symbols:pairs ()
       >>= fun session ->
       printf "Session state: %s\n"
         (Sexp.to_string_hum (Fluxum.Session_intf.State.sexp_of_t (Session.state session)));

       let events = Session.events session in

       don't_wait_for (
         Pipe.iter (Session.state_changes session) ~f:(fun state ->
           printf "\n>>> State: %s\n\n"
             (Sexp.to_string_hum (Fluxum.Session_intf.State.sexp_of_t state));
           Deferred.unit)
       );

       let book_monitors = List.map pairs ~f:(fun pair ->
         match Map.find (Session.Events.order_books events) pair with
         | Some book_pipe ->
           let count = ref 0 in
           Pipe.iter book_pipe ~f:(fun book_result ->
             incr count;
             match book_result with
             | Ok book ->
               printf "[%s] Book #%d - Bid: $%.2f  Ask: $%.2f\n"
                 pair !count
                 (Order_book.Book.best_bid book).price
                 (Order_book.Book.best_ask book).price;
               (match !count >= limit with
                | true -> Pipe.close_read book_pipe
                | false -> ());
               Deferred.unit
             | Error err ->
               eprintf "[%s] Book error: %s\n" pair err;
               Deferred.unit)
         | None ->
           printf "No order book for %s\n" pair;
           Deferred.unit
       ) in

       Deferred.all_unit book_monitors
       >>= fun () -> Session.close session
       >>= fun () ->
       printf "\nSession closed.\n";
       Deferred.unit
    ))

let command : Command.t =
  Command.group ~summary:"Bitstamp Exchange Commands"
    [ ticker_command
    ; depth_command
    ; trades_command
    ; pairs_command
    ; balances_command
    ; open_orders_command
    ; orderbook_ws_command
    ; session_command
    ]

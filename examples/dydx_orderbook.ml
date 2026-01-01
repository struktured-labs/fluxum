open Core
open Async

(** dYdX v4 Order Book Example

    Streams real-time order book data from dYdX v4 perpetual markets.

    Usage:
      dune exec examples/dydx_orderbook.exe -- --market BTC-USD --depth 10
*)

let display_book book ~depth =
  let symbol = Dydx.Order_book.Book.symbol book in
  let epoch = Dydx.Order_book.Book.epoch book in
  let mid = Dydx.Order_book.Book.mid_price book in
  let spread = Dydx.Order_book.Book.spread book in

  printf "\n=== dYdX %s Order Book (Epoch: %d) ===\n" symbol epoch;
  printf "Mid: $%.2f | Spread: $%.2f (%.4f%%)\n\n"
    mid spread (spread /. mid *. 100.);

  (* Display asks (sell orders) *)
  printf "Asks (Sell Orders):\n";
  let asks = Dydx.Order_book.Book.best_n_asks book ~n:depth () in
  List.iter (List.rev asks) ~f:(fun level ->
    printf "  %.2f @ %.8f\n"
      Exchange_common.Order_book_base.Price_level.(price level)
      Exchange_common.Order_book_base.Price_level.(volume level)
  );

  printf "\n--- Spread: $%.2f (%.4f%%) ---\n\n" spread (spread /. mid *. 100.);

  (* Display bids (buy orders) *)
  printf "Bids (Buy Orders):\n";
  let bids = Dydx.Order_book.Book.best_n_bids book ~n:depth () in
  List.iter bids ~f:(fun level ->
    printf "  %.2f @ %.8f\n"
      Exchange_common.Order_book_base.Price_level.(price level)
      Exchange_common.Order_book_base.Price_level.(volume level)
  );

  printf "\nEpoch: %d | Spread: $%.2f\n%!" epoch spread

let run ~market ~depth () =
  let module Cfg = Dydx.Cfg.Production in

  printf "Connecting to dYdX v4 mainnet...\n";
  printf "Market: %s\n" market;
  printf "Depth: %d levels\n%!" depth;

  let%bind book_pipe =
    Dydx.Order_book.Book.pipe
      (module Cfg)
      ~symbol:market
      ()
  in

  (* Track update count *)
  let update_count = ref 0 in

  Pipe.iter book_pipe ~f:(fun book ->
    Int.incr update_count;

    (* Display book every 10 updates to avoid spam *)
    if !update_count % 10 = 0 then
      display_book book ~depth;

    return ()
  )

let () =
  Command.async ~summary:"Stream dYdX v4 order book"
    (let%map_open.Command
       market = flag "--market" (optional_with_default "BTC-USD" string)
         ~doc:"MARKET Trading pair (e.g., BTC-USD, ETH-USD)"
     and depth = flag "--depth" (optional_with_default 10 int)
         ~doc:"DEPTH Number of price levels to display (default: 10)"
     in
     fun () -> run ~market ~depth ()
    )
  |> Command_unix.run

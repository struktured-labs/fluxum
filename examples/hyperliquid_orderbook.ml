open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Connecting to Hyperliquid order book for BTC...\n%!";

  (* Create order book pipe *)
  let%bind book_pipe = Hyperliquid.Order_book.Book.pipe ~symbol:"BTC" () in
  printf "✓ Connected to Hyperliquid WebSocket\n%!";

  (* Display updates *)
  let start_time = Time_float_unix.now () in
  let update_count = ref 0 in

  printf "Waiting for order book data...\n%!";

  let%bind () =
    Pipe.iter book_pipe ~f:(fun book_result ->
      printf "Received data from pipe\n%!";
      match book_result with
      | Error err ->
        printf "Error: %s\n%!" err;
        return ()
      | Ok book ->
        update_count := !update_count + 1;
        let elapsed = Time_float_unix.diff (Time_float_unix.now ()) start_time in

        (* Display order book info *)
        printf "\n=== Hyperliquid %s Order Book (Update #%d) ===\n"
          book.symbol !update_count;
        printf "Epoch: %d | Elapsed: %.1fs\n" book.epoch
          (Time_float_unix.Span.to_sec elapsed);

        (* Get best bid and ask *)
        let best_bid = Hyperliquid.Order_book.Book.best_bid book in
        let best_ask = Hyperliquid.Order_book.Book.best_ask book in

        printf "Best Bid: %.2f (%.4f) | Best Ask: %.2f (%.4f)\n"
          best_bid.price best_bid.volume
          best_ask.price best_ask.volume;

        let spread = best_ask.price -. best_bid.price in
        printf "Spread: $%.2f\n" spread;

        printf "Press Ctrl+C to exit\n%!";

        return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  Command.async
    ~summary:"Test Hyperliquid order book WebSocket"
    (Command.Param.return test)
  |> Command_unix.run

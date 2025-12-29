open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Testing Binance.US WebSocket orderbook for BTC/USDT...\n%!";

  let%bind book_pipe = Binance.Order_book.Book.pipe ~symbol:"BTCUSDT" ~depth:20 () in

  printf "✓ Connected to Binance.US\n%!";

  let%bind () =
    Pipe.iter book_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Book error: %s\n%!" err;
        return ()
      | Ok book ->
        printf "Received book update (epoch: %d)\n%!" (Binance.Order_book.Book.epoch book);
        let best_bid = Binance.Order_book.Book.best_bid book in
        let best_ask = Binance.Order_book.Book.best_ask book in
        printf "  Best bid: %.2f (%.4f)\n" best_bid.price best_bid.volume;
        printf "  Best ask: %.2f (%.4f)\n" best_ask.price best_ask.volume;
        printf "  Spread: $%.2f\n\n%!" (best_ask.price -. best_bid.price);
        return ()
    )
  in

  printf "Stream ended\n%!";
  return ()

let () =
  Command.async
    ~summary:"Binance orderbook test"
    (Command.Param.return test)
  |> Command_unix.run

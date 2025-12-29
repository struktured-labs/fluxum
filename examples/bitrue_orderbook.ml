open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Testing Bitrue WebSocket orderbook for BTC/USDT...\\n%!";

  let symbol = Fluxum.Types.Symbol.of_string "BTCUSDT" in
  let%bind book_pipe_result = Bitrue.Order_book.Book.pipe ~symbol () in

  match book_pipe_result with
  | Error err ->
    eprintf "Failed to create pipe: %s\\n%!" (Error.to_string_hum err);
    return ()
  | Ok book_pipe ->
    printf "✓ Connected to Bitrue\\n%!";

    let%bind () =
      Pipe.iter book_pipe ~f:(fun book_result ->
        match book_result with
        | Error err ->
          eprintf "Book error: %s\\n%!" err;
          return ()
        | Ok book ->
          printf "Received book update (epoch: %d)\\n%!" (Bitrue.Order_book.Book.epoch book);
          let best_bid = Bitrue.Order_book.Book.best_bid book in
          let best_ask = Bitrue.Order_book.Book.best_ask book in
          printf "  Best bid: %.2f (%.4f)\\n" best_bid.price best_bid.volume;
          printf "  Best ask: %.2f (%.4f)\\n" best_ask.price best_ask.volume;
          printf "  Spread: $%.2f\\n\\n%!" (best_ask.price -. best_bid.price);
          return ()
      )
    in

    printf "Stream ended\\n%!";
    return ()

let () =
  Command.async
    ~summary:"Bitrue orderbook test"
    (Command.Param.return test)
  |> Command_unix.run

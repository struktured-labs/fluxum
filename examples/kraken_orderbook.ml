open Core
open Async
open Kraken

let test () =
  let open Deferred.Let_syntax in

  printf "Connecting to Kraken order book for BTC/USD...\n%!";

  (* Create order book pipe *)
  let%bind book_pipe = Order_book.Book.pipe ~symbol:"XBT/USD" ~depth:10 () in

  (* Display updates for 10 seconds *)
  let start_time = Time_float_unix.now () in
  let update_count = ref 0 in

  let%bind () =
    Pipe.iter book_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        printf "Error: %s\n%!" err;
        return ()
      | Ok book ->
        update_count := !update_count + 1;
        let elapsed = Time_float_unix.diff (Time_float_unix.now ()) start_time in

        (* Display order book *)
        let mid = Order_book.Book.mid_price book in
        let spread = Order_book.Book.spread book in
        let best_bid = Order_book.Book.best_bid book in
        let best_ask = Order_book.Book.best_ask book in

        printf "=== Kraken BTC/USD (Update #%d) ===\n" !update_count;
        printf "Mid: $%.2f | Spread: $%.2f\n"mid spread;
        printf "Best Bid: %.2f @ %.8f\n"
          Exchange_common.Order_book_base.Price_level.(price best_bid)
          Exchange_common.Order_book_base.Price_level.(volume best_bid);
        printf "Best Ask: %.2f @ %.8f\n"
          Exchange_common.Order_book_base.Price_level.(price best_ask)
          Exchange_common.Order_book_base.Price_level.(volume best_ask);
        printf "Elapsed: %.1fs | Press Ctrl+C to exit\n\n%!"
          (Time_float_unix.Span.to_sec elapsed);

        return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  Command.async
    ~summary:"Test Kraken order book with curl WebSocket"
    (Command.Param.return test)
  |> Command_unix.run

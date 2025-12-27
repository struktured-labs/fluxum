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

        (* Pretty print the order book *)
        Order_book.Book.pretty_print ~max_depth:5 book ();

        (* Show update stats *)
        printf "Update #%d (%.1fs elapsed)\n" !update_count
          (Time_float_unix.Span.to_sec elapsed);
        printf "Press Ctrl+C to exit\n\n%!";

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

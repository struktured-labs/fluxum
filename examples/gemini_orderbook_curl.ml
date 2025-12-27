open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Testing Gemini order book with libcurl...\n%!";

  let module Gemini_cfg = Gemini.Cfg.Production () in
  let%bind gemini_pipe = Gemini.Order_book.Book.pipe_curl (module Gemini_cfg) ~symbol:`Btcusd () in

  printf "✓ Connected to Gemini libcurl WebSocket\n%!";
  printf "Waiting for order book data...\n%!";

  let start_time = Time_float_unix.now () in
  let update_count = ref 0 in

  let%bind () =
    Pipe.iter gemini_pipe ~f:(fun book_result ->
      update_count := !update_count + 1;
      let elapsed = Time_float_unix.diff (Time_float_unix.now ()) start_time in

      match book_result with
      | `Ok book ->
        printf "\n[Update #%d] Epoch: %d | Elapsed: %.1fs\n"
          !update_count (Gemini.Order_book.Book.epoch book)
          (Time_float_unix.Span.to_sec elapsed);
        return ()
      | `Channel_parse_error err ->
        eprintf "Channel parse error: %s\n%!" err;
        return ()
      | `Json_parse_error err ->
        eprintf "JSON parse error: %s\n%!" err;
        return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  Command.async
    ~summary:"Test Gemini order book with libcurl WebSocket"
    (Command.Param.return test)
  |> Command_unix.run

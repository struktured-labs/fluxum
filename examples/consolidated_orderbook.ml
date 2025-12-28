open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Starting consolidated order book for BTC/USD...\n";
  printf "Connecting to Gemini, Kraken, and Hyperliquid WebSockets...\n\n%!";

  (* Create consolidated book *)
  let consolidated = ref (Consolidated_order_book.Book.empty "BTC/USD") in
  let update_count = ref 0 in
  let last_print_time = ref (Time_float_unix.now ()) in

  (* Subscribe to Gemini order book (using libcurl for unified WebSocket layer) *)
  let module Gemini_cfg = Gemini.Cfg.Production () in
  let%bind gemini_pipe = Gemini.Order_book.Book.pipe_curl (module Gemini_cfg) ~symbol:`Btcusd () in

  (* Subscribe to Kraken order book *)
  let%bind kraken_pipe = Kraken.Order_book.Book.pipe ~symbol:"XBT/USD" ~depth:10 () in

  (* Subscribe to Hyperliquid order book *)
  let%bind hyperliquid_pipe = Hyperliquid.Order_book.Book.pipe ~symbol:"BTC" () in

  printf "✓ Connected to all three exchanges\n\n%!";

  (* Process Gemini updates in background *)
  let gemini_first_update = ref true in
  don't_wait_for (
    Pipe.iter gemini_pipe ~f:(fun book_result ->
      match book_result with
      | `Ok gemini_book ->
        if !gemini_first_update then (
          printf "[INFO] Gemini data connected!\n%!";
          gemini_first_update := false
        );
        consolidated := Consolidated_order_book.Book.update_gemini !consolidated gemini_book;
        return ()
      | `Channel_parse_error err ->
        eprintf "[GEMINI] Channel parse error: %s\n%!" err;
        return ()
      | `Json_parse_error err ->
        eprintf "[GEMINI] JSON parse error: %s\n%!" err;
        return ()
    )
  );

  (* Process Hyperliquid updates in background *)
  let hyperliquid_first_update = ref true in
  don't_wait_for (
    Pipe.iter hyperliquid_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Hyperliquid error: %s\n%!" err;
        return ()
      | Ok hyperliquid_book ->
        if !hyperliquid_first_update then (
          printf "[INFO] Hyperliquid data connected!\n%!";
          hyperliquid_first_update := false
        );
        consolidated := Consolidated_order_book.Book.update_hyperliquid !consolidated hyperliquid_book;
        return ()
    )
  );

  (* Process Kraken updates in foreground *)
  let%bind () =
    Pipe.iter kraken_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Kraken error: %s\n%!" err;
        return ()
      | Ok kraken_book ->
        consolidated := Consolidated_order_book.Book.update_kraken !consolidated kraken_book;
        update_count := !update_count + 1;

        (* Print every 5 updates or every 2 seconds *)
        let now = Time_float_unix.now () in
        let time_since_print = Time_float_unix.diff now !last_print_time in
        let should_print =
          !update_count mod 5 = 0 ||
          Time_float_unix.Span.(time_since_print > of_sec 2.0)
        in

        if should_print then (
          last_print_time := now;
          Consolidated_order_book.Book.pretty_print ~max_depth:8 !consolidated ();
        );

        return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  Command.async
    ~summary:"Consolidated order book (Kraken + Gemini support)"
    ~readme:(fun () ->
      "Demonstrates consolidated order book from multiple exchanges.\n\
      Currently connected to Kraken. To add Gemini, set environment variables:\n\
      GEMINI_SANDBOX_API_KEY and GEMINI_SANDBOX_API_SECRET")
    (Command.Param.return test)
  |> Command_unix.run

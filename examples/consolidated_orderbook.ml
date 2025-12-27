open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Starting consolidated order book for BTC/USD...\n";
  printf "Connecting to Kraken WebSocket...\n\n%!";

  (* Create consolidated book *)
  let consolidated = ref (Consolidated_order_book.Book.empty "BTC/USD") in
  let update_count = ref 0 in

  (* Subscribe to Kraken order book *)
  let%bind kraken_pipe = Kraken.Order_book.Book.pipe ~symbol:"XBT/USD" ~depth:10 () in

  (* For demonstration: You can add Gemini here once credentials are set up:
   *
   * module Gemini_cfg = Gemini.Cfg.Sandbox ()
   * let%bind gemini_pipe = Gemini.Order_book.Book.pipe
   *   (module Gemini_cfg) ~symbol:"BTCUSD" () in
   *
   * Then handle gemini updates similar to kraken below
   *)

  (* Process Kraken updates *)
  let%bind () =
    Pipe.iter kraken_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        printf "Kraken error: %s\n%!" err;
        return ()
      | Ok kraken_book ->
        consolidated := Consolidated_order_book.Book.update_kraken !consolidated kraken_book;
        update_count := !update_count + 1;

        (* Print every 5 updates *)
        if !update_count mod 5 = 0 then
          Consolidated_order_book.Book.pretty_print ~max_depth:8 !consolidated ();

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

(** Enhanced Consolidated Order Book Example

    Demonstrates:
    - Multi-exchange order book aggregation
    - Arbitrage opportunity detection
    - Market analytics and metrics
    - Volume-weighted average prices
*)

open Core
open Async

let main () =
  printf "Starting enhanced consolidated order book...\n%!";

  (* Create consolidated book for BTC/USD *)
  let book_ref = ref (Consolidated_order_book.Book.empty "BTC/USD") in
  let updates = ref 0 in

  (* Connect to Gemini *)
  let module Gemini_cfg = Gemini.Cfg.Production () in
  let%bind gemini_pipe = Gemini.Order_book.Book.pipe (module Gemini_cfg) ~symbol:`Btcusd () in

  (* Connect to Kraken *)
  let%bind kraken_pipe = Kraken.Order_book.Book.pipe ~symbol:"XBT/USD" () in

  (* Process Gemini updates *)
  don't_wait_for (
    Pipe.iter gemini_pipe ~f:(function
      | `Ok gemini_book ->
        book_ref := Consolidated_order_book.Book.update_gemini !book_ref gemini_book;
        updates := !updates + 1;

        (* Print consolidated book every 10 updates *)
        if !updates % 10 = 0 then (
          Consolidated_order_book.Book.pretty_print ~max_depth:5 !book_ref ();

          (* Calculate and print analytics *)
          let metrics = Consolidated_order_book.Analytics.calculate !book_ref in
          Consolidated_order_book.Analytics.print metrics;

          (* Detect and print arbitrage opportunities *)
          let opportunities = Consolidated_order_book.Arbitrage.detect_opportunities !book_ref in
          Consolidated_order_book.Arbitrage.print_opportunities opportunities;

          (* Calculate VWAP for different volumes *)
          printf "\n=== Volume-Weighted Average Prices ===\n";
          (match Consolidated_order_book.Book.vwap_ask !book_ref ~volume:1.0 with
           | Some vwap -> printf "VWAP to buy 1.0 BTC: $%.2f\n" vwap
           | None -> printf "Insufficient liquidity to buy 1.0 BTC\n");
          (match Consolidated_order_book.Book.vwap_bid !book_ref ~volume:1.0 with
           | Some vwap -> printf "VWAP to sell 1.0 BTC: $%.2f\n" vwap
           | None -> printf "Insufficient liquidity to sell 1.0 BTC\n");
          printf "\n%!";
        );
        Deferred.unit
      | `Channel_parse_error err ->
        printf "Gemini channel parse error: %s\n%!" err;
        Deferred.unit
      | `Json_parse_error err ->
        printf "Gemini JSON parse error: %s\n%!" err;
        Deferred.unit
    )
  );

  (* Process Kraken updates *)
  don't_wait_for (
    Pipe.iter kraken_pipe ~f:(function
      | Ok kraken_book ->
        book_ref := Consolidated_order_book.Book.update_kraken !book_ref kraken_book;
        Deferred.unit
      | Error err ->
        printf "Kraken error: %s\n%!" err;
        Deferred.unit
    )
  );

  (* Keep running *)
  Deferred.never ()

let () =
  Command.async
    ~summary:"Enhanced consolidated order book with arbitrage detection"
    Command.Param.(return (fun () -> main ()))
  |> Command_unix.run

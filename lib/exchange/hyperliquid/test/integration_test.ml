(** Hyperliquid Integration Tests - Hits actual API endpoints *)

open Core
open Async

let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  * %s\n" msg

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  X FAIL: %s\n" msg

(* ============================================================ *)
(* REST API Integration Tests *)
(* ============================================================ *)

let test_all_mids () =
  printf "\n[REST] All Mid Prices\n";
  let cfg = Hyperliquid.Cfg.production in
  Hyperliquid.Rest.all_mids cfg >>| function
  | Ok mids ->
    (match List.length mids > 0 with
     | true ->
       pass (sprintf "Got %d mid prices" (List.length mids));
       (* Find BTC *)
       (match List.find mids ~f:(fun (coin, _) -> String.equal coin "BTC") with
        | Some (_, price) -> pass (sprintf "BTC mid price: %s" price)
        | None -> pass "BTC not in list (may be on different market)")
     | false -> fail "No mid prices returned")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err)))

let test_l2_book () =
  printf "\n[REST] L2 Order Book\n";
  let cfg = Hyperliquid.Cfg.production in
  Hyperliquid.Rest.l2_book cfg ~coin:"BTC" >>| function
  | Ok book ->
    pass (sprintf "Coin: %s" book.coin);
    (match book.levels with
     | [bids; asks] ->
       pass (sprintf "Bids: %d levels, Asks: %d levels"
         (List.length bids) (List.length asks));
       (match List.length bids > 0 with
        | true ->
          let best_bid = List.hd_exn bids in
          pass (sprintf "Best bid: %s @ %s" best_bid.sz best_bid.px)
        | false -> ());
       (match List.length asks > 0 with
        | true ->
          let best_ask = List.hd_exn asks in
          pass (sprintf "Best ask: %s @ %s" best_ask.sz best_ask.px)
        | false -> ())
     | _ -> fail "Unexpected levels structure")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err)))

let test_meta () =
  printf "\n[REST] Exchange Metadata\n";
  let cfg = Hyperliquid.Cfg.production in
  Hyperliquid.Rest.meta cfg >>| function
  | Ok meta ->
    let count = List.length meta.universe in
    pass (sprintf "Universe has %d assets" count);
    (match count > 0 with
     | true ->
       let first = List.hd_exn meta.universe in
       pass (sprintf "First asset: %s (decimals: %d)" first.name first.szDecimals)
     | false -> ())
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err)))

let test_meta_and_asset_ctxs () =
  printf "\n[REST] Metadata with Asset Contexts\n";
  let cfg = Hyperliquid.Cfg.production in
  Hyperliquid.Rest.meta_and_asset_ctxs cfg >>| function
  | Ok (meta, ctxs) ->
    pass (sprintf "Universe: %d assets, Contexts: %d"
      (List.length meta.universe) (List.length ctxs));
    (match List.length ctxs > 0 with
     | true ->
       let ctx = List.hd_exn ctxs in
       pass (sprintf "First context - OI: %s, Funding: %s" ctx.openInterest ctx.funding)
     | false -> ())
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err)))

let test_recent_trades () =
  printf "\n[REST] Recent Trades\n";
  let cfg = Hyperliquid.Cfg.production in
  Hyperliquid.Rest.recent_trades cfg ~coin:"ETH" >>| function
  | Ok trades ->
    (match List.length trades > 0 with
     | true ->
       pass (sprintf "Got %d recent trades" (List.length trades));
       let trade = List.hd_exn trades in
       pass (sprintf "Latest: %s %s @ %s" trade.side trade.sz trade.px)
     | false -> pass "No recent trades (market may be quiet)")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Hyperliquid.Rest.Error.sexp_of_t err)))

(* ============================================================ *)
(* WebSocket Integration Tests *)
(* ============================================================ *)

let test_websocket_connection () =
  printf "\n[WebSocket] Connection and Subscription\n";
  let streams = [
    Hyperliquid.Ws.Stream.AllMids;
    Hyperliquid.Ws.Stream.Trades "BTC";
  ] in
  Hyperliquid.Ws.connect ~streams () >>= function
  | Error err ->
    fail (sprintf "Connection error: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected to WebSocket";
    (* Read a few messages with timeout *)
    let messages_received = ref 0 in
    let start_time = Time_float_unix.now () in
    let timeout = Time_float_unix.Span.of_sec 15.0 in

    let rec read_loop () =
      let elapsed = Time_float_unix.diff (Time_float_unix.now ()) start_time in
      (match Time_float_unix.Span.(elapsed > timeout) with
       | true ->
         (match !messages_received > 0 with
          | true -> pass (sprintf "Received %d messages before timeout" !messages_received)
          | false -> fail "No messages received within timeout");
         return ()
       | false ->
        match%bind Clock.with_timeout (Time_float.Span.of_sec 3.0) (Pipe.read (Hyperliquid.Ws.messages ws)) with
        | `Timeout ->
          (match !messages_received > 0 with
           | true ->
             pass (sprintf "Received %d messages total" !messages_received);
             return ()
           | false -> read_loop ())
        | `Result `Eof ->
          (match !messages_received > 0 with
           | true -> pass (sprintf "Connection closed after %d messages" !messages_received)
           | false -> fail "Connection closed without receiving messages");
          return ()
        | `Result (`Ok msg) ->
          incr messages_received;
          let parsed = Hyperliquid.Ws.parse_message msg in
          (match parsed with
           | Hyperliquid.Ws.Message.SubscriptionResponse resp ->
             pass (sprintf "Subscription ack: %s" resp.method_)
           | Hyperliquid.Ws.Message.AllMids mids ->
             pass (sprintf "AllMids update: %d prices" (List.length mids))
           | Hyperliquid.Ws.Message.Trades trades ->
             pass (sprintf "Trades update: %d trades" (List.length trades))
           | Hyperliquid.Ws.Message.L2Book book ->
             pass (sprintf "L2Book for %s" book.coin)
           | Hyperliquid.Ws.Message.Bbo bbo ->
             pass (sprintf "BBO for %s" bbo.coin)
           | Hyperliquid.Ws.Message.Pong ->
             pass "Received pong"
           | Hyperliquid.Ws.Message.Error err ->
             fail (sprintf "Error: %s" err)
           | Hyperliquid.Ws.Message.Unknown _ ->
             pass "Received message (unknown type)"
           | _ ->
             pass "Received other message type");
          (match !messages_received >= 5 with
           | true ->
             pass (sprintf "Successfully received %d messages" !messages_received);
             Hyperliquid.Ws.close ws;
             return ()
           | false -> read_loop ()))
    in
    read_loop ()

let test_websocket_l2_book () =
  printf "\n[WebSocket] L2 Book Subscription\n";
  let streams = [
    Hyperliquid.Ws.Stream.L2Book { coin = "ETH"; n_sig_figs = None; mantissa = None };
  ] in
  Hyperliquid.Ws.connect ~streams () >>= function
  | Error err ->
    fail (sprintf "Connection error: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected for L2 book";
    let messages_received = ref 0 in

    let rec read_loop () =
      (match !messages_received >= 3 with
       | true ->
         pass (sprintf "Received %d L2 book updates" !messages_received);
         Hyperliquid.Ws.close ws;
         return ()
       | false ->
        match%bind Clock.with_timeout (Time_float.Span.of_sec 5.0) (Pipe.read (Hyperliquid.Ws.messages ws)) with
        | `Timeout ->
          (match !messages_received > 0 with
           | true -> pass (sprintf "Got %d updates before timeout" !messages_received)
           | false -> fail "No L2 book updates received");
          Hyperliquid.Ws.close ws;
          return ()
        | `Result `Eof ->
          Hyperliquid.Ws.close ws;
          return ()
        | `Result (`Ok msg) ->
          let parsed = Hyperliquid.Ws.parse_message msg in
          (match parsed with
           | Hyperliquid.Ws.Message.L2Book book ->
             incr messages_received;
             (match book.levels with
              | [bids; asks] ->
                pass (sprintf "ETH book: %d bids, %d asks"
                  (List.length bids) (List.length asks))
              | _ -> ())
           | Hyperliquid.Ws.Message.SubscriptionResponse _ ->
             pass "Subscription confirmed"
           | _ -> ());
          read_loop ())
    in
    read_loop ()

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "Hyperliquid Integration Tests (Live API)\n";
  printf "===========================================\n";

  (* REST API tests *)
  test_all_mids () >>= fun () ->
  test_l2_book () >>= fun () ->
  test_meta () >>= fun () ->
  test_meta_and_asset_ctxs () >>= fun () ->
  test_recent_trades () >>= fun () ->

  (* WebSocket tests *)
  test_websocket_connection () >>= fun () ->
  test_websocket_l2_book () >>= fun () ->

  (* Summary *)
  printf "\n===========================================\n";
  printf "Integration Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  match !tests_failed > 0 with
  | true ->
    printf "\nSome tests failed\n";
    exit 1
  | false -> return ()

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

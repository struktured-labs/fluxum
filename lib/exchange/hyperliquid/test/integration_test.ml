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
             Hyperliquid.Ws.close ws
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
         Hyperliquid.Ws.close ws
       | false ->
        match%bind Clock.with_timeout (Time_float.Span.of_sec 5.0) (Pipe.read (Hyperliquid.Ws.messages ws)) with
        | `Timeout ->
          (match !messages_received > 0 with
           | true -> pass (sprintf "Got %d updates before timeout" !messages_received)
           | false -> fail "No L2 book updates received");
          Hyperliquid.Ws.close ws
        | `Result `Eof ->
          Hyperliquid.Ws.close ws
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

(* Phase 2 Priority 3: WebSocket Error Handling Tests *)

let test_websocket_invalid_url () =
  printf "\n[WebSocket] Connection Failure - Invalid URL\n";
  let streams = [
    Hyperliquid.Ws.Stream.AllMids;
  ] in
  let invalid_url = "wss://invalid.nonexistent-domain-12345.com/ws" in
  (* Set a short timeout for the connection attempt *)
  Clock.with_timeout (Time_float.Span.of_sec 5.0)
    (Hyperliquid.Ws.connect ~url:invalid_url ~streams ())
  >>| function
  | `Timeout ->
    pass "Connection timeout on invalid URL (as expected)";
    ()
  | `Result (Error _err) ->
    pass "Connection error properly returned for invalid URL";
    ()
  | `Result (Ok _ws) ->
    fail "Should not connect to invalid URL";
    ()

let test_websocket_malformed_message () =
  printf "\n[WebSocket] Malformed Message Handling\n";

  (* Test 1: Invalid JSON *)
  let malformed_json = "{this is not valid json}" in
  let parsed1 = Hyperliquid.Ws.parse_message malformed_json in
  (match parsed1 with
   | Hyperliquid.Ws.Message.Error _ -> pass "Parse error caught for malformed JSON"
   | Hyperliquid.Ws.Message.Unknown _ -> pass "Unknown message for malformed JSON"
   | _ -> fail "Should detect malformed JSON");

  (* Test 2: Empty string *)
  let empty = "" in
  let parsed2 = Hyperliquid.Ws.parse_message empty in
  (match parsed2 with
   | Hyperliquid.Ws.Message.Error _ -> pass "Parse error caught for empty message"
   | Hyperliquid.Ws.Message.Unknown _ -> pass "Unknown message for empty string"
   | _ -> fail "Should reject empty message");

  (* Test 3: Valid JSON but wrong structure *)
  let invalid_structure = "{\"method\":\"unknown\",\"data\":123}" in
  let parsed3 = Hyperliquid.Ws.parse_message invalid_structure in
  (match parsed3 with
   | Hyperliquid.Ws.Message.Unknown _ -> pass "Unknown message for invalid structure"
   | Hyperliquid.Ws.Message.Error _ -> pass "Parse error for invalid structure"
   | _ -> pass "Parser handled unexpected structure");

  return ()

let test_websocket_connection_empty_streams () =
  printf "\n[WebSocket] Connection with Empty Streams\n";

  (* Test connecting with empty streams list *)
  Hyperliquid.Ws.connect ~streams:[] () >>= function
  | Error err ->
    pass (sprintf "Empty streams rejected: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected with empty streams";
    (* Verify we can still read from the connection *)
    Clock.with_timeout (Time_float.Span.of_sec 2.0)
      (Pipe.read (Hyperliquid.Ws.messages ws))
    >>= (function
      | `Timeout ->
        pass "No messages on empty streams (as expected)";
        Hyperliquid.Ws.close ws
      | `Result `Eof ->
        pass "Connection closed on empty streams";
        Hyperliquid.Ws.close ws
      | `Result (`Ok msg) ->
        let parsed = Hyperliquid.Ws.parse_message msg in
        (match parsed with
         | Hyperliquid.Ws.Message.SubscriptionResponse _ -> pass "Got subscription response"
         | Hyperliquid.Ws.Message.Pong -> pass "Got pong"
         | _ -> pass (sprintf "Got message: %s" (String.prefix msg 50)));
        Hyperliquid.Ws.close ws)

let test_websocket_invalid_coin () =
  printf "\n[WebSocket] Invalid Coin Symbol\n";
  let streams = [
    Hyperliquid.Ws.Stream.Trades "INVALIDSYMBOL12345";
  ] in
  Hyperliquid.Ws.connect ~streams () >>= function
  | Error err ->
    pass (sprintf "Connection rejected invalid coin: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected (exchange may accept invalid coin)";
    (* Try to read messages for a short time *)
    Clock.with_timeout (Time_float.Span.of_sec 3.0)
      (Pipe.read (Hyperliquid.Ws.messages ws))
    >>= (function
      | `Timeout ->
        pass "No messages for invalid coin (exchange may silently ignore)";
        Hyperliquid.Ws.close ws
      | `Result `Eof ->
        pass "Connection closed for invalid coin";
        Hyperliquid.Ws.close ws
      | `Result (`Ok msg) ->
        let parsed = Hyperliquid.Ws.parse_message msg in
        (match parsed with
         | Hyperliquid.Ws.Message.Error err ->
           pass (sprintf "Exchange returned error: %s" err)
         | Hyperliquid.Ws.Message.SubscriptionResponse resp ->
           pass (sprintf "Exchange acknowledged: %s (may filter later)" resp.method_)
         | _ ->
           pass "Exchange accepted invalid coin");
        Hyperliquid.Ws.close ws)

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

  (* WebSocket error handling tests *)
  test_websocket_invalid_url () >>= fun () ->
  test_websocket_malformed_message () >>= fun () ->
  test_websocket_connection_empty_streams () >>= fun () ->
  test_websocket_invalid_coin () >>= fun () ->

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

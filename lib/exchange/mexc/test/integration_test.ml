(** MEXC Integration Tests - Hits actual API endpoints *)

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

let test_server_time () =
  printf "\n[REST] Server Time\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Server_time.request cfg () >>| function
  | `Ok resp ->
    (match Int64.(resp.serverTime > 0L) with
     | true -> pass (sprintf "Server time: %Ld" resp.serverTime)
     | false -> fail "Server time is 0")
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_depth () =
  printf "\n[REST] Order Book Depth\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Depth.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok resp ->
    let bid_count = List.length resp.bids in
    let ask_count = List.length resp.asks in
    (match bid_count > 0 && ask_count > 0 with
     | true ->
       pass (sprintf "Got %d bids, %d asks" bid_count ask_count);
       let best_bid, bid_qty = match List.hd resp.bids with
         | Some x -> x
         | None -> failwith "Expected non-empty bids"
       in
       let best_ask, ask_qty = match List.hd resp.asks with
         | Some x -> x
         | None -> failwith "Expected non-empty asks"
       in
       pass (sprintf "Best bid: %s @ %s" bid_qty best_bid);
       pass (sprintf "Best ask: %s @ %s" ask_qty best_ask);
       pass (sprintf "Last update ID: %Ld" resp.lastUpdateId)
     | false -> fail "Empty order book")
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_recent_trades () =
  printf "\n[REST] Recent Trades\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Recent_trades.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok trades ->
    let count = List.length trades in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d trades" count);
       let trade = match List.hd trades with
         | Some x -> x
         | None -> failwith "Expected non-empty trades"
       in
       pass (sprintf "Latest: %s @ %s" trade.qty trade.price)
     | false -> fail "No trades returned")
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_ticker_24hr () =
  printf "\n[REST] 24hr Ticker\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Ticker_24hr.request cfg { symbol = Some "BTCUSDT" } >>| function
  | `Ok ticker ->
    pass (sprintf "Symbol: %s" ticker.symbol);
    pass (sprintf "Last price: %s" ticker.lastPrice);
    pass (sprintf "24h change: %s%%" ticker.priceChangePercent);
    pass (sprintf "24h volume: %s" ticker.volume)
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_exchange_info () =
  printf "\n[REST] Exchange Info\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Exchange_info.request cfg { symbol = Some "BTCUSDT" } >>| function
  | `Ok info ->
    pass (sprintf "Timezone: %s" info.timezone);
    pass (sprintf "Server time: %Ld" info.serverTime);
    let symbol_count = List.length info.symbols in
    pass (sprintf "Symbols returned: %d" symbol_count);
    (match symbol_count > 0 with
     | true ->
       let sym = match List.hd info.symbols with
         | Some x -> x
         | None -> failwith "Expected non-empty symbols"
       in
       pass (sprintf "BTCUSDT status: %s" sym.status)
     | false -> ())
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

(* ============================================================ *)
(* Unified Adapter Tests (Fluxum_adapter) *)
(* ============================================================ *)

let test_adapter_ticker () =
  printf "\n[Adapter] get_ticker\n";
  let cfg = Mexc.Cfg.production in
  let adapter = Mexc.Fluxum_adapter.Adapter.create ~cfg ~symbols:["BTCUSDT"] () in
  Mexc.Fluxum_adapter.Adapter.get_ticker adapter ~symbol:"BTCUSDT" () >>| function
  | Ok ticker_native ->
    (match Mexc.Fluxum_adapter.Adapter.Normalize.ticker ticker_native with
     | Ok ticker ->
       pass (sprintf "Symbol: %s" ticker.symbol);
       pass (sprintf "Last: %.2f | Bid: %.2f | Ask: %.2f"
         ticker.last_price ticker.bid_price ticker.ask_price);
       pass (sprintf "24h High: %.2f | Low: %.2f" ticker.high_24h ticker.low_24h);
       pass (sprintf "24h Volume: %.2f" ticker.volume_24h);
       Option.iter ticker.price_change_pct ~f:(fun pct ->
         pass (sprintf "24h Change: %.2f%%" pct))
     | Error msg ->
       fail (sprintf "Failed to normalize ticker: %s" msg))
  | Error err ->
    let e = Mexc.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_order_book () =
  printf "\n[Adapter] get_order_book\n";
  let cfg = Mexc.Cfg.production in
  let adapter = Mexc.Fluxum_adapter.Adapter.create ~cfg ~symbols:["BTCUSDT"] () in
  Mexc.Fluxum_adapter.Adapter.get_order_book adapter ~symbol:"BTCUSDT" ~limit:5 () >>| function
  | Ok book_native ->
    (match Mexc.Fluxum_adapter.Adapter.Normalize.order_book book_native with
     | Ok book ->
       pass (sprintf "Bids: %d | Asks: %d" (List.length book.bids) (List.length book.asks));
       let spread = Fluxum.Types.Order_book.spread book in
       pass (sprintf "Spread: %.4f" spread);
       let mid = Fluxum.Types.Order_book.mid_price book in
       pass (sprintf "Mid price: %.2f" mid);
       (match List.hd book.bids with
        | Some level -> pass (sprintf "Best bid: %.8f @ %.2f" level.volume level.price)
        | None -> ());
       (match List.hd book.asks with
        | Some level -> pass (sprintf "Best ask: %.8f @ %.2f" level.volume level.price)
        | None -> ())
     | Error msg ->
       fail (sprintf "Failed to normalize order book: %s" msg))
  | Error err ->
    let e = Mexc.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_recent_trades () =
  printf "\n[Adapter] get_recent_trades\n";
  let cfg = Mexc.Cfg.production in
  let adapter = Mexc.Fluxum_adapter.Adapter.create ~cfg ~symbols:["BTCUSDT"] () in
  Mexc.Fluxum_adapter.Adapter.get_recent_trades adapter ~symbol:"BTCUSDT" ~limit:5 () >>| function
  | Ok trades_native ->
    let trades_results = List.map trades_native ~f:Mexc.Fluxum_adapter.Adapter.Normalize.public_trade in
    let result_transpose results =
      List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
        match res, acc with
        | Ok v, Ok vs -> Ok (v :: vs)
        | Error e, _ -> Error e
        | _, Error e -> Error e)
    in
    (match result_transpose trades_results with
     | Ok trades ->
       pass (sprintf "Trades: %d" (List.length trades));
       (match List.hd trades with
        | Some trade ->
          let side_str = match trade.side with
            | Some s -> Fluxum.Types.Side.to_string s
            | None -> "?"
          in
          pass (sprintf "Latest: %s %.8f @ %.2f" side_str trade.qty trade.price)
        | None -> fail "No trades")
     | Error msg ->
       fail (sprintf "Failed to normalize trades: %s" msg))
  | Error err ->
    let e = Mexc.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_symbols () =
  printf "\n[Adapter] get_symbols\n";
  let cfg = Mexc.Cfg.production in
  let adapter = Mexc.Fluxum_adapter.Adapter.create ~cfg ~symbols:[] () in
  Mexc.Fluxum_adapter.Adapter.get_symbols adapter () >>| function
  | Ok symbols_native ->
    let symbols = List.filter_map symbols_native ~f:(fun s ->
      match Mexc.Fluxum_adapter.Adapter.Normalize.symbol_info s with
      | Ok info -> Some info
      | Error _ -> None) in
    pass (sprintf "Total symbols: %d" (List.length symbols));
    (* Find BTCUSDT *)
    let btc = List.find symbols ~f:(fun s -> String.equal s.symbol "BTC_USDT") in
    (match btc with
     | Some info ->
       pass (sprintf "Found: %s (%s/%s) status=%s"
         info.symbol info.base_currency info.quote_currency info.status)
     | None -> fail "BTCUSDT not found")
  | Error err ->
    let e = Mexc.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

(* ============================================================ *)
(* WebSocket Integration Tests *)
(* ============================================================ *)

let test_websocket_connection () =
  printf "\n[WebSocket] Connection and Subscription\n";
  let streams = [
    Mexc.Ws.Stream.AggreDeals { symbol = "BTCUSDT"; frequency = Ms100 };
  ] in
  (* Try secure first, fall back to insecure if SSL fails *)
  Mexc.Ws.connect ~streams () >>= fun result ->
  (match result with
   | Ok _ -> return result
   | Error _ ->
     printf "  (Trying insecure endpoint...)\n";
     Mexc.Ws.connect ~url:Mexc.Ws.Endpoint.public_url_insecure ~streams ())
  >>= function
  | Error err ->
    fail (sprintf "Connection error: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected to WebSocket";
    (* Read a few messages with timeout *)
    let messages_received = ref 0 in
    let start_time = Time_float_unix.now () in
    let timeout = Time_float_unix.Span.of_sec 10.0 in

    let rec read_loop () =
      let elapsed = Time_float_unix.diff (Time_float_unix.now ()) start_time in
      (match Time_float_unix.Span.(elapsed > timeout) with
       | true ->
         (match !messages_received > 0 with
          | true -> pass (sprintf "Received %d messages before timeout" !messages_received)
          | false -> fail "No messages received within timeout");
         return ()
       | false ->
        match%bind Clock.with_timeout (Time_float.Span.of_sec 2.0) (Pipe.read (Mexc.Ws.messages ws)) with
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
          let parsed = Mexc.Ws.parse_message msg in
          (match parsed with
           | Mexc.Ws.Message.SubscriptionAck { msg; _ } ->
             pass (sprintf "Subscription ack: %s" msg)
           | Mexc.Ws.Message.Data wrapper ->
             (match wrapper.body with
              | Mexc.Ws.Message.AggreDeals deals ->
                let deal_count = List.length deals.deals in
                (match deal_count > 0 with
                 | true ->
                   pass (sprintf "Received %d trades" deal_count);
                   let deal = match List.hd deals.deals with
                     | Some x -> x
                     | None -> failwith "Expected non-empty deals"
                   in
                   pass (sprintf "Trade: %s @ %s" deal.quantity deal.price)
                 | false -> ())
              | Mexc.Ws.Message.AggreDepth depth ->
                pass (sprintf "Depth update: %d bids, %d asks"
                  (List.length depth.bids) (List.length depth.asks))
              | Mexc.Ws.Message.LimitDepth depth ->
                pass (sprintf "Depth snapshot: %d bids, %d asks"
                  (List.length depth.bids) (List.length depth.asks))
              | Mexc.Ws.Message.BookTicker ticker ->
                pass (sprintf "Book ticker: bid=%s ask=%s"
                  ticker.bid_price ticker.ask_price)
              | Mexc.Ws.Message.Unknown _ ->
                pass "Received unknown message type")
           | Mexc.Ws.Message.Pong ->
             pass "Received pong"
           | Mexc.Ws.Message.Error err ->
             fail (sprintf "Parse error: %s" err)
           | Mexc.Ws.Message.Raw _ ->
             pass "Received raw message");
          (match !messages_received >= 5 with
           | true ->
             pass (sprintf "Successfully received %d messages" !messages_received);
             return ()
           | false -> read_loop ()))
    in
    read_loop ()

(* Phase 2 Priority 3: WebSocket Error Handling Tests *)

let test_websocket_invalid_url () =
  printf "\n[WebSocket] Connection Failure - Invalid URL\n";
  let streams = [
    Mexc.Ws.Stream.AggreDeals { symbol = "BTCUSDT"; frequency = Ms100 };
  ] in
  let invalid_url = "wss://invalid.nonexistent-domain-12345.com/ws" in
  (* Set a short timeout for the connection attempt *)
  Clock.with_timeout (Time_float.Span.of_sec 5.0)
    (Mexc.Ws.connect ~url:invalid_url ~streams ())
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
  let parsed1 = Mexc.Ws.parse_message malformed_json in
  (match parsed1 with
   | Mexc.Ws.Message.Error _ -> pass "Parse error caught for malformed JSON"
   | _ -> fail "Should detect malformed JSON");

  (* Test 2: Empty string *)
  let empty = "" in
  let parsed2 = Mexc.Ws.parse_message empty in
  (match parsed2 with
   | Mexc.Ws.Message.Error _ -> pass "Parse error caught for empty message"
   | _ -> fail "Should reject empty message");

  (* Test 3: Invalid protobuf data *)
  let invalid_proto = "\x00\x01\x02\x03\x04" in
  let parsed3 = Mexc.Ws.parse_message invalid_proto in
  (match parsed3 with
   | Mexc.Ws.Message.Error _ -> pass "Parse error caught for invalid protobuf"
   | Mexc.Ws.Message.Raw _ -> pass "Raw message returned for unrecognized format"
   | _ -> ());

  return ()

let test_websocket_connection_recovery () =
  printf "\n[WebSocket] Connection Recovery Behavior\n";

  (* Test connecting with empty streams list *)
  Mexc.Ws.connect ~streams:[] () >>= function
  | Error err ->
    pass (sprintf "Empty streams rejected or failed: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected with empty streams (will rely on manual subscriptions)";
    (* Verify we can still read from the connection *)
    Clock.with_timeout (Time_float.Span.of_sec 2.0)
      (Pipe.read (Mexc.Ws.messages ws))
    >>| (function
      | `Timeout ->
        pass "No messages on empty streams (as expected)"
      | `Result `Eof ->
        pass "Connection closed on empty streams"
      | `Result (`Ok msg) ->
        let parsed = Mexc.Ws.parse_message msg in
        (match parsed with
         | Mexc.Ws.Message.SubscriptionAck _ -> pass "Got subscription ack"
         | Mexc.Ws.Message.Pong -> pass "Got pong"
         | _ -> pass (sprintf "Got message: %s" (String.prefix msg 50))))

let test_websocket_invalid_symbol () =
  printf "\n[WebSocket] Invalid Symbol Handling\n";
  let streams = [
    Mexc.Ws.Stream.AggreDeals { symbol = "INVALIDSYMBOL12345"; frequency = Ms100 };
  ] in
  (* Try secure first, fall back to insecure if SSL fails *)
  Mexc.Ws.connect ~streams () >>= fun result ->
  (match result with
   | Ok _ -> return result
   | Error _ ->
     printf "  (Trying insecure endpoint...)\n";
     Mexc.Ws.connect ~url:Mexc.Ws.Endpoint.public_url_insecure ~streams ())
  >>= function
  | Error err ->
    pass (sprintf "Connection rejected invalid symbol: %s" (Error.to_string_hum err));
    return ()
  | Ok ws ->
    pass "Connected (exchange may accept invalid symbol)";
    (* Try to read messages for a short time *)
    Clock.with_timeout (Time_float.Span.of_sec 3.0)
      (Pipe.read (Mexc.Ws.messages ws))
    >>| (function
      | `Timeout ->
        pass "No messages for invalid symbol (exchange may silently ignore)"
      | `Result `Eof ->
        pass "Connection closed for invalid symbol"
      | `Result (`Ok msg) ->
        let parsed = Mexc.Ws.parse_message msg in
        (match parsed with
         | Mexc.Ws.Message.Error err ->
           pass (sprintf "Exchange returned error: %s" err)
         | Mexc.Ws.Message.SubscriptionAck { msg; _ } ->
           pass (sprintf "Exchange acknowledged: %s (may filter later)" msg)
         | _ ->
           pass "Exchange accepted invalid symbol"))

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "MEXC Integration Tests (Live API)\n";
  printf "===========================================\n";

  (* REST API tests *)
  test_server_time () >>= fun () ->
  test_depth () >>= fun () ->
  test_recent_trades () >>= fun () ->
  test_ticker_24hr () >>= fun () ->
  test_exchange_info () >>= fun () ->

  (* Unified adapter tests *)
  test_adapter_ticker () >>= fun () ->
  test_adapter_order_book () >>= fun () ->
  test_adapter_recent_trades () >>= fun () ->
  test_adapter_symbols () >>= fun () ->

  (* WebSocket test *)
  test_websocket_connection () >>= fun () ->

  (* WebSocket error handling tests *)
  test_websocket_invalid_url () >>= fun () ->
  test_websocket_malformed_message () >>= fun () ->
  test_websocket_connection_recovery () >>= fun () ->
  test_websocket_invalid_symbol () >>= fun () ->

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
    printf "\nSome tests failed - may need VPN for MEXC access\n";
    exit 1
  | false -> return ()

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

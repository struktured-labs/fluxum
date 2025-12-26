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
    if Int64.(resp.serverTime > 0L) then
      pass (sprintf "Server time: %Ld" resp.serverTime)
    else
      fail "Server time is 0"
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_depth () =
  printf "\n[REST] Order Book Depth\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Depth.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok resp ->
    let bid_count = List.length resp.bids in
    let ask_count = List.length resp.asks in
    if bid_count > 0 && ask_count > 0 then begin
      pass (sprintf "Got %d bids, %d asks" bid_count ask_count);
      let best_bid, bid_qty = List.hd_exn resp.bids in
      let best_ask, ask_qty = List.hd_exn resp.asks in
      pass (sprintf "Best bid: %s @ %s" bid_qty best_bid);
      pass (sprintf "Best ask: %s @ %s" ask_qty best_ask);
      pass (sprintf "Last update ID: %Ld" resp.lastUpdateId)
    end else
      fail "Empty order book"
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

let test_recent_trades () =
  printf "\n[REST] Recent Trades\n";
  let cfg = Mexc.Cfg.production in
  Mexc.V1.Recent_trades.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok trades ->
    let count = List.length trades in
    if count > 0 then begin
      pass (sprintf "Got %d trades" count);
      let trade = List.hd_exn trades in
      pass (sprintf "Latest: %s @ %s" trade.qty trade.price)
    end else
      fail "No trades returned"
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
    if symbol_count > 0 then begin
      let sym = List.hd_exn info.symbols in
      pass (sprintf "BTCUSDT status: %s" sym.status)
    end
  | #Mexc.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Mexc.Rest.Error.sexp_of_t err)))

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
      if Time_float_unix.Span.(elapsed > timeout) then begin
        if !messages_received > 0 then
          pass (sprintf "Received %d messages before timeout" !messages_received)
        else
          fail "No messages received within timeout";
        return ()
      end else begin
        match%bind Clock.with_timeout (Time_float.Span.of_sec 2.0) (Pipe.read (Mexc.Ws.messages ws)) with
        | `Timeout ->
          if !messages_received > 0 then begin
            pass (sprintf "Received %d messages total" !messages_received);
            return ()
          end else
            read_loop ()
        | `Result `Eof ->
          if !messages_received > 0 then
            pass (sprintf "Connection closed after %d messages" !messages_received)
          else
            fail "Connection closed without receiving messages";
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
                if deal_count > 0 then begin
                  pass (sprintf "Received %d trades" deal_count);
                  let deal = List.hd_exn deals.deals in
                  pass (sprintf "Trade: %s @ %s" deal.quantity deal.price)
                end
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
          if !messages_received >= 5 then begin
            pass (sprintf "Successfully received %d messages" !messages_received);
            return ()
          end else
            read_loop ()
      end
    in
    read_loop ()

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

  (* WebSocket test *)
  test_websocket_connection () >>= fun () ->

  (* Summary *)
  printf "\n===========================================\n";
  printf "Integration Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  if !tests_failed > 0 then begin
    printf "\nSome tests failed - may need VPN for MEXC access\n";
    exit 1
  end else
    return ()

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

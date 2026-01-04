(** Binance.US Integration Tests - Public REST APIs (no auth required)
    Uses Binance.US (api.binance.us) since main Binance is geo-blocked in the US *)

open Core
open Async

(* Use Binance.US configuration *)
let cfg = Binance.Cfg.production_us

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
  Binance.V3.Server_time.request cfg () >>| function
  | `Ok resp ->
    if Int64.(resp.serverTime > 0L) then
      pass (sprintf "Server time: %Ld" resp.serverTime)
    else
      fail "Server time is 0"
  | #Binance.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)))

let test_exchange_info () =
  printf "\n[REST] Exchange Info\n";
  Binance.V3.Exchange_info.request cfg { symbol = Some "BTCUSDT" } >>| function
  | `Ok resp ->
    pass (sprintf "Timezone: %s" resp.timezone);
    pass (sprintf "Server time: %Ld" resp.serverTime);
    let symbol_count = List.length resp.symbols in
    pass (sprintf "Symbols returned: %d" symbol_count);
    (match List.hd resp.symbols with
     | Some sym -> pass (sprintf "BTCUSDT: %s/%s (status: %s)" sym.baseAsset sym.quoteAsset sym.status)
     | None -> fail "No symbols returned")
  | #Binance.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)))

let test_depth () =
  printf "\n[REST] Order Book Depth\n";
  Binance.V3.Depth.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok resp ->
    let bid_count = List.length resp.bids in
    let ask_count = List.length resp.asks in
    if bid_count > 0 && ask_count > 0 then begin
      pass (sprintf "Got %d bids, %d asks" bid_count ask_count);
      (match List.hd resp.bids with
       | Some (price, qty) -> pass (sprintf "Best bid: %s @ %s" qty price)
       | None -> ());
      (match List.hd resp.asks with
       | Some (price, qty) -> pass (sprintf "Best ask: %s @ %s" qty price)
       | None -> ());
      pass (sprintf "Last update ID: %Ld" resp.lastUpdateId)
    end else
      fail "Empty order book"
  | #Binance.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)))

let test_ticker_24hr () =
  printf "\n[REST] 24hr Ticker\n";
  Binance.V3.Ticker_24hr.request cfg { symbol = "ETHUSDT" } >>| function
  | `Ok resp ->
    pass (sprintf "Symbol: %s" resp.symbol);
    pass (sprintf "Last price: %s" resp.lastPrice);
    pass (sprintf "24h change: %s%%" resp.priceChangePercent);
    pass (sprintf "24h high: %s | low: %s" resp.highPrice resp.lowPrice);
    pass (sprintf "24h volume: %s" resp.volume);
    pass (sprintf "Trade count: %Ld" resp.count)
  | #Binance.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)))

let test_recent_trades () =
  printf "\n[REST] Recent Trades\n";
  Binance.V3.Recent_trades.request cfg { symbol = "BTCUSDT"; limit = Some 5 } >>| function
  | `Ok trades ->
    let count = List.length trades in
    if count > 0 then begin
      pass (sprintf "Got %d trades" count);
      (match List.hd trades with
       | Some trade ->
         let side = match trade.isBuyerMaker with true -> "SELL" | false -> "BUY" in
         pass (sprintf "Latest: %s %s @ %s" side trade.qty trade.price);
         pass (sprintf "Trade ID: %Ld" trade.id)
       | None -> ())
    end else
      fail "No trades returned"
  | #Binance.Rest.Error.t as err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)))

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "Binance.US Integration Tests (Public APIs)\n";
  printf "Using: api.binance.us\n";
  printf "===========================================\n";

  (* REST API tests *)
  test_server_time () >>= fun () ->
  test_exchange_info () >>= fun () ->
  test_depth () >>= fun () ->
  test_ticker_24hr () >>= fun () ->
  test_recent_trades () >>= fun () ->

  (* Summary *)
  printf "\n===========================================\n";
  printf "Integration Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  match !tests_failed > 0 with
  | true -> exit 1
  | false -> return ()

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

(** Bitrue Integration Tests - Public REST APIs (no auth required) *)

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

let test_exchange_info () =
  printf "\n[REST] Exchange Info\n";
  let cfg = (module Bitrue.Cfg.Production : Bitrue.Cfg.S) in
  Bitrue.Rest.exchange_info cfg >>| function
  | Ok info ->
    pass (sprintf "Timezone: %s" info.timezone);
    pass (sprintf "Server time: %Ld" info.serverTime);
    let symbol_count = List.length info.symbols in
    pass (sprintf "Symbols: %d" symbol_count);
    (match List.find info.symbols ~f:(fun s -> String.equal s.symbol "BTCUSDT") with
     | Some sym ->
       pass (sprintf "BTCUSDT: %s/%s (status: %s)" sym.baseAsset sym.quoteAsset sym.status)
     | None -> pass "BTCUSDT not found (may use different format)")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Bitrue.Rest.Error.sexp_of_t err)))

let test_depth () =
  printf "\n[REST] Order Book Depth (BTCUSDT)\n";
  let cfg = (module Bitrue.Cfg.Production : Bitrue.Cfg.S) in
  Bitrue.Rest.depth cfg ~symbol:"BTCUSDT" ~limit:5 () >>| function
  | Ok book ->
    let bid_count = List.length book.bids in
    let ask_count = List.length book.asks in
    if bid_count > 0 && ask_count > 0 then begin
      pass (sprintf "Got %d bids, %d asks" bid_count ask_count);
      (match List.hd book.bids with
       | Some (price, qty) -> pass (sprintf "Best bid: %s @ $%s" qty price)
       | None -> ());
      (match List.hd book.asks with
       | Some (price, qty) -> pass (sprintf "Best ask: %s @ $%s" qty price)
       | None -> ());
      pass (sprintf "Last update ID: %Ld" book.lastUpdateId)
    end else
      fail "Empty order book"
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Bitrue.Rest.Error.sexp_of_t err)))

let test_trades () =
  printf "\n[REST] Recent Trades (BTCUSDT)\n";
  let cfg = (module Bitrue.Cfg.Production : Bitrue.Cfg.S) in
  Bitrue.Rest.trades cfg ~symbol:"BTCUSDT" ~limit:5 () >>| function
  | Ok trades ->
    let count = List.length trades in
    if count > 0 then begin
      pass (sprintf "Got %d trades" count);
      (match List.hd trades with
       | Some t ->
         let side = match t.isBuyerMaker with true -> "SELL" | false -> "BUY" in
         pass (sprintf "Latest: %s %s @ $%s" side t.qty t.price);
         pass (sprintf "Trade ID: %Ld" t.id)
       | None -> ())
    end else
      fail "No trades returned"
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Bitrue.Rest.Error.sexp_of_t err)))

let test_ticker_24hr () =
  printf "\n[REST] 24hr Ticker (ETHUSDT)\n";
  let cfg = (module Bitrue.Cfg.Production : Bitrue.Cfg.S) in
  Bitrue.Rest.ticker_24hr cfg ~symbol:"ETHUSDT" >>| function
  | Ok ticker ->
    pass (sprintf "Symbol: %s" ticker.symbol);
    pass (sprintf "Last price: $%s" ticker.lastPrice);
    pass (sprintf "24h change: %s%%" ticker.priceChangePercent);
    pass (sprintf "24h high: $%s | low: $%s" ticker.highPrice ticker.lowPrice);
    pass (sprintf "24h volume: %s" ticker.volume);
    pass (sprintf "Trade count: %d" ticker.count)
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Bitrue.Rest.Error.sexp_of_t err)))

let test_klines () =
  printf "\n[REST] Klines/Candlesticks (BTCUSDT, 1h)\n";
  let cfg = (module Bitrue.Cfg.Production : Bitrue.Cfg.S) in
  Bitrue.Rest.klines cfg ~symbol:"BTCUSDT" ~interval:"1h" ~limit:5 () >>| function
  | Ok klines ->
    let count = List.length klines in
    if count > 0 then begin
      pass (sprintf "Got %d klines" count);
      (match List.hd klines with
       | Some k ->
         pass (sprintf "Latest: O=%s H=%s L=%s C=%s" k.open_ k.high k.low k.close);
         pass (sprintf "Volume: %s, Trades: %d" k.volume k.trades)
       | None -> ())
    end else
      fail "No klines returned"
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Bitrue.Rest.Error.sexp_of_t err)))

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "Bitrue Integration Tests (Public APIs)\n";
  printf "===========================================\n";

  (* REST API tests *)
  test_exchange_info () >>= fun () ->
  test_depth () >>= fun () ->
  test_trades () >>= fun () ->
  test_ticker_24hr () >>= fun () ->
  test_klines () >>= fun () ->

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

(** dYdX Integration Tests - Public REST APIs (no auth required) *)

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
(* Raw REST API Tests *)
(* ============================================================ *)

let test_markets () =
  printf "\n[REST] Perpetual Markets\n";
  Dydx.Rest.markets (module Dydx.Cfg.Production) () >>| function
  | Ok resp ->
    let count = List.length resp.markets in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d markets" count);
       (match List.hd resp.markets with
        | Some (ticker, market) ->
          let base = Dydx.Rest.Types.base_asset_of_ticker ticker in
          let quote = Dydx.Rest.Types.quote_asset_of_ticker ticker in
          pass (sprintf "%s: %s/%s (status: %s)" ticker base quote market.status);
          Option.iter market.oraclePrice ~f:(fun p ->
            pass (sprintf "Oracle price: %s" p));
          Option.iter market.volume24H ~f:(fun v ->
            pass (sprintf "24h volume: %s" v))
        | None -> ())
     | false -> fail "No markets returned")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_orderbook () =
  printf "\n[REST] Order Book (BTC-USD)\n";
  Dydx.Rest.orderbook (module Dydx.Cfg.Production) ~market:"BTC-USD" >>| function
  | Ok book ->
    let bid_count = List.length book.bids in
    let ask_count = List.length book.asks in
    pass (sprintf "Bids: %d levels | Asks: %d levels" bid_count ask_count);
    (match List.hd book.bids with
     | Some level -> pass (sprintf "Best bid: %s @ %s" level.size level.price)
     | None -> ());
    (match List.hd book.asks with
     | Some level -> pass (sprintf "Best ask: %s @ %s" level.size level.price)
     | None -> ())
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_trades () =
  printf "\n[REST] Recent Trades (BTC-USD)\n";
  Dydx.Rest.trades (module Dydx.Cfg.Production) ~market:"BTC-USD" ~limit:5 () >>| function
  | Ok trades ->
    let count = List.length trades in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d trades" count);
       (match List.hd trades with
        | Some trade ->
          pass (sprintf "Latest: %s %s @ %s" trade.side trade.size trade.price);
          pass (sprintf "Created: %s" trade.createdAt)
        | None -> ())
     | false -> fail "No trades returned")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_candles () =
  printf "\n[REST] Candles (ETH-USD, 1 hour)\n";
  Dydx.Rest.candles (module Dydx.Cfg.Production) ~market:"ETH-USD"
    ~resolution:Dydx.Common.ONE_HOUR ~limit:5 () >>| function
  | Ok candles ->
    let count = List.length candles in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d candles" count);
       (match List.hd candles with
        | Some candle ->
          pass (sprintf "O: %s H: %s L: %s C: %s" candle.open_ candle.high candle.low candle.close);
          pass (sprintf "Volume: %s USD" candle.usdVolume)
        | None -> ())
     | false -> fail "No candles returned")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_ticker () =
  printf "\n[REST] Ticker (SOL-USD)\n";
  Dydx.Rest.ticker (module Dydx.Cfg.Production) ~market:"SOL-USD" >>| function
  | Ok (Some (ticker, market)) ->
    pass (sprintf "Ticker: %s" ticker);
    pass (sprintf "Status: %s" market.status);
    Option.iter market.oraclePrice ~f:(fun p ->
      pass (sprintf "Oracle price: %s" p));
    Option.iter market.priceChange24H ~f:(fun c ->
      pass (sprintf "24h change: %s" c));
    Option.iter market.volume24H ~f:(fun v ->
      pass (sprintf "24h volume: %s" v))
  | Ok None ->
    fail "Ticker not found"
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

(* ============================================================ *)
(* Unified Adapter Tests (Fluxum_adapter) *)
(* ============================================================ *)

let test_adapter_symbols () =
  printf "\n[Adapter] get_symbols\n";
  let adapter = Dydx.Fluxum_adapter.Adapter.create
    ~cfg:(module Dydx.Cfg.Production) ~symbols:[] () in
  Dydx.Fluxum_adapter.Adapter.get_symbols adapter () >>| function
  | Ok symbols ->
    pass (sprintf "Total symbols: %d" (List.length symbols));
    (* Find BTC-USD *)
    let btc = List.find symbols ~f:(fun (ticker, _) ->
      String.equal ticker "BTC-USD") in
    (match btc with
     | Some (ticker, market) ->
       (match Dydx.Fluxum_adapter.Adapter.Normalize.symbol_info (ticker, market) with
        | Ok info ->
          pass (sprintf "Found: %s (%s/%s) status=%s"
            info.symbol info.base_currency info.quote_currency info.status)
        | Error msg ->
          fail (sprintf "Normalize error: %s" msg))
     | None -> fail "BTC-USD not found")
  | Error err ->
    let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_ticker () =
  printf "\n[Adapter] get_ticker\n";
  let adapter = Dydx.Fluxum_adapter.Adapter.create
    ~cfg:(module Dydx.Cfg.Production) ~symbols:[] () in
  Dydx.Fluxum_adapter.Adapter.get_ticker adapter ~symbol:"ETH-USD" () >>| function
  | Ok ticker_native ->
    (match Dydx.Fluxum_adapter.Adapter.Normalize.ticker ticker_native with
     | Ok ticker ->
       pass (sprintf "Symbol: %s" ticker.symbol);
       pass (sprintf "Last (oracle): %.2f" ticker.last_price);
       pass (sprintf "Volume 24h: %.2f" ticker.volume_24h);
       Option.iter ticker.price_change ~f:(fun c ->
         pass (sprintf "Price change: %.4f" c))
     | Error msg ->
       fail (sprintf "Normalize error: %s" msg))
  | Error err ->
    let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_order_book () =
  printf "\n[Adapter] get_order_book\n";
  let adapter = Dydx.Fluxum_adapter.Adapter.create
    ~cfg:(module Dydx.Cfg.Production) ~symbols:[] () in
  Dydx.Fluxum_adapter.Adapter.get_order_book adapter ~symbol:"BTC-USD" () >>| function
  | Ok book_native ->
    (match Dydx.Fluxum_adapter.Adapter.Normalize.order_book book_native with
     | Ok book ->
       pass (sprintf "Bids: %d | Asks: %d" (List.length book.bids) (List.length book.asks));
       let spread = Fluxum.Types.Order_book.spread book in
       pass (sprintf "Spread: %.4f" spread);
       let mid = Fluxum.Types.Order_book.mid_price book in
       pass (sprintf "Mid price: %.2f" mid)
     | Error msg ->
       fail (sprintf "Normalize error: %s" msg))
  | Error err ->
    let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

let test_adapter_recent_trades () =
  printf "\n[Adapter] get_recent_trades\n";
  let adapter = Dydx.Fluxum_adapter.Adapter.create
    ~cfg:(module Dydx.Cfg.Production) ~symbols:[] () in
  Dydx.Fluxum_adapter.Adapter.get_recent_trades adapter ~symbol:"ETH-USD" ~limit:5 () >>| function
  | Ok trades_native ->
    let trades = List.filter_map trades_native ~f:(fun t ->
      match Dydx.Fluxum_adapter.Adapter.Normalize.public_trade t with
      | Ok trade -> Some trade
      | Error _ -> None) in
    pass (sprintf "Trades: %d" (List.length trades));
    (match List.hd trades with
     | Some trade ->
       let side_str = match trade.side with
         | Some s -> Fluxum.Types.Side.to_string s
         | None -> "?"
       in
       pass (sprintf "Latest: %s %.6f @ %.2f" side_str trade.qty trade.price)
     | None -> fail "No trades")
  | Error err ->
    let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e)))

(* ============================================================ *)
(* Account Operations Tests (require valid dYdX address) *)
(* ============================================================ *)

(* Use a known active dYdX address for testing - this is a public indexer query *)
let test_address = "dydx1v88c3xv9xyv3eetdx0z5gnzcpu86j36npce9wf"

let test_subaccount () =
  printf "\n[Account] Subaccount Info\n";
  Dydx.Rest.subaccount (module Dydx.Cfg.Production)
    ~address:test_address ~subaccount_number:0 >>| function
  | Ok sub ->
    pass (sprintf "Address: %s" sub.address);
    pass (sprintf "Subaccount: %d" sub.subaccountNumber);
    pass (sprintf "Equity: %s" sub.equity);
    pass (sprintf "Free collateral: %s" sub.freeCollateral);
    pass (sprintf "Asset positions: %d" (List.length sub.assetPositions));
    pass (sprintf "Perpetual positions: %d" (List.length sub.perpetualPositions))
  | Error `Not_found ->
    pass "Subaccount not found (may not exist)"
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_transfers () =
  printf "\n[Account] Transfer History\n";
  Dydx.Rest.transfers (module Dydx.Cfg.Production)
    ~address:test_address ~subaccount_number:0 ~limit:5 () >>| function
  | Ok transfers ->
    pass (sprintf "Got %d transfers" (List.length transfers));
    (match List.hd transfers with
     | Some tr ->
       let type_str = match tr.type_ with
         | Dydx.Rest.Account_types.DEPOSIT -> "DEPOSIT"
         | Dydx.Rest.Account_types.WITHDRAWAL -> "WITHDRAWAL"
         | Dydx.Rest.Account_types.TRANSFER_IN -> "TRANSFER_IN"
         | Dydx.Rest.Account_types.TRANSFER_OUT -> "TRANSFER_OUT"
       in
       pass (sprintf "Latest: %s %s" type_str tr.size);
       pass (sprintf "Created: %s" tr.createdAt)
     | None -> pass "No transfers found")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_deposits () =
  printf "\n[Account] Deposit History\n";
  Dydx.Rest.deposits (module Dydx.Cfg.Production)
    ~address:test_address ~subaccount_number:0 ~limit:5 () >>| function
  | Ok deposits ->
    pass (sprintf "Got %d deposits" (List.length deposits));
    (match List.hd deposits with
     | Some tr ->
       pass (sprintf "Latest deposit: %s" tr.size);
       Option.iter tr.transactionHash ~f:(fun h ->
         pass (sprintf "Tx hash: %s" h))
     | None -> pass "No deposits found")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_withdrawals () =
  printf "\n[Account] Withdrawal History\n";
  Dydx.Rest.withdrawals (module Dydx.Cfg.Production)
    ~address:test_address ~subaccount_number:0 ~limit:5 () >>| function
  | Ok withdrawals ->
    pass (sprintf "Got %d withdrawals" (List.length withdrawals));
    (match List.hd withdrawals with
     | Some tr ->
       pass (sprintf "Latest withdrawal: %s" tr.size);
       (match tr.recipient with
        | Some r -> pass (sprintf "To: %s" r.recipient_address)
        | None -> ())
     | None -> pass "No withdrawals found")
  | Error err ->
    fail (sprintf "Error: %s" (Sexp.to_string_hum (Dydx.Rest.Error.sexp_of_t err)))

let test_adapter_account_ops () =
  printf "\n[Adapter] Account Operations\n";
  let adapter = Dydx.Fluxum_adapter.Adapter.create
    ~cfg:(module Dydx.Cfg.Production)
    ~address:test_address
    ~subaccount_number:0
    ~symbols:[]
    () in

  (* Test deposit address *)
  Dydx.Fluxum_adapter.Adapter.get_deposit_address adapter ~currency:"USDC" () >>= fun result ->
  (match result with
   | Ok addr_native ->
     (match Dydx.Fluxum_adapter.Adapter.Normalize.deposit_address addr_native with
      | Ok addr ->
        pass (sprintf "Deposit address: %s" addr.address);
        pass (sprintf "Currency: %s" addr.currency);
        Option.iter addr.network ~f:(fun n -> pass (sprintf "Network: %s" n))
      | Error msg -> fail (sprintf "Normalize error: %s" msg))
   | Error err ->
     let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
     fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e))));

  (* Test get deposits *)
  Dydx.Fluxum_adapter.Adapter.get_deposits adapter ~limit:3 () >>= fun result ->
  (match result with
   | Ok deposits_native ->
     pass (sprintf "Deposits from adapter: %d" (List.length deposits_native));
     (match List.hd deposits_native with
      | Some d ->
        (match Dydx.Fluxum_adapter.Adapter.Normalize.deposit d with
         | Ok deposit ->
           pass (sprintf "Normalized deposit: %.4f %s (status: %s)"
             deposit.amount deposit.currency
             (Fluxum.Types.Transfer_status.to_string deposit.status))
         | Error msg -> fail (sprintf "Normalize error: %s" msg))
      | None -> pass "No deposits to normalize")
   | Error err ->
     let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
     fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e))));

  (* Test get withdrawals *)
  Dydx.Fluxum_adapter.Adapter.get_withdrawals adapter ~limit:3 () >>| fun result ->
  (match result with
   | Ok withdrawals_native ->
     pass (sprintf "Withdrawals from adapter: %d" (List.length withdrawals_native));
     (match List.hd withdrawals_native with
      | Some w ->
        (match Dydx.Fluxum_adapter.Adapter.Normalize.withdrawal w with
         | Ok withdrawal ->
           pass (sprintf "Normalized withdrawal: %.4f %s (status: %s)"
             withdrawal.amount withdrawal.currency
             (Fluxum.Types.Transfer_status.to_string withdrawal.status))
         | Error msg -> fail (sprintf "Normalize error: %s" msg))
      | None -> pass "No withdrawals to normalize")
   | Error err ->
     let e = Dydx.Fluxum_adapter.Adapter.Normalize.error err in
     fail (sprintf "Error: %s" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t e))))

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "dYdX v4 Integration Tests (Public APIs)\n";
  printf "===========================================\n";

  (* Raw REST API tests *)
  test_markets () >>= fun () ->
  test_orderbook () >>= fun () ->
  test_trades () >>= fun () ->
  test_candles () >>= fun () ->
  test_ticker () >>= fun () ->

  (* Unified adapter tests *)
  test_adapter_symbols () >>= fun () ->
  test_adapter_ticker () >>= fun () ->
  test_adapter_order_book () >>= fun () ->
  test_adapter_recent_trades () >>= fun () ->

  (* Account operations tests *)
  test_subaccount () >>= fun () ->
  test_transfers () >>= fun () ->
  test_deposits () >>= fun () ->
  test_withdrawals () >>= fun () ->
  test_adapter_account_ops () >>= fun () ->

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

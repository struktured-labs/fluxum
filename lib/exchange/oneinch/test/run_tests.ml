open Core

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  ✓ %s\n" msg

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  ✗ FAIL: %s\n" msg

let assert_error result msg =
  incr tests_run;
  match result with
  | Error err ->
    incr tests_passed;
    printf "  ✓ %s: %s\n" msg err;
    true
  | Ok _ ->
    incr tests_failed;
    printf "  ✗ FAIL: %s (expected error, got Ok)\n" msg;
    false

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) <= tolerance) with
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false

(* Helper to create token_info *)
let make_token ~address ~symbol ~decimals =
  Oneinch.Rest.Types.{
    address;
    symbol;
    name = symbol;
    decimals;
    logoURI = None;
  }

(* ============================================================ *)
(* Normalize Function Tests *)
(* ============================================================ *)

let test_normalize_ticker_valid () =
  printf "\n[Normalize] Ticker with valid data\n";
  let quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "3000000000";  (* 3000 USDC *)
    gas = Some 21000;
  } in
  match Oneinch.Fluxum_adapter.Adapter.Normalize.ticker quote with
  | Ok t ->
    (* Price calculation: to_amount / 10^to_dec / (from_amount / 10^from_dec)
       = 3000000000 / 10^6 / (10^18 / 10^18)
       = 3000 / 1 = 3000 *)
    ignore (assert_float_equal 3000.0 t.last_price "ETH priced at 3000 USDC");
    ignore (assert_float_equal 3000.0 t.bid_price "Bid price equals last");
    pass "Valid ticker normalized"
  | Error msg ->
    fail (sprintf "Ticker normalization failed: %s" msg)

let test_normalize_ticker_invalid_to_amount () =
  printf "\n[Normalize] Ticker with invalid toAmount\n";
  let quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "not_a_number";
    gas = None;
  } in
  ignore (assert_error
    (Oneinch.Fluxum_adapter.Adapter.Normalize.ticker quote)
    "Rejected invalid toAmount")

let test_normalize_ticker_nan_to_amount () =
  printf "\n[Normalize] Ticker with NaN toAmount\n";
  let quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "NaN";
    gas = None;
  } in
  ignore (assert_error
    (Oneinch.Fluxum_adapter.Adapter.Normalize.ticker quote)
    "Rejected NaN toAmount")

let test_normalize_order_book_valid () =
  printf "\n[Normalize] Order book with valid quotes\n";
  let sell_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "3010000000";  (* 3010 USDC - selling ETH *)
    gas = None;
  } in
  let buy_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toAmount = "1000000000000000000";  (* 1 ETH - buying ETH *)
    gas = None;
  } in
  match Oneinch.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote) with
  | Ok b ->
    (* sell_price = sell_to_amt / 10^to_dec = 3010000000 / 10^6 = 3010 *)
    ignore (assert_float_equal 3010.0 (List.hd_exn b.asks).price "Ask price");
    (* buy_price = 10^buy_to_dec / buy_to_amt = 10^18 / 10^18 = 1.0 *)
    ignore (assert_float_equal 1.0 (List.hd_exn b.bids).price "Bid price");
    pass "Synthetic order book created"
  | Error msg ->
    fail (sprintf "Order book normalization failed: %s" msg)

let test_normalize_order_book_invalid_sell_to_amount () =
  printf "\n[Normalize] Order book with invalid sell toAmount\n";
  let sell_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "invalid";
    gas = None;
  } in
  let buy_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toAmount = "1000000000000000000";
    gas = None;
  } in
  ignore (assert_error
    (Oneinch.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected invalid sell toAmount")

let test_normalize_order_book_invalid_buy_to_amount () =
  printf "\n[Normalize] Order book with invalid buy toAmount\n";
  let sell_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "3010000000";
    gas = None;
  } in
  let buy_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toAmount = "NaN";
    gas = None;
  } in
  ignore (assert_error
    (Oneinch.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected NaN buy toAmount")

let test_normalize_order_book_negative_amount () =
  printf "\n[Normalize] Order book with negative toAmount\n";
  let sell_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toAmount = "-3010000000";
    gas = None;
  } in
  let buy_quote : Oneinch.Rest.Types.quote = {
    fromToken = make_token ~address:"0xUsdc" ~symbol:"USDC" ~decimals:6;
    toToken = make_token ~address:"0xEth" ~symbol:"ETH" ~decimals:18;
    toAmount = "1000000000000000000";
    gas = None;
  } in
  ignore (assert_error
    (Oneinch.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected negative toAmount")

(* ============================================================ *)
(* Main Test Runner *)
(* ============================================================ *)

let () =
  printf "\n";
  printf "===========================================\n";
  printf "1inch DEX Aggregator Unit Tests\n";
  printf "===========================================\n";

  (* Ticker tests *)
  test_normalize_ticker_valid ();
  test_normalize_ticker_invalid_to_amount ();
  test_normalize_ticker_nan_to_amount ();

  (* Order book tests *)
  test_normalize_order_book_valid ();
  test_normalize_order_book_invalid_sell_to_amount ();
  test_normalize_order_book_invalid_buy_to_amount ();
  test_normalize_order_book_negative_amount ();

  (* Summary *)
  printf "\n";
  printf "===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d ✓\n" !tests_passed;
  printf "Failed:       %d ✗\n" !tests_failed;
  printf "Success rate: %.1f%%\n" (Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0);
  printf "===========================================\n";
  printf "\n";

  match !tests_failed with
  | 0 -> exit 0
  | _ -> exit 1

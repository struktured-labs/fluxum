(** Comprehensive unit test suite for Gemini exchange adapter

    Focus: Testing normalize functions with error paths (Phase 1)
    These tests verify that Result.t returns work correctly for malformed data.
*)

open Core

(* Test infrastructure *)
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

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

(* ============================================================ *)
(* Ticker Normalization Error Path Tests *)
(* ============================================================ *)

let test_ticker_null_bid () =
  printf "\n[Normalize.ticker] Null bid field\n";
  let json = `Assoc [
    ("bid", `Null);
    ("ask", `String "50000");
    ("last", `String "49999");
    ("volume", `Assoc [("BTCUSD", `String "100.5")])
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.ticker json with
  | Error msg ->
    pass (sprintf "Correctly rejected null bid: %s" msg);
    ()
  | Ok _ ->
    fail "Should reject null bid";
    ()

let test_ticker_malformed_float () =
  printf "\n[Normalize.ticker] Malformed float in bid\n";
  let json = `Assoc [
    ("bid", `String "not_a_number");
    ("ask", `String "50000");
    ("last", `String "49999");
    ("volume", `Assoc [("BTCUSD", `String "100.5")])
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.ticker json with
  | Error msg ->
    pass (sprintf "Correctly rejected malformed float: %s" msg);
    ()
  | Ok _ ->
    fail "Should reject non-numeric bid";
    ()

let test_ticker_valid_happy_path () =
  printf "\n[Normalize.ticker] Valid ticker succeeds\n";
  let json = `Assoc [
    ("bid", `String "49950.50");
    ("ask", `String "50050.25");
    ("last", `String "50000.00");
    ("volume", `Assoc [("BTCUSD", `String "123.456")])
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.ticker json with
  | Ok ticker ->
    ignore (assert_float_equal 49950.50 ticker.bid_price "Bid price parsed");
    ignore (assert_float_equal 50050.25 ticker.ask_price "Ask price parsed");
    ignore (assert_float_equal 50000.00 ticker.last_price "Last price parsed");
    ignore (assert_float_equal 123.456 ticker.volume_24h "Volume parsed");
    ()
  | Error msg ->
    fail (sprintf "Valid ticker should succeed: %s" msg);
    ()

(* ============================================================ *)
(* Order Book Normalization Tests *)
(* ============================================================ *)

let test_order_book_missing_bids () =
  printf "\n[Normalize.order_book] Missing bids field\n";
  let json = `Assoc [
    ("asks", `List [
      `Assoc [("price", `String "50100"); ("amount", `String "1.0"); ("timestamp", `String "0")]
    ])
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.order_book json with
  | Error msg ->
    pass (sprintf "Correctly rejected missing bids: %s" msg);
    ()
  | Ok _ ->
    fail "Should require bids field";
    ()

let test_order_book_valid () =
  printf "\n[Normalize.order_book] Valid order book succeeds\n";
  let json = `Assoc [
    ("bids", `List [
      `Assoc [("price", `String "49900"); ("amount", `String "2.5"); ("timestamp", `String "0")]
    ]);
    ("asks", `List [
      `Assoc [("price", `String "50100"); ("amount", `String "1.5"); ("timestamp", `String "0")]
    ])
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.order_book json with
  | Ok book ->
    ignore (assert_float_equal 49900.0 (List.hd_exn book.bids).price "Bid price");
    ignore (assert_float_equal 2.5 (List.hd_exn book.bids).volume "Bid volume");
    ignore (assert_float_equal 50100.0 (List.hd_exn book.asks).price "Ask price");
    ignore (assert_float_equal 1.5 (List.hd_exn book.asks).volume "Ask volume");
    ()
  | Error msg ->
    fail (sprintf "Valid order book should succeed: %s" msg);
    ()

(* ============================================================ *)
(* Public Trade Normalization Tests *)
(* ============================================================ *)

let test_public_trade_valid_buy () =
  printf "\n[Normalize.public_trade] Valid buy trade\n";
  let json = `Assoc [
    ("price", `String "50000");
    ("amount", `String "1.5");
    ("type", `String "buy");
    ("timestampms", `Int 1234567890000);
    ("tid", `Int 999888777)
  ] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.public_trade json with
  | Ok trade ->
    ignore (assert_float_equal 50000.0 trade.price "Trade price");
    ignore (assert_float_equal 1.5 trade.qty "Trade quantity");
    ()
  | Error msg ->
    fail (sprintf "Valid buy trade should succeed: %s" msg);
    ()

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  Gemini Exchange Adapter - Unit Test Suite\n";
  printf "====================================================\n";

  (* Ticker normalize tests *)
  printf "\n=== Ticker Normalization Tests ===\n";
  test_ticker_null_bid ();
  test_ticker_malformed_float ();
  test_ticker_valid_happy_path ();

  (* Order book tests *)
  printf "\n=== Order Book Normalization Tests ===\n";
  test_order_book_missing_bids ();
  test_order_book_valid ();

  (* Public trade tests *)
  printf "\n=== Public Trade Normalization Tests ===\n";
  test_public_trade_valid_buy ();

  (* Balance tests *)
  (* printf "\n=== Balance Normalization Tests ===\n"; *)
  (* test_balance_valid (); *)

  printf "\n";
  printf "====================================================\n";
  printf "  Test Results: %d run, %d passed, %d failed\n"
    !tests_run !tests_passed !tests_failed;
  printf "====================================================\n";

  match !tests_failed with
  | 0 ->
    printf "\n✅ All tests passed!\n\n";
    exit 0
  | n ->
    printf "\n❌ %d test(s) failed\n\n" n;
    exit 1

let () = run_all_tests ()

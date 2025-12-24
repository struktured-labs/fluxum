(** Simple test runner for Kraken modules *)

open Core

module Ledger = Kraken.Ledger
module Order_book = Kraken.Order_book

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  if Float.(abs (expected - actual) > tolerance) then begin
    printf "FAIL: %s: expected %.8f, got %.8f\n" msg expected actual;
    failwith msg
  end else
    printf "PASS: %s\n" msg

let test_ledger_basic () =
  printf "\n=== Ledger Basic Tests ===\n";

  (* Test 1: Create empty entry *)
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  assert_float_equal 0.0 entry.pnl "Empty entry should have 0 pnl";
  assert_float_equal 0.0 entry.position "Empty entry should have 0 position";

  (* Test 2: Simple buy trade *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  assert_float_equal 1.0 entry.position "Position should be 1.0 after buy";
  assert_float_equal (-50000.0) entry.notional "Notional should be -$50,000";
  assert_float_equal 0.0 entry.pnl "PnL should be 0 at entry price";

  (* Test 3: Spot price update *)
  let entry = Ledger.Entry.update_spot entry 55000.0 in
  assert_float_equal 55000.0 entry.spot "Spot should be $55,000";
  assert_float_equal 5000.0 entry.pnl "PnL should be $5,000";

  printf "Ledger basic tests passed!\n"

let test_order_book_basic () =
  printf "\n=== Order Book Basic Tests ===\n";

  (* Test 1: Create empty book *)
  let book = Order_book.Book.empty "BTC/USD" in
  if not (String.equal "BTC/USD" (Order_book.Book.symbol book)) then
    failwith "Symbol mismatch";

  (* Test 2: Add bid *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 50000.0 best_bid.price "Best bid should be $50,000";
  assert_float_equal 1.5 best_bid.volume "Best bid volume should be 1.5";

  (* Test 3: Add ask *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_ask = Order_book.Book.best_ask book in
  assert_float_equal 51000.0 best_ask.price "Best ask should be $51,000";
  assert_float_equal 2.0 best_ask.volume "Best ask volume should be 2.0";

  (* Test 4: Bid sorting (descending) *)
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:51000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 51000.0 best_bid.price "Best bid should be highest ($51,000)";

  printf "Order book basic tests passed!\n"

let () =
  printf "Running Kraken unified adapter tests...\n";

  try
    test_ledger_basic ();
    test_order_book_basic ();
    printf "\n✓ All tests passed!\n"
  with e ->
    printf "\n✗ Tests failed: %s\n" (Exn.to_string e);
    exit 1

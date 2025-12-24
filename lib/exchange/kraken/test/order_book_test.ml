(** Unit tests for Kraken Order_book module *)

open Core
open Kraken

(** Test helper to compare floats with tolerance *)
let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  if Float.abs (expected -. actual) > tolerance then
    failwith (Printf.sprintf "%s: expected %.8f, got %.8f" msg expected actual)

(** Test 1: Create empty book *)
let%test_unit "create_empty_book" =
  let book = Order_book.Book.empty "BTC/USD" in
  [%test_result: string] ~expect:"BTC/USD" (Order_book.Book.symbol book);
  [%test_result: int] ~expect:0 (Order_book.Book.epoch book);
  let best_bid = Order_book.Book.best_bid book in
  let best_ask = Order_book.Book.best_ask book in
  assert_float_equal 0.0 best_bid.price "best_bid should be 0 for empty book";
  assert_float_equal 0.0 best_ask.price "best_ask should be 0 for empty book"

(** Test 2: Add single bid *)
let%test_unit "add_single_bid" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 50000.0 best_bid.price "best_bid price should be $50,000";
  assert_float_equal 1.5 best_bid.volume "best_bid volume should be 1.5 BTC"

(** Test 3: Add single ask *)
let%test_unit "add_single_ask" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_ask = Order_book.Book.best_ask book in
  assert_float_equal 51000.0 best_ask.price "best_ask price should be $51,000";
  assert_float_equal 2.0 best_ask.volume "best_ask volume should be 2.0 BTC"

(** Test 4: Bid sorting (descending - highest first) *)
let%test_unit "bid_sorting_descending" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in

  (* Best bid should be highest price: $51,000 *)
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 51000.0 best_bid.price "best_bid should be highest price $51,000";

  (* Get top 3 bids - should be in descending order *)
  let top_bids = Order_book.Book.best_n_bids book ~n:3 () in
  [%test_result: int] ~expect:3 (List.length top_bids);
  assert_float_equal 51000.0 (List.nth_exn top_bids 0).price "1st bid should be $51,000";
  assert_float_equal 50000.0 (List.nth_exn top_bids 1).price "2nd bid should be $50,000";
  assert_float_equal 49000.0 (List.nth_exn top_bids 2).price "3rd bid should be $49,000"

(** Test 5: Ask sorting (ascending - lowest first) *)
let%test_unit "ask_sorting_ascending" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:2.0 in

  (* Best ask should be lowest price: $51,000 *)
  let best_ask = Order_book.Book.best_ask book in
  assert_float_equal 51000.0 best_ask.price "best_ask should be lowest price $51,000";

  (* Get top 3 asks - should be in ascending order *)
  let top_asks = Order_book.Book.best_n_asks book ~n:3 () in
  [%test_result: int] ~expect:3 (List.length top_asks);
  assert_float_equal 51000.0 (List.nth_exn top_asks 0).price "1st ask should be $51,000";
  assert_float_equal 52000.0 (List.nth_exn top_asks 1).price "2nd ask should be $52,000";
  assert_float_equal 53000.0 (List.nth_exn top_asks 2).price "3rd ask should be $53,000"

(** Test 6: Remove price level with zero size *)
let%test_unit "remove_with_zero_size" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 50000.0 best_bid.price "bid should exist at $50,000";

  (* Remove by setting size to zero *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 0.0 best_bid.price "bid should be removed (price=0)"

(** Test 7: Update operation - add to existing level *)
let%test_unit "update_add_to_level" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  (* Update adds to existing size *)
  let book = Order_book.Book.update book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 50000.0 best_bid.price "price should still be $50,000";
  assert_float_equal 1.5 best_bid.volume "volume should be 1.0 + 0.5 = 1.5"

(** Test 8: Update operation - reduce from existing level *)
let%test_unit "update_reduce_from_level" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:2.0 in
  (* Update subtracts from existing size *)
  let book = Order_book.Book.update book ~side:`Bid ~price:50000.0 ~size:(-0.5) in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 50000.0 best_bid.price "price should still be $50,000";
  assert_float_equal 1.5 best_bid.volume "volume should be 2.0 - 0.5 = 1.5"

(** Test 9: Update operation - reduce to zero removes level *)
let%test_unit "update_reduce_to_zero" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  (* Update reduces to zero *)
  let book = Order_book.Book.update book ~side:`Bid ~price:50000.0 ~size:(-1.0) in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 0.0 best_bid.price "level should be removed when size reaches 0"

(** Test 10: Add operation (alias for update) *)
let%test_unit "add_operation" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.add book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 1.5 best_bid.volume "add should increase volume to 1.5"

(** Test 11: Remove operation (update with negative) *)
let%test_unit "remove_operation" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:2.0 in
  let book = Order_book.Book.remove book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let best_bid = Order_book.Book.best_bid book in
  assert_float_equal 1.5 best_bid.volume "remove should decrease volume to 1.5"

(** Test 12: Market price calculation - buy side (takes asks) *)
let%test_unit "market_price_buy" =
  let book = Order_book.Book.empty "BTC/USD" in
  (* Add ask levels *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:3.0 in

  (* Buy 0.5 BTC - should get at $51,000 *)
  let result = Order_book.Book.ask_market_price book ~volume:0.5 in
  assert_float_equal 51000.0 result.price "avg price for 0.5 BTC should be $51,000";
  assert_float_equal 0.5 result.volume "filled volume should be 0.5";

  (* Buy 2.5 BTC - takes all of $51k (1.0) and 1.5 from $52k *)
  (* Cost = 1.0 * 51000 + 1.5 * 52000 = 51000 + 78000 = 129000 *)
  (* Avg = 129000 / 2.5 = 51600 *)
  let result = Order_book.Book.ask_market_price book ~volume:2.5 in
  assert_float_equal 51600.0 result.price "avg price for 2.5 BTC should be $51,600";
  assert_float_equal 2.5 result.volume "filled volume should be 2.5"

(** Test 13: Market price calculation - sell side (takes bids) *)
let%test_unit "market_price_sell" =
  let book = Order_book.Book.empty "BTC/USD" in
  (* Add bid levels *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:48000.0 ~size:3.0 in

  (* Sell 0.5 BTC - should get $50,000 *)
  let result = Order_book.Book.bid_market_price book ~volume:0.5 in
  assert_float_equal 50000.0 result.price "avg price for 0.5 BTC should be $50,000";
  assert_float_equal 0.5 result.volume "filled volume should be 0.5";

  (* Sell 2.5 BTC - takes all of $50k (1.0) and 1.5 from $49k *)
  (* Cost = 1.0 * 50000 + 1.5 * 49000 = 50000 + 73500 = 123500 *)
  (* Avg = 123500 / 2.5 = 49400 *)
  let result = Order_book.Book.bid_market_price book ~volume:2.5 in
  assert_float_equal 49400.0 result.price "avg price for 2.5 BTC should be $49,400";
  assert_float_equal 2.5 result.volume "filled volume should be 2.5"

(** Test 14: Market price - insufficient liquidity *)
let%test_unit "market_price_insufficient_liquidity" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in

  (* Try to buy 2.0 BTC but only 1.0 available *)
  let result = Order_book.Book.ask_market_price book ~volume:2.0 in
  assert_float_equal 51000.0 result.price "avg price should be $51,000";
  assert_float_equal 1.0 result.volume "only 1.0 BTC filled (partial)"

(** Test 15: Mid market price *)
let%test_unit "mid_market_price" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in

  let result = Order_book.Book.mid_market_price book ~volume:0.5 in
  (* Mid = (50000 + 51000) / 2 = 50500 *)
  assert_float_equal 50500.0 result.price "mid price should be $50,500"

(** Test 16: Total volume at price level *)
let%test_unit "total_volume_at_price_level" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:2.5 in

  let level = Order_book.Book.total_bid_volume_at_price_level book ~price:50000.0 in
  assert_float_equal 50000.0 level.price "price should be $50,000";
  assert_float_equal 2.5 level.volume "volume should be 2.5";

  (* Non-existent price level *)
  let level = Order_book.Book.total_bid_volume_at_price_level book ~price:49000.0 in
  assert_float_equal 0.0 level.price "non-existent level should return empty";
  assert_float_equal 0.0 level.volume "non-existent level should have 0 volume"

(** Test 17: Quantity from notional - bid *)
let%test_unit "quantity_from_notional_bid" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in

  (* $100,000 / $50,000 = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_bid book ~notional:100000.0 in
  assert_float_equal 2.0 qty "should be 2.0 BTC for $100,000"

(** Test 18: Quantity from notional - ask *)
let%test_unit "quantity_from_notional_ask" =
  let book = Order_book.Book.empty "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in

  (* $102,000 / $51,000 = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_ask book ~notional:102000.0 in
  assert_float_equal 2.0 qty "should be 2.0 BTC for $102,000"

(** Test 19: Epoch increments on updates *)
let%test_unit "epoch_increments" =
  let book = Order_book.Book.empty "BTC/USD" in
  [%test_result: int] ~expect:0 (Order_book.Book.epoch book);

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  [%test_result: int] ~expect:1 (Order_book.Book.epoch book);

  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  [%test_result: int] ~expect:2 (Order_book.Book.epoch book);

  let book = Order_book.Book.update book ~side:`Bid ~price:50000.0 ~size:0.5 in
  [%test_result: int] ~expect:3 (Order_book.Book.epoch book)

(** Test 20: Multi-symbol Books manager *)
let%test_unit "books_multi_symbol" =
  let books = Order_book.Books.empty in

  (* Add BTC book *)
  let books = Order_book.Books.set books ~symbol:"BTC/USD" ~side:`Bid ~price:50000.0 ~size:1.0 in
  (* Add ETH book *)
  let books = Order_book.Books.set books ~symbol:"ETH/USD" ~side:`Bid ~price:3000.0 ~size:10.0 in

  (* Check symbols *)
  let symbols = Order_book.Books.symbols books in
  [%test_result: int] ~expect:2 (List.length symbols);

  (* Get BTC book *)
  let btc_book = Order_book.Books.book_exn books "BTC/USD" in
  let best_bid = Order_book.Book.best_bid btc_book in
  assert_float_equal 50000.0 best_bid.price "BTC best bid should be $50,000";

  (* Get ETH book *)
  let eth_book = Order_book.Books.book_exn books "ETH/USD" in
  let best_bid = Order_book.Book.best_bid eth_book in
  assert_float_equal 3000.0 best_bid.price "ETH best bid should be $3,000"

(** Test 21: Books update operation *)
let%test_unit "books_update_operation" =
  let books = Order_book.Books.empty in
  let books = Order_book.Books.set books ~symbol:"BTC/USD" ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Order_book.Books.update books ~symbol:"BTC/USD" ~side:`Bid ~price:50000.0 ~size:0.5 in

  let btc_book = Order_book.Books.book_exn books "BTC/USD" in
  let best_bid = Order_book.Book.best_bid btc_book in
  assert_float_equal 1.5 best_bid.volume "volume should be 1.0 + 0.5 = 1.5"

let () = print_endline "All Order_book tests defined"

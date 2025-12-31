open Core

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_string_equal expected actual msg =
  incr tests_run;
  if not (String.equal expected actual) then begin
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  end

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  if Float.(abs (expected - actual) > tolerance) then begin
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  end

let assert_int64_equal expected actual msg =
  incr tests_run;
  if not (Int64.equal expected actual) then begin
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %Ld, Got: %Ld\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  end

let assert_true condition msg =
  incr tests_run;
  if not condition then begin
    incr tests_failed;
    printf "  ✗ FAIL: %s\n" msg;
    false
  end else begin
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  end

(* Order Book Tests *)
let test_order_book_empty () =
  printf "\n[Order Book] Empty book\n";
  let book = Bitrue.Order_book.Book.empty "BTCUSDT" in
  let best_bid = Bitrue.Order_book.Book.best_bid book in
  let best_ask = Bitrue.Order_book.Book.best_ask book in
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Empty book has zero best bid price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Empty book has zero best bid volume");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Empty book has zero best ask price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Empty book has zero best ask volume")

let test_order_book_operations () =
  printf "\n[Order Book] Basic operations\n";
  let book = Bitrue.Order_book.Book.empty "BTCUSDT" in
  let book = Bitrue.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let book = Bitrue.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_bid = Bitrue.Order_book.Book.best_bid book in
  let best_ask = Bitrue.Order_book.Book.best_ask book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid price");
  ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume");
  ignore (assert_float_equal 51000.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask price");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume")

let test_order_book_sorting () =
  printf "\n[Order Book] Price level sorting\n";
  let book = Bitrue.Order_book.Book.empty "BTCUSDT" in
  let book = Bitrue.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Bitrue.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Bitrue.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Bitrue.Order_book.Book.set book ~side:`Ask ~price:50900.0 ~size:2.0 in
  let best_bid = Bitrue.Order_book.Book.best_bid book in
  let best_ask = Bitrue.Order_book.Book.best_ask book in
  ignore (assert_float_equal 50100.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid is highest");
  ignore (assert_float_equal 50900.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask is lowest")

(* WebSocket Message Parsing Tests *)
let test_ws_parse_ping () =
  printf "\n[WebSocket] Parse ping message\n";
  let msg = {|{"ping": 1234567890}|} in
  match Bitrue.Ws.parse_message msg with
  | Bitrue.Ws.Message.Ping ts ->
    ignore (assert_int64_equal 1234567890L ts "Ping timestamp is correct")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Ping message\n"

let test_ws_parse_sub_response () =
  printf "\n[WebSocket] Parse subscription response\n";
  let msg = {|{
    "event_rep": "subed",
    "channel": "market_btcusdt_simple_depth_step0",
    "cb_id": "market_btcusdt_simple_depth_step0",
    "status": "ok"
  }|} in
  match Bitrue.Ws.parse_message msg with
  | Bitrue.Ws.Message.SubResponse resp ->
    ignore (assert_string_equal "subed" resp.event_rep "Event rep is 'subed'");
    (match resp.status with
     | Some "ok" -> ignore (assert_true true "Status is ok")
     | _ -> incr tests_failed; printf "  ✗ FAIL: Status not ok\n")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected SubResponse message\n"

let test_ws_parse_depth () =
  printf "\n[WebSocket] Parse depth message\n";
  let msg = {|{
    "channel": "market_btcusdt_simple_depth_step0",
    "ts": 1609459200000,
    "tick": {
      "buys": [["50000.00", "1.5"], ["49900.00", "2.0"]],
      "asks": [["51000.00", "1.0"], ["51100.00", "0.5"]]
    }
  }|} in
  match Bitrue.Ws.parse_message msg with
  | Bitrue.Ws.Message.Depth depth ->
    ignore (assert_true (String.is_substring depth.channel ~substring:"depth") "Channel contains 'depth'");
    ignore (assert_int64_equal 1609459200000L depth.ts "Timestamp is correct");
    ignore (assert_true (List.length depth.tick.buys = 2) "Has 2 bids");
    ignore (assert_true (List.length depth.tick.asks = 2) "Has 2 asks")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Depth message\n"

let test_ws_stream_names () =
  printf "\n[WebSocket] Stream name generation\n";
  let depth = Bitrue.Ws.Stream.Depth "BTCUSDT" in
  let trade = Bitrue.Ws.Stream.Trade "ETHUSDT" in
  let ticker = Bitrue.Ws.Stream.Ticker "BNBUSDT" in
  ignore (assert_string_equal "market_btcusdt_simple_depth_step0" (Bitrue.Ws.Stream.to_channel depth) "Depth channel");
  ignore (assert_string_equal "market_ethusdt_trade_ticker" (Bitrue.Ws.Stream.to_channel trade) "Trade channel");
  ignore (assert_string_equal "market_bnbusdt_ticker" (Bitrue.Ws.Stream.to_channel ticker) "Ticker channel")

(* Books Tests *)
let test_books_operations () =
  printf "\n[Books] Multi-symbol operations\n";
  let books = Bitrue.Order_book.Books.empty in
  let book1 = Bitrue.Order_book.Book.empty "BTCUSDT" in
  let book1 = Bitrue.Order_book.Book.set book1 ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Bitrue.Order_book.Books.set_book books book1 in
  let book2 = Bitrue.Order_book.Book.empty "ETHUSDT" in
  let books = Bitrue.Order_book.Books.set_book books book2 in
  let symbols = Bitrue.Order_book.Books.symbols books in
  ignore (assert_true (List.length symbols = 2) "Books contains 2 symbols");
  ignore (assert_true (List.mem symbols "BTCUSDT" ~equal:String.equal) "Contains BTCUSDT");
  ignore (assert_true (List.mem symbols "ETHUSDT" ~equal:String.equal) "Contains ETHUSDT")

(* Main test runner *)
let () =
  printf "===========================================\n";
  printf "Bitrue Exchange Test Suite\n";
  printf "===========================================\n";

  (* Order Book Tests *)
  let _ = test_order_book_empty () in
  let _ = test_order_book_operations () in
  let _ = test_order_book_sorting () in

  (* WebSocket Tests *)
  let _ = test_ws_parse_ping () in
  let _ = test_ws_parse_sub_response () in
  let _ = test_ws_parse_depth () in
  let _ = test_ws_stream_names () in

  (* Books Tests *)
  let _ = test_books_operations () in

  (* Summary *)
  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d ✓\n" !tests_passed;
  printf "Failed:       %d ✗\n" !tests_failed;
  printf "Success rate: %.1f%%\n" (Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0);
  printf "===========================================\n";

  if !tests_failed > 0 then exit 1

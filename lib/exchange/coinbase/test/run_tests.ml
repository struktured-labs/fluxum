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
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let best_bid = Coinbase.Order_book.Book.best_bid book in
  let best_ask = Coinbase.Order_book.Book.best_ask book in
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Empty book has zero best bid price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Empty book has zero best bid volume");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Empty book has zero best ask price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Empty book has zero best ask volume")

let test_order_book_add_bid () =
  printf "\n[Order Book] Add bid\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Coinbase.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid price is 50000.0");
  ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.5")

let test_order_book_add_ask () =
  printf "\n[Order Book] Add ask\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_ask = Coinbase.Order_book.Book.best_ask book in
  ignore (assert_float_equal 51000.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask price is 51000.0");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 2.0")

let test_order_book_multiple_bids () =
  printf "\n[Order Book] Multiple bids - best is highest\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:49900.0 ~size:1.5 in
  let best_bid = Coinbase.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50100.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid is highest price (50100.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 2.0")

let test_order_book_multiple_asks () =
  printf "\n[Order Book] Multiple asks - best is lowest\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Ask ~price:50900.0 ~size:2.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Ask ~price:51100.0 ~size:1.5 in
  let best_ask = Coinbase.Order_book.Book.best_ask book in
  ignore (assert_float_equal 50900.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask is lowest price (50900.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 2.0")

let test_order_book_remove_level () =
  printf "\n[Order Book] Remove price level with zero size\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:0.0 in
  let best_bid = Coinbase.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "After removing 50100, best bid is 50000.0");
  ignore (assert_float_equal 1.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.0")

let test_order_book_update_level () =
  printf "\n[Order Book] Update existing price level\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:3.5 in
  let best_bid = Coinbase.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Price remains 50000.0");
  ignore (assert_float_equal 3.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Volume updated to 3.5")

let test_order_book_epoch () =
  printf "\n[Order Book] Epoch increments on updates\n";
  let book = Coinbase.Order_book.Book.empty "BTC-USD" in
  let epoch0 = Coinbase.Order_book.Book.epoch book in
  let book = Coinbase.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let epoch1 = Coinbase.Order_book.Book.epoch book in
  let book = Coinbase.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let epoch2 = Coinbase.Order_book.Book.epoch book in
  ignore (assert_true (epoch0 = 0) "Initial epoch is 0");
  ignore (assert_true (epoch1 = 1) "Epoch increments to 1 after first update");
  ignore (assert_true (epoch2 = 2) "Epoch increments to 2 after second update")

(* WebSocket Message Parsing Tests *)
let test_ws_parse_subscribe_response () =
  printf "\n[WebSocket] Parse subscriptions response\n";
  let msg = {|{
    "type": "subscriptions",
    "channels": [
      {
        "channel": "level2",
        "product_ids": ["BTC-USD"]
      }
    ]
  }|} in
  match Coinbase.Ws.parse_message msg with
  | Coinbase.Ws.Message.Subscriptions resp ->
    ignore (assert_string_equal "subscriptions" resp.type_ "Type is subscriptions");
    ignore (assert_true (List.length resp.channels = 1) "Has 1 channel")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Subscriptions message\n"

let test_ws_parse_level2 () =
  printf "\n[WebSocket] Parse level2 message\n";
  let msg = {|{
    "channel": "l2_data",
    "client_id": "",
    "timestamp": "2023-01-01T00:00:00Z",
    "sequence_num": 1,
    "events": [
      {
        "product_id": "BTC-USD",
        "updates": [
          {"price_level": "50000.00", "new_quantity": "1.5"},
          {"price_level": "49900.00", "new_quantity": "2.0"}
        ],
        "side": "bid",
        "event_time": "2023-01-01T00:00:00Z"
      }
    ]
  }|} in
  match Coinbase.Ws.parse_message msg with
  | Coinbase.Ws.Message.Level2 level2 ->
    ignore (assert_string_equal "l2_data" level2.channel "Channel is l2_data");
    ignore (assert_int64_equal 1L level2.sequence_num "Sequence num is 1");
    ignore (assert_true (List.length level2.events = 1) "Has 1 event");
    (match List.hd level2.events with
     | Some event ->
       ignore (assert_string_equal "BTC-USD" event.product_id "Product ID is BTC-USD");
       ignore (assert_string_equal "bid" event.side "Side is bid");
       ignore (assert_true (List.length event.updates = 2) "Has 2 updates")
     | None ->
       incr tests_run;
       incr tests_failed;
       printf "  ✗ FAIL: No events in level2 message\n")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Level2 message\n"

let test_ws_parse_market_trades () =
  printf "\n[WebSocket] Parse market_trades message\n";
  let msg = {|{
    "channel": "market_trades",
    "client_id": "",
    "timestamp": "2023-01-01T00:00:00Z",
    "sequence_num": 1,
    "events": [
      {
        "trade_id": "12345",
        "product_id": "BTC-USD",
        "price": "50000.00",
        "size": "0.1",
        "side": "BUY",
        "time": "2023-01-01T00:00:00Z"
      }
    ]
  }|} in
  match Coinbase.Ws.parse_message msg with
  | Coinbase.Ws.Message.MarketTrades trades ->
    ignore (assert_string_equal "market_trades" trades.channel "Channel is market_trades");
    ignore (assert_true (List.length trades.events = 1) "Has 1 trade");
    (match List.hd trades.events with
     | Some trade ->
       ignore (assert_string_equal "12345" trade.trade_id "Trade ID is 12345");
       ignore (assert_string_equal "BTC-USD" trade.product_id "Product ID is BTC-USD");
       ignore (assert_string_equal "50000.00" trade.price "Price is 50000.00");
       ignore (assert_string_equal "0.1" trade.size "Size is 0.1")
     | None ->
       incr tests_run;
       incr tests_failed;
       printf "  ✗ FAIL: No events in market_trades message\n")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected MarketTrades message\n"

(* Stream name generation tests *)
let test_stream_names () =
  printf "\n[WebSocket] Stream configuration\n";
  let level2 = Coinbase.Ws.Stream.Level2 ["BTC-USD"; "ETH-USD"] in
  let trades = Coinbase.Ws.Stream.MarketTrades ["BTC-USD"] in
  ignore (assert_string_equal "level2" (Coinbase.Ws.Stream.channel_name level2) "Level2 channel name");
  ignore (assert_string_equal "market_trades" (Coinbase.Ws.Stream.channel_name trades) "MarketTrades channel name");
  ignore (assert_true (List.length (Coinbase.Ws.Stream.product_ids level2) = 2) "Level2 has 2 products");
  ignore (assert_true (List.length (Coinbase.Ws.Stream.product_ids trades) = 1) "Trades has 1 product")

(* Books (multi-symbol) tests *)
let test_books_empty () =
  printf "\n[Books] Empty multi-symbol books\n";
  let books = Coinbase.Order_book.Books.empty in
  let symbols = Coinbase.Order_book.Books.symbols books in
  ignore (assert_true (List.is_empty symbols) "Empty books has no symbols")

let test_books_set_and_get () =
  printf "\n[Books] Set and get books\n";
  let books = Coinbase.Order_book.Books.empty in
  let book1 = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book1 = Coinbase.Order_book.Book.set book1 ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Coinbase.Order_book.Books.set_book books book1 in
  match Coinbase.Order_book.Books.book books "BTC-USD" with
  | Some book ->
    let best_bid = Coinbase.Order_book.Book.best_bid book in
    ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Retrieved book has correct bid")
  | None ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Book not found\n"

let test_books_multiple_symbols () =
  printf "\n[Books] Multiple symbols\n";
  let books = Coinbase.Order_book.Books.empty in
  let book1 = Coinbase.Order_book.Book.empty "BTC-USD" in
  let book2 = Coinbase.Order_book.Book.empty "ETH-USD" in
  let books = Coinbase.Order_book.Books.set_book books book1 in
  let books = Coinbase.Order_book.Books.set_book books book2 in
  let symbols = Coinbase.Order_book.Books.symbols books in
  ignore (assert_true (List.length symbols = 2) "Books contains 2 symbols");
  ignore (assert_true (List.mem symbols "BTC-USD" ~equal:String.equal) "Contains BTC-USD");
  ignore (assert_true (List.mem symbols "ETH-USD" ~equal:String.equal) "Contains ETH-USD")

(* Main test runner *)
let () =
  printf "===========================================\n";
  printf "Coinbase Advanced Trade Test Suite\n";
  printf "===========================================\n";

  (* Order Book Tests *)
  let _ = test_order_book_empty () in
  let _ = test_order_book_add_bid () in
  let _ = test_order_book_add_ask () in
  let _ = test_order_book_multiple_bids () in
  let _ = test_order_book_multiple_asks () in
  let _ = test_order_book_remove_level () in
  let _ = test_order_book_update_level () in
  let _ = test_order_book_epoch () in

  (* WebSocket Message Parsing Tests *)
  let _ = test_ws_parse_subscribe_response () in
  let _ = test_ws_parse_level2 () in
  let _ = test_ws_parse_market_trades () in

  (* Stream Tests *)
  let _ = test_stream_names () in

  (* Books Tests *)
  let _ = test_books_empty () in
  let _ = test_books_set_and_get () in
  let _ = test_books_multiple_symbols () in

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

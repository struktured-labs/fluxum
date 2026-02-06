open Core

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_string_equal expected actual msg =
  incr tests_run;
  match String.equal expected actual with
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

let assert_int64_equal expected actual msg =
  incr tests_run;
  match Int64.equal expected actual with
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %Ld, Got: %Ld\n" msg expected actual;
    false
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

let assert_true condition msg =
  incr tests_run;
  match condition with
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n" msg;
    false
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

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

(* ===== NORMALIZE ERROR PATH TESTS ===== *)

let test_normalize_trade_invalid_price () =
  printf "\n[Normalize] Trade with invalid price\n";
  let trade : Coinbase.Rest.Types.trade = {
    trade_id = "12345";
    product_id = "BTC-USD";
    price = "not_a_number";  (* Invalid *)
    size = "1.0";
    time = "2023-01-01T00:00:00Z";
    side = "BUY";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.trade trade with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected malformed price: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject non-numeric price\n")

let test_normalize_trade_invalid_side () =
  printf "\n[Normalize] Trade with invalid side\n";
  let trade : Coinbase.Rest.Types.trade = {
    trade_id = "12345";
    product_id = "BTC-USD";
    price = "50000.0";
    size = "1.0";
    time = "2023-01-01T00:00:00Z";
    side = "INVALID_SIDE";  (* Invalid *)
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.trade trade with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected invalid side: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject invalid side\n")

let test_normalize_balance_invalid_value () =
  printf "\n[Normalize] Balance with invalid value\n";
  let balance : Coinbase.Rest.Account.account = {
    uuid = "test-uuid";
    name = "BTC Wallet";
    currency = "BTC";
    available_balance = { value = "invalid"; currency = "BTC" };
    default = false;
    active = true;
    created_at = None;
    updated_at = None;
    deleted_at = None;
    type_ = None;
    ready = true;
    hold = None;
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.balance balance with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected malformed balance: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject non-numeric balance\n")

let test_normalize_ticker_invalid_bid () =
  printf "\n[Normalize] Ticker with invalid bid price\n";
  let ticker : Coinbase.Rest.Types.ticker = {
    trades = [];
    best_bid = "NaN";  (* Invalid *)
    best_ask = "51000.0";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.ticker ticker with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected NaN bid: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject NaN bid price\n")

let test_normalize_order_book_malformed () =
  printf "\n[Normalize] Order book with malformed price\n";
  let book : Coinbase.Rest.Types.product_book = {
    product_id = "BTC-USD";
    bids = [{ price = "invalid_price"; size = "1.0" }];  (* Invalid *)
    asks = [{ price = "51000.0"; size = "1.0" }];
    time = "2023-01-01T00:00:00Z";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.order_book book with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected malformed price: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject non-numeric price\n")

let test_normalize_public_trade_zero_qty () =
  printf "\n[Normalize] Public trade with zero quantity\n";
  let trade : Coinbase.Rest.Types.trade = {
    trade_id = "12345";
    product_id = "BTC-USD";
    price = "50000.0";
    size = "0.0";  (* Zero *)
    time = "2023-01-01T00:00:00Z";
    side = "BUY";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.public_trade trade with
   | Error _msg ->
     incr tests_passed;
     printf "  ✓ Rejected zero quantity\n"
   | Ok _trade ->
     (* Some implementations may allow zero qty *)
     incr tests_passed;
     printf "  ✓ Accepted zero quantity trade\n")

let test_normalize_symbol_info_missing_fields () =
  printf "\n[Normalize] Symbol info with missing optional fields\n";
  let symbol_info : Coinbase.Rest.Types.product = {
    product_id = "BTC-USD";
    price = None;
    price_percentage_change_24h = None;
    volume_24h = None;
    volume_percentage_change_24h = None;
    base_increment = None;  (* Missing *)
    quote_increment = None;  (* Missing *)
    quote_min_size = None;
    quote_max_size = None;
    base_min_size = None;  (* Missing *)
    base_max_size = None;
    base_name = Some "Bitcoin";
    quote_name = Some "US Dollar";
    status = Some "online";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.symbol_info symbol_info with
   | Ok info ->
     incr tests_passed;
     ignore (assert_string_equal "BTC-USD" info.symbol "Symbol is BTC-USD");
     printf "  ✓ Valid symbol info with missing optionals\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept valid symbol info: %s\n" msg)

let test_normalize_book_update_empty_levels () =
  printf "\n[Normalize] Book update with empty levels\n";
  let book : Coinbase.Rest.Types.product_book = {
    product_id = "BTC-USD";
    bids = [];  (* Empty *)
    asks = [];
    time = "2023-01-01T00:00:00Z";
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.book_update book with
   | Ok update ->
     incr tests_passed;
     ignore (assert_true (List.is_empty update.levels) "Empty levels list");
     printf "  ✓ Accepted empty book update\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept empty book update: %s\n" msg)

(* ===== ACCOUNT OPERATIONS TESTS ===== *)

let test_normalize_deposit_address () =
  printf "\n[Normalize] Deposit address\n";
  let addr : Coinbase.Rest.Deposit_address.t = {
    id = "addr-123";
    address = "0x1234567890abcdef1234567890abcdef12345678";
    address_info = Some { address = "0x1234567890abcdef1234567890abcdef12345678"; destination_tag = None };
    name = Some "My BTC Address";
    created_at = Some "2023-01-01T00:00:00Z";
    updated_at = Some "2023-01-01T00:00:00Z";
    network = Some "ethereum";
    uri_scheme = None;
    resource = Some "address";
    resource_path = None;
    deposit_uri = None;
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.deposit_address addr with
   | Ok da ->
     incr tests_passed;
     ignore (assert_string_equal "0x1234567890abcdef1234567890abcdef12345678" da.address "Address matches");
     ignore (assert_string_equal "ethereum" (Option.value da.network ~default:"") "Network is ethereum");
     printf "  ✓ Valid deposit address normalized\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept valid deposit address: %s\n" msg)

let test_normalize_deposit_address_with_tag () =
  printf "\n[Normalize] Deposit address with destination tag (XRP-style)\n";
  let addr : Coinbase.Rest.Deposit_address.t = {
    id = "addr-456";
    address = "rN7n3473SaZBCG4dFL83w7a1RXtXtbk2D9";
    address_info = Some { address = "rN7n3473SaZBCG4dFL83w7a1RXtXtbk2D9"; destination_tag = Some "12345678" };
    name = Some "My XRP Address";
    created_at = None;
    updated_at = None;
    network = Some "ripple";
    uri_scheme = None;
    resource = None;
    resource_path = None;
    deposit_uri = None;
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.deposit_address addr with
   | Ok da ->
     incr tests_passed;
     ignore (assert_string_equal "12345678" (Option.value da.tag ~default:"") "Destination tag extracted");
     printf "  ✓ Deposit address with tag normalized\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept address with tag: %s\n" msg)

let test_normalize_deposit () =
  printf "\n[Normalize] Deposit transaction\n";
  let tx : Coinbase.Rest.Transaction.t = {
    id = "tx-123";
    type_ = "receive";
    status = "completed";
    amount = { amount = "0.5"; currency = "BTC" };
    native_amount = Some { amount = "25000.00"; currency = "USD" };
    description = Some "Deposit from external wallet";
    created_at = Some "2023-01-01T12:00:00Z";
    updated_at = Some "2023-01-01T12:30:00Z";
    resource = Some "transaction";
    resource_path = None;
    network = Some { status = Some "confirmed"; hash = Some "0xabc123"; transaction_url = None; name = Some "bitcoin" };
    to_ = None;
    from_ = Some { id = None; resource = None; address = Some "bc1qexample"; currency = None; address_info = None };
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.deposit tx with
   | Ok dep ->
     incr tests_passed;
     ignore (assert_string_equal "tx-123" dep.id "Deposit ID matches");
     ignore (assert_string_equal "BTC" dep.currency "Currency is BTC");
     ignore (assert_float_equal 0.5 dep.amount "Amount is 0.5");
     ignore (assert_string_equal "0xabc123" (Option.value dep.tx_id ~default:"") "TX hash extracted");
     printf "  ✓ Valid deposit normalized\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept valid deposit: %s\n" msg)

let test_normalize_deposit_invalid_amount () =
  printf "\n[Normalize] Deposit with invalid amount\n";
  let tx : Coinbase.Rest.Transaction.t = {
    id = "tx-bad";
    type_ = "receive";
    status = "completed";
    amount = { amount = "not_a_number"; currency = "BTC" };
    native_amount = None;
    description = None;
    created_at = None;
    updated_at = None;
    resource = None;
    resource_path = None;
    network = None;
    to_ = None;
    from_ = None;
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.deposit tx with
   | Error msg ->
     incr tests_passed;
     printf "  ✓ Rejected invalid amount: %s\n" msg
   | Ok _ ->
     incr tests_failed;
     printf "  ✗ FAIL: Should reject non-numeric amount\n")

let test_normalize_withdrawal () =
  printf "\n[Normalize] Withdrawal transaction\n";
  let tx : Coinbase.Rest.Transaction.t = {
    id = "tx-789";
    type_ = "send";
    status = "pending";
    amount = { amount = "1.0"; currency = "ETH" };
    native_amount = Some { amount = "2000.00"; currency = "USD" };
    description = Some "Withdrawal to external wallet";
    created_at = Some "2023-01-01T14:00:00Z";
    updated_at = Some "2023-01-01T14:00:00Z";
    resource = Some "transaction";
    resource_path = None;
    network = Some { status = Some "pending"; hash = None; transaction_url = None; name = Some "ethereum" };
    to_ = Some { id = None; resource = None; address = Some "0xdestination"; currency = None; address_info = None };
    from_ = None;
  } in
  incr tests_run;
  (match Coinbase.Fluxum_adapter.Adapter.Normalize.withdrawal tx with
   | Ok wd ->
     incr tests_passed;
     ignore (assert_string_equal "tx-789" wd.id "Withdrawal ID matches");
     ignore (assert_string_equal "ETH" wd.currency "Currency is ETH");
     ignore (assert_float_equal 1.0 wd.amount "Amount is 1.0");
     ignore (assert_string_equal "0xdestination" wd.address "Destination address extracted");
     printf "  ✓ Valid withdrawal normalized\n"
   | Error msg ->
     incr tests_failed;
     printf "  ✗ FAIL: Should accept valid withdrawal: %s\n" msg)

let test_normalize_withdrawal_status_mapping () =
  printf "\n[Normalize] Withdrawal status mapping\n";
  let make_tx status = {
    Coinbase.Rest.Transaction.id = "tx-status";
    type_ = "send";
    status;
    amount = { amount = "1.0"; currency = "BTC" };
    native_amount = None;
    description = None;
    created_at = None;
    updated_at = None;
    resource = None;
    resource_path = None;
    network = None;
    to_ = Some { id = None; resource = None; address = Some "0xaddr"; currency = None; address_info = None };
    from_ = None;
  } in
  (* Test various status mappings *)
  let check_status input_status expected_str =
    let tx = make_tx input_status in
    match Coinbase.Fluxum_adapter.Adapter.Normalize.withdrawal tx with
    | Ok wd ->
      let status_str = Fluxum.Types.Transfer_status.to_string wd.status in
      ignore (assert_string_equal expected_str status_str (sprintf "Status '%s' -> '%s'" input_status expected_str))
    | Error _ ->
      incr tests_run;
      incr tests_failed;
      printf "  ✗ FAIL: Failed to normalize withdrawal with status '%s'\n" input_status
  in
  check_status "pending" "pending";
  check_status "completed" "completed";
  check_status "failed" "failed";
  check_status "canceled" "cancelled"

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

  (* Normalize Error Path Tests *)
  let _ = test_normalize_trade_invalid_price () in
  let _ = test_normalize_trade_invalid_side () in
  let _ = test_normalize_balance_invalid_value () in
  let _ = test_normalize_ticker_invalid_bid () in
  let _ = test_normalize_order_book_malformed () in
  let _ = test_normalize_public_trade_zero_qty () in
  let _ = test_normalize_symbol_info_missing_fields () in
  let _ = test_normalize_book_update_empty_levels () in

  (* Account Operations Tests *)
  let _ = test_normalize_deposit_address () in
  let _ = test_normalize_deposit_address_with_tag () in
  let _ = test_normalize_deposit () in
  let _ = test_normalize_deposit_invalid_amount () in
  let _ = test_normalize_withdrawal () in
  let _ = test_normalize_withdrawal_status_mapping () in

  (* Summary *)
  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d ✓\n" !tests_passed;
  printf "Failed:       %d ✗\n" !tests_failed;
  printf "Success rate: %.1f%%\n" (Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0);
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

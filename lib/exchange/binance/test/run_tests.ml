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
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let best_bid = Binance.Order_book.Book.best_bid book in
  let best_ask = Binance.Order_book.Book.best_ask book in
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Empty book has zero best bid price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Empty book has zero best bid volume");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Empty book has zero best ask price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Empty book has zero best ask volume")

let test_order_book_add_bid () =
  printf "\n[Order Book] Add bid\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Binance.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid price is 50000.0");
  ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.5")

let test_order_book_add_ask () =
  printf "\n[Order Book] Add ask\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_ask = Binance.Order_book.Book.best_ask book in
  ignore (assert_float_equal 51000.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask price is 51000.0");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 2.0")

let test_order_book_multiple_bids () =
  printf "\n[Order Book] Multiple bids - best is highest\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:49900.0 ~size:1.5 in
  let best_bid = Binance.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50100.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid is highest price (50100.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 2.0")

let test_order_book_multiple_asks () =
  printf "\n[Order Book] Multiple asks - best is lowest\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Binance.Order_book.Book.set book ~side:`Ask ~price:50900.0 ~size:2.0 in
  let book = Binance.Order_book.Book.set book ~side:`Ask ~price:51100.0 ~size:1.5 in
  let best_ask = Binance.Order_book.Book.best_ask book in
  ignore (assert_float_equal 50900.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask is lowest price (50900.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 2.0")

let test_order_book_remove_level () =
  printf "\n[Order Book] Remove price level with zero size\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:0.0 in
  let best_bid = Binance.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "After removing 50100, best bid is 50000.0");
  ignore (assert_float_equal 1.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.0")

let test_order_book_update_level () =
  printf "\n[Order Book] Update existing price level\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:3.5 in
  let best_bid = Binance.Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Price remains 50000.0");
  ignore (assert_float_equal 3.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Volume updated to 3.5")

let test_order_book_epoch () =
  printf "\n[Order Book] Epoch increments on updates\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let epoch0 = Binance.Order_book.Book.epoch book in
  let book = Binance.Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let epoch1 = Binance.Order_book.Book.epoch book in
  let book = Binance.Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let epoch2 = Binance.Order_book.Book.epoch book in
  ignore (assert_true (epoch0 = 0) "Initial epoch is 0");
  ignore (assert_true (epoch1 = 1) "Epoch increments to 1 after first update");
  ignore (assert_true (epoch2 = 2) "Epoch increments to 2 after second update")

(* WebSocket Message Parsing Tests *)
let test_ws_parse_trade () =
  printf "\n[WebSocket] Parse trade message\n";
  let msg = {|{
    "e": "trade",
    "E": 1672531200000,
    "s": "BTCUSDT",
    "t": 12345,
    "p": "50000.00",
    "q": "1.5",
    "b": 88,
    "a": 50,
    "T": 1672531200000,
    "m": true
  }|} in
  match Binance.Ws.parse_message msg with
  | Binance.Ws.Message.Trade trade ->
    ignore (assert_string_equal "trade" trade.event_type "Event type is 'trade'");
    ignore (assert_string_equal "BTCUSDT" trade.symbol "Symbol is BTCUSDT");
    ignore (assert_string_equal "50000.00" trade.price "Price is 50000.00");
    ignore (assert_string_equal "1.5" trade.quantity "Quantity is 1.5")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Trade message\n"

let test_ws_parse_depth_update () =
  printf "\n[WebSocket] Parse depth update message\n";
  let msg = {|{
    "e": "depthUpdate",
    "E": 1672531200000,
    "s": "BTCUSDT",
    "U": 157,
    "u": 160,
    "b": [["50000.00", "1.5"], ["49900.00", "2.0"]],
    "a": [["51000.00", "1.0"], ["51100.00", "0.5"]]
  }|} in
  match Binance.Ws.parse_message msg with
  | Binance.Ws.Message.DepthUpdate update ->
    ignore (assert_string_equal "depthUpdate" update.event_type "Event type is 'depthUpdate'");
    ignore (assert_string_equal "BTCUSDT" update.symbol "Symbol is BTCUSDT");
    ignore (assert_int64_equal 157L update.first_update_id "First update ID is 157");
    ignore (assert_int64_equal 160L update.final_update_id "Final update ID is 160");
    ignore (assert_true (List.length update.bids = 2) "Has 2 bid updates");
    ignore (assert_true (List.length update.asks = 2) "Has 2 ask updates")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected DepthUpdate message\n"

let test_ws_parse_depth_snapshot () =
  printf "\n[WebSocket] Parse depth snapshot message\n";
  let msg = {|{
    "lastUpdateId": 160,
    "bids": [["50000.00", "1.5"], ["49900.00", "2.0"]],
    "asks": [["51000.00", "1.0"], ["51100.00", "0.5"]]
  }|} in
  match Binance.Ws.parse_message msg with
  | Binance.Ws.Message.Depth snapshot ->
    ignore (assert_int64_equal 160L snapshot.last_update_id "Last update ID is 160");
    ignore (assert_true (List.length snapshot.bids = 2) "Has 2 bids");
    ignore (assert_true (List.length snapshot.asks = 2) "Has 2 asks")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected Depth snapshot message\n"

(* Order Book Update Tests *)
let test_apply_depth_update () =
  printf "\n[Order Book] Apply depth update\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let update : Binance.Ws.Message.depth_update = {
    event_type = "depthUpdate";
    event_time = 1672531200000L;
    symbol = "BTCUSDT";
    first_update_id = 1L;
    final_update_id = 2L;
    bids = [("50000.00", "1.5"); ("49900.00", "2.0")];
    asks = [("51000.00", "1.0"); ("51100.00", "0.5")];
  } in
  (match Binance.Order_book.Book.apply_depth_update book update with
   | Ok book ->
     let best_bid = Binance.Order_book.Book.best_bid book in
     let best_ask = Binance.Order_book.Book.best_ask book in
     ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid is 50000.0");
     ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.5");
     ignore (assert_float_equal 51000.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask is 51000.0");
     ignore (assert_float_equal 1.0 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 1.0");
     ignore (assert_int64_equal 2L (Binance.Order_book.Book.last_update_id book) "Last update ID is 2")
   | Error msg ->
     printf "  X FAIL: Failed to apply depth update: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_apply_depth_snapshot () =
  printf "\n[Order Book] Apply depth snapshot\n";
  let book = Binance.Order_book.Book.empty "BTCUSDT" in
  let snapshot : Binance.Ws.Message.depth = {
    last_update_id = 100L;
    bids = [("50100.00", "1.0"); ("50000.00", "2.5"); ("49900.00", "3.0")];
    asks = [("50900.00", "1.5"); ("51000.00", "2.0"); ("51100.00", "1.0")];
  } in
  (match Binance.Order_book.Book.apply_depth_snapshot book snapshot with
   | Ok book ->
     let best_bid = Binance.Order_book.Book.best_bid book in
     let best_ask = Binance.Order_book.Book.best_ask book in
     ignore (assert_float_equal 50100.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Best bid is 50100.0");
     ignore (assert_float_equal 1.0 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Best bid volume is 1.0");
     ignore (assert_float_equal 50900.0 (Exchange_common.Order_book_base.Price_level.price best_ask) "Best ask is 50900.0");
     ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_ask) "Best ask volume is 1.5");
     ignore (assert_int64_equal 100L (Binance.Order_book.Book.last_update_id book) "Last update ID is 100")
   | Error msg ->
     printf "  X FAIL: Failed to apply depth snapshot: %s\n" msg;
     incr tests_run; incr tests_failed)

(* Stream name generation tests *)
let test_stream_names () =
  printf "\n[WebSocket] Stream name generation\n";
  let trade = Binance.Ws.Stream.Trade "BTCUSDT" in
  let depth_20 = Binance.Ws.Stream.Depth { symbol = "ETHUSDT"; levels = Some 20 } in
  let depth_full = Binance.Ws.Stream.Depth { symbol = "BNBUSDT"; levels = None } in
  let ticker = Binance.Ws.Stream.Ticker "ADAUSDT" in
  ignore (assert_string_equal "btcusdt@trade" (Binance.Ws.Stream.to_stream_name trade) "Trade stream name");
  ignore (assert_string_equal "ethusdt@depth20" (Binance.Ws.Stream.to_stream_name depth_20) "Depth 20 stream name");
  ignore (assert_string_equal "bnbusdt@depth" (Binance.Ws.Stream.to_stream_name depth_full) "Depth full stream name");
  ignore (assert_string_equal "adausdt@ticker" (Binance.Ws.Stream.to_stream_name ticker) "Ticker stream name")

(* Books (multi-symbol) tests *)
let test_books_empty () =
  printf "\n[Books] Empty multi-symbol books\n";
  let books = Binance.Order_book.Books.empty in
  let symbols = Binance.Order_book.Books.symbols books in
  ignore (assert_true (List.is_empty symbols) "Empty books has no symbols")

let test_books_set_and_get () =
  printf "\n[Books] Set and get books\n";
  let books = Binance.Order_book.Books.empty in
  let book1 = Binance.Order_book.Book.empty "BTCUSDT" in
  let book1 = Binance.Order_book.Book.set book1 ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Binance.Order_book.Books.set_book books book1 in
  match Binance.Order_book.Books.book books "BTCUSDT" with
  | Some book ->
    let best_bid = Binance.Order_book.Book.best_bid book in
    ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid) "Retrieved book has correct bid")
  | None ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Book not found\n"

let test_books_multiple_symbols () =
  printf "\n[Books] Multiple symbols\n";
  let books = Binance.Order_book.Books.empty in
  let book1 = Binance.Order_book.Book.empty "BTCUSDT" in
  let book2 = Binance.Order_book.Book.empty "ETHUSDT" in
  let books = Binance.Order_book.Books.set_book books book1 in
  let books = Binance.Order_book.Books.set_book books book2 in
  let symbols = Binance.Order_book.Books.symbols books in
  ignore (assert_true (List.length symbols = 2) "Books contains 2 symbols");
  ignore (assert_true (List.mem symbols "BTCUSDT" ~equal:String.equal) "Contains BTCUSDT");
  ignore (assert_true (List.mem symbols "ETHUSDT" ~equal:String.equal) "Contains ETHUSDT")

(* ===== REST API Tests ===== *)

(* Signature Tests *)
let test_signature_query_string () =
  printf "\n[Signature] Query string building\n";
  let params = [("symbol", "LTCBTC"); ("side", "BUY"); ("type", "LIMIT")] in
  let query = Binance.Signature.build_query_string params in
  ignore (assert_string_equal "symbol=LTCBTC&side=BUY&type=LIMIT" query "Basic query string");

  (* Test URL encoding *)
  let params_encoded = [("key", "hello world"); ("value", "test@123")] in
  let query_encoded = Binance.Signature.build_query_string params_encoded in
  ignore (assert_true (String.is_substring query_encoded ~substring:"hello%20world") "Query string URL encoding")

let test_signature_hmac_format () =
  printf "\n[Signature] HMAC-SHA256 signature format\n";
  let params = [("symbol", "BTCUSDT"); ("side", "BUY")] in
  let signature = Binance.Signature.sign ~api_secret:"test_secret" ~params in
  (* Signature should be 64 hex characters (32 bytes * 2) *)
  ignore (assert_true (String.length signature = 64) "Signature is 64 hex characters");
  (* Signature should be lowercase hex *)
  ignore (assert_true (String.for_all signature ~f:(fun c -> Char.is_alphanum c && not (Char.is_uppercase c))) "Signature is lowercase hex")

let test_signature_official_example () =
  printf "\n[Signature] Official Binance signature example\n";
  (* From Binance API docs - https://developers.binance.com/docs/binance-spot-api-docs/rest-api#signed-trade-and-user_data-endpoint-security *)
  let api_secret = "NhqPtmdSJYdKjVHjA7PZj4Mge3R5YNiP1e3UZjInClVN65XAbvqqM6A7H5fATj0j" in
  let params =
    [ ("symbol", "LTCBTC")
    ; ("side", "BUY")
    ; ("type", "LIMIT")
    ; ("timeInForce", "GTC")
    ; ("quantity", "1")
    ; ("price", "0.1")
    ; ("recvWindow", "5000")
    ; ("timestamp", "1499827319559")
    ] in
  let signature = Binance.Signature.sign ~api_secret ~params in
  let expected = "c8db56825ae71d6d79447849e617115f4a920fa2acdcab2b053c4b2838bd6b71" in
  ignore (assert_string_equal expected signature "Official Binance example signature matches")

let test_signature_deterministic () =
  printf "\n[Signature] Deterministic signatures\n";
  let params = [("symbol", "BTCUSDT"); ("timestamp", "1234567890")] in
  let sig1 = Binance.Signature.sign ~api_secret:"secret123" ~params in
  let sig2 = Binance.Signature.sign ~api_secret:"secret123" ~params in
  ignore (assert_string_equal sig1 sig2 "Same inputs produce same signature")

(* Common Types Tests *)
let test_common_side () =
  printf "\n[Common] Side enum\n";
  ignore (assert_string_equal "BUY" (Binance.Common.Side.to_string `BUY) "BUY to string");
  ignore (assert_string_equal "SELL" (Binance.Common.Side.to_string `SELL) "SELL to string");
  ignore (assert_true (Option.is_some (Binance.Common.Side.of_string_opt "BUY")) "Parse BUY");
  ignore (assert_true (Option.is_some (Binance.Common.Side.of_string_opt "SELL")) "Parse SELL");
  ignore (assert_true (Option.is_none (Binance.Common.Side.of_string_opt "INVALID")) "Invalid side returns None")

let test_common_order_type () =
  printf "\n[Common] Order type enum\n";
  ignore (assert_string_equal "LIMIT" (Binance.Common.Order_type.to_string `LIMIT) "LIMIT to string");
  ignore (assert_string_equal "MARKET" (Binance.Common.Order_type.to_string `MARKET) "MARKET to string");
  ignore (assert_string_equal "STOP_LOSS" (Binance.Common.Order_type.to_string `STOP_LOSS) "STOP_LOSS to string");
  ignore (assert_true (Option.is_some (Binance.Common.Order_type.of_string_opt "LIMIT")) "Parse LIMIT");
  ignore (assert_true (Option.is_some (Binance.Common.Order_type.of_string_opt "MARKET")) "Parse MARKET")

let test_common_time_in_force () =
  printf "\n[Common] Time in force enum\n";
  ignore (assert_string_equal "GTC" (Binance.Common.Time_in_force.to_string `GTC) "GTC to string");
  ignore (assert_string_equal "IOC" (Binance.Common.Time_in_force.to_string `IOC) "IOC to string");
  ignore (assert_string_equal "FOK" (Binance.Common.Time_in_force.to_string `FOK) "FOK to string");
  ignore (assert_true (Option.is_some (Binance.Common.Time_in_force.of_string_opt "GTC")) "Parse GTC")

let test_common_order_status () =
  printf "\n[Common] Order status enum\n";
  ignore (assert_string_equal "NEW" (Binance.Common.Order_status.to_string `NEW) "NEW to string");
  ignore (assert_string_equal "FILLED" (Binance.Common.Order_status.to_string `FILLED) "FILLED to string");
  ignore (assert_string_equal "CANCELED" (Binance.Common.Order_status.to_string `CANCELED) "CANCELED to string");
  ignore (assert_true (Option.is_some (Binance.Common.Order_status.of_string_opt "NEW")) "Parse NEW");
  ignore (assert_true (Option.is_some (Binance.Common.Order_status.of_string_opt "FILLED")) "Parse FILLED")

(* Response Parsing Tests *)
let test_response_parse_success () =
  printf "\n[Response] Parse success response\n";
  let json = Yojson.Safe.from_string {|{"serverTime": 1234567890}|} in
  let parse_server_time json =
    match json with
    | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "serverTime" with
      | Some (`Int time) -> Ok time
      | Some (`Intlit s) -> Ok (Int.of_string s)
      | _ -> Error "Missing serverTime")
    | _ -> Error "Not an object"
  in
  let result = Binance.Rest.Response.parse json parse_server_time in
  match result with
  | `Ok time -> ignore (assert_true (time = 1234567890) "Parsed serverTime correctly")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected success response\n"

let test_response_parse_error () =
  printf "\n[Response] Parse error response\n";
  let json = Yojson.Safe.from_string {|{"code": -1100, "msg": "Invalid parameter"}|} in
  let parse_any _json = Ok () in
  let result = Binance.Rest.Response.parse json parse_any in
  match result with
  | `Api_error err ->
    ignore (assert_true (err.code = -1100) "Error code is -1100");
    ignore (assert_string_equal "Invalid parameter" err.msg "Error message matches")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected error response\n"

let test_response_parse_array () =
  printf "\n[Response] Parse array response\n";
  let json = Yojson.Safe.from_string {|[{"symbol": "BTCUSDT"}, {"symbol": "ETHUSDT"}]|} in
  let parse_symbols json =
    match json with
    | `List items -> Ok (List.length items)
    | _ -> Error "Not a list"
  in
  let result = Binance.Rest.Response.parse json parse_symbols in
  match result with
  | `Ok count -> ignore (assert_true (count = 2) "Parsed array with 2 items")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: Expected array response\n"

(* JSON Deserialization Tests *)
let test_json_server_time () =
  printf "\n[JSON] Server time deserialization\n";
  let json_str = {|{"serverTime": 1499827319559}|} in
  let json = Yojson.Safe.from_string json_str in
  match Binance.V3.Server_time.T.response_of_yojson json with
  | Ok response ->
    ignore (assert_int64_equal 1499827319559L response.serverTime "Server time parsed correctly")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: JSON parse error: %s\n" msg

let test_json_depth () =
  printf "\n[JSON] Depth deserialization\n";
  let json_str = {|{
    "lastUpdateId": 1027024,
    "bids": [["4.00000000", "431.00000000"]],
    "asks": [["4.00000200", "12.00000000"]]
  }|} in
  let json = Yojson.Safe.from_string json_str in
  match Binance.V3.Depth.T.response_of_yojson json with
  | Ok response ->
    ignore (assert_int64_equal 1027024L response.lastUpdateId "Last update ID");
    ignore (assert_true (List.length response.bids = 1) "Has 1 bid");
    ignore (assert_true (List.length response.asks = 1) "Has 1 ask")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: JSON parse error: %s\n" msg

let test_json_ticker () =
  printf "\n[JSON] 24hr ticker deserialization\n";
  let json_str = {|{
    "symbol": "BTCUSDT",
    "priceChange": "100.00",
    "priceChangePercent": "2.5",
    "weightedAvgPrice": "49500.00",
    "prevClosePrice": "49900.00",
    "lastPrice": "50000.00",
    "lastQty": "0.1",
    "bidPrice": "49999.00",
    "bidQty": "1.0",
    "askPrice": "50001.00",
    "askQty": "1.0",
    "openPrice": "49900.00",
    "highPrice": "50100.00",
    "lowPrice": "49800.00",
    "volume": "12345.67",
    "quoteVolume": "123456789.00",
    "openTime": 1499827319559,
    "closeTime": 1499913719559,
    "firstId": 28385,
    "lastId": 28460,
    "count": 76
  }|} in
  let json = Yojson.Safe.from_string json_str in
  match Binance.V3.Ticker_24hr.T.response_of_yojson json with
  | Ok response ->
    ignore (assert_string_equal "BTCUSDT" response.symbol "Ticker symbol");
    ignore (assert_string_equal "50000.00" response.lastPrice "Last price");
    ignore (assert_int64_equal 76L response.count "Trade count")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: JSON parse error: %s\n" msg

let test_json_account () =
  printf "\n[JSON] Account info deserialization\n";
  let json_str = {|{
    "makerCommission": 10,
    "takerCommission": 10,
    "canTrade": true,
    "canWithdraw": true,
    "canDeposit": true,
    "balances": [
      {"asset": "BTC", "free": "1.5", "locked": "0.0"},
      {"asset": "ETH", "free": "10.0", "locked": "0.5"}
    ]
  }|} in
  let json = Yojson.Safe.from_string json_str in
  match Binance.V3.Account.T.response_of_yojson json with
  | Ok response ->
    ignore (assert_true (response.makerCommission = 10) "Maker commission");
    ignore (assert_true response.canTrade "Can trade");
    ignore (assert_true (List.length response.balances = 2) "Has 2 balances")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  ✗ FAIL: JSON parse error: %s\n" msg

(* Main test runner *)
let () =
  printf "===========================================\n";
  printf "Binance Exchange Test Suite\n";
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
  let _ = test_ws_parse_trade () in
  let _ = test_ws_parse_depth_update () in
  let _ = test_ws_parse_depth_snapshot () in

  (* Order Book Update Tests *)
  let _ = test_apply_depth_update () in
  let _ = test_apply_depth_snapshot () in

  (* Stream Tests *)
  let _ = test_stream_names () in

  (* Books Tests *)
  let _ = test_books_empty () in
  let _ = test_books_set_and_get () in
  let _ = test_books_multiple_symbols () in

  (* REST API Tests - Signature *)
  let _ = test_signature_query_string () in
  let _ = test_signature_hmac_format () in
  let _ = test_signature_official_example () in
  let _ = test_signature_deterministic () in

  (* REST API Tests - Common Types *)
  let _ = test_common_side () in
  let _ = test_common_order_type () in
  let _ = test_common_time_in_force () in
  let _ = test_common_order_status () in

  (* REST API Tests - Response Parsing *)
  let _ = test_response_parse_success () in
  let _ = test_response_parse_error () in
  let _ = test_response_parse_array () in

  (* REST API Tests - JSON Deserialization *)
  let _ = test_json_server_time () in
  let _ = test_json_depth () in
  let _ = test_json_ticker () in
  let _ = test_json_account () in

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

(** Comprehensive test suite for MEXC exchange adapter *)

open Core

module Signature = Mexc.Signature
module Order_book = Mexc.Order_book
module Common = Mexc.Common
module Ws = Mexc.Ws

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_string_equal expected actual msg =
  incr tests_run;
  if not (String.equal expected actual) then begin
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  end

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  if Float.(abs (expected - actual) > tolerance) then begin
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  end

let assert_int_equal expected actual msg =
  incr tests_run;
  if not (Int.equal expected actual) then begin
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %d, Got: %d\n" msg expected actual;
    false
  end else begin
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  end

let assert_true condition msg =
  incr tests_run;
  if not condition then begin
    incr tests_failed;
    printf "  X FAIL: %s\n" msg;
    false
  end else begin
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  end

(* ============================================================ *)
(* Signature Tests *)
(* ============================================================ *)

let test_signature_query_string () =
  printf "\n[Signature] Query string building\n";
  let params = [("symbol", "BTCUSDT"); ("side", "BUY"); ("type", "LIMIT")] in
  let query = Signature.build_query_string params in
  ignore (assert_string_equal "symbol=BTCUSDT&side=BUY&type=LIMIT" query
    "Basic query string building");

  let params_with_spaces = [("symbol", "BTC USDT")] in
  let query_encoded = Signature.build_query_string params_with_spaces in
  ignore (assert_string_equal "symbol=BTC%20USDT" query_encoded
    "Query string with URL encoding")

let test_signature_hmac_sha256 () =
  printf "\n[Signature] HMAC-SHA256 signature\n";
  (* Test vector: simple known input *)
  let params = [("timestamp", "1699000000000")] in
  let signature = Signature.sign ~api_secret:"test_secret" ~params in
  (* Verify signature is 64 hex characters (256 bits = 64 hex chars) *)
  ignore (assert_int_equal 64 (String.length signature)
    "Signature is 64 hex characters");
  (* Verify signature is lowercase hex *)
  let is_hex = String.for_all signature ~f:(fun c ->
    Char.is_digit c || (Char.is_alpha c && Char.is_lowercase c)) in
  ignore (assert_true is_hex "Signature is lowercase hex")

let test_signature_deterministic () =
  printf "\n[Signature] Deterministic signatures\n";
  let params = [("symbol", "BTCUSDT"); ("timestamp", "1699000000000")] in
  let sig1 = Signature.sign ~api_secret:"my_secret" ~params in
  let sig2 = Signature.sign ~api_secret:"my_secret" ~params in
  ignore (assert_string_equal sig1 sig2
    "Same inputs produce same signature")

let test_signature_different_secrets () =
  printf "\n[Signature] Different secrets produce different signatures\n";
  let params = [("symbol", "BTCUSDT")] in
  let sig1 = Signature.sign ~api_secret:"secret1" ~params in
  let sig2 = Signature.sign ~api_secret:"secret2" ~params in
  ignore (assert_true (not (String.equal sig1 sig2))
    "Different secrets produce different signatures")

let test_signature_empty_params () =
  printf "\n[Signature] Empty params signature\n";
  let signature = Signature.sign ~api_secret:"secret" ~params:[] in
  ignore (assert_int_equal 64 (String.length signature)
    "Empty params still produces valid signature")

(* ============================================================ *)
(* Common Types Tests *)
(* ============================================================ *)

let test_common_side () =
  printf "\n[Common] Side enum\n";
  ignore (assert_string_equal "BUY" (Common.Side.to_string `BUY)
    "BUY side to string");
  ignore (assert_string_equal "SELL" (Common.Side.to_string `SELL)
    "SELL side to string");
  ignore (assert_true (Option.is_some (Common.Side.of_string_opt "BUY"))
    "Parse BUY string");
  ignore (assert_true (Option.is_some (Common.Side.of_string_opt "SELL"))
    "Parse SELL string");
  ignore (assert_true (Option.is_none (Common.Side.of_string_opt "INVALID"))
    "Invalid side returns None")

let test_common_order_type () =
  printf "\n[Common] Order type enum\n";
  ignore (assert_string_equal "LIMIT" (Common.Order_type.to_string `LIMIT)
    "LIMIT order type to string");
  ignore (assert_string_equal "MARKET" (Common.Order_type.to_string `MARKET)
    "MARKET order type to string");
  ignore (assert_string_equal "LIMIT_MAKER" (Common.Order_type.to_string `LIMIT_MAKER)
    "LIMIT_MAKER order type to string");
  ignore (assert_true (Option.is_some (Common.Order_type.of_string_opt "LIMIT"))
    "Parse LIMIT string");
  ignore (assert_true (Option.is_some (Common.Order_type.of_string_opt "MARKET"))
    "Parse MARKET string")

let test_common_time_in_force () =
  printf "\n[Common] Time in force enum\n";
  ignore (assert_string_equal "GTC" (Common.Time_in_force.to_string `GTC)
    "GTC to string");
  ignore (assert_string_equal "IOC" (Common.Time_in_force.to_string `IOC)
    "IOC to string");
  ignore (assert_string_equal "FOK" (Common.Time_in_force.to_string `FOK)
    "FOK to string")

(* ============================================================ *)
(* Order Book Tests *)
(* ============================================================ *)

let test_order_book_empty () =
  printf "\n[Order Book] Empty book\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let best_bid = Order_book.Book.best_bid book in
  let best_ask = Order_book.Book.best_ask book in
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
    "Empty book has zero best bid price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_bid)
    "Empty book has zero best bid volume");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best_ask)
    "Empty book has zero best ask price");
  ignore (assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.volume best_ask)
    "Empty book has zero best ask volume")

let test_order_book_add_bid () =
  printf "\n[Order Book] Add bid\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
    "Best bid price is 50000.0");
  ignore (assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid)
    "Best bid volume is 1.5")

let test_order_book_add_ask () =
  printf "\n[Order Book] Add ask\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let best_ask = Order_book.Book.best_ask book in
  ignore (assert_float_equal 51000.0 (Exchange_common.Order_book_base.Price_level.price best_ask)
    "Best ask price is 51000.0");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask)
    "Best ask volume is 2.0")

let test_order_book_multiple_bids () =
  printf "\n[Order Book] Multiple bids - best is highest\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49900.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  ignore (assert_float_equal 50100.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
    "Best bid is highest price (50100.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_bid)
    "Best bid volume is 2.0")

let test_order_book_multiple_asks () =
  printf "\n[Order Book] Multiple asks - best is lowest\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:50900.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51100.0 ~size:1.5 in
  let best_ask = Order_book.Book.best_ask book in
  ignore (assert_float_equal 50900.0 (Exchange_common.Order_book_base.Price_level.price best_ask)
    "Best ask is lowest price (50900.0)");
  ignore (assert_float_equal 2.0 (Exchange_common.Order_book_base.Price_level.volume best_ask)
    "Best ask volume is 2.0")

let test_order_book_remove_level () =
  printf "\n[Order Book] Remove price level with zero size\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50100.0 ~size:0.0 in
  let best_bid = Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
    "After removing 50100, best bid is 50000.0");
  ignore (assert_float_equal 1.0 (Exchange_common.Order_book_base.Price_level.volume best_bid)
    "Best bid volume is 1.0")

let test_order_book_update_level () =
  printf "\n[Order Book] Update existing price level\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:3.5 in
  let best_bid = Order_book.Book.best_bid book in
  ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
    "Price remains 50000.0");
  ignore (assert_float_equal 3.5 (Exchange_common.Order_book_base.Price_level.volume best_bid)
    "Volume updated to 3.5")

let test_order_book_epoch () =
  printf "\n[Order Book] Epoch increments on updates\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let epoch0 = Order_book.Book.epoch book in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let epoch1 = Order_book.Book.epoch book in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:2.0 in
  let epoch2 = Order_book.Book.epoch book in
  ignore (assert_int_equal 0 epoch0 "Initial epoch is 0");
  ignore (assert_int_equal 1 epoch1 "Epoch increments to 1 after first update");
  ignore (assert_int_equal 2 epoch2 "Epoch increments to 2 after second update")

let test_order_book_best_n () =
  printf "\n[Order Book] Get best N levels\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49900.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49800.0 ~size:3.0 in
  let best_3 = Order_book.Book.best_n_bids book ~n:3 () in
  ignore (assert_int_equal 3 (List.length best_3) "Got 3 bids");
  ignore (assert_float_equal 50000.0 (List.nth_exn best_3 0).price "1st bid = 50000");
  ignore (assert_float_equal 49900.0 (List.nth_exn best_3 1).price "2nd bid = 49900");
  ignore (assert_float_equal 49800.0 (List.nth_exn best_3 2).price "3rd bid = 49800")

(* DISABLED: These tests use old API (ask_market_price, bid_market_price, mid_market_price)
   which have been replaced by vwap_buy/vwap_sell

let test_order_book_market_price () =
  printf "\n[Order Book] Market price calculation\n";
  let book = Order_book.Book.create ~symbol:"BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:3.0 in

  (* Buy 0.5 BTC - takes from $51k level *)
  let result = Order_book.Book.ask_market_price book ~volume:0.5 in
  ignore (assert_float_equal 51000.0 result.price "Buy 0.5 BTC @ $51k");
  ignore (assert_float_equal 0.5 result.volume "Filled 0.5 BTC");

  (* Buy 2.5 BTC - takes 1.0@$51k + 1.5@$52k *)
  (* Cost = 1.0*51000 + 1.5*52000 = 51000 + 78000 = 129000 *)
  (* Avg = 129000/2.5 = 51600 *)
  let result = Order_book.Book.ask_market_price book ~volume:2.5 in
  ignore (assert_float_equal 51600.0 result.price "Buy 2.5 BTC avg = $51,600");
  ignore (assert_float_equal 2.5 result.volume "Filled 2.5 BTC")

let test_order_book_bid_market_price () =
  printf "\n[Order Book] Bid market price (selling)\n";
  let book = Order_book.Book.create ~symbol:"BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:48000.0 ~size:3.0 in

  (* Sell 0.5 BTC - takes best bid $50k *)
  let result = Order_book.Book.bid_market_price book ~volume:0.5 in
  ignore (assert_float_equal 50000.0 result.price "Sell 0.5 BTC @ $50k");

  (* Sell 2.5 BTC - takes 1.0@$50k + 1.5@$49k *)
  (* Cost = 1.0*50000 + 1.5*49000 = 50000 + 73500 = 123500 *)
  (* Avg = 123500/2.5 = 49400 *)
  let result = Order_book.Book.bid_market_price book ~volume:2.5 in
  ignore (assert_float_equal 49400.0 result.price "Sell 2.5 BTC avg = $49,400")

let test_order_book_mid_price () =
  printf "\n[Order Book] Mid-market price\n";
  let book = Order_book.Book.create ~symbol:"BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in

  let result = Order_book.Book.mid_market_price book ~volume:0.5 in
  (* Mid = (50000 + 51000) / 2 = 50500 *)
  ignore (assert_float_equal 50500.0 result.price "Mid price = $50,500")
*)

(* DISABLED: Uses old API (quantity_from_notional_bid/ask)
let test_order_book_quantity_conversions () =
  printf "\n[Order Book] Notional to quantity conversions\n";
  let book = Order_book.Book.create ~symbol:"BTCUSDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:5.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:5.0 in

  (* $100k / $50k = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_bid book ~notional:100000.0 in
  ignore (assert_float_equal 2.0 qty "Bid: $100k = 2.0 BTC @ $50k");

  (* $102k / $51k = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_ask book ~notional:102000.0 in
  ignore (assert_float_equal 2.0 qty "Ask: $102k = 2.0 BTC @ $51k")
*)

(* ============================================================ *)
(* Books (Multi-Symbol) Tests *)
(* ============================================================ *)

let test_books_empty () =
  printf "\n[Books] Empty multi-symbol books\n";
  let books = Order_book.Books.empty in
  let symbols = Order_book.Books.symbols books in
  ignore (assert_true (List.is_empty symbols) "Empty books has no symbols")

let test_books_set_and_get () =
  printf "\n[Books] Set and get books\n";
  let books = Order_book.Books.empty in
  let book1 = Order_book.Book.empty "BTCUSDT" in
  let book1 = Order_book.Book.set book1 ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Order_book.Books.set_book books book1 in
  match Order_book.Books.book books "BTCUSDT" with
  | Some book ->
    let best_bid = Order_book.Book.best_bid book in
    ignore (assert_float_equal 50000.0 (Exchange_common.Order_book_base.Price_level.price best_bid)
      "Retrieved book has correct bid")
  | None ->
    incr tests_run;
    incr tests_failed;
    printf "  X FAIL: Book not found\n"

let test_books_multiple_symbols () =
  printf "\n[Books] Multiple symbols\n";
  let books = Order_book.Books.empty in
  let book1 = Order_book.Book.empty "BTCUSDT" in
  let book2 = Order_book.Book.empty "ETHUSDT" in
  let books = Order_book.Books.set_book books book1 in
  let books = Order_book.Books.set_book books book2 in
  let symbols = Order_book.Books.symbols books in
  ignore (assert_int_equal 2 (List.length symbols) "Books contains 2 symbols");
  ignore (assert_true (List.mem symbols "BTCUSDT" ~equal:String.equal) "Contains BTCUSDT");
  ignore (assert_true (List.mem symbols "ETHUSDT" ~equal:String.equal) "Contains ETHUSDT")

(* DISABLED: Uses old API (Order_book.Books.set)
let test_books_update_symbol () =
  printf "\n[Books] Update existing symbol book\n";
  let books = Order_book.Books.empty in
  let books = Order_book.Books.set books ~symbol:"BTCUSDT" ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Order_book.Books.set books ~symbol:"BTCUSDT" ~side:`Ask ~price:51000.0 ~size:2.0 in
  let book = Order_book.Books.book_exn books "BTCUSDT" in
  let best_bid = Order_book.Book.best_bid book in
  let best_ask = Order_book.Book.best_ask book in
  ignore (assert_float_equal 50000.0 best_bid.price "BTC bid = 50000");
  ignore (assert_float_equal 51000.0 best_ask.price "BTC ask = 51000")
*)

(* ============================================================ *)
(* Config Tests *)
(* ============================================================ *)

let test_config_of_string () =
  printf "\n[Config] Configuration loading\n";
  let cfg = Mexc.Cfg.of_string "production" in
  let module Cfg = (val cfg : Mexc.Cfg.S) in
  ignore (assert_string_equal "https://api.mexc.com" Cfg.base_url
    "Production base URL is correct")

(* ============================================================ *)
(* WebSocket Tests *)
(* ============================================================ *)

let test_ws_stream_channels () =
  printf "\n[WebSocket] Stream channel names\n";
  let aggre_deals = Ws.Stream.AggreDeals { symbol = "BTCUSDT"; frequency = Ms100 } in
  ignore (assert_string_equal
    "spot@public.aggre.deals.v3.api@100ms@BTCUSDT"
    (Ws.Stream.to_channel aggre_deals)
    "AggreDeals channel name");

  let aggre_depths = Ws.Stream.AggreDepths { symbol = "ethusdt"; frequency = Ms10 } in
  ignore (assert_string_equal
    "spot@public.aggre.depth.v3.api@10ms@ETHUSDT"
    (Ws.Stream.to_channel aggre_depths)
    "AggreDepths channel name (lowercase converted to uppercase)");

  let limit_depths = Ws.Stream.LimitDepths { symbol = "BTCUSDT"; levels = 20 } in
  ignore (assert_string_equal
    "spot@public.limit.depth.v3.api@BTCUSDT@20"
    (Ws.Stream.to_channel limit_depths)
    "LimitDepths channel name");

  let book_ticker = Ws.Stream.BookTicker "BTCUSDT" in
  ignore (assert_string_equal
    "spot@public.bookTicker.v3.api@BTCUSDT"
    (Ws.Stream.to_channel book_ticker)
    "BookTicker channel name")

let test_ws_subscribe_message () =
  printf "\n[WebSocket] Subscription message\n";
  let streams = [
    Ws.Stream.AggreDeals { symbol = "BTCUSDT"; frequency = Ms100 };
    Ws.Stream.BookTicker "ETHUSDT";
  ] in
  let msg = Ws.Subscribe.subscribe ~id:1 streams in
  let json = Yojson.Safe.from_string msg in
  let method_ = Yojson.Safe.Util.member "method" json |> Yojson.Safe.Util.to_string in
  let id = Yojson.Safe.Util.member "id" json |> Yojson.Safe.Util.to_int in
  let params = Yojson.Safe.Util.member "params" json |> Yojson.Safe.Util.to_list in
  ignore (assert_string_equal "SUBSCRIPTION" method_ "Method is SUBSCRIPTION");
  ignore (assert_int_equal 1 id "ID is 1");
  ignore (assert_int_equal 2 (List.length params) "Has 2 stream params")

let test_ws_unsubscribe_message () =
  printf "\n[WebSocket] Unsubscription message\n";
  let streams = [ Ws.Stream.BookTicker "BTCUSDT" ] in
  let msg = Ws.Subscribe.unsubscribe ~id:2 streams in
  let json = Yojson.Safe.from_string msg in
  let method_ = Yojson.Safe.Util.member "method" json |> Yojson.Safe.Util.to_string in
  ignore (assert_string_equal "UNSUBSCRIPTION" method_ "Method is UNSUBSCRIPTION")

let test_ws_ping_message () =
  printf "\n[WebSocket] Ping message\n";
  let msg = Ws.Subscribe.ping () in
  let json = Yojson.Safe.from_string msg in
  let method_ = Yojson.Safe.Util.member "method" json |> Yojson.Safe.Util.to_string in
  ignore (assert_string_equal "PING" method_ "Method is PING")

let test_ws_parse_json_subscription_ack () =
  printf "\n[WebSocket] Parse subscription ack\n";
  let json_msg = {|{"id":1,"msg":"spot@public.aggre.deals.v3.api@100ms@BTCUSDT"}|} in
  match Ws.parse_message json_msg with
  | Ws.Message.SubscriptionAck { id; msg } ->
    ignore (assert_int_equal 1 id "Subscription ack ID is 1");
    ignore (assert_true (String.is_substring msg ~substring:"BTCUSDT") "Ack contains symbol")
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  X FAIL: Expected SubscriptionAck\n"

let test_ws_protobuf_varint () =
  printf "\n[WebSocket] Protobuf varint decoding\n";
  (* Test simple varint: 0x01 = 1 *)
  let d = Ws.Protobuf.create_decoder "\x01" in
  (match Ws.Protobuf.read_varint d with
   | Some v -> ignore (assert_true (Int64.equal v 1L) "Varint 0x01 = 1")
   | None -> incr tests_run; incr tests_failed; printf "  X FAIL: Failed to read varint\n");

  (* Test multi-byte varint: 0xAC 0x02 = 300 *)
  let d = Ws.Protobuf.create_decoder "\xac\x02" in
  (match Ws.Protobuf.read_varint d with
   | Some v -> ignore (assert_true (Int64.equal v 300L) "Varint 0xAC02 = 300")
   | None -> incr tests_run; incr tests_failed; printf "  X FAIL: Failed to read varint\n")

let test_ws_protobuf_string () =
  printf "\n[WebSocket] Protobuf string decoding\n";
  (* Length-delimited string: length=5, "hello" *)
  let d = Ws.Protobuf.create_decoder "\x05hello" in
  (match Ws.Protobuf.read_string d with
   | Some s -> ignore (assert_string_equal "hello" s "Read string 'hello'")
   | None -> incr tests_run; incr tests_failed; printf "  X FAIL: Failed to read string\n")

let test_ws_protobuf_tag () =
  printf "\n[WebSocket] Protobuf tag decoding\n";
  (* Tag: field 1, wire type 2 (length-delimited) = (1 << 3) | 2 = 0x0A *)
  let d = Ws.Protobuf.create_decoder "\x0a" in
  (match Ws.Protobuf.read_tag d with
   | Some (field, Ws.Protobuf.LengthDelim) ->
     ignore (assert_int_equal 1 field "Field number is 1");
     ignore (assert_true true "Wire type is LengthDelim")
   | Some (field, _) ->
     ignore (assert_int_equal 1 field "Field number is 1");
     incr tests_run; incr tests_failed; printf "  X FAIL: Wrong wire type\n"
   | None -> incr tests_run; incr tests_failed; printf "  X FAIL: Failed to read tag\n")

let test_ws_deal_to_trade () =
  printf "\n[WebSocket] Deal to trade conversion\n";
  let deal = Ws.Message.{
    price = "50000.00";
    quantity = "1.5";
    trade_type = 1;  (* buy *)
    time = 1700000000000L;
  } in
  let trade = Ws.deal_to_trade ~symbol:"BTCUSDT" deal in
  ignore (assert_float_equal 50000.0 trade.price "Trade price is 50000");
  ignore (assert_float_equal 1.5 trade.qty "Trade qty is 1.5");
  ignore (assert_true (Fluxum.Types.Side.equal trade.side Fluxum.Types.Side.Buy) "Trade side is Buy")

let test_ws_apply_depth () =
  printf "\n[WebSocket] Apply depth to order book\n";
  let book = Order_book.Book.empty "BTCUSDT" in
  let depth = Ws.Message.{
    asks = [{ price = "51000.00"; quantity = "1.0" }; { price = "52000.00"; quantity = "2.0" }];
    bids = [{ price = "50000.00"; quantity = "1.5" }; { price = "49000.00"; quantity = "2.5" }];
    event_type = "depth";
    from_version = "1";
    to_version = "2";
  } in
  let book = Ws.apply_depth_to_book book depth in
  let best_bid = Order_book.Book.best_bid book in
  let best_ask = Order_book.Book.best_ask book in
  ignore (assert_float_equal 50000.0 best_bid.price "Best bid is 50000");
  ignore (assert_float_equal 1.5 best_bid.volume "Best bid volume is 1.5");
  ignore (assert_float_equal 51000.0 best_ask.price "Best ask is 51000");
  ignore (assert_float_equal 1.0 best_ask.volume "Best ask volume is 1.0")

(* ============================================================ *)
(* Main Test Runner *)
(* ============================================================ *)

let () =
  printf "===========================================\n";
  printf "MEXC Exchange Test Suite\n";
  printf "===========================================\n";

  (* Signature Tests *)
  test_signature_query_string ();
  test_signature_hmac_sha256 ();
  test_signature_deterministic ();
  test_signature_different_secrets ();
  test_signature_empty_params ();

  (* Common Types Tests *)
  test_common_side ();
  test_common_order_type ();
  test_common_time_in_force ();

  (* Order Book Tests *)
  test_order_book_empty ();
  test_order_book_add_bid ();
  test_order_book_add_ask ();
  test_order_book_multiple_bids ();
  test_order_book_multiple_asks ();
  test_order_book_remove_level ();
  test_order_book_update_level ();
  test_order_book_epoch ();
  test_order_book_best_n ();
  (* test_order_book_market_price (); *)
  (* test_order_book_bid_market_price (); *)
  (* test_order_book_mid_price (); *)
  (* test_order_book_quantity_conversions (); *)

  (* Books Tests *)
  test_books_empty ();
  test_books_set_and_get ();
  test_books_multiple_symbols ();
  (* test_books_update_symbol (); *)

  (* Config Tests *)
  test_config_of_string ();

  (* WebSocket Tests *)
  test_ws_stream_channels ();
  test_ws_subscribe_message ();
  test_ws_unsubscribe_message ();
  test_ws_ping_message ();
  test_ws_parse_json_subscription_ack ();
  test_ws_protobuf_varint ();
  test_ws_protobuf_string ();
  test_ws_protobuf_tag ();
  test_ws_deal_to_trade ();
  test_ws_apply_depth ();

  (* Summary *)
  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "Success rate: %.1f%%\n"
    (if !tests_run > 0 then Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0 else 0.0);
  printf "===========================================\n";

  if !tests_failed > 0 then exit 1

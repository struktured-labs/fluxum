(** Hyperliquid Unit Tests *)

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

let test_assert name condition =
  match condition with true -> pass name | false -> fail name

(* ============================================================ *)
(* Configuration Tests *)
(* ============================================================ *)

let test_cfg () =
  printf "\n[Configuration]\n";

  (* Test mainnet config *)
  test_assert "Mainnet REST URL"
    (String.is_prefix Hyperliquid.Cfg.mainnet.rest_url ~prefix:"https://api.hyperliquid.xyz");

  test_assert "Mainnet WS URL"
    (String.is_prefix Hyperliquid.Cfg.mainnet.ws_url ~prefix:"wss://api.hyperliquid.xyz");

  test_assert "Mainnet has no wallet by default"
    (Option.is_none Hyperliquid.Cfg.mainnet.wallet_address);

  (* Test testnet config *)
  test_assert "Testnet REST URL"
    (String.is_prefix Hyperliquid.Cfg.testnet.rest_url ~prefix:"https://api.hyperliquid-testnet.xyz");

  test_assert "Testnet WS URL"
    (String.is_prefix Hyperliquid.Cfg.testnet.ws_url ~prefix:"wss://api.hyperliquid-testnet.xyz");

  (* Test with_wallet *)
  let cfg = Hyperliquid.Cfg.with_wallet Hyperliquid.Cfg.mainnet "0x1234567890abcdef" in
  test_assert "with_wallet sets address"
    (Option.equal String.equal cfg.wallet_address (Some "0x1234567890abcdef"));

  (* Test production alias *)
  test_assert "Production is mainnet alias"
    (String.equal Hyperliquid.Cfg.production.rest_url Hyperliquid.Cfg.mainnet.rest_url)

(* ============================================================ *)
(* WebSocket Stream Tests *)
(* ============================================================ *)

let test_ws_streams () =
  printf "\n[WebSocket Streams]\n";

  (* Test interval to string *)
  let open Hyperliquid.Ws.Stream in
  test_assert "Interval M1" (String.equal (interval_to_string M1) "1m");
  test_assert "Interval H4" (String.equal (interval_to_string H4) "4h");
  test_assert "Interval D1" (String.equal (interval_to_string D1) "1d");
  test_assert "Interval Month1" (String.equal (interval_to_string Month1) "1M");

  (* Test stream to subscription JSON *)
  let all_mids_sub = to_subscription_json AllMids in
  test_assert "AllMids subscription type"
    (match all_mids_sub with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "type" && Yojson.Safe.equal v (`String "allMids"))
     | _ -> false);

  let trades_sub = to_subscription_json (Trades "BTC") in
  test_assert "Trades subscription has coin"
    (match trades_sub with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "coin" && Yojson.Safe.equal v (`String "BTC"))
     | _ -> false);

  let l2_sub = to_subscription_json (L2Book { coin = "ETH"; n_sig_figs = Some 3; mantissa = None }) in
  test_assert "L2Book subscription has nSigFigs"
    (match l2_sub with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "nSigFigs" && Yojson.Safe.equal v (`Int 3))
     | _ -> false);

  let candle_sub = to_subscription_json (Candle { coin = "SOL"; interval = H1 }) in
  test_assert "Candle subscription has interval"
    (match candle_sub with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "interval" && Yojson.Safe.equal v (`String "1h"))
     | _ -> false);

  (* Test subscribe/unsubscribe messages *)
  let sub_msg = to_subscribe_message (Bbo "AVAX") in
  test_assert "Subscribe message has method"
    (match sub_msg with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "method" && Yojson.Safe.equal v (`String "subscribe"))
     | _ -> false);

  let unsub_msg = to_unsubscribe_message (Bbo "AVAX") in
  test_assert "Unsubscribe message has method"
    (match unsub_msg with
     | `Assoc pairs ->
       List.exists pairs ~f:(fun (k, v) ->
         String.equal k "method" && Yojson.Safe.equal v (`String "unsubscribe"))
     | _ -> false)

(* ============================================================ *)
(* WebSocket Message Parsing Tests *)
(* ============================================================ *)

let test_ws_message_parsing () =
  printf "\n[WebSocket Message Parsing]\n";

  (* Test subscription response parsing *)
  let sub_resp_json = {|{
    "channel": "subscriptionResponse",
    "data": {
      "method": "subscribe",
      "subscription": {"type": "trades", "coin": "BTC"}
    }
  }|} in
  let parsed = Hyperliquid.Ws.parse_message sub_resp_json in
  test_assert "Parse subscription response"
    (match parsed with
     | Hyperliquid.Ws.Message.SubscriptionResponse resp ->
       String.equal resp.method_ "subscribe"
     | _ -> false);

  (* Test allMids parsing *)
  let all_mids_json = {|{
    "channel": "allMids",
    "data": {
      "mids": {
        "BTC": "42000.5",
        "ETH": "2500.25"
      }
    }
  }|} in
  let parsed = Hyperliquid.Ws.parse_message all_mids_json in
  test_assert "Parse allMids message"
    (match parsed with
     | Hyperliquid.Ws.Message.AllMids mids ->
       List.length mids = 2
     | _ -> false);

  (* Test l2Book parsing *)
  let l2_json = {|{
    "channel": "l2Book",
    "data": {
      "coin": "BTC",
      "levels": [
        [{"px": "42000", "sz": "1.5", "n": 3}],
        [{"px": "42001", "sz": "2.0", "n": 2}]
      ],
      "time": 1703500000000
    }
  }|} in
  let parsed = Hyperliquid.Ws.parse_message l2_json in
  test_assert "Parse l2Book message"
    (match parsed with
     | Hyperliquid.Ws.Message.L2Book book ->
       String.equal book.coin "BTC" && List.length book.levels = 2
     | _ -> false);

  (* Test trades parsing *)
  let trades_json = {|{
    "channel": "trades",
    "data": [
      {"coin": "ETH", "side": "B", "px": "2500.0", "sz": "0.5", "time": 1703500000000, "hash": "0xabc"}
    ]
  }|} in
  let parsed = Hyperliquid.Ws.parse_message trades_json in
  test_assert "Parse trades message"
    (match parsed with
     | Hyperliquid.Ws.Message.Trades trades ->
       List.length trades = 1 &&
       (match List.hd trades with
        | Some t -> String.equal t.coin "ETH" && String.equal t.side "B"
        | None -> false)
     | _ -> false);

  (* Test bbo parsing *)
  let bbo_json = {|{
    "channel": "bbo",
    "data": {
      "coin": "SOL",
      "bid": "100.0",
      "bidSz": "50.0",
      "ask": "100.1",
      "askSz": "30.0",
      "time": 1703500000000
    }
  }|} in
  let parsed = Hyperliquid.Ws.parse_message bbo_json in
  test_assert "Parse bbo message"
    (match parsed with
     | Hyperliquid.Ws.Message.Bbo bbo ->
       String.equal bbo.coin "SOL" &&
       Option.equal String.equal bbo.bid (Some "100.0")
     | _ -> false);

  (* Test pong parsing *)
  let pong_json = {|{"channel": "pong"}|} in
  let parsed = Hyperliquid.Ws.parse_message pong_json in
  test_assert "Parse pong message"
    (match parsed with
     | Hyperliquid.Ws.Message.Pong -> true
     | _ -> false);

  (* Test unknown message *)
  let unknown_json = {|{"channel": "unknown", "data": {}}|} in
  let parsed = Hyperliquid.Ws.parse_message unknown_json in
  test_assert "Parse unknown message"
    (match parsed with
     | Hyperliquid.Ws.Message.Unknown _ -> true
     | _ -> false);

  (* Test malformed JSON *)
  let malformed = "not valid json{" in
  let parsed = Hyperliquid.Ws.parse_message malformed in
  test_assert "Parse malformed JSON"
    (match parsed with
     | Hyperliquid.Ws.Message.Unknown _ -> true
     | _ -> false)

(* ============================================================ *)
(* REST Types Tests *)
(* ============================================================ *)

let test_rest_types () =
  printf "\n[REST Types]\n";

  (* Test level parsing *)
  let level_json = `Assoc [("px", `String "42000"); ("sz", `String "1.5"); ("n", `Int 3)] in
  let level_result = Hyperliquid.Rest.Types.level_of_yojson level_json in
  test_assert "Parse level"
    (match level_result with
     | Ok level -> String.equal level.px "42000" && level.n = 3
     | Error _ -> false);

  (* Test all_mids parsing *)
  let mids_json = `Assoc [("BTC", `String "42000"); ("ETH", `String "2500")] in
  let mids_result = Hyperliquid.Rest.Types.all_mids_of_yojson mids_json in
  test_assert "Parse all_mids"
    (match mids_result with
     | Ok mids -> List.length mids = 2
     | Error _ -> false);

  (* Test l2_book parsing *)
  let book_json = `Assoc [
    ("coin", `String "BTC");
    ("levels", `List [
      `List [`Assoc [("px", `String "42000"); ("sz", `String "1.0"); ("n", `Int 1)]];
      `List [`Assoc [("px", `String "42001"); ("sz", `String "2.0"); ("n", `Int 2)]];
    ]);
    ("time", `Int 1703500000000);
  ] in
  let book_result = Hyperliquid.Rest.Types.l2_book_of_yojson book_json in
  test_assert "Parse l2_book"
    (match book_result with
     | Ok book -> String.equal book.coin "BTC" && List.length book.levels = 2
     | Error _ -> false);

  (* Test trade parsing *)
  let trade_json = `Assoc [
    ("coin", `String "ETH");
    ("px", `String "2500");
    ("sz", `String "1.0");
    ("side", `String "B");
    ("time", `Int 1703500000);
    ("hash", `String "0xabc123");
    ("tid", `Int 12345);
  ] in
  let trade_result = Hyperliquid.Rest.Types.trade_of_yojson trade_json in
  test_assert "Parse trade"
    (match trade_result with
     | Ok trade -> String.equal trade.coin "ETH" && String.equal trade.side "B"
     | Error _ -> false);

  (* Test candle parsing *)
  let candle_json = `Assoc [
    ("t", `Int 1703500000);
    ("T", `Int 1703503600);
    ("s", `String "BTC");
    ("i", `String "1h");
    ("o", `String "42000");
    ("c", `String "42100");
    ("h", `String "42200");
    ("l", `String "41900");
    ("v", `String "100.5");
    ("n", `Int 1000);
  ] in
  let candle_result = Hyperliquid.Rest.Types.candle_of_yojson candle_json in
  test_assert "Parse candle"
    (match candle_result with
     | Ok candle -> String.equal candle.s "BTC" && candle.n = 1000
     | Error _ -> false);

  (* Test meta parsing *)
  let meta_json = `Assoc [
    ("universe", `List [
      `Assoc [("name", `String "BTC"); ("szDecimals", `Int 4)];
      `Assoc [("name", `String "ETH"); ("szDecimals", `Int 3)];
    ]);
  ] in
  let meta_result = Hyperliquid.Rest.Types.meta_of_yojson meta_json in
  test_assert "Parse meta"
    (match meta_result with
     | Ok meta -> List.length meta.universe = 2
     | Error _ -> false)

(* ============================================================ *)
(* Order Book Tests *)
(* ============================================================ *)

let test_order_book () =
  printf "\n[Order Book]\n";

  let symbol = Fluxum.Types.Symbol.of_string "BTC" in

  (* Test empty book *)
  let book = Hyperliquid.Order_book.Book.empty symbol in
  test_assert "Empty book has symbol"
    (Fluxum.Types.Symbol.equal (Hyperliquid.Order_book.Book.symbol book) symbol);

  test_assert "Empty book has epoch 0"
    (Hyperliquid.Order_book.Book.epoch book = 0);

  (* Test best_bid on empty book *)
  let best_bid = Hyperliquid.Order_book.Book.best_bid book in
  test_assert "Empty book best_bid is empty"
    (Float.equal best_bid.Exchange_common.Order_book_base.Price_level.price 0.0);

  (* Test best_ask on empty book *)
  let best_ask = Hyperliquid.Order_book.Book.best_ask book in
  test_assert "Empty book best_ask is empty"
    (Float.equal best_ask.Exchange_common.Order_book_base.Price_level.price 0.0);

  (* Test set bid *)
  let book = Hyperliquid.Order_book.Book.set book ~side:`Bid ~price:42000.0 ~size:1.5 in
  let best_bid = Hyperliquid.Order_book.Book.best_bid book in
  test_assert "Set bid updates best_bid"
    (Float.equal best_bid.price 42000.0 && Float.equal best_bid.volume 1.5);

  test_assert "Set bid increments epoch"
    (Hyperliquid.Order_book.Book.epoch book = 1);

  (* Test set ask *)
  let book = Hyperliquid.Order_book.Book.set book ~side:`Ask ~price:42001.0 ~size:2.0 in
  let best_ask = Hyperliquid.Order_book.Book.best_ask book in
  test_assert "Set ask updates best_ask"
    (Float.equal best_ask.price 42001.0 && Float.equal best_ask.volume 2.0);

  (* Test remove level with zero size *)
  let book = Hyperliquid.Order_book.Book.set book ~side:`Bid ~price:42000.0 ~size:0.0 in
  let best_bid = Hyperliquid.Order_book.Book.best_bid book in
  test_assert "Zero size removes level"
    (Float.equal best_bid.price 0.0);

  (* Test multiple levels - bids should be descending *)
  let book = Hyperliquid.Order_book.Book.empty symbol in
  let book = Hyperliquid.Order_book.Book.set book ~side:`Bid ~price:42000.0 ~size:1.0 in
  let book = Hyperliquid.Order_book.Book.set book ~side:`Bid ~price:42100.0 ~size:2.0 in
  let book = Hyperliquid.Order_book.Book.set book ~side:`Bid ~price:41900.0 ~size:0.5 in
  let best_bid = Hyperliquid.Order_book.Book.best_bid book in
  test_assert "Best bid is highest price"
    (Float.equal best_bid.price 42100.0);

  (* Test multiple levels - asks should be ascending *)
  let book = Hyperliquid.Order_book.Book.set book ~side:`Ask ~price:42200.0 ~size:1.0 in
  let book = Hyperliquid.Order_book.Book.set book ~side:`Ask ~price:42150.0 ~size:2.0 in
  let book = Hyperliquid.Order_book.Book.set book ~side:`Ask ~price:42300.0 ~size:0.5 in
  let best_ask = Hyperliquid.Order_book.Book.best_ask book in
  test_assert "Best ask is lowest price"
    (Float.equal best_ask.price 42150.0)

(* ============================================================ *)
(* Books (Multi-Symbol) Tests *)
(* ============================================================ *)

let test_books () =
  printf "\n[Books (Multi-Symbol)]\n";

  let btc = Fluxum.Types.Symbol.of_string "BTC" in
  let eth = Fluxum.Types.Symbol.of_string "ETH" in

  (* Test empty books *)
  let books = Hyperliquid.Order_book.Books.empty in
  test_assert "Empty books has no symbols"
    (List.is_empty (Hyperliquid.Order_book.Books.symbols books));

  (* Test add book *)
  let btc_book = Hyperliquid.Order_book.Book.empty btc in
  let btc_book = Hyperliquid.Order_book.Book.set btc_book ~side:`Bid ~price:42000.0 ~size:1.0 in
  let books = Hyperliquid.Order_book.Books.set_book books btc_book in
  test_assert "Books has one symbol"
    (List.length (Hyperliquid.Order_book.Books.symbols books) = 1);

  (* Test get book *)
  let retrieved = Hyperliquid.Order_book.Books.book books btc in
  test_assert "Get book returns Some"
    (Option.is_some retrieved);

  let retrieved = Hyperliquid.Order_book.Books.book books eth in
  test_assert "Get missing book returns None"
    (Option.is_none retrieved);

  (* Test multiple books *)
  let eth_book = Hyperliquid.Order_book.Book.empty eth in
  let books = Hyperliquid.Order_book.Books.set_book books eth_book in
  test_assert "Books has two symbols"
    (List.length (Hyperliquid.Order_book.Books.symbols books) = 2)

(* ============================================================ *)
(* REST Error Types Tests *)
(* ============================================================ *)

let test_rest_errors () =
  printf "\n[REST Error Types]\n";

  let http_err : Hyperliquid.Rest.Error.t = `Http (500, "Internal Server Error") in
  let sexp = Hyperliquid.Rest.Error.sexp_of_t http_err in
  test_assert "HTTP error sexp"
    (String.is_substring (Sexp.to_string sexp) ~substring:"500");

  let json_err : Hyperliquid.Rest.Error.t = `Json_parse "bad json" in
  let sexp = Hyperliquid.Rest.Error.sexp_of_t json_err in
  test_assert "JSON error sexp"
    (String.is_substring (Sexp.to_string sexp) ~substring:"Json_parse");

  let api_err : Hyperliquid.Rest.Error.t = `Api_error "rate limited" in
  let sexp = Hyperliquid.Rest.Error.sexp_of_t api_err in
  test_assert "API error sexp"
    (String.is_substring (Sexp.to_string sexp) ~substring:"Api_error")

(* ============================================================ *)
(* Normalize Error Path Tests (Phase 1) *)
(* ============================================================ *)

let test_normalize_balance_error_paths () =
  printf "\n[Normalize] Balance error paths\n";

  (* Test malformed balance - invalid float in total *)
  let bad_balance : Hyperliquid.Rest.Types.clearinghouse_state = {
    assetPositions = [];
    marginSummary = {
      accountValue = "not_a_number";  (* Invalid float *)
      totalNtlPos = "0.0";
      totalRawUsd = "0.0";
      totalMarginUsed = "0.0";
    };
    crossMarginSummary = {
      accountValue = "0.0";
      totalNtlPos = "0.0";
      totalRawUsd = "0.0";
      totalMarginUsed = "0.0";
    };
    crossMaintenanceMarginUsed = "0.0";
    withdrawable = "0.0";
  } in
  (match Hyperliquid.Fluxum_adapter.Adapter.Normalize.balance bad_balance with
   | Error _ ->
     printf "  * Rejected invalid balance (bad accountValue)\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject non-numeric accountValue\n";
     incr tests_run; incr tests_failed);

  (* Test valid balance *)
  let good_balance : Hyperliquid.Rest.Types.clearinghouse_state = {
    assetPositions = [];
    marginSummary = {
      accountValue = "1000.5";
      totalNtlPos = "500.0";
      totalRawUsd = "1000.5";
      totalMarginUsed = "50.0";
    };
    crossMarginSummary = {
      accountValue = "1000.5";
      totalNtlPos = "500.0";
      totalRawUsd = "1000.5";
      totalMarginUsed = "50.0";
    };
    crossMaintenanceMarginUsed = "25.0";
    withdrawable = "950.0";
  } in
  (match Hyperliquid.Fluxum_adapter.Adapter.Normalize.balance good_balance with
   | Ok bal ->
     test_assert "Valid balance total" Float.(abs (bal.total - 1000.5) < 0.0001);
     test_assert "Valid balance available" Float.(abs (bal.available - 950.0) < 0.0001);
     test_assert "Currency is USD" (String.equal "USD" bal.currency)
   | Error msg ->
     printf "  X FAIL: Valid balance should succeed: %s\n" msg;
     incr tests_run; incr tests_failed);
  ()

(* Phase 2 Priority 2: Additional error path tests *)

let test_normalize_float_conversion_errors () =
  printf "\n[Normalize] Float conversion errors\n";

  (* Test malformed float *)
  (match Fluxum.Normalize_common.Float_conv.of_string "not_a_float" with
   | Error _ ->
     printf "  * Rejected malformed float string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject malformed float\n";
     incr tests_run; incr tests_failed);

  (* Test infinity *)
  (match Fluxum.Normalize_common.Float_conv.of_string "inf" with
   | Error _ ->
     printf "  * Rejected non-finite float (infinity)\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject non-finite float\n";
     incr tests_run; incr tests_failed);

  (* Test price validation (must be positive) *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "-100.0" with
   | Error _ ->
     printf "  * Rejected negative price\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Price should be positive\n";
     incr tests_run; incr tests_failed);

  (* Test valid price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "50000.5" with
   | Ok f ->
     test_assert "Valid price parsed" Float.(abs (f - 50000.5) < 0.0001);
     ()
   | Error msg ->
     printf "  X FAIL: Valid price should succeed: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test quantity validation (must be non-negative) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "-1.5" with
   | Error _ ->
     printf "  * Rejected negative quantity\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Quantity should be non-negative\n";
     incr tests_run; incr tests_failed);

  (* Test zero quantity (allowed) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "0.0" with
   | Ok f ->
     test_assert "Zero quantity allowed" Float.(abs f < 0.0001);
     ()
   | Error msg ->
     printf "  X FAIL: Zero quantity should be allowed: %s\n" msg;
     incr tests_run; incr tests_failed);

  ()

let test_normalize_edge_cases () =
  printf "\n[Normalize] Edge cases\n";

  (* Test very large number *)
  (match Fluxum.Normalize_common.Float_conv.of_string "999999999999.123456" with
   | Ok f ->
     test_assert "Very large number parsed" Float.(abs (f - 999999999999.123456) < 1.0);
     ()
   | Error msg ->
     printf "  X FAIL: Large number should parse: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test very small number *)
  (match Fluxum.Normalize_common.Float_conv.of_string "0.00000001" with
   | Ok f ->
     test_assert "Very small number parsed" Float.(abs (f - 0.00000001) < 0.000000001);
     ()
   | Error msg ->
     printf "  X FAIL: Small number should parse: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test scientific notation *)
  (match Fluxum.Normalize_common.Float_conv.of_string "1.5e10" with
   | Ok f ->
     test_assert "Scientific notation parsed" Float.(abs (f - 1.5e10) < 1.0);
     ()
   | Error msg ->
     printf "  X FAIL: Scientific notation should parse: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test negative exponent *)
  (match Fluxum.Normalize_common.Float_conv.of_string "1.5e-8" with
   | Ok f ->
     test_assert "Negative exponent parsed" Float.(abs (f - 1.5e-8) < 1e-9);
     ()
   | Error msg ->
     printf "  X FAIL: Negative exponent should parse: %s\n" msg;
     incr tests_run; incr tests_failed);

  ()

(* ============================================================ *)
(* EIP-712 Signing Tests *)
(* ============================================================ *)

let test_keccak256_hashing () =
  printf "\n[Signing] Keccak-256 hashing\n";

  (* Test vector from Ethereum tests *)
  let empty_hash = Hyperliquid.Signing.Keccak.hash_string "" in
  test_assert "Empty string Keccak-256"
    (String.equal empty_hash "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

  (* Test "hello world" *)
  let hello_hash = Hyperliquid.Signing.Keccak.hash_string "hello world" in
  test_assert "Hello world Keccak-256"
    (String.equal hello_hash "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad");

  (* Test "The quick brown fox jumps over the lazy dog" *)
  let fox_hash = Hyperliquid.Signing.Keccak.hash_string "The quick brown fox jumps over the lazy dog" in
  test_assert "Fox sentence Keccak-256"
    (String.equal fox_hash "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15");

  (* Test bytes hashing *)
  let test_bytes = Bytes.of_string "test" in
  let bytes_hash = Hyperliquid.Signing.Keccak.hash_bytes test_bytes in
  test_assert "Bytes hashing works"
    (String.equal bytes_hash "9c22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658")

let test_eip712_domain_separator () =
  printf "\n[Signing] EIP-712 Domain Separator\n";

  (* Test Hyperliquid L1 domain separator *)
  let domain_sep = Hyperliquid.Signing.Domain.domain_separator
    Hyperliquid.Signing.Domain.hyperliquid_l1 in

  (* Domain separator should be 64 hex characters (32 bytes) *)
  test_assert "Domain separator is 64 hex chars"
    (String.length domain_sep = 64);

  (* Should be deterministic *)
  let domain_sep2 = Hyperliquid.Signing.Domain.domain_separator
    Hyperliquid.Signing.Domain.hyperliquid_l1 in
  test_assert "Domain separator is deterministic"
    (String.equal domain_sep domain_sep2);

  (* Test custom domain *)
  let custom_domain : Hyperliquid.Signing.Domain.t = {
    name = "Test";
    version = "1";
    chain_id = 1;
    verifying_contract = "0x1111111111111111111111111111111111111111";
  } in
  let custom_sep = Hyperliquid.Signing.Domain.domain_separator custom_domain in
  test_assert "Custom domain separator differs from Hyperliquid"
    (not (String.equal domain_sep custom_sep))

let test_address_normalization () =
  printf "\n[Signing] Address normalization\n";

  (* Test lowercase address *)
  let addr1 = Hyperliquid.Signing.Address.normalize "0xabcdef1234567890abcdef1234567890abcdef12" in
  test_assert "Lowercase address preserved"
    (String.equal addr1 "0xabcdef1234567890abcdef1234567890abcdef12");

  (* Test uppercase address *)
  let addr2 = Hyperliquid.Signing.Address.normalize "0xABCDEF1234567890ABCDEF1234567890ABCDEF12" in
  test_assert "Uppercase address lowercased"
    (String.equal addr2 "0xabcdef1234567890abcdef1234567890abcdef12");

  (* Test address without 0x prefix *)
  let addr3 = Hyperliquid.Signing.Address.normalize "abcdef1234567890abcdef1234567890abcdef12" in
  test_assert "Address without 0x gets prefix"
    (String.equal addr3 "0xabcdef1234567890abcdef1234567890abcdef12")

let test_address_derivation_from_public_key () =
  printf "\n[Signing] Address derivation from public key\n";

  (* Test with known private key -> address mapping *)
  (* Private key: 0x0000000000000000000000000000000000000000000000000000000000000001 *)
  (* Expected address: 0x7e5f4552091a69125d5dfcb7b8c2659029395bdf *)
  let ctx = Secp256k1.Context.create [Sign; Verify] in

  (* Create private key from bytes *)
  let privkey_hex = "0000000000000000000000000000000000000000000000000000000000000001" in
  let privkey_bytes = Hex.to_string (`Hex privkey_hex) |> Bytes.of_string in
  let privkey_buf = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout 32 in
  for i = 0 to 31 do
    Bigarray.Array1.set privkey_buf i (Bytes.get privkey_bytes i)
  done;

  (try
    let privkey = Secp256k1.Key.read_sk_exn ctx privkey_buf in
    let pubkey = Secp256k1.Key.neuterize_exn ctx privkey in
    let derived_addr = Hyperliquid.Signing.Address.from_public_key ctx pubkey in

    (* Known Ethereum address for private key 0x01 *)
    let expected_addr = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf" in
    test_assert "Derived address matches expected"
      (String.equal (String.lowercase derived_addr) expected_addr)
  with e ->
    printf "  X FAIL: Address derivation failed: %s\n" (Exn.to_string e);
    incr tests_run; incr tests_failed)

let test_msgpack_order_serialization () =
  printf "\n[Signing] MessagePack order serialization\n";

  (* Test single order serialization *)
  let order : Hyperliquid.Signing.order_request = {
    asset = 0;
    is_buy = true;
    limit_px = "42000.0";
    sz = "1.5";
    reduce_only = false;
    time_in_force = "Gtc";
    cloid = None;
  } in

  let msgpack_bytes = Hyperliquid.Signing.Msgpack.serialize_order_action
    ~orders:[order] ~grouping:"na" in

  (* Msgpack should produce non-empty bytes *)
  test_assert "Msgpack serialization produces bytes"
    (Bytes.length msgpack_bytes > 0);

  (* Test with client order ID *)
  let order_with_cloid : Hyperliquid.Signing.order_request = {
    asset = 1;
    is_buy = false;
    limit_px = "2500.0";
    sz = "10.0";
    reduce_only = true;
    time_in_force = "Ioc";
    cloid = Some "test-order-123";
  } in

  let msgpack_with_cloid = Hyperliquid.Signing.Msgpack.serialize_order_action
    ~orders:[order_with_cloid] ~grouping:"na" in

  test_assert "Msgpack with cloid is different"
    (not (Bytes.equal msgpack_bytes msgpack_with_cloid));

  (* Test multiple orders *)
  let multi_orders = Hyperliquid.Signing.Msgpack.serialize_order_action
    ~orders:[order; order_with_cloid] ~grouping:"na" in

  test_assert "Multiple orders produce larger msgpack"
    (Bytes.length multi_orders > Bytes.length msgpack_bytes)

let test_msgpack_cancel_serialization () =
  printf "\n[Signing] MessagePack cancel serialization\n";

  (* Test cancel request *)
  let cancel : Hyperliquid.Signing.cancel_request = {
    asset = 0;
    oid = 12345L;
  } in

  let msgpack_bytes = Hyperliquid.Signing.Msgpack.serialize_cancel_action
    ~cancels:[cancel] in

  test_assert "Cancel msgpack produces bytes"
    (Bytes.length msgpack_bytes > 0);

  (* Test multiple cancels *)
  let cancel2 : Hyperliquid.Signing.cancel_request = {
    asset = 1;
    oid = 67890L;
  } in

  let multi_cancel = Hyperliquid.Signing.Msgpack.serialize_cancel_action
    ~cancels:[cancel; cancel2] in

  test_assert "Multiple cancels produce larger msgpack"
    (Bytes.length multi_cancel > Bytes.length msgpack_bytes)

let test_phantom_agent_construction () =
  printf "\n[Signing] Phantom agent construction\n";

  (* Test phantom agent hash generation *)
  let connection_id = "0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb0" in  (* Valid 40-char address *)
  let action_hash = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890" in  (* 64 chars = 32 bytes *)

  let phantom_hash = Hyperliquid.Signing.PhantomAgent.construct
    ~connection_id ~action_hash in

  (* Should produce 64 hex character hash *)
  test_assert "Phantom agent hash is 64 hex chars"
    (String.length phantom_hash = 64);

  (* Should be deterministic *)
  let phantom_hash2 = Hyperliquid.Signing.PhantomAgent.construct
    ~connection_id ~action_hash in
  test_assert "Phantom agent hash is deterministic"
    (String.equal phantom_hash phantom_hash2);

  (* Different connection ID should produce different hash *)
  let phantom_hash3 = Hyperliquid.Signing.PhantomAgent.construct
    ~connection_id:"0x0000000000000000000000000000000000000000" ~action_hash in
  test_assert "Different connection ID changes phantom hash"
    (not (String.equal phantom_hash phantom_hash3))

let test_eip712_digest_creation () =
  printf "\n[Signing] EIP-712 digest creation\n";

  (* Test digest creation with known values *)
  let domain_sep = "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef" in
  let struct_hash = "fedcba0987654321fedcba0987654321fedcba0987654321fedcba0987654321" in

  let digest = Hyperliquid.Signing.Signature.create_digest
    ~domain_separator:domain_sep ~struct_hash in

  (* Digest should be 64 hex characters *)
  test_assert "EIP-712 digest is 64 hex chars"
    (String.length digest = 64);

  (* Should be deterministic *)
  let digest2 = Hyperliquid.Signing.Signature.create_digest
    ~domain_separator:domain_sep ~struct_hash in
  test_assert "EIP-712 digest is deterministic"
    (String.equal digest digest2);

  (* Different struct hash should produce different digest *)
  let digest3 = Hyperliquid.Signing.Signature.create_digest
    ~domain_separator:domain_sep
    ~struct_hash:"0000000000000000000000000000000000000000000000000000000000000000" in
  test_assert "Different struct hash changes digest"
    (not (String.equal digest digest3))

let test_signature_generation () =
  printf "\n[Signing] Signature generation\n";

  (* Test with a known private key *)
  let private_key = "0000000000000000000000000000000000000000000000000000000000000001" in
  let test_digest = "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890" in

  (match Hyperliquid.Signing.Signature.sign ~private_key_hex:private_key ~digest_hex:test_digest with
   | Ok signature ->
     (* Ethereum signature is 65 bytes (130 hex chars): r (32) + s (32) + v (1) *)
     test_assert "Signature is 130 hex chars (65 bytes)"
       (String.length signature = 130);

     (* v should be 27 or 28 (last byte) *)
     let v_hex = String.suffix signature 2 in
     let v_int = int_of_string ("0x" ^ v_hex) in
     test_assert "Recovery ID v is 27 or 28"
       (v_int = 27 || v_int = 28);

     (* Signature should be deterministic (RFC 6979) *)
     (match Hyperliquid.Signing.Signature.sign ~private_key_hex:private_key ~digest_hex:test_digest with
      | Ok signature2 ->
        test_assert "Signature is deterministic"
          (String.equal signature signature2)
      | Error _ ->
        printf "  X FAIL: Second signature failed\n";
        incr tests_run; incr tests_failed)

   | Error msg ->
     printf "  X FAIL: Signature generation failed: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_signature_error_handling () =
  printf "\n[Signing] Signature error handling\n";

  (* Test with invalid private key (wrong length) *)
  (match Hyperliquid.Signing.Signature.sign
    ~private_key_hex:"0001"
    ~digest_hex:"abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890" with
   | Error _ ->
     printf "  * Rejected invalid private key length\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject invalid private key\n";
     incr tests_run; incr tests_failed);

  (* Test with invalid digest (wrong length) *)
  (match Hyperliquid.Signing.Signature.sign
    ~private_key_hex:"0000000000000000000000000000000000000000000000000000000000000001"
    ~digest_hex:"abc" with
   | Error _ ->
     printf "  * Rejected invalid digest length\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject invalid digest\n";
     incr tests_run; incr tests_failed);

  (* Test with all zeros private key (invalid on secp256k1 curve) *)
  (match Hyperliquid.Signing.Signature.sign
    ~private_key_hex:"0000000000000000000000000000000000000000000000000000000000000000"
    ~digest_hex:"abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890" with
   | Error _ ->
     printf "  * Rejected zero private key\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  X FAIL: Should reject zero private key\n";
     incr tests_run; incr tests_failed)

let test_full_order_signing_flow () =
  printf "\n[Signing] Full order signing flow\n";

  (* Test complete signing flow *)
  let private_key = "0000000000000000000000000000000000000000000000000000000000000001" in
  let order : Hyperliquid.Signing.order_request = {
    asset = 0;
    is_buy = true;
    limit_px = "42000.0";
    sz = "0.001";
    reduce_only = false;
    time_in_force = "Gtc";
    cloid = None;
  } in

  (match Hyperliquid.Signing.sign_place_order
    ~private_key
    ~orders:[order]
    ~grouping:"na"
    ~nonce:123456789L with
   | Ok signature ->
     test_assert "Full signing flow produces valid signature"
       (String.length signature = 130);

     (* Should be deterministic *)
     (match Hyperliquid.Signing.sign_place_order
        ~private_key
        ~orders:[order]
        ~grouping:"na"
        ~nonce:123456789L with
      | Ok signature2 ->
        test_assert "Full signing flow is deterministic"
          (String.equal signature signature2)
      | Error msg ->
        printf "  X FAIL: Second signing failed: %s\n" msg;
        incr tests_run; incr tests_failed)

   | Error msg ->
     printf "  X FAIL: Full signing flow failed: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_full_cancel_signing_flow () =
  printf "\n[Signing] Full cancel signing flow\n";

  let private_key = "0000000000000000000000000000000000000000000000000000000000000001" in
  let cancel : Hyperliquid.Signing.cancel_request = {
    asset = 0;
    oid = 12345L;
  } in

  (match Hyperliquid.Signing.sign_cancel_order
    ~private_key
    ~cancels:[cancel]
    ~nonce:123456789L with
   | Ok signature ->
     test_assert "Cancel signing produces valid signature"
       (String.length signature = 130)
   | Error msg ->
     printf "  X FAIL: Cancel signing failed: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_signing_with_multiple_orders () =
  printf "\n[Signing] Signing with multiple orders\n";

  let private_key = "0000000000000000000000000000000000000000000000000000000000000001" in
  let order1 : Hyperliquid.Signing.order_request = {
    asset = 0;
    is_buy = true;
    limit_px = "42000.0";
    sz = "1.0";
    reduce_only = false;
    time_in_force = "Gtc";
    cloid = None;
  } in
  let order2 : Hyperliquid.Signing.order_request = {
    asset = 1;
    is_buy = false;
    limit_px = "2500.0";
    sz = "5.0";
    reduce_only = false;
    time_in_force = "Ioc";
    cloid = Some "test-123";
  } in

  (match Hyperliquid.Signing.sign_place_order
    ~private_key
    ~orders:[order1; order2]
    ~grouping:"na"
    ~nonce:123456789L with
   | Ok signature ->
     test_assert "Multiple orders signing succeeds"
       (String.length signature = 130)
   | Error msg ->
     printf "  X FAIL: Multiple orders signing failed: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_withdraw_signing () =
  printf "\n[Signing] Withdraw3 action signing\n";

  let private_key = "0000000000000000000000000000000000000000000000000000000000000001" in
  let withdraw_req : Hyperliquid.Signing.withdraw_request = {
    hyperliquid_chain = "Mainnet";
    signature_chain_id = "0xa4b1";  (* Arbitrum *)
    destination = "0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb0";
    amount = "100.0";
    time = 1716531066415L;
  } in

  (match Hyperliquid.Signing.sign_withdraw ~private_key ~req:withdraw_req with
   | Ok signature ->
     test_assert "Withdraw signing produces valid signature"
       (String.length signature = 130);

     (* Should be deterministic *)
     (match Hyperliquid.Signing.sign_withdraw ~private_key ~req:withdraw_req with
      | Ok signature2 ->
        test_assert "Withdraw signing is deterministic"
          (String.equal signature signature2)
      | Error msg ->
        printf "  X FAIL: Second withdraw signing failed: %s\n" msg;
        incr tests_run; incr tests_failed)
   | Error msg ->
     printf "  X FAIL: Withdraw signing failed: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test with testnet config *)
  let withdraw_testnet : Hyperliquid.Signing.withdraw_request = {
    hyperliquid_chain = "Testnet";
    signature_chain_id = "0xa4b1";
    destination = "0x0000000000000000000000000000000000000000";
    amount = "1.0";
    time = 1716531066415L;
  } in

  (match Hyperliquid.Signing.sign_withdraw ~private_key ~req:withdraw_testnet with
   | Ok signature ->
     test_assert "Testnet withdraw signing succeeds"
       (String.length signature = 130)
   | Error msg ->
     printf "  X FAIL: Testnet withdraw signing failed: %s\n" msg;
     incr tests_run; incr tests_failed)

let test_msgpack_withdraw_serialization () =
  printf "\n[Signing] MessagePack withdraw serialization\n";

  let withdraw_req : Hyperliquid.Signing.withdraw_request = {
    hyperliquid_chain = "Mainnet";
    signature_chain_id = "0xa4b1";
    destination = "0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb0";
    amount = "100.0";
    time = 1716531066415L;
  } in

  let msgpack_bytes = Hyperliquid.Signing.Msgpack.serialize_withdraw_action ~req:withdraw_req in

  test_assert "Withdraw msgpack produces bytes"
    (Bytes.length msgpack_bytes > 0);

  (* Different amounts should produce different msgpack *)
  let withdraw_req2 : Hyperliquid.Signing.withdraw_request = {
    hyperliquid_chain = "Mainnet";
    signature_chain_id = "0xa4b1";
    destination = "0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb0";
    amount = "200.0";
    time = 1716531066415L;
  } in

  let msgpack_bytes2 = Hyperliquid.Signing.Msgpack.serialize_withdraw_action ~req:withdraw_req2 in

  test_assert "Different amounts produce different msgpack"
    (not (Bytes.equal msgpack_bytes msgpack_bytes2))

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let () =
  printf "===========================================\n";
  printf "Hyperliquid Unit Tests\n";
  printf "===========================================\n";

  test_cfg ();
  test_ws_streams ();
  test_ws_message_parsing ();
  test_rest_types ();
  test_order_book ();
  test_books ();
  test_rest_errors ();

  (* Phase 1 normalize error path tests *)
  test_normalize_balance_error_paths ();

  (* Phase 2 Priority 2: Additional error path tests *)
  test_normalize_float_conversion_errors ();
  test_normalize_edge_cases ();

  (* EIP-712 Signing Tests *)
  test_keccak256_hashing ();
  test_eip712_domain_separator ();
  test_address_normalization ();
  test_address_derivation_from_public_key ();
  test_msgpack_order_serialization ();
  test_msgpack_cancel_serialization ();
  test_msgpack_withdraw_serialization ();
  test_phantom_agent_construction ();
  test_eip712_digest_creation ();
  test_signature_generation ();
  test_signature_error_handling ();
  test_full_order_signing_flow ();
  test_full_cancel_signing_flow ();
  test_signing_with_multiple_orders ();
  test_withdraw_signing ();

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

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
  if condition then pass name else fail name

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
    (Float.equal best_bid.Fluxum.Order_book_intf.Price_level.price 0.0);

  (* Test best_ask on empty book *)
  let best_ask = Hyperliquid.Order_book.Book.best_ask book in
  test_assert "Empty book best_ask is empty"
    (Float.equal best_ask.Fluxum.Order_book_intf.Price_level.price 0.0);

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

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  if !tests_failed > 0 then exit 1

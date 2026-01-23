(** KuCoin Unit Tests *)

open Core

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

  (* Test production config *)
  test_assert "Production REST URL"
    (String.is_prefix Kucoin.Cfg.production.rest_url ~prefix:"https://api.kucoin.com");

  test_assert "Production WS URL"
    (String.is_prefix Kucoin.Cfg.production.ws_url ~prefix:"wss://ws-api.kucoin.com");

  test_assert "Production has no API key by default"
    (Option.is_none Kucoin.Cfg.production.api_key);

  test_assert "Production default API version is 2"
    (Kucoin.Cfg.production.api_version = 2);

  (* Test with_auth *)
  let cfg = Kucoin.Cfg.with_auth
    ~api_key:"test_key"
    ~api_secret:"test_secret"
    ~passphrase:"test_pass"
    ~api_version:3
    Kucoin.Cfg.production
  in
  test_assert "with_auth sets API key"
    (Option.equal String.equal cfg.api_key (Some "test_key"));
  test_assert "with_auth sets API secret"
    (Option.equal String.equal cfg.api_secret (Some "test_secret"));
  test_assert "with_auth sets passphrase"
    (Option.equal String.equal cfg.passphrase (Some "test_pass"));
  test_assert "with_auth sets API version"
    (cfg.api_version = 3);

  (* Test futures config *)
  test_assert "Futures has different REST URL"
    (String.is_substring Kucoin.Cfg.futures.rest_url ~substring:"futures");

  (* Test mainnet alias *)
  test_assert "Mainnet is production alias"
    (String.equal Kucoin.Cfg.mainnet.rest_url Kucoin.Cfg.production.rest_url)

(* ============================================================ *)
(* Types Tests *)
(* ============================================================ *)

let test_types () =
  printf "\n[Types]\n";

  (* Test ticker parsing *)
  let ticker_json = `Assoc [
    ("symbol", `String "BTC-USDT");
    ("high", `String "50000.00");
    ("low", `String "48000.00");
    ("last", `String "49500.00");
    ("vol", `String "1234.56");
    ("volValue", `String "61234567.89");
    ("buy", `String "49450.00");
    ("sell", `String "49550.00");
    ("changePrice", `String "500.00");
    ("changeRate", `String "0.0101");
    ("averagePrice", `String "49250.00");
    ("time", `Int 1699000000000);
  ] in
  (match Kucoin.Types.ticker_of_yojson ticker_json with
   | Ok ticker ->
     test_assert "Ticker parses correctly" true;
     test_assert "Ticker symbol" (String.equal ticker.symbol "BTC-USDT");
     test_assert "Ticker last price" (String.equal ticker.last "49500.00");
     test_assert "Ticker buy" (String.equal ticker.buy "49450.00");
     test_assert "Ticker sell" (String.equal ticker.sell "49550.00")
   | Error _ -> fail "Ticker parsing failed");

  (* Test order book entry parsing *)
  let entry_json = `List [`String "50000.0"; `String "1.5"] in
  (match Kucoin.Types.order_book_entry_of_yojson entry_json with
   | Ok (price, size) ->
     test_assert "Order book entry parses" true;
     test_assert "Entry price" (String.equal price "50000.0");
     test_assert "Entry size" (String.equal size "1.5")
   | Error _ -> fail "Order book entry parsing failed");

  (* Test order book parsing *)
  let book_json = `Assoc [
    ("sequence", `Int 1699000000);
    ("time", `Int 1699000000000);
    ("bids", `List [
      `List [`String "50000.0"; `String "1.5"];
      `List [`String "49999.0"; `String "2.0"];
    ]);
    ("asks", `List [
      `List [`String "50001.0"; `String "1.0"];
      `List [`String "50002.0"; `String "0.5"];
    ]);
  ] in
  (match Kucoin.Types.order_book_of_yojson book_json with
   | Ok book ->
     test_assert "Order book parses" true;
     test_assert "Order book has 2 bids" (List.length book.bids = 2);
     test_assert "Order book has 2 asks" (List.length book.asks = 2)
   | Error _ -> fail "Order book parsing failed");

  (* Test trade parsing *)
  let trade_json = `Assoc [
    ("sequence", `String "123456");
    ("price", `String "50000.0");
    ("size", `String "0.5");
    ("side", `String "buy");
    ("time", `Int 1699000000000000000);
  ] in
  (match Kucoin.Types.trade_of_yojson trade_json with
   | Ok trade ->
     test_assert "Trade parses" true;
     test_assert "Trade price" (String.equal trade.price "50000.0");
     test_assert "Trade size" (String.equal trade.size "0.5");
     test_assert "Trade side" (String.equal trade.side "buy")
   | Error _ -> fail "Trade parsing failed");

  (* Test balance parsing *)
  let balance_json = `Assoc [
    ("id", `String "acc123");
    ("currency", `String "BTC");
    ("type", `String "trade");
    ("balance", `String "10.0");
    ("available", `String "9.5");
    ("holds", `String "0.5");
  ] in
  (match Kucoin.Types.balance_of_yojson balance_json with
   | Ok balance ->
     test_assert "Balance parses" true;
     test_assert "Balance currency" (String.equal balance.currency "BTC");
     test_assert "Balance total" (String.equal balance.balance "10.0");
     test_assert "Balance available" (String.equal balance.available "9.5")
   | Error _ -> fail "Balance parsing failed")

(* ============================================================ *)
(* REST Signature Tests *)
(* ============================================================ *)

let test_rest_signature () =
  printf "\n[REST Signature]\n";

  (* Test timestamp generation *)
  let ts1 = Kucoin.Rest.generate_timestamp () in
  let ts2 = Kucoin.Rest.generate_timestamp () in
  test_assert "Timestamp is numeric string"
    (String.for_all ts1 ~f:Char.is_digit);
  test_assert "Timestamps increase or equal"
    (Int.of_string ts2 >= Int.of_string ts1);

  (* Test signature creation *)
  let signature = Kucoin.Rest.create_signature
    ~api_secret:"test_secret"
    ~message:"1699000000GET/api/v1/accounts"
  in
  test_assert "Signature is not empty"
    (String.length signature > 0);

  (* Test passphrase encryption *)
  let encrypted = Kucoin.Rest.encrypt_passphrase
    ~api_secret:"test_secret"
    ~passphrase:"test_pass"
  in
  test_assert "Encrypted passphrase is not empty"
    (String.length encrypted > 0);
  test_assert "Encrypted passphrase differs from plaintext"
    (not (String.equal encrypted "test_pass"));

  (* Test signature determinism *)
  let sig1 = Kucoin.Rest.create_signature
    ~api_secret:"secret"
    ~message:"test_message"
  in
  let sig2 = Kucoin.Rest.create_signature
    ~api_secret:"secret"
    ~message:"test_message"
  in
  test_assert "Signatures are deterministic"
    (String.equal sig1 sig2)

(* ============================================================ *)
(* Error Type Tests *)
(* ============================================================ *)

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Kucoin.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Kucoin.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404");

  let json_err : Kucoin.Rest.Error.t = `Json_parse "bad json" in
  let err_str = Kucoin.Rest.Error.to_string json_err in
  test_assert "JSON error to_string"
    (String.is_substring err_str ~substring:"JSON");

  let api_err : Kucoin.Rest.Error.t = `Api_error "rate limited" in
  let err_str = Kucoin.Rest.Error.to_string api_err in
  test_assert "API error to_string"
    (String.is_substring err_str ~substring:"API")

(* ============================================================ *)
(* Symbol Info Tests *)
(* ============================================================ *)

let test_symbol_info () =
  printf "\n[Symbol Info]\n";

  let symbol_json = `Assoc [
    ("symbol", `String "BTC-USDT");
    ("name", `String "BTC-USDT");
    ("baseCurrency", `String "BTC");
    ("quoteCurrency", `String "USDT");
    ("baseMinSize", `String "0.00001");
    ("quoteMinSize", `String "0.1");
    ("baseMaxSize", `String "10000");
    ("quoteMaxSize", `String "100000000");
    ("baseIncrement", `String "0.00000001");
    ("quoteIncrement", `String "0.001");
    ("priceIncrement", `String "0.1");
    ("enableTrading", `Bool true);
  ] in
  (match Kucoin.Types.symbol_info_of_yojson symbol_json with
   | Ok info ->
     test_assert "Symbol info parses" true;
     test_assert "Symbol name" (String.equal info.symbol "BTC-USDT");
     test_assert "Base currency" (String.equal info.baseCurrency "BTC");
     test_assert "Quote currency" (String.equal info.quoteCurrency "USDT");
     test_assert "Trading enabled" info.enableTrading
   | Error _ -> fail "Symbol info parsing failed")

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let () =
  printf "===========================================\n";
  printf "KuCoin Unit Tests\n";
  printf "===========================================\n";

  test_cfg ();
  test_types ();
  test_rest_signature ();
  test_error_types ();
  test_symbol_info ();

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

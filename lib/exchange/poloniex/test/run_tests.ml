(** Poloniex Unit Tests *)

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

let test_cfg () =
  printf "\n[Configuration]\n";

  test_assert "Production REST URL"
    (String.is_prefix Poloniex.Cfg.production.rest_url ~prefix:"https://api.poloniex.com");

  test_assert "Production has no API key"
    (Option.is_none Poloniex.Cfg.production.api_key);

  let cfg = Poloniex.Cfg.with_auth
    ~api_key:"test_key"
    ~api_secret:"test_secret"
    Poloniex.Cfg.production
  in
  test_assert "with_auth sets API key"
    (Option.equal String.equal cfg.api_key (Some "test_key"));
  test_assert "with_auth sets API secret"
    (Option.equal String.equal cfg.api_secret (Some "test_secret"))

let test_types () =
  printf "\n[Types]\n";

  let ticker_json = `Assoc [
    ("symbol", `String "BTC_USDT");
    ("high", `String "51000");
    ("low", `String "48000");
    ("close", `String "50000");
    ("quantity", `String "1234.5");
    ("amount", `String "62000000");
    ("open", `String "49000");
    ("bid", `String "49950");
    ("ask", `String "50050");
  ] in
  (match Poloniex.Types.ticker_of_yojson ticker_json with
   | Ok ticker ->
     test_assert "Ticker parses" true;
     test_assert "Ticker symbol" (String.equal ticker.symbol "BTC_USDT");
     test_assert "Ticker close" (String.equal ticker.close "50000")
   | Error _ -> fail "Ticker parsing failed");

  let entry_json = `List [`String "50000"; `String "1.5"] in
  (match Poloniex.Types.order_book_entry_of_yojson entry_json with
   | Ok (price, qty) ->
     test_assert "Order book entry parses" true;
     test_assert "Entry price" (String.equal price "50000");
     test_assert "Entry qty" (String.equal qty "1.5")
   | Error _ -> fail "Order book entry failed");

  let book_json = `Assoc [
    ("time", `Int 1699000000);
    ("scale", `String "0.01");
    ("bids", `List [`List [`String "49999"; `String "2.0"]]);
    ("asks", `List [`List [`String "50001"; `String "1.0"]]);
  ] in
  (match Poloniex.Types.order_book_of_yojson book_json with
   | Ok book ->
     test_assert "Order book parses" true;
     test_assert "Book has bids" (List.length book.bids = 1);
     test_assert "Book has asks" (List.length book.asks = 1)
   | Error _ -> fail "Order book parsing failed");

  let trade_json = `Assoc [
    ("id", `String "123456");
    ("price", `String "50000");
    ("quantity", `String "0.5");
    ("takerSide", `String "buy");
    ("ts", `Int 1699000000);
  ] in
  (match Poloniex.Types.trade_of_yojson trade_json with
   | Ok trade ->
     test_assert "Trade parses" true;
     test_assert "Trade price" (String.equal trade.price "50000");
     test_assert "Trade side" (String.equal trade.takerSide "buy")
   | Error _ -> fail "Trade parsing failed");

  let balance_json = `Assoc [
    ("currency", `String "BTC");
    ("available", `String "10.5");
    ("hold", `String "0.5");
  ] in
  (match Poloniex.Types.balance_of_yojson balance_json with
   | Ok balance ->
     test_assert "Balance parses" true;
     test_assert "Balance currency" (String.equal balance.currency "BTC");
     test_assert "Balance available" (String.equal balance.available "10.5")
   | Error _ -> fail "Balance parsing failed")

let test_rest_signature () =
  printf "\n[REST Signature]\n";

  let ts1 = Poloniex.Rest.generate_timestamp () in
  let ts2 = Poloniex.Rest.generate_timestamp () in
  test_assert "Timestamp is numeric"
    (String.for_all ts1 ~f:Char.is_digit);
  test_assert "Timestamps increase or equal"
    (Int.of_string ts2 >= Int.of_string ts1);

  let sig1 = Poloniex.Rest.create_signature
    ~api_secret:"test_secret"
    ~request_string:"GET\n/accounts/balances\nrequestBody=&signTimestamp=1699000000"
  in
  let sig2 = Poloniex.Rest.create_signature
    ~api_secret:"test_secret"
    ~request_string:"GET\n/accounts/balances\nrequestBody=&signTimestamp=1699000000"
  in
  test_assert "Signatures are deterministic"
    (String.equal sig1 sig2);
  test_assert "Signature is base64"
    (String.for_all sig1 ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '+' || Char.equal c '/' || Char.equal c '='))

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Poloniex.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Poloniex.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404")

let () =
  printf "===========================================\n";
  printf "Poloniex Unit Tests\n";
  printf "===========================================\n";

  test_cfg ();
  test_types ();
  test_rest_signature ();
  test_error_types ();

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

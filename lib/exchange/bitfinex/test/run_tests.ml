(** Bitfinex Unit Tests *)

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
    (String.is_prefix Bitfinex.Cfg.production.rest_url ~prefix:"https://api.bitfinex.com");

  test_assert "Production has no API key"
    (Option.is_none Bitfinex.Cfg.production.api_key);

  let cfg = Bitfinex.Cfg.with_auth
    ~api_key:"test_key"
    ~api_secret:"test_secret"
    Bitfinex.Cfg.production
  in
  test_assert "with_auth sets API key"
    (Option.equal String.equal cfg.api_key (Some "test_key"));
  test_assert "with_auth sets API secret"
    (Option.equal String.equal cfg.api_secret (Some "test_secret"))

let test_types () =
  printf "\n[Types]\n";

  let ticker_json = `List [
    `Float 50100.0; `Float 1.5; `Float 50150.0; `Float 2.0;
    `Float 500.0; `Float 0.01; `Float 50000.0; `Float 1234.5;
    `Float 51000.0; `Float 48000.0
  ] in
  (match Bitfinex.Types.ticker_of_yojson ticker_json with
   | Ok ticker ->
     test_assert "Ticker parses as float list" true;
     test_assert "Ticker has 10 elements" (List.length ticker = 10)
   | Error _ -> fail "Ticker parsing failed");

  let entry_json = `List [`Float 50000.0; `Int 5; `Float 1.5] in
  (match Bitfinex.Types.order_book_entry_of_yojson entry_json with
   | Ok (price, count, amount) ->
     test_assert "Order book entry parses" true;
     test_assert "Entry price" Float.(abs (price -. 50000.0) < 0.1);
     test_assert "Entry count" (count = 5);
     test_assert "Entry amount" Float.(abs (amount -. 1.5) < 0.1)
   | Error _ -> fail "Order book entry failed");

  let book_json = `List [
    `List [`Float 50000.0; `Int 5; `Float 1.5];
    `List [`Float 49999.0; `Int 3; `Float 2.0];
  ] in
  (match Bitfinex.Types.order_book_of_yojson book_json with
   | Ok book ->
     test_assert "Order book parses" true;
     test_assert "Book has 2 entries" (List.length book = 2)
   | Error _ -> fail "Order book parsing failed");

  let trade_json = `List [`Int 123456; `Int 1699000000; `Float 0.5; `Float 50000.0] in
  (match Bitfinex.Types.trade_of_yojson trade_json with
   | Ok (id, _mts, amount, _price) ->
     test_assert "Trade parses" true;
     test_assert "Trade ID" Int64.(id = of_int 123456);
     test_assert "Trade amount" Float.(abs (amount -. 0.5) < 0.1)
   | Error _ -> fail "Trade parsing failed");

  let wallet_json = `List [`String "exchange"; `String "BTC"; `Float 10.5; `Float 0.0; `Float 10.5] in
  (match Bitfinex.Types.wallet_of_yojson wallet_json with
   | Ok (wtype, currency, balance, _, avail) ->
     test_assert "Wallet parses" true;
     test_assert "Wallet type" (String.equal wtype "exchange");
     test_assert "Wallet currency" (String.equal currency "BTC");
     test_assert "Wallet balance" Float.(abs (balance -. 10.5) < 0.1);
     test_assert "Wallet has available" (Option.is_some avail)
   | Error _ -> fail "Wallet parsing failed")

let test_rest_signature () =
  printf "\n[REST Signature]\n";

  let nonce1 = Bitfinex.Rest.generate_nonce () in
  let nonce2 = Bitfinex.Rest.generate_nonce () in
  test_assert "Nonce is numeric"
    (String.for_all nonce1 ~f:Char.is_digit);
  test_assert "Nonces increase"
    (Int.of_string nonce2 >= Int.of_string nonce1);

  let sig1 = Bitfinex.Rest.create_signature
    ~api_secret:"test_secret"
    ~path:"/v2/auth/r/wallets"
    ~nonce:"1699000000"
    ~body:"{}"
  in
  let sig2 = Bitfinex.Rest.create_signature
    ~api_secret:"test_secret"
    ~path:"/v2/auth/r/wallets"
    ~nonce:"1699000000"
    ~body:"{}"
  in
  test_assert "Signatures are deterministic"
    (String.equal sig1 sig2);
  test_assert "Signature is hex lowercase"
    (String.for_all sig1 ~f:(fun c -> Char.is_digit c || (Char.is_alpha c && Char.is_lowercase c)));
  test_assert "Signature is 128 chars (SHA512 fallback)"
    (String.length sig1 = 128)

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Bitfinex.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Bitfinex.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404")

let () =
  printf "===========================================\n";
  printf "Bitfinex Unit Tests\n";
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

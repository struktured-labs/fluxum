(** HTX (Huobi) Unit Tests *)

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
    (String.is_prefix Htx.Cfg.production.rest_url ~prefix:"https://api.htx.com");

  test_assert "Production has no API key"
    (Option.is_none Htx.Cfg.production.api_key);

  let cfg = Htx.Cfg.with_auth
    ~api_key:"test_key"
    ~api_secret:"test_secret"
    Htx.Cfg.production
  in
  test_assert "with_auth sets API key"
    (Option.equal String.equal cfg.api_key (Some "test_key"));
  test_assert "with_auth sets API secret"
    (Option.equal String.equal cfg.api_secret (Some "test_secret"))

let test_types () =
  printf "\n[Types]\n";

  let ticker_json = `Assoc [
    ("symbol", `String "btcusdt");
    ("open", `Float 50000.0);
    ("high", `Float 51000.0);
    ("low", `Float 48000.0);
    ("close", `Float 49500.0);
    ("amount", `Float 1234.5);
    ("vol", `Float 62000000.0);
    ("count", `Int 10000);
    ("bid", `Float 49450.0);
    ("bidSize", `Float 1.5);
    ("ask", `Float 49550.0);
    ("askSize", `Float 2.0);
  ] in
  (match Htx.Types.ticker_of_yojson ticker_json with
   | Ok ticker ->
     test_assert "Ticker parses" true;
     test_assert "Ticker symbol" (String.equal ticker.symbol "btcusdt");
     test_assert "Ticker close" Float.(abs (ticker.close -. 49500.0) < 0.1)
   | Error _ -> fail "Ticker parsing failed");

  let entry_json = `List [`Float 50000.0; `Float 1.5] in
  (match Htx.Types.order_book_entry_of_yojson entry_json with
   | Ok (price, amount) ->
     test_assert "Order book entry parses" true;
     test_assert "Entry price" Float.(abs (price -. 50000.0) < 0.1);
     test_assert "Entry amount" Float.(abs (amount -. 1.5) < 0.1)
   | Error _ -> fail "Order book entry failed");

  let book_json = `Assoc [
    ("ts", `Int 1699000000);
    ("bids", `List [`List [`Float 49999.0; `Float 2.0]]);
    ("asks", `List [`List [`Float 50001.0; `Float 1.0]]);
  ] in
  (match Htx.Types.order_book_of_yojson book_json with
   | Ok book ->
     test_assert "Order book parses" true;
     test_assert "Book has bids" (List.length book.bids = 1);
     test_assert "Book has asks" (List.length book.asks = 1)
   | Error _ -> fail "Order book parsing failed")

let test_rest_signature () =
  printf "\n[REST Signature]\n";

  let ts = Htx.Rest.generate_timestamp () in
  test_assert "Timestamp format YYYY-MM-DDTHH:MM:SS"
    (String.contains ts 'T' && String.contains ts ':');

  let sig1 = Htx.Rest.create_signature
    ~api_secret:"test_secret"
    ~message:"GET\napi.htx.com\n/v1/account/accounts\ntest=1"
  in
  let sig2 = Htx.Rest.create_signature
    ~api_secret:"test_secret"
    ~message:"GET\napi.htx.com\n/v1/account/accounts\ntest=1"
  in
  test_assert "Signatures are deterministic"
    (String.equal sig1 sig2);
  test_assert "Signature is base64"
    (String.for_all sig1 ~f:(fun c ->
      Char.is_alphanum c || Char.equal c '+' || Char.equal c '/' || Char.equal c '='))

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Htx.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Htx.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404")

let () =
  printf "===========================================\n";
  printf "HTX (Huobi) Unit Tests\n";
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

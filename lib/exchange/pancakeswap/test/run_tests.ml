(** PancakeSwap DEX Unit Tests *)

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

  test_assert "Production API URL"
    (String.is_prefix Pancakeswap.Cfg.production.api_url ~prefix:"https://api.pancakeswap.info");

  test_assert "Mainnet is production"
    (String.equal Pancakeswap.Cfg.mainnet.api_url Pancakeswap.Cfg.production.api_url)

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("name", `String "Wrapped BNB");
    ("symbol", `String "WBNB");
    ("price", `String "300.5");
    ("price_BNB", `String "1.0");
  ] in
  (match Pancakeswap.Types.token_data_of_yojson token_json with
   | Ok token ->
     test_assert "Token data parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WBNB");
     test_assert "Token price" (String.equal token.price "300.5")
   | Error _ -> fail "Token parsing failed");

  let pair_json = `Assoc [
    ("pair_address", `String "0x123");
    ("base_name", `String "CAKE");
    ("base_symbol", `String "CAKE");
    ("quote_name", `String "WBNB");
    ("quote_symbol", `String "WBNB");
    ("price", `String "2.5");
    ("base_volume", `String "1000000");
    ("quote_volume", `String "2500000");
  ] in
  (match Pancakeswap.Types.pair_data_of_yojson pair_json with
   | Ok pair ->
     test_assert "Pair data parses" true;
     test_assert "Pair base symbol" (String.equal pair.base_symbol "CAKE");
     test_assert "Pair price" (String.equal pair.price "2.5")
   | Error _ -> fail "Pair parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Pancakeswap.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Pancakeswap.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404")

let () =
  printf "===========================================\n";
  printf "PancakeSwap DEX Unit Tests (BSC)\n";
  printf "===========================================\n";

  test_cfg ();
  test_types ();
  test_error_types ();

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

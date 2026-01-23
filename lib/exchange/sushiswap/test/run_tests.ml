(** SushiSwap DEX Unit Tests **)

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
    (String.is_prefix Sushiswap.Cfg.production.api_url ~prefix:"https://api.sushi.com");
  test_assert "Mainnet is production"
    (String.equal Sushiswap.Cfg.mainnet.api_url Sushiswap.Cfg.production.api_url);
  test_assert "Default no API key"
    (Option.is_none Sushiswap.Cfg.production.api_key);

  let with_key = Sushiswap.Cfg.with_api_key ~api_key:"test_key" Sushiswap.Cfg.production in
  test_assert "Can set API key"
    (Option.equal String.equal (Some "test_key") with_key.api_key);
  test_assert "API key retrieval"
    (Option.equal String.equal (Some "test_key") (Sushiswap.Cfg.api_key_opt with_key))

let test_types () =
  printf "\n[Types]\n";

  let quote_json = `Assoc [
    ("tokenIn", `String "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2");
    ("tokenOut", `String "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48");
    ("amountIn", `String "1000000000000000000");
    ("amountOut", `String "1500000000");
    ("priceImpact", `Float 0.025);
    ("route", `List [`String "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"; `String "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"]);
  ] in
  (match Sushiswap.Types.swap_quote_of_yojson quote_json with
   | Ok quote ->
     test_assert "Swap quote parses" true;
     test_assert "Token in" (String.equal quote.tokenIn "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2");
     test_assert "Token out" (String.equal quote.tokenOut "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48");
     test_assert "Amount in" (String.equal quote.amountIn "1000000000000000000");
     test_assert "Amount out" (String.equal quote.amountOut "1500000000");
     test_assert "Price impact" Float.(abs (quote.priceImpact -. 0.025) < 0.0001);
     test_assert "Route length" (Int.equal (List.length quote.route) 2)
   | Error _ -> fail "Swap quote parsing failed");

  let pool_json = `Assoc [
    ("address", `String "0x397FF1542f962076d0BFE58eA045FfA2d347ACa0");
    ("token0", `String "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2");
    ("token1", `String "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48");
    ("reserve0", `String "100000000000000000000");
    ("reserve1", `String "150000000000");
    ("totalSupply", `String "12247448713915890585");
  ] in
  (match Sushiswap.Types.pool_info_of_yojson pool_json with
   | Ok pool ->
     test_assert "Pool info parses" true;
     test_assert "Pool address" (String.equal pool.address "0x397FF1542f962076d0BFE58eA045FfA2d347ACa0");
     test_assert "Token0" (String.equal pool.token0 "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2");
     test_assert "Token1" (String.equal pool.token1 "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48");
     test_assert "Reserve0" (String.equal pool.reserve0 "100000000000000000000");
     test_assert "Reserve1" (String.equal pool.reserve1 "150000000000");
     test_assert "Total supply" (String.equal pool.totalSupply "12247448713915890585")
   | Error _ -> fail "Pool info parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Sushiswap.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Sushiswap.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Sushiswap.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Sushiswap.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Sushiswap.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Sushiswap.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid")

let () =
  printf "===========================================\n";
  printf "SushiSwap DEX Unit Tests (Multi-chain)\n";
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

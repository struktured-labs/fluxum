(** Orca DEX Unit Tests *)

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
    (String.is_prefix Orca.Cfg.production.api_url ~prefix:"https://api.mainnet.orca.so");
  test_assert "Mainnet is production"
    (String.equal Orca.Cfg.mainnet.api_url Orca.Cfg.production.api_url)

let test_types () =
  printf "\n[Types]\n";

  let pool_json = `Assoc [
    ("address", `String "whirLbMiicVdio4qvUfM5KAg6Ct8VwpYzGff3uctyCc");
    ("tokenA", `String "So11111111111111111111111111111111111111112");
    ("tokenB", `String "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v");
    ("tickSpacing", `Int 64);
    ("price", `Float 50.5);
    ("liquidity", `String "1000000000");
    ("volume24h", `Float 5000000.0);
    ("volumeWeek", `Float 35000000.0);
    ("feeRate", `Float 0.003);
  ] in
  (match Orca.Types.whirlpool_of_yojson pool_json with
   | Ok pool ->
     test_assert "Whirlpool parses" true;
     test_assert "Pool price" Float.(abs (pool.price -. 50.5) < 0.1);
     test_assert "Pool fee rate" Float.(abs (pool.feeRate -. 0.003) < 0.0001)
   | Error _ -> fail "Whirlpool parsing failed");

  let price_json = `Assoc [
    ("mint", `String "So11111111111111111111111111111111111111112");
    ("price", `Float 100.5);
    ("decimals", `Int 9);
  ] in
  (match Orca.Types.token_price_of_yojson price_json with
   | Ok price ->
     test_assert "Token price parses" true;
     test_assert "Price value" Float.(abs (price.price -. 100.5) < 0.1)
   | Error _ -> fail "Token price parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Orca.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Orca.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404")

let () =
  printf "===========================================\n";
  printf "Orca DEX Unit Tests (Solana)\n";
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

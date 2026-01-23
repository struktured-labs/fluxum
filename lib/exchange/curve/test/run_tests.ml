(** Curve Finance DEX Unit Tests *)

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
  test_assert "Ethereum mainnet URL"
    (String.is_substring Curve.Cfg.ethereum_mainnet.api_url ~substring:"curve.fi");
  test_assert "Ethereum mainnet network"
    (String.equal Curve.Cfg.ethereum_mainnet.network "ethereum");
  test_assert "Production is mainnet"
    (String.equal Curve.Cfg.production.network Curve.Cfg.ethereum_mainnet.network);
  test_assert "Polygon network"
    (String.equal Curve.Cfg.polygon.network "polygon");
  test_assert "Arbitrum network"
    (String.equal Curve.Cfg.arbitrum.network "arbitrum");
  test_assert "Optimism network"
    (String.equal Curve.Cfg.optimism.network "optimism");
  test_assert "Avalanche network"
    (String.equal Curve.Cfg.avalanche.network "avalanche");
  test_assert "Fantom network"
    (String.equal Curve.Cfg.fantom.network "fantom")

let test_types () =
  printf "\n[Types]\n";

  let coin_json = `Assoc [
    ("address", `String "0x6b175474e89094c44da98b954eedeac495271d0f");
    ("symbol", `String "DAI");
    ("decimals", `Int 18);
  ] in
  (match Curve.Types.coin_of_yojson coin_json with
   | Ok coin ->
     test_assert "Coin parses" true;
     test_assert "Coin symbol" (String.equal coin.symbol "DAI");
     test_assert "Coin decimals" (Int.equal coin.decimals 18)
   | Error _ -> fail "Coin parsing failed");

  let pool_json = `Assoc [
    ("id", `String "3pool");
    ("address", `String "0xbebc44782c7db0a1a60cb6fe97d0b483032ff1c7");
    ("name", `String "3pool");
    ("symbol", `String "3Crv");
    ("assetType", `Int 0);
    ("coins", `List [
      `Assoc [
        ("address", `String "0x6b175474e89094c44da98b954eedeac495271d0f");
        ("symbol", `String "DAI");
        ("decimals", `Int 18);
      ];
      `Assoc [
        ("address", `String "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48");
        ("symbol", `String "USDC");
        ("decimals", `Int 6);
      ];
    ]);
    ("coinAddresses", `List [`String "0x6b175474e89094c44da98b954eedeac495271d0f"; `String "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"]);
    ("decimals", `List [`String "18"; `String "6"]);
    ("totalSupply", `String "1234567890123456789012345678");
    ("lpTokenAddress", `String "0x6c3f90f043a72fa612cbac8115ee7e52bde6e490");
    ("virtualPrice", `String "1020304050607080901");
    ("usdTotal", `Float 1234567890.12);
    ("amplificationCoefficient", `String "2000");
  ] in
  (match Curve.Types.pool_data_of_yojson pool_json with
   | Ok pool ->
     test_assert "Pool parses" true;
     test_assert "Pool name" (String.equal pool.name "3pool");
     test_assert "Pool symbol" (String.equal pool.symbol "3Crv");
     test_assert "Pool coins count" (Int.equal (List.length pool.coins) 2);
     test_assert "Pool coin addresses count" (Int.equal (List.length pool.coinAddresses) 2);
     test_assert "Pool usdTotal" Float.(abs (pool.usdTotal -. 1234567890.12) < 1.0)
   | Error msg -> fail (sprintf "Pool parsing failed: %s" msg));

  let volume_json = `Assoc [
    ("date", `String "2024-01-01");
    ("volume", `Float 12345678.90);
  ] in
  (match Curve.Types.volume_data_of_yojson volume_json with
   | Ok vol ->
     test_assert "Volume parses" true;
     test_assert "Volume date" (String.equal vol.date "2024-01-01");
     test_assert "Volume value" Float.(abs (vol.volume -. 12345678.90) < 1.0)
   | Error _ -> fail "Volume parsing failed");

  let trade_json = `Assoc [
    ("tx", `String "0x123abc");
    ("timestamp", `String "1640000000");
    ("tokenIn", `String "0x6b175474e89094c44da98b954eedeac495271d0f");
    ("tokenOut", `String "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48");
    ("amountIn", `String "1000000000000000000");
    ("amountOut", `String "1000000");
    ("trader", `String "0xtrader");
  ] in
  (match Curve.Types.trade_of_yojson trade_json with
   | Ok trade ->
     test_assert "Trade parses" true;
     test_assert "Trade tx" (String.equal trade.tx "0x123abc");
     test_assert "Trade tokenIn" (String.equal trade.tokenIn "0x6b175474e89094c44da98b954eedeac495271d0f");
     test_assert "Trade tokenOut" (String.equal trade.tokenOut "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48");
     test_assert "Trade amountIn" (String.equal trade.amountIn "1000000000000000000");
     test_assert "Trade amountOut" (String.equal trade.amountOut "1000000");
     test_assert "Trade trader" (String.equal trade.trader "0xtrader")
   | Error _ -> fail "Trade parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Curve.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Curve.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Curve.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Curve.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Curve.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Curve.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid");

  let api_err : Curve.Rest.error = `Api_error "Request failed" in
  let api_str = Sexplib.Sexp.to_string_hum (Curve.Rest.sexp_of_error api_err) in
  test_assert "API error to_sexp" (String.is_substring api_str ~substring:"failed")

let () =
  printf "===========================================\n";
  printf "Curve Finance DEX Unit Tests\n";
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

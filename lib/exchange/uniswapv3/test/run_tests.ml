(** Uniswap V3 DEX Unit Tests *)

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
    (String.is_substring Uniswapv3.Cfg.ethereum_mainnet.subgraph_url ~substring:"uniswap-v3");
  test_assert "Production is mainnet"
    (String.equal Uniswapv3.Cfg.production.subgraph_url Uniswapv3.Cfg.ethereum_mainnet.subgraph_url);
  test_assert "Polygon URL"
    (String.is_substring Uniswapv3.Cfg.polygon.subgraph_url ~substring:"polygon");
  test_assert "Arbitrum URL"
    (String.is_substring Uniswapv3.Cfg.arbitrum.subgraph_url ~substring:"arbitrum");
  test_assert "Optimism URL"
    (String.is_substring Uniswapv3.Cfg.optimism.subgraph_url ~substring:"optimism");
  test_assert "Default no API key"
    (Option.is_none Uniswapv3.Cfg.production.api_key);

  let with_key = Uniswapv3.Cfg.with_api_key ~api_key:"test_key" Uniswapv3.Cfg.production in
  test_assert "Can set API key"
    (Option.equal String.equal (Some "test_key") with_key.api_key);
  test_assert "API key retrieval"
    (Option.equal String.equal (Some "test_key") (Uniswapv3.Cfg.api_key_opt with_key))

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("id", `String "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
    ("symbol", `String "WETH");
    ("name", `String "Wrapped Ether");
    ("decimals", `Int 18);
  ] in
  (match Uniswapv3.Uni_types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WETH");
     test_assert "Token name" (String.equal token.name "Wrapped Ether");
     test_assert "Token decimals" (Int.equal token.decimals 18)
   | Error _ -> fail "Token parsing failed");

  let pool_json = `Assoc [
    ("id", `String "0x8ad599c3a0ff1de082011efddc58f1908eb6e6d8");
    ("token0", `Assoc [
      ("id", `String "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48");
      ("symbol", `String "USDC");
      ("name", `String "USD Coin");
      ("decimals", `Int 6);
    ]);
    ("token1", `Assoc [
      ("id", `String "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
      ("symbol", `String "WETH");
      ("name", `String "Wrapped Ether");
      ("decimals", `Int 18);
    ]);
    ("feeTier", `Int 3000);
    ("liquidity", `String "12345678901234567890");
    ("sqrtPrice", `String "1234567890123456789012345678901234567890");
    ("tick", `Int 276324);
    ("volumeUSD", `Float 1234567.89);
    ("txCount", `Int 567890);
  ] in
  (match Uniswapv3.Uni_types.pool_of_yojson pool_json with
   | Ok pool ->
     test_assert "Pool parses" true;
     test_assert "Pool token0 symbol" (String.equal pool.token0.symbol "USDC");
     test_assert "Pool token1 symbol" (String.equal pool.token1.symbol "WETH");
     test_assert "Pool fee tier" (Int.equal pool.feeTier 3000);
     test_assert "Pool tick" (Int.equal pool.tick 276324);
     test_assert "Pool volume" Float.(abs (pool.volumeUSD -. 1234567.89) < 1.0);
     test_assert "Pool tx count" (Int.equal pool.txCount 567890)
   | Error _ -> fail "Pool parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x123...#456");
    ("timestamp", `String "1640000000");
    ("amount0", `String "1000000000");
    ("amount1", `String "-500000000000000000");
    ("amountUSD", `Float 1500.25);
    ("sender", `String "0xsender");
    ("recipient", `String "0xrecipient");
  ] in
  (match Uniswapv3.Uni_types.swap_of_yojson swap_json with
   | Ok swap ->
     test_assert "Swap parses" true;
     test_assert "Swap timestamp" (String.equal swap.timestamp "1640000000");
     test_assert "Swap amount0" (String.equal swap.amount0 "1000000000");
     test_assert "Swap amount1" (String.equal swap.amount1 "-500000000000000000");
     test_assert "Swap amountUSD" Float.(abs (swap.amountUSD -. 1500.25) < 0.01);
     test_assert "Swap sender" (String.equal swap.sender "0xsender");
     test_assert "Swap recipient" (String.equal swap.recipient "0xrecipient")
   | Error _ -> fail "Swap parsing failed");

  let tick_json = `Assoc [
    ("tickIdx", `Int 276324);
    ("liquidityGross", `String "12345678901234567890");
    ("liquidityNet", `String "-12345678901234567890");
    ("price0", `Float 1500.25);
    ("price1", `Float 0.000666);
  ] in
  (match Uniswapv3.Uni_types.tick_of_yojson tick_json with
   | Ok tick ->
     test_assert "Tick parses" true;
     test_assert "Tick index" (Int.equal tick.tickIdx 276324);
     test_assert "Tick price0" Float.(abs (tick.price0 -. 1500.25) < 0.01);
     test_assert "Tick price1" Float.(abs (tick.price1 -. 0.000666) < 0.000001)
   | Error _ -> fail "Tick parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Uniswapv3.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Uniswapv3.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Uniswapv3.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Uniswapv3.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Uniswapv3.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Uniswapv3.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid");

  let graphql_err : Uniswapv3.Rest.error = `GraphQL "Query error" in
  let gql_str = Sexplib.Sexp.to_string_hum (Uniswapv3.Rest.sexp_of_error graphql_err) in
  test_assert "GraphQL error to_sexp" (String.is_substring gql_str ~substring:"Query")

let () =
  printf "===========================================\n";
  printf "Uniswap V3 DEX Unit Tests\n";
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

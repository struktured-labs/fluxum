(** Balancer DEX Unit Tests *)

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
    (String.is_substring Balancer.Cfg.ethereum_mainnet.subgraph_url ~substring:"balancer-v2");
  test_assert "Production is mainnet"
    (String.equal Balancer.Cfg.production.subgraph_url Balancer.Cfg.ethereum_mainnet.subgraph_url);
  test_assert "Polygon URL"
    (String.is_substring Balancer.Cfg.polygon.subgraph_url ~substring:"polygon");
  test_assert "Arbitrum URL"
    (String.is_substring Balancer.Cfg.arbitrum.subgraph_url ~substring:"arbitrum");
  test_assert "Optimism URL"
    (String.is_substring Balancer.Cfg.optimism.subgraph_url ~substring:"optimism");
  test_assert "Gnosis URL"
    (String.is_substring Balancer.Cfg.gnosis.subgraph_url ~substring:"gnosis")

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("address", `String "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
    ("symbol", `String "WETH");
    ("name", `String "Wrapped Ether");
    ("decimals", `Int 18);
    ("balance", `String "100000000000000000000");
    ("weight", `String "0.5");
  ] in
  (match Balancer.Types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WETH");
     test_assert "Token name" (String.equal token.name "Wrapped Ether");
     test_assert "Token decimals" (Int.equal token.decimals 18);
     test_assert "Token balance" (String.equal token.balance "100000000000000000000");
     test_assert "Token weight" (Option.equal String.equal token.weight (Some "0.5"))
   | Error _ -> fail "Token parsing failed");

  let pool_json = `Assoc [
    ("id", `String "0x5c6ee304399dbdb9c8ef030ab642b10820db8f56000200000000000000000014");
    ("address", `String "0x5c6ee304399dbdb9c8ef030ab642b10820db8f56");
    ("poolType", `String "Weighted");
    ("swapFee", `String "0.001");
    ("totalLiquidity", `String "123456789.12");
    ("totalShares", `String "987654321.98");
    ("tokens", `List [
      `Assoc [
        ("address", `String "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
        ("symbol", `String "WETH");
        ("name", `String "Wrapped Ether");
        ("decimals", `Int 18);
        ("balance", `String "50000000000000000000");
        ("weight", `String "0.8");
      ];
      `Assoc [
        ("address", `String "0xba100000625a3754423978a60c9317c58a424e3d");
        ("symbol", `String "BAL");
        ("name", `String "Balancer");
        ("decimals", `Int 18);
        ("balance", `String "10000000000000000000");
        ("weight", `String "0.2");
      ];
    ]);
    ("swapsCount", `String "12345");
    ("holdersCount", `String "567");
  ] in
  (match Balancer.Types.pool_of_yojson pool_json with
   | Ok pool ->
     test_assert "Pool parses" true;
     test_assert "Pool type" (String.equal pool.poolType "Weighted");
     test_assert "Pool swap fee" (String.equal pool.swapFee "0.001");
     test_assert "Pool tokens count" (Int.equal (List.length pool.tokens) 2);
     test_assert "Pool swaps count" (String.equal pool.swapsCount "12345");
     test_assert "Pool holders count" (String.equal pool.holdersCount "567")
   | Error _ -> fail "Pool parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x123-456");
    ("timestamp", `String "1640000000");
    ("tokenIn", `String "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
    ("tokenOut", `String "0xba100000625a3754423978a60c9317c58a424e3d");
    ("tokenAmountIn", `String "1000000000000000000");
    ("tokenAmountOut", `String "5000000000000000000");
    ("valueUSD", `String "1500.25");
    ("userAddress", `String "0xuser");
  ] in
  (match Balancer.Types.swap_of_yojson swap_json with
   | Ok swap ->
     test_assert "Swap parses" true;
     test_assert "Swap timestamp" (String.equal swap.timestamp "1640000000");
     test_assert "Swap tokenIn" (String.equal swap.tokenIn "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2");
     test_assert "Swap tokenOut" (String.equal swap.tokenOut "0xba100000625a3754423978a60c9317c58a424e3d");
     test_assert "Swap tokenAmountIn" (String.equal swap.tokenAmountIn "1000000000000000000");
     test_assert "Swap tokenAmountOut" (String.equal swap.tokenAmountOut "5000000000000000000");
     test_assert "Swap valueUSD" (String.equal swap.valueUSD "1500.25");
     test_assert "Swap userAddress" (String.equal swap.userAddress "0xuser")
   | Error _ -> fail "Swap parsing failed");

  let snapshot_json = `Assoc [
    ("id", `String "0x123-456-1640000000");
    ("timestamp", `String "1640000000");
    ("liquidity", `String "123456789.12");
    ("swapVolume", `String "987654.32");
    ("swapFees", `String "987.65");
  ] in
  (match Balancer.Types.pool_snapshot_of_yojson snapshot_json with
   | Ok snapshot ->
     test_assert "Snapshot parses" true;
     test_assert "Snapshot timestamp" (String.equal snapshot.timestamp "1640000000");
     test_assert "Snapshot liquidity" (String.equal snapshot.liquidity "123456789.12");
     test_assert "Snapshot volume" (String.equal snapshot.swapVolume "987654.32");
     test_assert "Snapshot fees" (String.equal snapshot.swapFees "987.65")
   | Error _ -> fail "Snapshot parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Balancer.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Balancer.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Balancer.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Balancer.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Balancer.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Balancer.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid");

  let graphql_err : Balancer.Rest.error = `GraphQL "Query error" in
  let gql_str = Sexplib.Sexp.to_string_hum (Balancer.Rest.sexp_of_error graphql_err) in
  test_assert "GraphQL error to_sexp" (String.is_substring gql_str ~substring:"Query")

let () =
  printf "===========================================\n";
  printf "Balancer DEX Unit Tests\n";
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

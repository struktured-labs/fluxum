(** SpookySwap DEX Unit Tests *)

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
  test_assert "Fantom URL"
    (String.is_substring Spookyswap.Cfg.fantom.subgraph_url ~substring:"spookyswap");
  test_assert "Production is Fantom"
    (String.equal Spookyswap.Cfg.production.subgraph_url Spookyswap.Cfg.fantom.subgraph_url)

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("id", `String "0x21be370d5312f44cb42ce377bc9b8a0cef1a4c83");
    ("symbol", `String "WFTM");
    ("name", `String "Wrapped Fantom");
    ("decimals", `String "18");
  ] in
  (match Spookyswap.Types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WFTM");
     test_assert "Token name" (String.equal token.name "Wrapped Fantom");
     test_assert "Token decimals" (String.equal token.decimals "18")
   | Error _ -> fail "Token parsing failed");

  let pair_json = `Assoc [
    ("id", `String "0x2b4c76d0dc16be1c31d4c1dc53bf9b45987fc75c");
    ("token0", `Assoc [
      ("id", `String "0x21be370d5312f44cb42ce377bc9b8a0cef1a4c83");
      ("symbol", `String "WFTM");
      ("name", `String "Wrapped Fantom");
      ("decimals", `String "18");
    ]);
    ("token1", `Assoc [
      ("id", `String "0x04068da6c83afcfa0e13ba15a6696662335d5b75");
      ("symbol", `String "USDC");
      ("name", `String "USD Coin");
      ("decimals", `String "6");
    ]);
    ("reserve0", `String "1000000000000000000000");
    ("reserve1", `String "50000000000");
    ("totalSupply", `String "7071067811865475244");
    ("reserveUSD", `String "100000.50");
    ("volumeUSD", `String "1234567.89");
    ("txCount", `String "12345");
  ] in
  (match Spookyswap.Types.pair_of_yojson pair_json with
   | Ok pair ->
     test_assert "Pair parses" true;
     test_assert "Pair token0 symbol" (String.equal pair.token0.symbol "WFTM");
     test_assert "Pair token1 symbol" (String.equal pair.token1.symbol "USDC");
     test_assert "Pair reserve0" (String.equal pair.reserve0 "1000000000000000000000");
     test_assert "Pair reserve1" (String.equal pair.reserve1 "50000000000");
     test_assert "Pair reserveUSD" (String.equal pair.reserveUSD "100000.50");
     test_assert "Pair volumeUSD" (String.equal pair.volumeUSD "1234567.89");
     test_assert "Pair txCount" (String.equal pair.txCount "12345")
   | Error _ -> fail "Pair parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x123-456");
    ("timestamp", `String "1640000000");
    ("amount0In", `String "1000000000000000000");
    ("amount1In", `String "0");
    ("amount0Out", `String "0");
    ("amount1Out", `String "50000000");
    ("amountUSD", `String "1500.25");
    ("to", `String "0xrecipient");
  ] in
  (match Spookyswap.Types.swap_of_yojson swap_json with
   | Ok swap ->
     test_assert "Swap parses" true;
     test_assert "Swap timestamp" (String.equal swap.timestamp "1640000000");
     test_assert "Swap amount0In" (String.equal swap.amount0In "1000000000000000000");
     test_assert "Swap amount1In" (String.equal swap.amount1In "0");
     test_assert "Swap amount0Out" (String.equal swap.amount0Out "0");
     test_assert "Swap amount1Out" (String.equal swap.amount1Out "50000000");
     test_assert "Swap amountUSD" (String.equal swap.amountUSD "1500.25");
     test_assert "Swap to" (String.equal swap.to_ "0xrecipient")
   | Error _ -> fail "Swap parsing failed");

  let position_json = `Assoc [
    ("id", `String "0x2b4c76d0dc16be1c31d4c1dc53bf9b45987fc75c-0xuser");
    ("user", `String "0xuser");
    ("liquidityTokenBalance", `String "7071067811865475244");
  ] in
  (match Spookyswap.Types.liquidity_position_of_yojson position_json with
   | Ok position ->
     test_assert "Liquidity position parses" true;
     test_assert "Position user" (String.equal position.user "0xuser");
     test_assert "Position balance" (String.equal position.liquidityTokenBalance "7071067811865475244")
   | Error _ -> fail "Liquidity position parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Spookyswap.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Spookyswap.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Spookyswap.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Spookyswap.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Spookyswap.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Spookyswap.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid");

  let graphql_err : Spookyswap.Rest.error = `GraphQL "Query error" in
  let gql_str = Sexplib.Sexp.to_string_hum (Spookyswap.Rest.sexp_of_error graphql_err) in
  test_assert "GraphQL error to_sexp" (String.is_substring gql_str ~substring:"Query")

let () =
  printf "===========================================\n";
  printf "SpookySwap DEX Unit Tests\n";
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

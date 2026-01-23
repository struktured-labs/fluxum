(** Thena DEX Unit Tests *)

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
  test_assert "BSC URL" (String.is_substring Thena.Cfg.bsc.subgraph_url ~substring:"thena");
  test_assert "Production is BSC" (String.equal Thena.Cfg.production.subgraph_url Thena.Cfg.bsc.subgraph_url)

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("id", `String "0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c");
    ("symbol", `String "WBNB");
    ("decimals", `String "18");
  ] in
  (match Thena.Types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WBNB");
     test_assert "Token decimals" (String.equal token.decimals "18")
   | Error _ -> fail "Token parsing failed");

  let pair_json = `Assoc [
    ("id", `String "0x123");
    ("token0", `Assoc [
      ("id", `String "0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c");
      ("symbol", `String "WBNB");
      ("decimals", `String "18");
    ]);
    ("token1", `Assoc [
      ("id", `String "0x55d398326f99059ff775485246999027b3197955");
      ("symbol", `String "USDT");
      ("decimals", `String "18");
    ]);
    ("reserve0", `String "1000000000000000000000");
    ("reserve1", `String "500000000000000000000000");
    ("totalSupply", `String "22360679774997896964");
  ] in
  (match Thena.Types.pair_of_yojson pair_json with
   | Ok pair ->
     test_assert "Pair parses" true;
     test_assert "Pair token0 symbol" (String.equal pair.token0.symbol "WBNB");
     test_assert "Pair token1 symbol" (String.equal pair.token1.symbol "USDT");
     test_assert "Pair reserve0" (String.equal pair.reserve0 "1000000000000000000000")
   | Error _ -> fail "Pair parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x456-1");
    ("timestamp", `String "1640000000");
    ("amount0In", `String "1000000000000000000");
    ("amount1In", `String "0");
    ("amount0Out", `String "0");
    ("amount1Out", `String "500000000000000000000");
    ("to", `String "0xswapper");
  ] in
  (match Thena.Types.swap_of_yojson swap_json with
   | Ok swap ->
     test_assert "Swap parses" true;
     test_assert "Swap timestamp" (String.equal swap.timestamp "1640000000");
     test_assert "Swap to" (String.equal swap.to_ "0xswapper")
   | Error _ -> fail "Swap parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Thena.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Thena.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Thena.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Thena.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let graphql_err : Thena.Rest.error = `GraphQL "Query error" in
  let gql_str = Sexplib.Sexp.to_string_hum (Thena.Rest.sexp_of_error graphql_err) in
  test_assert "GraphQL error to_sexp" (String.is_substring gql_str ~substring:"Query")

let () =
  printf "===========================================\n";
  printf "Thena DEX Unit Tests\n";
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

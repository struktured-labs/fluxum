(** GMX DEX Unit Tests *)

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
  test_assert "Arbitrum URL" (String.is_substring Gmx.Cfg.arbitrum.subgraph_url ~substring:"gmx");
  test_assert "Avalanche URL" (String.is_substring Gmx.Cfg.avalanche.subgraph_url ~substring:"avalanche");
  test_assert "Production is Arbitrum" (String.equal Gmx.Cfg.production.subgraph_url Gmx.Cfg.arbitrum.subgraph_url)

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("id", `String "0x82af49447d8a07e3bd95bd0d56f35241523fbab1");
    ("symbol", `String "WETH");
    ("decimals", `Int 18);
  ] in
  (match Gmx.Types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WETH");
     test_assert "Token decimals" (Int.equal token.decimals 18)
   | Error _ -> fail "Token parsing failed");

  let trade_json = `Assoc [
    ("id", `String "0x123");
    ("timestamp", `String "1640000000");
    ("account", `String "0xtrader");
    ("sizeDelta", `String "1000000000000000000000000000000");
    ("collateralDelta", `String "100000000000000000000000000000");
    ("isLong", `Bool true);
    ("price", `String "2000000000000000000000000000000");
    ("fee", `String "1000000000000000000000000000");
  ] in
  (match Gmx.Types.trade_of_yojson trade_json with
   | Ok trade ->
     test_assert "Trade parses" true;
     test_assert "Trade account" (String.equal trade.account "0xtrader");
     test_assert "Trade isLong" (Bool.equal trade.isLong true);
     test_assert "Trade timestamp" (String.equal trade.timestamp "1640000000")
   | Error _ -> fail "Trade parsing failed");

  let position_json = `Assoc [
    ("id", `String "0x456");
    ("account", `String "0xtrader");
    ("collateral", `String "100000000000000000000000000000");
    ("size", `String "1000000000000000000000000000000");
    ("averagePrice", `String "2000000000000000000000000000000");
    ("entryFundingRate", `String "1000000");
    ("realisedPnl", `String "50000000000000000000000000000");
  ] in
  (match Gmx.Types.position_of_yojson position_json with
   | Ok position ->
     test_assert "Position parses" true;
     test_assert "Position account" (String.equal position.account "0xtrader");
     test_assert "Position size" (String.equal position.size "1000000000000000000000000000000")
   | Error _ -> fail "Position parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Gmx.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Gmx.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Gmx.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Gmx.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let graphql_err : Gmx.Rest.error = `GraphQL "Query error" in
  let gql_str = Sexplib.Sexp.to_string_hum (Gmx.Rest.sexp_of_error graphql_err) in
  test_assert "GraphQL error to_sexp" (String.is_substring gql_str ~substring:"Query")

let () =
  printf "===========================================\n";
  printf "GMX DEX Unit Tests\n";
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

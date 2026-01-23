(** Trader Joe DEX Unit Tests *)

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
  test_assert "Avalanche mainnet URL"
    (String.is_substring Traderjoe.Cfg.avalanche_mainnet.api_url ~substring:"barn.traderjoexyz.com");
  test_assert "Avalanche mainnet network"
    (String.equal Traderjoe.Cfg.avalanche_mainnet.network "avalanche");
  test_assert "Production is mainnet"
    (String.equal Traderjoe.Cfg.production.network Traderjoe.Cfg.avalanche_mainnet.network);
  test_assert "Arbitrum network"
    (String.equal Traderjoe.Cfg.arbitrum.network "arbitrum");
  test_assert "BSC network"
    (String.equal Traderjoe.Cfg.bsc.network "bsc")

let test_types () =
  printf "\n[Types]\n";

  let token_json = `Assoc [
    ("address", `String "0xb31f66aa3c1e785363f0875a1b74e27b85fd66c7");
    ("symbol", `String "WAVAX");
    ("decimals", `Int 18);
  ] in
  (match Traderjoe.Types.token_of_yojson token_json with
   | Ok token ->
     test_assert "Token parses" true;
     test_assert "Token symbol" (String.equal token.symbol "WAVAX");
     test_assert "Token decimals" (Int.equal token.decimals 18)
   | Error _ -> fail "Token parsing failed");

  let pair_json = `Assoc [
    ("address", `String "0x454e67025631c065d3cfad6d71e6892f74487a15");
    ("token0", `Assoc [
      ("address", `String "0xb31f66aa3c1e785363f0875a1b74e27b85fd66c7");
      ("symbol", `String "WAVAX");
      ("decimals", `Int 18);
    ]);
    ("token1", `Assoc [
      ("address", `String "0xb97ef9ef8734c71904d8002f8b6bc66dd9c48a6e");
      ("symbol", `String "USDC");
      ("decimals", `Int 6);
    ]);
    ("reserve0", `String "1000000000000000000000");
    ("reserve1", `String "50000000000");
    ("totalSupply", `String "7071067811865475244");
    ("volumeUSD", `Float 1234567.89);
    ("txCount", `Int 12345);
  ] in
  (match Traderjoe.Types.pair_of_yojson pair_json with
   | Ok pair ->
     test_assert "Pair parses" true;
     test_assert "Pair token0 symbol" (String.equal pair.token0.symbol "WAVAX");
     test_assert "Pair token1 symbol" (String.equal pair.token1.symbol "USDC");
     test_assert "Pair reserve0" (String.equal pair.reserve0 "1000000000000000000000");
     test_assert "Pair reserve1" (String.equal pair.reserve1 "50000000000");
     test_assert "Pair volumeUSD" Float.(abs (pair.volumeUSD -. 1234567.89) < 1.0);
     test_assert "Pair txCount" (Int.equal pair.txCount 12345)
   | Error _ -> fail "Pair parsing failed");

  let bin_json = `Assoc [
    ("binId", `Int 8388608);
    ("price", `Float 1.0);
    ("reserveX", `String "1000000000000000000");
    ("reserveY", `String "1000000");
    ("liquidity", `String "1000000000");
  ] in
  (match Traderjoe.Types.liquidity_bin_of_yojson bin_json with
   | Ok bin ->
     test_assert "Liquidity bin parses" true;
     test_assert "Bin ID" (Int.equal bin.binId 8388608);
     test_assert "Bin price" Float.(abs (bin.price -. 1.0) < 0.01);
     test_assert "Bin reserveX" (String.equal bin.reserveX "1000000000000000000");
     test_assert "Bin reserveY" (String.equal bin.reserveY "1000000")
   | Error _ -> fail "Liquidity bin parsing failed");

  let lb_pair_json = `Assoc [
    ("address", `String "0xd446eb1660f766d533beceef890df7a69d26f7d1");
    ("token0", `Assoc [
      ("address", `String "0xb31f66aa3c1e785363f0875a1b74e27b85fd66c7");
      ("symbol", `String "WAVAX");
      ("decimals", `Int 18);
    ]);
    ("token1", `Assoc [
      ("address", `String "0xb97ef9ef8734c71904d8002f8b6bc66dd9c48a6e");
      ("symbol", `String "USDC");
      ("decimals", `Int 6);
    ]);
    ("activeId", `Int 8388608);
    ("binStep", `Int 25);
    ("bins", `List [
      `Assoc [
        ("binId", `Int 8388608);
        ("price", `Float 1.0);
        ("reserveX", `String "1000000000000000000");
        ("reserveY", `String "1000000");
        ("liquidity", `String "1000000000");
      ];
    ]);
    ("volumeUSD", `Float 987654.32);
    ("feesUSD", `Float 987.65);
  ] in
  (match Traderjoe.Types.lb_pair_of_yojson lb_pair_json with
   | Ok pair ->
     test_assert "LB pair parses" true;
     test_assert "LB pair token0 symbol" (String.equal pair.token0.symbol "WAVAX");
     test_assert "LB pair activeId" (Int.equal pair.activeId 8388608);
     test_assert "LB pair binStep" (Int.equal pair.binStep 25);
     test_assert "LB pair bins count" (Int.equal (List.length pair.bins) 1);
     test_assert "LB pair volumeUSD" Float.(abs (pair.volumeUSD -. 987654.32) < 1.0);
     test_assert "LB pair feesUSD" Float.(abs (pair.feesUSD -. 987.65) < 0.01)
   | Error _ -> fail "LB pair parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x123-456");
    ("timestamp", `String "1640000000");
    ("amountIn", `String "1000000000000000000");
    ("amountOut", `String "50000000");
    ("tokenIn", `String "0xb31f66aa3c1e785363f0875a1b74e27b85fd66c7");
    ("tokenOut", `String "0xb97ef9ef8734c71904d8002f8b6bc66dd9c48a6e");
    ("sender", `String "0xsender");
  ] in
  (match Traderjoe.Types.swap_of_yojson swap_json with
   | Ok swap ->
     test_assert "Swap parses" true;
     test_assert "Swap timestamp" (String.equal swap.timestamp "1640000000");
     test_assert "Swap amountIn" (String.equal swap.amountIn "1000000000000000000");
     test_assert "Swap amountOut" (String.equal swap.amountOut "50000000");
     test_assert "Swap tokenIn" (String.equal swap.tokenIn "0xb31f66aa3c1e785363f0875a1b74e27b85fd66c7");
     test_assert "Swap tokenOut" (String.equal swap.tokenOut "0xb97ef9ef8734c71904d8002f8b6bc66dd9c48a6e")
   | Error _ -> fail "Swap parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Traderjoe.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Traderjoe.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Traderjoe.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Traderjoe.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection");

  let json_err : Traderjoe.Rest.error = `Json_parse "Invalid JSON" in
  let json_str = Sexplib.Sexp.to_string_hum (Traderjoe.Rest.sexp_of_error json_err) in
  test_assert "JSON error to_sexp" (String.is_substring json_str ~substring:"Invalid")

let () =
  printf "===========================================\n";
  printf "Trader Joe DEX Unit Tests\n";
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

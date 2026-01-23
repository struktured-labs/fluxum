(** Velodrome DEX Unit Tests *)

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
  test_assert "Optimism URL" (String.is_substring Velodrome.Cfg.optimism.subgraph_url ~substring:"velodrome");
  test_assert "Production is Optimism" (String.equal Velodrome.Cfg.production.subgraph_url Velodrome.Cfg.optimism.subgraph_url)

let test_types () =
  printf "\n[Types]\n";
  let token_json = `Assoc [("id", `String "0x1"); ("symbol", `String "OP"); ("decimals", `String "18")] in
  (match Velodrome.Types.token_of_yojson token_json with
   | Ok t -> test_assert "Token parses" true; test_assert "Token symbol" (String.equal t.symbol "OP")
   | Error _ -> fail "Token parsing failed");

  let pair_json = `Assoc [
    ("id", `String "0x2");
    ("token0", `Assoc [("id", `String "0x1"); ("symbol", `String "OP"); ("decimals", `String "18")]);
    ("token1", `Assoc [("id", `String "0x3"); ("symbol", `String "USDC"); ("decimals", `String "6")]);
    ("stable", `Bool false);
    ("reserve0", `String "1000");
    ("reserve1", `String "2000");
    ("totalSupply", `String "3000");
    ("reserveUSD", `String "4000");
  ] in
  (match Velodrome.Types.pair_of_yojson pair_json with
   | Ok p -> test_assert "Pair parses" true; test_assert "Pair stable" (Bool.equal p.stable false)
   | Error _ -> fail "Pair parsing failed");

  let swap_json = `Assoc [
    ("id", `String "0x4");
    ("timestamp", `String "1640000000");
    ("amount0In", `String "100");
    ("amount1In", `String "0");
    ("amount0Out", `String "0");
    ("amount1Out", `String "200");
    ("to", `String "0x5");
  ] in
  (match Velodrome.Types.swap_of_yojson swap_json with
   | Ok s -> test_assert "Swap parses" true; test_assert "Swap to" (String.equal s.to_ "0x5")
   | Error _ -> fail "Swap parsing failed")

let test_errors () =
  printf "\n[Error Types]\n";
  let http_err : Velodrome.Rest.error = `Http (404, "Not found") in
  test_assert "HTTP error" (Sexplib.Sexp.to_string_hum (Velodrome.Rest.sexp_of_error http_err) |> String.is_substring ~substring:"404")

let () =
  printf "===========================================\n";
  printf "Velodrome DEX Unit Tests\n";
  printf "===========================================\n";
  test_cfg (); test_types (); test_errors ();
  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";
  (match !tests_failed > 0 with true -> exit 1 | false -> ())

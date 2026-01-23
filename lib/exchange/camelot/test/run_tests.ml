(** Camelot DEX Unit Tests *)

open Core

let tests_run = ref 0
let tests_passed = ref 0

let pass msg =
  incr tests_run; incr tests_passed; printf "  * %s\n" msg

let test_assert name condition =
  match condition with true -> pass name | false -> failwith ("FAIL: " ^ name)

let () =
  printf "===========================================\n";
  printf "Camelot DEX Unit Tests\n";
  printf "===========================================\n";
  printf "\n[Configuration]\n";
  test_assert "Arbitrum URL" (String.is_substring Camelot.Cfg.arbitrum.subgraph_url ~substring:"camelot");
  test_assert "Production is Arbitrum" (String.equal Camelot.Cfg.production.subgraph_url Camelot.Cfg.arbitrum.subgraph_url);
  printf "\n[Types]\n";
  let t = `Assoc [("id", `String "0x1"); ("symbol", `String "ARB"); ("decimals", `String "18")] in
  (match Camelot.Types.token_of_yojson t with
   | Ok tok -> test_assert "Token parses" true; test_assert "Token symbol" (String.equal tok.symbol "ARB")
   | Error _ -> failwith "Token parse failed");
  let p = `Assoc [
    ("id", `String "0x2");
    ("token0", `Assoc [("id", `String "0x1"); ("symbol", `String "ARB"); ("decimals", `String "18")]);
    ("token1", `Assoc [("id", `String "0x3"); ("symbol", `String "USDC"); ("decimals", `String "6")]);
    ("reserve0", `String "1000");
    ("reserve1", `String "2000");
    ("totalSupply", `String "3000");
    ("reserveUSD", `String "4000");
  ] in
  (match Camelot.Types.pair_of_yojson p with
   | Ok pair -> test_assert "Pair parses" true; test_assert "Pair token0 symbol" (String.equal pair.token0.symbol "ARB")
   | Error _ -> failwith "Pair parse failed");
  printf "\n===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "===========================================\n"

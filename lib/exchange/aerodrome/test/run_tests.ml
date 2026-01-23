(** Aerodrome DEX Unit Tests *)

open Core

let tests_run = ref 0
let tests_passed = ref 0

let pass msg =
  incr tests_run; incr tests_passed; printf "  * %s\n" msg

let test_assert name condition =
  match condition with true -> pass name | false -> failwith ("FAIL: " ^ name)

let () =
  printf "===========================================\n";
  printf "Aerodrome DEX Unit Tests\n";
  printf "===========================================\n";
  printf "\n[Configuration]\n";
  test_assert "Base URL" (String.is_substring Aerodrome.Cfg.base.subgraph_url ~substring:"aerodrome");
  test_assert "Production is Base" (String.equal Aerodrome.Cfg.production.subgraph_url Aerodrome.Cfg.base.subgraph_url);
  printf "\n[Types]\n";
  let t = `Assoc [("id", `String "0x1"); ("symbol", `String "ETH"); ("decimals", `String "18")] in
  (match Aerodrome.Types.token_of_yojson t with
   | Ok tok -> test_assert "Token parses" true; test_assert "Token symbol" (String.equal tok.symbol "ETH")
   | Error _ -> failwith "Token parse failed");
  printf "\n===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "===========================================\n"

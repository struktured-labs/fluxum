open Core
let tests_run = ref 0
let tests_passed = ref 0
let pass msg = incr tests_run; incr tests_passed; printf "  * %s\n" msg
let test_assert name condition = match condition with true -> pass name | false -> failwith ("FAIL: " ^ name)
let () =
  printf "===========================================\n";
  printf "Thena DEX Unit Tests\n";
  printf "===========================================\n";
  printf "\n[Configuration]\n";
  test_assert "BSC URL" (String.is_substring Thena.Cfg.bsc.subgraph_url ~substring:"thena");
  test_assert "Production is BSC" (String.equal Thena.Cfg.production.subgraph_url Thena.Cfg.bsc.subgraph_url);
  printf "\n[Types]\n";
  let t = `Assoc [("id", `String "0x1"); ("symbol", `String "BNB"); ("decimals", `String "18")] in
  (match Thena.Types.token_of_yojson t with | Ok _tok -> test_assert "Token parses" true | Error _ -> failwith "Token parse failed");
  printf "\n===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "===========================================\n"

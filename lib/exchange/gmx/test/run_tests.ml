open Core
let tests_run = ref 0
let tests_passed = ref 0
let pass msg = incr tests_run; incr tests_passed; printf "  * %s\n" msg
let test_assert name condition = match condition with true -> pass name | false -> failwith ("FAIL: " ^ name)
let () =
  printf "===========================================\n";
  printf "GMX DEX Unit Tests\n";
  printf "===========================================\n";
  printf "\n[Configuration]\n";
  test_assert "Arbitrum URL" (String.is_substring Gmx.Cfg.arbitrum.subgraph_url ~substring:"gmx");
  test_assert "Avalanche URL" (String.is_substring Gmx.Cfg.avalanche.subgraph_url ~substring:"gmx");
  printf "\n[Types]\n";
  let t = `Assoc [("id", `String "0x1"); ("symbol", `String "ETH"); ("decimals", `Int 18)] in
  (match Gmx.Types.token_of_yojson t with | Ok _tok -> test_assert "Token parses" true | Error _ -> failwith "Failed");
  printf "\n===========================================\n";
  printf "Total: %d | Passed: %d\n" !tests_run !tests_passed;
  printf "===========================================\n"

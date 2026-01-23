open Core
let tests_run = ref 0
let tests_passed = ref 0
let pass msg = incr tests_run; incr tests_passed; printf "  * %s\n" msg
let test_assert name condition = match condition with true -> pass name | false -> failwith ("FAIL: " ^ name)
let () =
  printf "===========================================\n";
  printf "Osmosis DEX Unit Tests\n";
  printf "===========================================\n";
  printf "\n[Configuration]\n";
  test_assert "Mainnet URL" (String.is_substring Osmosis.Cfg.mainnet.api_url ~substring:"osmosis");
  printf "\n[Types]\n";
  let ti = `Assoc [("denom", `String "uosmo"); ("amount", `String "1000000")] in
  (match Osmosis.Types.token_info_of_yojson ti with | Ok _t -> test_assert "Token info parses" true | Error _ -> failwith "Failed");
  printf "\n===========================================\n";
  printf "Total: %d | Passed: %d\n" !tests_run !tests_passed;
  printf "===========================================\n"

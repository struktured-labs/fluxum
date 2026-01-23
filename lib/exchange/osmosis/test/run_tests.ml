(** Osmosis DEX Unit Tests *)

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
  test_assert "Mainnet URL" (String.is_substring Osmosis.Cfg.mainnet.api_url ~substring:"osmosis");
  test_assert "Production is Mainnet" (String.equal Osmosis.Cfg.production.api_url Osmosis.Cfg.mainnet.api_url)

let test_types () =
  printf "\n[Types]\n";

  let token_info_json = `Assoc [
    ("denom", `String "uosmo");
    ("amount", `String "1000000000");
  ] in
  (match Osmosis.Types.token_info_of_yojson token_info_json with
   | Ok ti ->
     test_assert "Token info parses" true;
     test_assert "Token denom" (String.equal ti.denom "uosmo");
     test_assert "Token amount" (String.equal ti.amount "1000000000")
   | Error _ -> fail "Token info parsing failed");

  let pool_asset_json = `Assoc [
    ("token", `Assoc [
      ("denom", `String "uosmo");
      ("amount", `String "1000000000");
    ]);
    ("weight", `String "5368709120");
  ] in
  (match Osmosis.Types.pool_asset_of_yojson pool_asset_json with
   | Ok pa ->
     test_assert "Pool asset parses" true;
     test_assert "Pool asset weight" (String.equal pa.weight "5368709120")
   | Error _ -> fail "Pool asset parsing failed");

  let swap_msg_json = `Assoc [
    ("pool_id", `String "1");
    ("token_in_denom", `String "uosmo");
    ("token_out_denom", `String "uatom");
    ("token_in_amount", `String "1000000");
    ("token_out_min_amount", `String "900000");
  ] in
  (match Osmosis.Types.swap_msg_of_yojson swap_msg_json with
   | Ok sm ->
     test_assert "Swap msg parses" true;
     test_assert "Swap pool_id" (String.equal sm.pool_id "1");
     test_assert "Swap token_in_denom" (String.equal sm.token_in_denom "uosmo")
   | Error _ -> fail "Swap msg parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Osmosis.Rest.error = `Http (404, "Not found") in
  let err_str = Sexplib.Sexp.to_string_hum (Osmosis.Rest.sexp_of_error http_err) in
  test_assert "HTTP error to_sexp" (String.is_substring err_str ~substring:"404");

  let network_err : Osmosis.Rest.error = `Network "Connection timeout" in
  let net_str = Sexplib.Sexp.to_string_hum (Osmosis.Rest.sexp_of_error network_err) in
  test_assert "Network error to_sexp" (String.is_substring net_str ~substring:"Connection")

let () =
  printf "===========================================\n";
  printf "Osmosis DEX Unit Tests\n";
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

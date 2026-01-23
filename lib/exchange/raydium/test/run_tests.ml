(** Raydium DEX Unit Tests *)

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

  test_assert "Production swap URL"
    (String.is_prefix Raydium.Cfg.production.swap_url ~prefix:"https://api.raydium.io");

  test_assert "Mainnet is production"
    (String.equal Raydium.Cfg.mainnet.swap_url Raydium.Cfg.production.swap_url)

let test_types () =
  printf "\n[Types]\n";

  let quote_json = `Assoc [
    ("inputMint", `String "So11111111111111111111111111111111111111112");
    ("outputMint", `String "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v");
    ("inAmount", `String "1000000000");
    ("outAmount", `String "50000000");
    ("priceImpact", `Float 0.001);
    ("minOutAmount", `String "49500000");
    ("swapMode", `String "ExactIn");
  ] in
  (match Raydium.Types.swap_quote_of_yojson quote_json with
   | Ok quote ->
     test_assert "Swap quote parses" true;
     test_assert "Quote input mint" (String.equal quote.inputMint "So11111111111111111111111111111111111111112");
     test_assert "Quote out amount" (String.equal quote.outAmount "50000000");
     test_assert "Quote price impact" Float.(abs (quote.priceImpact -. 0.001) < 0.0001)
   | Error _ -> fail "Swap quote parsing failed");

  let pair_json = `Assoc [
    ("name", `String "SOL-USDC");
    ("ammId", `String "58oQChx4yWmvKdwLLZzBi4ChoCc2fqCUWBkwMihLYQo2");
    ("lpMint", `String "8HoQnePLqPj4M7PUDzfw8e3Ymdwgc7NLGnaTUapubyvu");
    ("price", `Float 50.0);
    ("volume24h", `Float 1000000.0);
    ("liquidity", `Float 5000000.0);
  ] in
  (match Raydium.Types.pair_info_of_yojson pair_json with
   | Ok pair ->
     test_assert "Pair info parses" true;
     test_assert "Pair name" (String.equal pair.name "SOL-USDC");
     test_assert "Pair price" Float.(abs (pair.price -. 50.0) < 0.1);
     test_assert "Pair volume" Float.(abs (pair.volume24h -. 1000000.0) < 1.0)
   | Error _ -> fail "Pair info parsing failed")

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Raydium.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Raydium.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404");

  let json_err : Raydium.Rest.Error.t = `Json_parse "bad json" in
  let err_str = Raydium.Rest.Error.to_string json_err in
  test_assert "JSON error to_string"
    (String.is_substring err_str ~substring:"JSON")

let () =
  printf "===========================================\n";
  printf "Raydium DEX Unit Tests (Solana)\n";
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

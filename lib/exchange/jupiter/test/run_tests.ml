open Core

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  ✓ %s\n" msg

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  ✗ FAIL: %s\n" msg

let assert_error result msg =
  incr tests_run;
  match result with
  | Error err ->
    incr tests_passed;
    printf "  ✓ %s: %s\n" msg err;
    true
  | Ok _ ->
    incr tests_failed;
    printf "  ✗ FAIL: %s (expected error, got Ok)\n" msg;
    false

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) <= tolerance) with
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false

(* ============================================================ *)
(* Normalize Function Tests *)
(* ============================================================ *)

let test_normalize_ticker_valid () =
  printf "\n[Normalize] Ticker with valid data\n";
  let quote : Jupiter.Rest.Types.quote = {
    inputMint = "So11111111111111111111111111111111111111112";  (* SOL *)
    inAmount = "1000000000";  (* 1 SOL in lamports *)
    outputMint = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v";  (* USDC *)
    outAmount = "50000000";  (* 50 USDC (6 decimals) *)
    otherAmountThreshold = "49500000";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  match Jupiter.Fluxum_adapter.Adapter.Normalize.ticker quote with
  | Ok t ->
    (* Price is raw amounts: 50000000/1000000000 = 0.05 *)
    ignore (assert_float_equal 0.05 t.last_price "Calculated raw price");
    ignore (assert_float_equal 0.05 t.bid_price "Bid price equals last");
    pass "Valid ticker normalized"
  | Error msg ->
    fail (sprintf "Ticker normalization failed: %s" msg)

let test_normalize_ticker_invalid_in_amount () =
  printf "\n[Normalize] Ticker with invalid inAmount\n";
  let quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "not_a_number";
    outputMint = "USDC";
    outAmount = "50000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  ignore (assert_error
    (Jupiter.Fluxum_adapter.Adapter.Normalize.ticker quote)
    "Rejected invalid inAmount")

let test_normalize_ticker_invalid_out_amount () =
  printf "\n[Normalize] Ticker with invalid outAmount\n";
  let quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "1000000000";
    outputMint = "USDC";
    outAmount = "NaN";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  ignore (assert_error
    (Jupiter.Fluxum_adapter.Adapter.Normalize.ticker quote)
    "Rejected NaN outAmount")

let test_normalize_ticker_zero_in_amount () =
  printf "\n[Normalize] Ticker with zero inAmount (division by zero)\n";
  let quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "0";
    outputMint = "USDC";
    outAmount = "50000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  match Jupiter.Fluxum_adapter.Adapter.Normalize.ticker quote with
  | Ok t ->
    (* Division by zero should give infinity *)
    (match Float.is_finite t.last_price with
     | true -> fail "Zero inAmount should produce infinity/NaN"
     | false -> pass "Zero inAmount handled (infinity)")
  | Error msg ->
    pass (sprintf "Zero inAmount rejected: %s" msg)

let test_normalize_order_book_valid () =
  printf "\n[Normalize] Order book with valid quotes\n";
  let sell_quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "1000000000";  (* 1 SOL *)
    outputMint = "USDC";
    outAmount = "51000000";  (* 51 USDC - selling SOL *)
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  let buy_quote : Jupiter.Rest.Types.quote = {
    inputMint = "USDC";
    inAmount = "50000000";  (* 50 USDC - buying SOL *)
    outputMint = "SOL";
    outAmount = "1000000000";  (* 1 SOL *)
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  match Jupiter.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote) with
  | Ok b ->
    (* Raw amounts: sell_price = 51000000/1000000000 = 0.051, buy_price = 50000000/1000000000 = 0.05 *)
    (match List.hd b.asks with
     | Some level -> ignore (assert_float_equal 0.051 level.price "Ask price (raw)")
     | None -> failwith "Expected non-empty asks");
    (match List.hd b.bids with
     | Some level -> ignore (assert_float_equal 0.05 level.price "Bid price (raw)")
     | None -> failwith "Expected non-empty bids");
    pass "Synthetic order book created"
  | Error msg ->
    fail (sprintf "Order book normalization failed: %s" msg)

let test_normalize_order_book_invalid_sell_out () =
  printf "\n[Normalize] Order book with invalid sell outAmount\n";
  let sell_quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "1000000000";
    outputMint = "USDC";
    outAmount = "invalid";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  let buy_quote : Jupiter.Rest.Types.quote = {
    inputMint = "USDC";
    inAmount = "50000000";
    outputMint = "SOL";
    outAmount = "1000000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  ignore (assert_error
    (Jupiter.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected invalid sell outAmount")

let test_normalize_order_book_invalid_buy_in () =
  printf "\n[Normalize] Order book with invalid buy inAmount\n";
  let sell_quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "1000000000";
    outputMint = "USDC";
    outAmount = "51000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  let buy_quote : Jupiter.Rest.Types.quote = {
    inputMint = "USDC";
    inAmount = "NaN";
    outputMint = "SOL";
    outAmount = "1000000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  ignore (assert_error
    (Jupiter.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected NaN buy inAmount")

let test_normalize_order_book_negative_amounts () =
  printf "\n[Normalize] Order book with negative amounts\n";
  let sell_quote : Jupiter.Rest.Types.quote = {
    inputMint = "SOL";
    inAmount = "-1000000000";
    outputMint = "USDC";
    outAmount = "51000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  let buy_quote : Jupiter.Rest.Types.quote = {
    inputMint = "USDC";
    inAmount = "50000000";
    outputMint = "SOL";
    outAmount = "1000000000";
    otherAmountThreshold = "0";
    swapMode = "ExactIn";
    slippageBps = 50;
    priceImpactPct = "0.1";
    routePlan = [];
    contextSlot = None;
    timeTaken = None;
  } in
  ignore (assert_error
    (Jupiter.Fluxum_adapter.Adapter.Normalize.order_book (sell_quote, buy_quote))
    "Rejected negative amounts")

(* ============================================================ *)
(* Main Test Runner *)
(* ============================================================ *)

let () =
  printf "\n";
  printf "===========================================\n";
  printf "Jupiter DEX Aggregator Unit Tests\n";
  printf "===========================================\n";

  (* Ticker tests *)
  test_normalize_ticker_valid ();
  test_normalize_ticker_invalid_in_amount ();
  test_normalize_ticker_invalid_out_amount ();
  test_normalize_ticker_zero_in_amount ();

  (* Order book tests *)
  test_normalize_order_book_valid ();
  test_normalize_order_book_invalid_sell_out ();
  test_normalize_order_book_invalid_buy_in ();
  test_normalize_order_book_negative_amounts ();

  (* Summary *)
  printf "\n";
  printf "===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d ✓\n" !tests_passed;
  printf "Failed:       %d ✗\n" !tests_failed;
  printf "Success rate: %.1f%%\n" (Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0);
  printf "===========================================\n";
  printf "\n";

  match !tests_failed with
  | 0 -> exit 0
  | _ -> exit 1

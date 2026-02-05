(** Unit tests for Normalize_common - Shared normalization utilities

    These tests verify that the shared conversion functions handle
    all exchange variations correctly.
*)

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

(* ============================================================ *)
(* Side Conversion Tests *)
(* ============================================================ *)

let test_side_conversions () =
  printf "\n=== Side Conversions ===\n";

  (* Test lowercase variations *)
  (match Fluxum.Normalize_common.Side.of_string "buy" with
   | Ok Fluxum.Types.Side.Buy -> pass "buy -> Buy"
   | _ -> fail "buy should be Buy");

  (match Fluxum.Normalize_common.Side.of_string "sell" with
   | Ok Fluxum.Types.Side.Sell -> pass "sell -> Sell"
   | _ -> fail "sell should be Sell");

  (* Test uppercase variations (Binance) *)
  (match Fluxum.Normalize_common.Side.of_string "BUY" with
   | Ok Fluxum.Types.Side.Buy -> pass "BUY -> Buy"
   | _ -> fail "BUY should be Buy");

  (match Fluxum.Normalize_common.Side.of_string "SELL" with
   | Ok Fluxum.Types.Side.Sell -> pass "SELL -> Sell"
   | _ -> fail "SELL should be Sell");

  (* Test abbreviations (Kraken) *)
  (match Fluxum.Normalize_common.Side.of_string "b" with
   | Ok Fluxum.Types.Side.Buy -> pass "b -> Buy"
   | _ -> fail "b should be Buy");

  (match Fluxum.Normalize_common.Side.of_string "s" with
   | Ok Fluxum.Types.Side.Sell -> pass "s -> Sell"
   | _ -> fail "s should be Sell");

  (* Test mixed case *)
  (match Fluxum.Normalize_common.Side.of_string "Buy" with
   | Ok Fluxum.Types.Side.Buy -> pass "Buy -> Buy"
   | _ -> fail "Buy should be Buy");

  (* Test bid/ask (some exchanges use this) *)
  (match Fluxum.Normalize_common.Side.of_string "bid" with
   | Ok Fluxum.Types.Side.Buy -> pass "bid -> Buy"
   | _ -> fail "bid should be Buy");

  (match Fluxum.Normalize_common.Side.of_string "ask" with
   | Ok Fluxum.Types.Side.Sell -> pass "ask -> Sell"
   | _ -> fail "ask should be Sell");

  (* Test error case *)
  (match Fluxum.Normalize_common.Side.of_string "invalid" with
   | Error msg -> pass (sprintf "Rejected invalid side: %s" msg)
   | Ok _ -> fail "Should reject invalid side");

  (* Test default fallback *)
  let default_result = Fluxum.Normalize_common.Side.of_string_exn ~default:Fluxum.Types.Side.Sell "invalid" in
  (match default_result with
   | Fluxum.Types.Side.Sell -> pass "of_string_exn uses default on error"
   | _ -> fail "of_string_exn should use default");

  ()

(* ============================================================ *)
(* Order Status Conversion Tests *)
(* ============================================================ *)

let test_order_status_conversions () =
  printf "\n=== Order Status Conversions ===\n";

  (* Test "new" variations *)
  (match Fluxum.Normalize_common.Order_status.of_string "new" with
   | Ok Fluxum.Types.Order_status.New -> pass "new -> New"
   | _ -> fail "new should be New");

  (match Fluxum.Normalize_common.Order_status.of_string "pending" with
   | Ok Fluxum.Types.Order_status.New -> pass "pending -> New"
   | _ -> fail "pending should be New");

  (match Fluxum.Normalize_common.Order_status.of_string "open" with
   | Ok Fluxum.Types.Order_status.New -> pass "open -> New"
   | _ -> fail "open should be New");

  (* Test "filled" variations *)
  (match Fluxum.Normalize_common.Order_status.of_string "filled" with
   | Ok Fluxum.Types.Order_status.Filled -> pass "filled -> Filled"
   | _ -> fail "filled should be Filled");

  (match Fluxum.Normalize_common.Order_status.of_string "FILLED" with
   | Ok Fluxum.Types.Order_status.Filled -> pass "FILLED -> Filled"
   | _ -> fail "FILLED should be Filled");

  (match Fluxum.Normalize_common.Order_status.of_string "closed" with
   | Ok Fluxum.Types.Order_status.Filled -> pass "closed -> Filled"
   | _ -> fail "closed should be Filled");

  (match Fluxum.Normalize_common.Order_status.of_string "executed" with
   | Ok Fluxum.Types.Order_status.Filled -> pass "executed -> Filled"
   | _ -> fail "executed should be Filled");

  (* Test "partially_filled" variations *)
  (match Fluxum.Normalize_common.Order_status.of_string "partially_filled" with
   | Ok Fluxum.Types.Order_status.Partially_filled -> pass "partially_filled -> Partially_filled"
   | _ -> fail "partially_filled should be Partially_filled");

  (match Fluxum.Normalize_common.Order_status.of_string "PARTIALLY_FILLED" with
   | Ok Fluxum.Types.Order_status.Partially_filled -> pass "PARTIALLY_FILLED -> Partially_filled"
   | _ -> fail "PARTIALLY_FILLED should be Partially_filled");

  (match Fluxum.Normalize_common.Order_status.of_string "partial" with
   | Ok Fluxum.Types.Order_status.Partially_filled -> pass "partial -> Partially_filled"
   | _ -> fail "partial should be Partially_filled");

  (* Test "canceled" variations *)
  (match Fluxum.Normalize_common.Order_status.of_string "canceled" with
   | Ok Fluxum.Types.Order_status.Canceled -> pass "canceled -> Canceled"
   | _ -> fail "canceled should be Canceled");

  (match Fluxum.Normalize_common.Order_status.of_string "cancelled" with
   | Ok Fluxum.Types.Order_status.Canceled -> pass "cancelled -> Canceled"
   | _ -> fail "cancelled should be Canceled");

  (match Fluxum.Normalize_common.Order_status.of_string "CANCELED" with
   | Ok Fluxum.Types.Order_status.Canceled -> pass "CANCELED -> Canceled"
   | _ -> fail "CANCELED should be Canceled");

  (match Fluxum.Normalize_common.Order_status.of_string "expired" with
   | Ok Fluxum.Types.Order_status.Canceled -> pass "expired -> Canceled"
   | _ -> fail "expired should be Canceled");

  (* Test "rejected" *)
  (match Fluxum.Normalize_common.Order_status.of_string "rejected" with
   | Ok (Fluxum.Types.Order_status.Rejected _) -> pass "rejected -> Rejected"
   | _ -> fail "rejected should be Rejected");

  (match Fluxum.Normalize_common.Order_status.of_string "REJECTED" with
   | Ok (Fluxum.Types.Order_status.Rejected _) -> pass "REJECTED -> Rejected"
   | _ -> fail "REJECTED should be Rejected");

  (* Test error case *)
  (match Fluxum.Normalize_common.Order_status.of_string "invalid" with
   | Error msg -> pass (sprintf "Rejected invalid status: %s" msg)
   | Ok _ -> fail "Should reject invalid status");

  ()

(* ============================================================ *)
(* Order Type Conversion Tests *)
(* ============================================================ *)

let test_order_type_conversions () =
  printf "\n=== Order Type Conversions ===\n";

  (* Test "market" *)
  (match Fluxum.Normalize_common.Order_type.of_string "market" with
   | Ok (Fluxum.Types.Order_kind.Basic Market) -> pass "market -> Market"
   | _ -> fail "market should be Market");

  (match Fluxum.Normalize_common.Order_type.of_string "MARKET" with
   | Ok (Fluxum.Types.Order_kind.Basic Market) -> pass "MARKET -> Market"
   | _ -> fail "MARKET should be Market");

  (* Test "limit" *)
  (match Fluxum.Normalize_common.Order_type.of_string "limit" with
   | Ok (Fluxum.Types.Order_kind.Basic (Limit _)) -> pass "limit -> Limit"
   | _ -> fail "limit should be Limit");

  (match Fluxum.Normalize_common.Order_type.of_string "LIMIT" with
   | Ok (Fluxum.Types.Order_kind.Basic (Limit _)) -> pass "LIMIT -> Limit"
   | _ -> fail "LIMIT should be Limit");

  (* Test "post_only" / "limit_maker" *)
  (match Fluxum.Normalize_common.Order_type.of_string "post_only" with
   | Ok (Fluxum.Types.Order_kind.Basic (Post_only _)) -> pass "post_only -> Post_only"
   | _ -> fail "post_only should be Post_only");

  (match Fluxum.Normalize_common.Order_type.of_string "maker_only" with
   | Ok (Fluxum.Types.Order_kind.Basic (Post_only _)) -> pass "maker_only -> Post_only"
   | _ -> fail "maker_only should be Post_only");

  (match Fluxum.Normalize_common.Order_type.of_string "limit_maker" with
   | Ok (Fluxum.Types.Order_kind.Basic (Post_only _)) -> pass "limit_maker -> Post_only"
   | _ -> fail "limit_maker should be Post_only");

  (* Test stop orders *)
  (match Fluxum.Normalize_common.Order_type.of_string "stop_loss" with
   | Ok (Fluxum.Types.Order_kind.Conditional (Stop_market _)) -> pass "stop_loss -> Stop_market"
   | _ -> fail "stop_loss should be Stop_market");

  (match Fluxum.Normalize_common.Order_type.of_string "stop_limit" with
   | Ok (Fluxum.Types.Order_kind.Conditional (Stop_limit _)) -> pass "stop_limit -> Stop_limit"
   | _ -> fail "stop_limit should be Stop_limit");

  (* Test error case *)
  (match Fluxum.Normalize_common.Order_type.of_string "invalid" with
   | Error msg -> pass (sprintf "Rejected invalid type: %s" msg)
   | Ok _ -> fail "Should reject invalid type");

  ()

(* ============================================================ *)
(* Float Conversion Tests *)
(* ============================================================ *)

let test_float_conversions () =
  printf "\n=== Float Conversions ===\n";

  (* Test valid float *)
  (match Fluxum.Normalize_common.Float_conv.of_string "123.456" with
   | Ok f when Float.(abs (f - 123.456) < 0.0001) -> pass "Valid float parsed"
   | _ -> fail "Should parse valid float");

  (* Test price (must be positive) *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "50000.25" with
   | Ok f when Float.(abs (f - 50000.25) < 0.01) -> pass "Valid price parsed"
   | _ -> fail "Should parse valid price");

  (match Fluxum.Normalize_common.Float_conv.price_of_string "-100.0" with
   | Error msg -> pass (sprintf "Rejected negative price: %s" msg)
   | Ok _ -> fail "Should reject negative price");

  (match Fluxum.Normalize_common.Float_conv.price_of_string "0.0" with
   | Error _ -> pass "Rejected zero price"
   | Ok _ -> fail "Should reject zero price");

  (* Test quantity (must be non-negative) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "10.5" with
   | Ok f when Float.(abs (f - 10.5) < 0.01) -> pass "Valid quantity parsed"
   | _ -> fail "Should parse valid quantity");

  (match Fluxum.Normalize_common.Float_conv.qty_of_string "0.0" with
   | Ok f when Float.(abs f < 0.0001) -> pass "Zero quantity allowed"
   | _ -> fail "Should allow zero quantity");

  (match Fluxum.Normalize_common.Float_conv.qty_of_string "-5.0" with
   | Error msg -> pass (sprintf "Rejected negative quantity: %s" msg)
   | Ok _ -> fail "Should reject negative quantity");

  (* Test invalid float strings *)
  (match Fluxum.Normalize_common.Float_conv.of_string "not_a_number" with
   | Error msg -> pass (sprintf "Rejected non-numeric: %s" msg)
   | Ok _ -> fail "Should reject non-numeric string");

  (match Fluxum.Normalize_common.Float_conv.of_string "inf" with
   | Error msg -> pass (sprintf "Rejected infinity: %s" msg)
   | Ok _ -> fail "Should reject infinity");

  ()

(* ============================================================ *)
(* Result Utilities Tests *)
(* ============================================================ *)

let test_result_utilities () =
  printf "\n=== Result Utilities ===\n";

  (* Test transpose with all Ok *)
  let all_ok = [Ok 1; Ok 2; Ok 3] in
  (match Fluxum.Normalize_common.Result_util.transpose all_ok with
   | Ok [1; 2; 3] -> pass "Transposed all Ok results"
   | _ -> fail "Should transpose all Ok results");

  (* Test transpose with one Error *)
  let with_error = [Ok 1; Error "fail"; Ok 3] in
  (match Fluxum.Normalize_common.Result_util.transpose with_error with
   | Error "fail" -> pass "Error wins in transpose"
   | _ -> fail "Error should win in transpose");

  (* Test transpose with empty list *)
  (match Fluxum.Normalize_common.Result_util.transpose [] with
   | Ok [] -> pass "Empty list transposes to Ok []"
   | _ -> fail "Empty list should be Ok []");

  ()

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  Normalize_common - Unit Test Suite\n";
  printf "====================================================\n";

  test_side_conversions ();
  test_order_status_conversions ();
  test_order_type_conversions ();
  test_float_conversions ();
  test_result_utilities ();

  printf "\n";
  printf "====================================================\n";
  printf "  Test Results: %d run, %d passed, %d failed\n"
    !tests_run !tests_passed !tests_failed;
  printf "====================================================\n";

  match !tests_failed with
  | 0 ->
    printf "\n✅ All tests passed!\n\n";
    exit 0
  | n ->
    printf "\n❌ %d test(s) failed\n\n" n;
    exit 1

let () = run_all_tests ()

(** Unit tests for Account Operation Types

    Tests for Transfer_status, Deposit_address, Deposit, and Withdrawal types.
    These are the normalized types used across all exchanges for account operations.

    Test Categories:
    1. Transfer_status sexp roundtrip and string conversion
    2. Deposit_address creation, comparison, and equality
    3. Deposit creation and sexp roundtrip
    4. Withdrawal creation and sexp roundtrip
    5. Edge cases (empty strings, None values, special characters)
*)

open Core

module Types = Fluxum.Types

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  * %s\n" msg

let _ = pass  (* Suppress unused warning *)

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  X FAIL: %s\n" msg

let _ = fail  (* Suppress unused warning *)

let assert_true condition msg =
  incr tests_run;
  match condition with
  | true ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  X FAIL: %s\n" msg;
    false

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match equal expected actual with
  | false ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | true ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

let assert_string_equal expected actual msg =
  incr tests_run;
  match String.equal expected actual with
  | false ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false
  | true ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

(* ============================================================ *)
(* Transfer_status Tests *)
(* ============================================================ *)

let test_transfer_status_sexp_roundtrip () =
  printf "\n=== Transfer_status Sexp Roundtrip ===\n";

  let statuses = [
    Types.Transfer_status.Pending;
    Types.Transfer_status.Processing;
    Types.Transfer_status.Completed;
    Types.Transfer_status.Failed;
    Types.Transfer_status.Cancelled;
  ] in

  List.iter statuses ~f:(fun status ->
    let sexp = Types.Transfer_status.sexp_of_t status in
    let roundtrip = Types.Transfer_status.t_of_sexp sexp in
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      status roundtrip
      (sprintf "Roundtrip %s" (Types.Transfer_status.to_string status))))

let test_transfer_status_to_string () =
  printf "\n=== Transfer_status to_string ===\n";

  ignore (assert_string_equal "pending" (Types.Transfer_status.to_string Types.Transfer_status.Pending) "Pending -> pending");
  ignore (assert_string_equal "processing" (Types.Transfer_status.to_string Types.Transfer_status.Processing) "Processing -> processing");
  ignore (assert_string_equal "completed" (Types.Transfer_status.to_string Types.Transfer_status.Completed) "Completed -> completed");
  ignore (assert_string_equal "failed" (Types.Transfer_status.to_string Types.Transfer_status.Failed) "Failed -> failed");
  ignore (assert_string_equal "cancelled" (Types.Transfer_status.to_string Types.Transfer_status.Cancelled) "Cancelled -> cancelled")

let test_transfer_status_of_string () =
  printf "\n=== Transfer_status of_string_opt ===\n";

  (* Test lowercase *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    (Some Types.Transfer_status.Pending)
    (Types.Transfer_status.of_string_opt "pending")
    "pending -> Pending");

  (* Test capitalized *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    (Some Types.Transfer_status.Processing)
    (Types.Transfer_status.of_string_opt "Processing")
    "Processing -> Processing");

  (* Test uppercase *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    (Some Types.Transfer_status.Completed)
    (Types.Transfer_status.of_string_opt "COMPLETED")
    "COMPLETED -> Completed");

  (* Test complete alias *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    (Some Types.Transfer_status.Completed)
    (Types.Transfer_status.of_string_opt "complete")
    "complete -> Completed");

  (* Test canceled (American spelling) *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    (Some Types.Transfer_status.Cancelled)
    (Types.Transfer_status.of_string_opt "canceled")
    "canceled -> Cancelled");

  (* Test invalid *)
  ignore (assert_equal
    ~equal:[%equal: Types.Transfer_status.t option]
    ~sexp_of_t:[%sexp_of: Types.Transfer_status.t option]
    None
    (Types.Transfer_status.of_string_opt "invalid_status")
    "invalid_status -> None")

let test_transfer_status_compare () =
  printf "\n=== Transfer_status compare ===\n";

  let pending = Types.Transfer_status.Pending in
  let completed = Types.Transfer_status.Completed in

  ignore (assert_true
    (Types.Transfer_status.compare pending pending = 0)
    "Pending = Pending");

  ignore (assert_true
    (Types.Transfer_status.compare pending completed <> 0)
    "Pending <> Completed")

(* ============================================================ *)
(* Deposit_address Tests *)
(* ============================================================ *)

let test_deposit_address_creation () =
  printf "\n=== Deposit_address Creation ===\n";

  let addr = Types.Deposit_address.create
    ~venue:Types.Venue.Gemini
    ~currency:"BTC"
    ~address:"bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ()
  in

  ignore (assert_equal
    ~equal:Types.Venue.equal
    ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Gemini addr.venue
    "Venue is Gemini");

  ignore (assert_string_equal "BTC" addr.currency "Currency is BTC");
  ignore (assert_string_equal "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh" addr.address "Address matches");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    None addr.tag
    "Tag is None");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    None addr.network
    "Network is None")

let test_deposit_address_with_tag () =
  printf "\n=== Deposit_address with Tag ===\n";

  let addr = Types.Deposit_address.create
    ~venue:Types.Venue.Binance
    ~currency:"XRP"
    ~address:"rEb8TK3gBgk5auZkwc6sHnwrGVJH8DuaLh"
    ~tag:"123456789"
    ~network:"XRP"
    ()
  in

  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "123456789") addr.tag
    "Tag is set");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "XRP") addr.network
    "Network is set")

let test_deposit_address_equality () =
  printf "\n=== Deposit_address Equality ===\n";

  let addr1 = Types.Deposit_address.create
    ~venue:Types.Venue.Kraken
    ~currency:"ETH"
    ~address:"0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ()
  in

  let addr2 = Types.Deposit_address.create
    ~venue:Types.Venue.Kraken
    ~currency:"ETH"
    ~address:"0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ()
  in

  let addr3 = Types.Deposit_address.create
    ~venue:Types.Venue.Gemini
    ~currency:"ETH"
    ~address:"0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ()
  in

  ignore (assert_true
    (Types.Deposit_address.equal addr1 addr2)
    "Same addresses are equal");

  ignore (assert_true
    (not (Types.Deposit_address.equal addr1 addr3))
    "Different venues are not equal")

let test_deposit_address_sexp_roundtrip () =
  printf "\n=== Deposit_address Sexp Roundtrip ===\n";

  let addr = Types.Deposit_address.create
    ~venue:Types.Venue.Coinbase
    ~currency:"USDC"
    ~address:"0xabc123"
    ~network:"ERC20"
    ()
  in

  let sexp = Types.Deposit_address.sexp_of_t addr in
  let roundtrip = Types.Deposit_address.t_of_sexp sexp in

  ignore (assert_true
    (Types.Deposit_address.equal addr roundtrip)
    "Sexp roundtrip preserves equality")

let test_deposit_address_edge_cases () =
  printf "\n=== Deposit_address Edge Cases ===\n";

  (* Empty string currency - should be allowed *)
  let empty_currency = Types.Deposit_address.create
    ~venue:Types.Venue.Gemini
    ~currency:""
    ~address:"test_address"
    ()
  in
  ignore (assert_string_equal "" empty_currency.currency "Empty currency allowed");

  (* Very long address *)
  let long_addr = String.make 500 'x' in
  let long_address = Types.Deposit_address.create
    ~venue:Types.Venue.Binance
    ~currency:"BTC"
    ~address:long_addr
    ()
  in
  ignore (assert_true
    (String.length long_address.address = 500)
    "Long address (500 chars) allowed");

  (* Special characters in tag *)
  let special_tag = Types.Deposit_address.create
    ~venue:Types.Venue.Kraken
    ~currency:"XLM"
    ~address:"stellar_address"
    ~tag:"memo-with-special_chars!@#"
    ()
  in
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "memo-with-special_chars!@#") special_tag.tag
    "Special characters in tag allowed")

(* ============================================================ *)
(* Deposit Tests *)
(* ============================================================ *)

let test_deposit_creation () =
  printf "\n=== Deposit Creation ===\n";

  let deposit = Types.Deposit.create
    ~venue:Types.Venue.Gemini
    ~id:"dep_12345"
    ~currency:"BTC"
    ~amount:0.5
    ~status:Types.Transfer_status.Completed
    ()
  in

  ignore (assert_equal
    ~equal:Types.Venue.equal
    ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Gemini deposit.venue
    "Venue is Gemini");
  ignore (assert_string_equal "dep_12345" deposit.id "ID matches");
  ignore (assert_string_equal "BTC" deposit.currency "Currency is BTC");
  ignore (assert_float_equal 0.5 deposit.amount "Amount is 0.5");
  ignore (assert_equal
    ~equal:Types.Transfer_status.equal
    ~sexp_of_t:Types.Transfer_status.sexp_of_t
    Types.Transfer_status.Completed deposit.status
    "Status is Completed")

let test_deposit_with_all_fields () =
  printf "\n=== Deposit with All Fields ===\n";

  let now = Time_float_unix.now () in
  let deposit = Types.Deposit.create
    ~venue:Types.Venue.Binance
    ~id:"dep_67890"
    ~currency:"ETH"
    ~amount:10.25
    ~status:Types.Transfer_status.Processing
    ~address:"0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ~tx_id:"0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
    ~created_at:now
    ~updated_at:now
    ()
  in

  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2") deposit.address
    "Address is set");
  ignore (assert_true (Option.is_some deposit.tx_id) "Transaction ID is set");
  ignore (assert_true (Option.is_some deposit.created_at) "Created_at is set");
  ignore (assert_true (Option.is_some deposit.updated_at) "Updated_at is set")

let test_deposit_sexp_roundtrip () =
  printf "\n=== Deposit Sexp Roundtrip ===\n";

  let deposit = Types.Deposit.create
    ~venue:Types.Venue.Kraken
    ~id:"test_deposit"
    ~currency:"USDT"
    ~amount:1000.0
    ~status:Types.Transfer_status.Pending
    ~address:"TRx123456"
    ()
  in

  let sexp = Types.Deposit.sexp_of_t deposit in
  let roundtrip = Types.Deposit.t_of_sexp sexp in

  ignore (assert_true
    (Types.Deposit.equal deposit roundtrip)
    "Sexp roundtrip preserves equality")

let test_deposit_edge_cases () =
  printf "\n=== Deposit Edge Cases ===\n";

  (* Zero amount *)
  let zero_deposit = Types.Deposit.create
    ~venue:Types.Venue.Gemini
    ~id:"zero_dep"
    ~currency:"BTC"
    ~amount:0.0
    ~status:Types.Transfer_status.Pending
    ()
  in
  ignore (assert_float_equal 0.0 zero_deposit.amount "Zero amount allowed");

  (* Very small amount *)
  let dust_deposit = Types.Deposit.create
    ~venue:Types.Venue.Binance
    ~id:"dust_dep"
    ~currency:"BTC"
    ~amount:0.00000001
    ~status:Types.Transfer_status.Completed
    ()
  in
  ignore (assert_float_equal 0.00000001 dust_deposit.amount ~tolerance:0.000000001 "Dust amount (1 satoshi) allowed");

  (* Very large amount *)
  let large_deposit = Types.Deposit.create
    ~venue:Types.Venue.Coinbase
    ~id:"large_dep"
    ~currency:"USDT"
    ~amount:999999999999.99
    ~status:Types.Transfer_status.Completed
    ()
  in
  ignore (assert_float_equal 999999999999.99 large_deposit.amount ~tolerance:0.01 "Large amount allowed")

(* ============================================================ *)
(* Withdrawal Tests *)
(* ============================================================ *)

let test_withdrawal_creation () =
  printf "\n=== Withdrawal Creation ===\n";

  let withdrawal = Types.Withdrawal.create
    ~venue:Types.Venue.Gemini
    ~id:"wdl_12345"
    ~currency:"BTC"
    ~amount:1.0
    ~status:Types.Transfer_status.Pending
    ~address:"bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ()
  in

  ignore (assert_equal
    ~equal:Types.Venue.equal
    ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Gemini withdrawal.venue
    "Venue is Gemini");
  ignore (assert_string_equal "wdl_12345" withdrawal.id "ID matches");
  ignore (assert_string_equal "BTC" withdrawal.currency "Currency is BTC");
  ignore (assert_float_equal 1.0 withdrawal.amount "Amount is 1.0");
  ignore (assert_string_equal "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh" withdrawal.address "Address matches");
  ignore (assert_equal
    ~equal:[%equal: float option]
    ~sexp_of_t:[%sexp_of: float option]
    None withdrawal.fee
    "Fee is None by default")

let test_withdrawal_with_fee () =
  printf "\n=== Withdrawal with Fee ===\n";

  let withdrawal = Types.Withdrawal.create
    ~venue:Types.Venue.Binance
    ~id:"wdl_fee_test"
    ~currency:"ETH"
    ~amount:5.0
    ~fee:0.005
    ~status:Types.Transfer_status.Completed
    ~address:"0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ()
  in

  ignore (assert_equal
    ~equal:[%equal: float option]
    ~sexp_of_t:[%sexp_of: float option]
    (Some 0.005) withdrawal.fee
    "Fee is set to 0.005");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    None withdrawal.tag
    "Tag is None");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    None withdrawal.tx_id
    "Transaction ID is None initially")

let test_withdrawal_with_tag () =
  printf "\n=== Withdrawal with Tag (XRP/XLM) ===\n";

  let withdrawal = Types.Withdrawal.create
    ~venue:Types.Venue.Kraken
    ~id:"wdl_xrp_test"
    ~currency:"XRP"
    ~amount:100.0
    ~fee:0.25
    ~status:Types.Transfer_status.Processing
    ~address:"rEb8TK3gBgk5auZkwc6sHnwrGVJH8DuaLh"
    ~tag:"destination_tag_12345"
    ~tx_id:"ABC123XYZ"
    ()
  in

  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "destination_tag_12345") withdrawal.tag
    "Tag is set");
  ignore (assert_equal
    ~equal:[%equal: string option]
    ~sexp_of_t:[%sexp_of: string option]
    (Some "ABC123XYZ") withdrawal.tx_id
    "Transaction ID is set")

let test_withdrawal_sexp_roundtrip () =
  printf "\n=== Withdrawal Sexp Roundtrip ===\n";

  (* Use a fixed timestamp with integer seconds to avoid float precision issues *)
  let fixed_time = Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec 1700000000.0) in
  let withdrawal = Types.Withdrawal.create
    ~venue:Types.Venue.Coinbase
    ~id:"wdl_roundtrip"
    ~currency:"USDC"
    ~amount:500.0
    ~fee:1.0
    ~status:Types.Transfer_status.Completed
    ~address:"0xabc123"
    ~tag:"memo123"
    ~tx_id:"txid_456"
    ~created_at:fixed_time
    ~updated_at:fixed_time
    ()
  in

  let sexp = Types.Withdrawal.sexp_of_t withdrawal in
  let roundtrip = Types.Withdrawal.t_of_sexp sexp in

  ignore (assert_true
    (Types.Withdrawal.equal withdrawal roundtrip)
    "Sexp roundtrip preserves equality")

let test_withdrawal_equality () =
  printf "\n=== Withdrawal Equality ===\n";

  let wdl1 = Types.Withdrawal.create
    ~venue:Types.Venue.Gemini
    ~id:"wdl_1"
    ~currency:"BTC"
    ~amount:1.0
    ~status:Types.Transfer_status.Pending
    ~address:"bc1qtest"
    ()
  in

  let wdl2 = Types.Withdrawal.create
    ~venue:Types.Venue.Gemini
    ~id:"wdl_1"
    ~currency:"BTC"
    ~amount:1.0
    ~status:Types.Transfer_status.Pending
    ~address:"bc1qtest"
    ()
  in

  let wdl3 = Types.Withdrawal.create
    ~venue:Types.Venue.Gemini
    ~id:"wdl_2"  (* Different ID *)
    ~currency:"BTC"
    ~amount:1.0
    ~status:Types.Transfer_status.Pending
    ~address:"bc1qtest"
    ()
  in

  ignore (assert_true
    (Types.Withdrawal.equal wdl1 wdl2)
    "Same withdrawals are equal");

  ignore (assert_true
    (not (Types.Withdrawal.equal wdl1 wdl3))
    "Different IDs are not equal")

let test_withdrawal_edge_cases () =
  printf "\n=== Withdrawal Edge Cases ===\n";

  (* Zero fee *)
  let zero_fee = Types.Withdrawal.create
    ~venue:Types.Venue.Binance
    ~id:"wdl_zero_fee"
    ~currency:"BNB"
    ~amount:10.0
    ~fee:0.0
    ~status:Types.Transfer_status.Completed
    ~address:"bnb_address"
    ()
  in
  ignore (assert_equal
    ~equal:[%equal: float option]
    ~sexp_of_t:[%sexp_of: float option]
    (Some 0.0) zero_fee.fee
    "Zero fee allowed");

  (* Amount equals fee (edge case for small withdrawals) *)
  let fee_equals_amount = Types.Withdrawal.create
    ~venue:Types.Venue.Kraken
    ~id:"wdl_fee_edge"
    ~currency:"BTC"
    ~amount:0.0001
    ~fee:0.0001
    ~status:Types.Transfer_status.Failed
    ~address:"bc1qtest"
    ()
  in
  ignore (assert_true
    (Float.(=) (Option.value ~default:0.0 fee_equals_amount.fee) fee_equals_amount.amount)
    "Fee equals amount edge case");

  (* All statuses for withdrawal *)
  let statuses = [
    Types.Transfer_status.Pending;
    Types.Transfer_status.Processing;
    Types.Transfer_status.Completed;
    Types.Transfer_status.Failed;
    Types.Transfer_status.Cancelled;
  ] in

  List.iter statuses ~f:(fun status ->
    let wdl = Types.Withdrawal.create
      ~venue:Types.Venue.Gemini
      ~id:(sprintf "wdl_%s" (Types.Transfer_status.to_string status))
      ~currency:"USD"
      ~amount:100.0
      ~status
      ~address:"test_address"
      ()
    in
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      status wdl.status
      (sprintf "Status %s preserved" (Types.Transfer_status.to_string status))))

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  Account Operation Types - Unit Test Suite\n";
  printf "====================================================\n";

  (* Transfer_status tests *)
  printf "\n=== Transfer_status Tests ===\n";
  test_transfer_status_sexp_roundtrip ();
  test_transfer_status_to_string ();
  test_transfer_status_of_string ();
  test_transfer_status_compare ();

  (* Deposit_address tests *)
  printf "\n=== Deposit_address Tests ===\n";
  test_deposit_address_creation ();
  test_deposit_address_with_tag ();
  test_deposit_address_equality ();
  test_deposit_address_sexp_roundtrip ();
  test_deposit_address_edge_cases ();

  (* Deposit tests *)
  printf "\n=== Deposit Tests ===\n";
  test_deposit_creation ();
  test_deposit_with_all_fields ();
  test_deposit_sexp_roundtrip ();
  test_deposit_edge_cases ();

  (* Withdrawal tests *)
  printf "\n=== Withdrawal Tests ===\n";
  test_withdrawal_creation ();
  test_withdrawal_with_fee ();
  test_withdrawal_with_tag ();
  test_withdrawal_sexp_roundtrip ();
  test_withdrawal_equality ();
  test_withdrawal_edge_cases ();

  printf "\n";
  printf "====================================================\n";
  printf "  Test Results: %d run, %d passed, %d failed\n"
    !tests_run !tests_passed !tests_failed;
  printf "====================================================\n";

  match !tests_failed with
  | 0 ->
    printf "\n* All tests passed!\n\n";
    exit 0
  | n ->
    printf "\nX %d test(s) failed\n\n" n;
    exit 1

let () = run_all_tests ()

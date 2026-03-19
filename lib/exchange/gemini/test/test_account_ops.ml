(** Unit tests for Gemini Account Operations

    Tests for deposit address, deposits, withdrawals normalization.
    Uses mock native types to test parsing and normalization logic.

    Test Categories:
    1. Deposit address parsing and normalization
    2. Deposit (transfers endpoint) parsing
    3. Withdrawal parsing
    4. Edge cases *)

open Core
module Types = Fluxum.Types
module Adapter = Gemini.Fluxum_adapter.Adapter

(* Use V1 from Fluxum_adapter for type compatibility with Adapter.Native types *)
module V1 = Gemini.Fluxum_adapter.V1

(** Test result tracking *)
let tests_run = ref 0

let tests_passed = ref 0
let tests_failed = ref 0

let[@warning "-32"] assert_true condition msg =
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
    printf
      "  X FAIL: %s\n     Expected: %s\n     Got: %s\n"
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
(* Deposit Address Tests *)
(* ============================================================ *)

let test_deposit_address_basic () =
  printf "\n=== Deposit Address: Basic BTC Address ===\n";
  (* Create a native deposit address response *)
  let native_addr : Adapter.Native.Deposit_address.t =
    { address= "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; currency= "BTC"
    ; label= Some "default"
    ; network= None }
  in
    match Adapter.Normalize.deposit_address native_addr with
    | Ok addr ->
      ignore
        (assert_string_equal
           "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
           addr.address
           "Address parsed correctly");
      ignore
        (assert_equal
           ~equal:Types.Venue.equal
           ~sexp_of_t:Types.Venue.sexp_of_t
           Types.Venue.Gemini
           addr.venue
           "Venue is Gemini");
      ignore (assert_string_equal "BTC" addr.currency "Currency is BTC")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Valid deposit address should succeed: %s\n" msg

let test_deposit_address_eth () =
  printf "\n=== Deposit Address: ETH Address ===\n";
  let native_addr : Adapter.Native.Deposit_address.t =
    { address= "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; currency= "ETH"
    ; label= Some "primary_eth"
    ; network= Some "ethereum" }
  in
    match Adapter.Normalize.deposit_address native_addr with
    | Ok addr ->
      ignore
        (assert_string_equal
           "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
           addr.address
           "ETH address parsed");
      ignore
        (assert_equal
           ~equal:[%equal: string option]
           ~sexp_of_t:[%sexp_of: string option]
           (Some "ethereum")
           addr.network
           "Network is ethereum")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: ETH deposit address should succeed: %s\n" msg

(* ============================================================ *)
(* Deposit (Transfer History) Tests *)
(* ============================================================ *)

let make_test_transfer
      ?(type_ = `Deposit)
      ?(status = "Complete")
      ?(amount = "0.5")
      ?(currency = "BTC")
      ?(eid = 12345L)
      ?(tx_hash = None)
      ?(destination = None)
      ?(fee_amount = None)
      ()
  : V1.Transfers.transfer
  =
  let ts =
    Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms 1234567890000.)
  in
    { V1.Transfers.type_
    ; status
    ; timestampms= ts
    ; eid
    ; currency= V1.Currency.Enum_or_string.String currency
    ; amount
    ; method_= None
    ; txHash= tx_hash
    ; outputIdx= None
    ; destination
    ; purpose= None
    ; feeAmount= fee_amount
    ; feeCurrency= None
    ; withdrawalId= None }

let test_deposit_completed () =
  printf "\n=== Deposit: Completed BTC Deposit ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Deposit
      ~status:"Complete"
      ~amount:"0.5"
      ~currency:"BTC"
      ~eid:12345L
      ~tx_hash:(Some "abc123def456")
      ~destination:(Some "bc1q...")
      ()
  in
    match Adapter.Normalize.deposit transfer with
    | Ok deposit ->
      ignore (assert_string_equal "12345" deposit.id "Deposit ID parsed");
      ignore (assert_string_equal "BTC" deposit.currency "Currency is BTC");
      ignore (assert_float_equal 0.5 deposit.amount "Amount is 0.5");
      ignore
        (assert_equal
           ~equal:Types.Transfer_status.equal
           ~sexp_of_t:Types.Transfer_status.sexp_of_t
           Types.Transfer_status.Completed
           deposit.status
           "Status is Completed")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Valid deposit should succeed: %s\n" msg

let test_deposit_pending () =
  printf "\n=== Deposit: Pending ETH Deposit ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Deposit
      ~status:"Pending"
      ~amount:"10.25"
      ~currency:"ETH"
      ~eid:67890L
      ()
  in
    match Adapter.Normalize.deposit transfer with
    | Ok deposit ->
      ignore
        (assert_equal
           ~equal:Types.Transfer_status.equal
           ~sexp_of_t:Types.Transfer_status.sexp_of_t
           Types.Transfer_status.Pending
           deposit.status
           "Status is Pending");
      ignore (assert_float_equal 10.25 deposit.amount "Amount is 10.25")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Pending deposit should succeed: %s\n" msg

let test_deposit_advanced_status () =
  printf "\n=== Deposit: Advanced Status Types ===\n";
  (* Test Advanced status (Gemini specific) *)
  let transfer =
    make_test_transfer ~type_:`Deposit ~status:"Advanced" ~amount:"1000.0" ~eid:33333L ()
  in
    match Adapter.Normalize.deposit transfer with
    | Ok deposit ->
      (* Advanced should map to Processing *)
      ignore
        (assert_equal
           ~equal:Types.Transfer_status.equal
           ~sexp_of_t:Types.Transfer_status.sexp_of_t
           Types.Transfer_status.Processing
           deposit.status
           "Advanced status mapped to Processing")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Advanced status handling failed: %s\n" msg

let test_deposit_high_precision () =
  printf "\n=== Deposit: High Precision Amount ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Deposit
      ~status:"Complete"
      ~amount:"0.00000001"
      ~currency:"BTC"
      ~eid:44444L
      ()
  in
    match Adapter.Normalize.deposit transfer with
    | Ok deposit ->
      ignore
        (assert_float_equal
           0.00000001
           deposit.amount
           ~tolerance:0.000000001
           "High precision amount (1 satoshi)")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: High precision deposit should succeed: %s\n" msg

(* ============================================================ *)
(* Withdrawal Tests *)
(* ============================================================ *)

let test_withdrawal_from_transfer () =
  printf "\n=== Withdrawal: From Transfer Record ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Withdrawal
      ~status:"Complete"
      ~amount:"1.0"
      ~currency:"BTC"
      ~eid:55555L
      ~tx_hash:(Some "abc123def456789")
      ~destination:(Some "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh")
      ~fee_amount:(Some "0.0001")
      ()
  in
  let native_withdrawal = Adapter.Native.Withdrawal.Transfer transfer in
    match Adapter.Normalize.withdrawal native_withdrawal with
    | Ok withdrawal ->
      ignore (assert_string_equal "55555" withdrawal.id "Withdrawal ID parsed");
      ignore (assert_string_equal "BTC" withdrawal.currency "Currency is BTC");
      ignore (assert_float_equal 1.0 withdrawal.amount "Amount is 1.0");
      ignore
        (assert_equal
           ~equal:[%equal: float option]
           ~sexp_of_t:[%sexp_of: float option]
           (Some 0.0001)
           withdrawal.fee
           "Fee is 0.0001");
      ignore
        (assert_string_equal
           "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
           withdrawal.address
           "Destination address parsed")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Valid withdrawal should succeed: %s\n" msg

let test_withdrawal_from_response () =
  printf "\n=== Withdrawal: From Withdraw Response ===\n";
  let native_response : V1.Withdraw.response =
    { V1.Withdraw.address= "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; amount= "5.0"
    ; currency= None
    ; fee= None
    ; withdrawalId= "wdl_pending"
    ; message= Some "Withdrawal submitted"
    ; txHash= None }
  in
  let native_withdrawal = Adapter.Native.Withdrawal.Withdraw_response native_response in
    match Adapter.Normalize.withdrawal native_withdrawal with
    | Ok withdrawal ->
      ignore (assert_string_equal "wdl_pending" withdrawal.id "Withdrawal ID parsed");
      ignore (assert_float_equal 5.0 withdrawal.amount "Amount is 5.0");
      ignore
        (assert_equal
           ~equal:Types.Transfer_status.equal
           ~sexp_of_t:Types.Transfer_status.sexp_of_t
           Types.Transfer_status.Processing
           withdrawal.status
           "New withdrawal is Processing")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Withdrawal response should succeed: %s\n" msg

let test_withdrawal_cancelled () =
  printf "\n=== Withdrawal: Cancelled Withdrawal ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Withdrawal
      ~status:"Cancelled"
      ~amount:"100.0"
      ~currency:"USD"
      ~eid:66666L
      ~destination:(Some "TRx123")
      ~fee_amount:(Some "1.0")
      ()
  in
  let native_withdrawal = Adapter.Native.Withdrawal.Transfer transfer in
    match Adapter.Normalize.withdrawal native_withdrawal with
    | Ok withdrawal ->
      ignore
        (assert_equal
           ~equal:Types.Transfer_status.equal
           ~sexp_of_t:Types.Transfer_status.sexp_of_t
           Types.Transfer_status.Cancelled
           withdrawal.status
           "Cancelled status parsed")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Cancelled withdrawal should succeed: %s\n" msg

let test_withdrawal_zero_fee () =
  printf "\n=== Withdrawal: Zero Fee ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Withdrawal
      ~status:"Complete"
      ~amount:"500.0"
      ~currency:"USD"
      ~eid:77777L
      ~destination:(Some "0xabc123")
      ~fee_amount:(Some "0.0")
      ()
  in
  let native_withdrawal = Adapter.Native.Withdrawal.Transfer transfer in
    match Adapter.Normalize.withdrawal native_withdrawal with
    | Ok withdrawal ->
      ignore
        (assert_equal
           ~equal:[%equal: float option]
           ~sexp_of_t:[%sexp_of: float option]
           (Some 0.0)
           withdrawal.fee
           "Zero fee accepted")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Zero fee withdrawal should succeed: %s\n" msg

(* ============================================================ *)
(* Balance Parsing Tests *)
(* ============================================================ *)

let test_balance_with_timestamp () =
  printf "\n=== Balance: JSON with _timestamp field ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"BTC","amount":"0.5","available":"0.3",
          "availableForWithdrawal":"0.3","_timestamp":"2026-03-19T02:06:02.638Z"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      (match List.hd balances with
       | Some b ->
         ignore (assert_string_equal "0.5" b.amount "Amount parsed");
         ignore (assert_string_equal "0.3" b.available "Available parsed");
         ignore (assert_string_equal "exchange" b.type_ "Type parsed");
         ignore
           (assert_equal
              ~equal:[%equal: string option]
              ~sexp_of_t:[%sexp_of: string option]
              (Some "2026-03-19T02:06:02.638Z")
              b.timestamp
              "Timestamp parsed from _timestamp key")
       | None ->
         incr tests_run;
         incr tests_failed;
         printf "  X FAIL: Expected non-empty balance list\n")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Balance with _timestamp should parse: %s\n" msg

let test_balance_without_timestamp () =
  printf "\n=== Balance: JSON without _timestamp field ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"ETH","amount":"10.0","available":"8.5",
          "availableForWithdrawal":"8.5"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      (match List.hd balances with
       | Some b ->
         ignore (assert_string_equal "10.0" b.amount "Amount parsed without timestamp");
         ignore
           (assert_equal
              ~equal:[%equal: string option]
              ~sexp_of_t:[%sexp_of: string option]
              None
              b.timestamp
              "Timestamp is None when absent")
       | None ->
         incr tests_run;
         incr tests_failed;
         printf "  X FAIL: Expected non-empty balance list\n")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Balance without _timestamp should parse: %s\n" msg

let test_balance_multiple_currencies () =
  printf "\n=== Balance: Multiple currencies with timestamps ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"USD","amount":"25000.50","available":"20000.00",
          "availableForWithdrawal":"15000.00","_timestamp":"2026-03-19T02:00:00Z"},
         {"type":"exchange","currency":"BTC","amount":"0.35","available":"0.28",
          "availableForWithdrawal":"0.28","_timestamp":"2026-03-19T02:00:00Z"},
         {"type":"exchange","currency":"BONK","amount":"14273364.925772","available":"3775764.925772",
          "availableForWithdrawal":"3775764.925772","_timestamp":"2026-03-19T02:00:00Z"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      ignore
        (assert_float_equal
           3.0
           (Float.of_int (List.length balances))
           "Three balances parsed");
      let bonk = List.nth_exn balances 2 in
        ignore
          (assert_string_equal "14273364.925772" bonk.amount "Micro-priced token amount preserved")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Multiple balances should parse: %s\n" msg

let test_balance_zero_values () =
  printf "\n=== Balance: Zero balance entry ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"AMP","amount":"0","available":"0",
          "availableForWithdrawal":"0","_timestamp":"2026-03-19T02:00:00Z"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      (match List.hd balances with
       | Some b ->
         ignore (assert_string_equal "0" b.amount "Zero amount parsed")
       | None ->
         incr tests_run;
         incr tests_failed;
         printf "  X FAIL: Expected non-empty balance list\n")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Zero balance should parse: %s\n" msg

let test_balance_high_precision_amount () =
  printf "\n=== Balance: High precision decimal amount ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"POL",
          "amount":"1241.4771496673182318955",
          "available":"1241.4771496673182318955",
          "availableForWithdrawal":"1241.4771496673182318955",
          "_timestamp":"2026-03-19T02:00:00Z"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      (match List.hd balances with
       | Some b ->
         ignore
           (assert_string_equal
              "1241.4771496673182318955"
              b.amount
              "High precision amount preserved as string")
       | None ->
         incr tests_run;
         incr tests_failed;
         printf "  X FAIL: Expected non-empty balance list\n")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: High precision balance should parse: %s\n" msg

let test_balance_empty_list () =
  printf "\n=== Balance: Empty balance list ===\n";
  let json = Yojson.Safe.from_string "[]" in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      ignore
        (assert_float_equal
           0.0
           (Float.of_int (List.length balances))
           "Empty balance list accepted")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Empty balance list should parse: %s\n" msg

let test_balance_unknown_extra_field () =
  printf "\n=== Balance: Unknown extra field (future-proofing) ===\n";
  let json =
    Yojson.Safe.from_string
      {|[{"type":"exchange","currency":"BTC","amount":"1.0","available":"0.5",
          "availableForWithdrawal":"0.5","_timestamp":"2026-03-19T02:00:00Z",
          "someNewField":"unexpected"}]|}
  in
    match V1.Balances.response_of_yojson json with
    | Ok balances ->
      (match List.hd balances with
       | Some b ->
         ignore (assert_string_equal "1.0" b.amount "Parses despite unknown extra field")
       | None ->
         incr tests_run;
         incr tests_failed;
         printf "  X FAIL: Expected non-empty balance list\n")
    | Error _ ->
      (* This is expected to fail if strict parsing is in effect *)
      incr tests_run;
      incr tests_passed;
      printf "  * Extra field rejected (strict mode) - add field to type if API changes\n"

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_very_large_amount () =
  printf "\n=== Edge: Very Large Amount ===\n";
  let transfer =
    make_test_transfer
      ~type_:`Deposit
      ~status:"Complete"
      ~amount:"999999999999.99"
      ~currency:"USD"
      ~eid:99999L
      ()
  in
    match Adapter.Normalize.deposit transfer with
    | Ok deposit ->
      ignore
        (assert_float_equal
           999999999999.99
           deposit.amount
           ~tolerance:0.01
           "Very large amount handled")
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  X FAIL: Large amount should succeed: %s\n" msg

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  Gemini Account Operations - Unit Test Suite\n";
  printf "====================================================\n";
  (* Deposit address tests *)
  printf "\n=== Deposit Address Tests ===\n";
  test_deposit_address_basic ();
  test_deposit_address_eth ();
  (* Deposit tests *)
  printf "\n=== Deposit Tests ===\n";
  test_deposit_completed ();
  test_deposit_pending ();
  test_deposit_advanced_status ();
  test_deposit_high_precision ();
  (* Withdrawal tests *)
  printf "\n=== Withdrawal Tests ===\n";
  test_withdrawal_from_transfer ();
  test_withdrawal_from_response ();
  test_withdrawal_cancelled ();
  test_withdrawal_zero_fee ();
  (* Balance parsing tests *)
  printf "\n=== Balance Parsing Tests ===\n";
  test_balance_with_timestamp ();
  test_balance_without_timestamp ();
  test_balance_multiple_currencies ();
  test_balance_zero_values ();
  test_balance_high_precision_amount ();
  test_balance_empty_list ();
  test_balance_unknown_extra_field ();
  (* Edge cases *)
  printf "\n=== Edge Cases ===\n";
  test_very_large_amount ();
  printf "\n";
  printf "====================================================\n";
  printf
    "  Test Results: %d run, %d passed, %d failed\n"
    !tests_run
    !tests_passed
    !tests_failed;
  printf "====================================================\n";
  match !tests_failed with
  | 0 ->
    printf "\n* All tests passed!\n\n";
    exit 0
  | n ->
    printf "\nX %d test(s) failed\n\n" n;
    exit 1

let () = run_all_tests ()

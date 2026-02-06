(** Unit tests for OKX Account Operations

    Tests for deposit address, deposits, withdrawals normalization.
    Uses mock native types to test parsing and normalization logic.

    Test Categories:
    1. Deposit address parsing and normalization
    2. Deposit status mapping (0,1,2,8,11,12 -> Transfer_status.t)
    3. Withdrawal status mapping (-3,-2,-1,0,1,2,3,4,5 -> Transfer_status.t)
    4. Withdrawal normalization with fees
    5. Edge cases: empty tag, multiple chains
*)

open Core

module Types = Fluxum.Types
module V5 = Okx.V5

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match equal expected actual with
  | false ->
    incr tests_failed;
    printf "  FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | true ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true

let assert_string_equal expected actual msg =
  incr tests_run;
  match String.equal expected actual with
  | false ->
    incr tests_failed;
    printf "  FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false
  | true ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true

let _assert_ok result msg =
  incr tests_run;
  match result with
  | Ok _ ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true
  | Error e ->
    incr tests_failed;
    printf "  FAIL: %s (expected Ok, got Error: %s)\n" msg e;
    false

let assert_error result msg =
  incr tests_run;
  match result with
  | Error _ ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true
  | Ok _ ->
    incr tests_failed;
    printf "  FAIL: %s (expected Error, got Ok)\n" msg;
    false

(* ============================================================ *)
(* OKX Deposit Status Mapping *)
(* ============================================================ *)

(** OKX deposit states from V5 API documentation:
    0 = waiting for confirmation
    1 = deposit credited (waiting for settlement)
    2 = deposit successful
    8 = pending due to temporary deposit suspension
    11 = match the address
    12 = account or deposit is frozen *)

let okx_deposit_status_to_transfer_status state_code =
  match state_code with
  | "0" -> Ok Types.Transfer_status.Pending       (* waiting for confirmation *)
  | "1" -> Ok Types.Transfer_status.Processing    (* deposit credited *)
  | "2" -> Ok Types.Transfer_status.Completed     (* deposit successful *)
  | "8" -> Ok Types.Transfer_status.Pending       (* pending due to suspension *)
  | "11" -> Ok Types.Transfer_status.Processing   (* match the address *)
  | "12" -> Ok Types.Transfer_status.Failed       (* account/deposit frozen *)
  | s -> Error (sprintf "Unknown OKX deposit status: %s" s)

let test_okx_deposit_status_mapping () =
  printf "\n=== OKX Deposit Status Mapping ===\n";

  let cases =
    [ ("0", Types.Transfer_status.Pending, "waiting for confirmation -> Pending")
    ; ("1", Types.Transfer_status.Processing, "deposit credited -> Processing")
    ; ("2", Types.Transfer_status.Completed, "deposit successful -> Completed")
    ; ("8", Types.Transfer_status.Pending, "pending suspension -> Pending")
    ; ("11", Types.Transfer_status.Processing, "match address -> Processing")
    ; ("12", Types.Transfer_status.Failed, "account frozen -> Failed")
    ]
  in
  List.iter cases ~f:(fun (okx_status, expected, description) ->
    match okx_deposit_status_to_transfer_status okx_status with
    | Ok result ->
      ignore (assert_equal
        ~equal:Types.Transfer_status.equal
        ~sexp_of_t:Types.Transfer_status.sexp_of_t
        expected result
        description)
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  FAIL: %s - %s\n" description msg);

  (* Test unknown status *)
  let _ = assert_error
    (okx_deposit_status_to_transfer_status "999")
    "Unknown deposit status returns Error" in
  ()

(* ============================================================ *)
(* OKX Withdrawal Status Mapping *)
(* ============================================================ *)

(** OKX withdrawal states from V5 API documentation:
    -3 = canceling
    -2 = cancelled
    -1 = failed
    0 = pending
    1 = sending
    2 = sent
    3 = awaiting email verification
    4 = awaiting manual verification
    5 = awaiting identity verification *)

let okx_withdrawal_status_to_transfer_status state_code =
  match state_code with
  | "-3" -> Ok Types.Transfer_status.Processing   (* canceling in progress *)
  | "-2" -> Ok Types.Transfer_status.Cancelled    (* cancelled *)
  | "-1" -> Ok Types.Transfer_status.Failed       (* failed *)
  | "0" -> Ok Types.Transfer_status.Pending       (* pending *)
  | "1" -> Ok Types.Transfer_status.Processing    (* sending *)
  | "2" -> Ok Types.Transfer_status.Completed     (* sent *)
  | "3" -> Ok Types.Transfer_status.Pending       (* awaiting email verification *)
  | "4" -> Ok Types.Transfer_status.Pending       (* awaiting manual verification *)
  | "5" -> Ok Types.Transfer_status.Pending       (* awaiting identity verification *)
  | s -> Error (sprintf "Unknown OKX withdrawal status: %s" s)

let test_okx_withdrawal_status_mapping () =
  printf "\n=== OKX Withdrawal Status Mapping ===\n";

  let cases =
    [ ("-3", Types.Transfer_status.Processing, "canceling -> Processing")
    ; ("-2", Types.Transfer_status.Cancelled, "cancelled -> Cancelled")
    ; ("-1", Types.Transfer_status.Failed, "failed -> Failed")
    ; ("0", Types.Transfer_status.Pending, "pending -> Pending")
    ; ("1", Types.Transfer_status.Processing, "sending -> Processing")
    ; ("2", Types.Transfer_status.Completed, "sent -> Completed")
    ; ("3", Types.Transfer_status.Pending, "awaiting email -> Pending")
    ; ("4", Types.Transfer_status.Pending, "awaiting manual -> Pending")
    ; ("5", Types.Transfer_status.Pending, "awaiting identity -> Pending")
    ]
  in
  List.iter cases ~f:(fun (okx_status, expected, description) ->
    match okx_withdrawal_status_to_transfer_status okx_status with
    | Ok result ->
      ignore (assert_equal
        ~equal:Types.Transfer_status.equal
        ~sexp_of_t:Types.Transfer_status.sexp_of_t
        expected result
        description)
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  FAIL: %s - %s\n" description msg);

  (* Test unknown status *)
  let _ = assert_error
    (okx_withdrawal_status_to_transfer_status "999")
    "Unknown withdrawal status returns Error" in
  ()

(* ============================================================ *)
(* Deposit Address Normalization *)
(* ============================================================ *)

(** Normalize OKX deposit address to Types.Deposit_address.t *)
let normalize_deposit_address (addr : V5.Deposit_address.address_info)
    : (Types.Deposit_address.t, string) Result.t =
  (* OKX provides tag, memo, and pmtId for different networks *)
  let tag =
    match addr.tag, addr.memo, addr.pmtId with
    | t, _, _ when not (String.is_empty t) -> Some t
    | _, m, _ when not (String.is_empty m) -> Some m
    | _, _, p when not (String.is_empty p) -> Some p
    | _, _, _ -> None
  in
  (* Extract network from chain field (e.g., "BTC-Bitcoin" -> "Bitcoin") *)
  let network =
    match String.lsplit2 addr.chain ~on:'-' with
    | Some (_, net) -> Some net
    | None -> Some addr.chain
  in
  Ok (Types.Deposit_address.create
    ~venue:Types.Venue.Okx
    ~currency:addr.ccy
    ~address:addr.addr
    ?tag
    ?network
    ())

let test_deposit_address_basic () =
  printf "\n=== Deposit Address: Basic BTC Address ===\n";

  let native_addr : V5.Deposit_address.address_info =
    { addr = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; tag = ""
    ; memo = ""
    ; pmtId = ""
    ; ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; ctAddr = ""
    ; selected = true
    }
  in

  match normalize_deposit_address native_addr with
  | Ok addr ->
    ignore (assert_string_equal "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh" addr.address
      "Address parsed correctly");
    ignore (assert_equal
      ~equal:Types.Venue.equal
      ~sexp_of_t:Types.Venue.sexp_of_t
      Types.Venue.Okx addr.venue
      "Venue is OKX");
    ignore (assert_string_equal "BTC" addr.currency "Currency is BTC");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "Bitcoin") addr.network
      "Network extracted from chain")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Valid deposit address should succeed: %s\n" msg

let test_deposit_address_with_tag () =
  printf "\n=== Deposit Address: XRP with Tag ===\n";

  let native_addr : V5.Deposit_address.address_info =
    { addr = "rLW9gnQo7BQhU6igk5keqYnH3TVrCxGRzm"
    ; tag = "1234567890"
    ; memo = ""
    ; pmtId = ""
    ; ccy = "XRP"
    ; chain = "XRP-Ripple"
    ; ctAddr = ""
    ; selected = true
    }
  in

  match normalize_deposit_address native_addr with
  | Ok addr ->
    ignore (assert_string_equal "rLW9gnQo7BQhU6igk5keqYnH3TVrCxGRzm" addr.address
      "XRP address parsed");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "1234567890") addr.tag
      "Tag extracted correctly")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: XRP deposit address should succeed: %s\n" msg

let test_deposit_address_with_memo () =
  printf "\n=== Deposit Address: XLM with Memo ===\n";

  let native_addr : V5.Deposit_address.address_info =
    { addr = "GCQVEST7KIWV3KOSNDDUJKEPZLBFWKM7DUS4TCLW2VNVPCBGTDCE"
    ; tag = ""
    ; memo = "okx_xlm_memo_12345"
    ; pmtId = ""
    ; ccy = "XLM"
    ; chain = "XLM-Stellar"
    ; ctAddr = ""
    ; selected = true
    }
  in

  match normalize_deposit_address native_addr with
  | Ok addr ->
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "okx_xlm_memo_12345") addr.tag
      "Memo used as tag")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: XLM deposit address should succeed: %s\n" msg

let test_deposit_address_eth_erc20 () =
  printf "\n=== Deposit Address: ETH ERC-20 Token ===\n";

  let native_addr : V5.Deposit_address.address_info =
    { addr = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; tag = ""
    ; memo = ""
    ; pmtId = ""
    ; ccy = "USDT"
    ; chain = "USDT-ERC20"
    ; ctAddr = "0xdAC17F958D2ee523a2206206994597C13D831ec7"
    ; selected = false
    }
  in

  match normalize_deposit_address native_addr with
  | Ok addr ->
    ignore (assert_string_equal "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2" addr.address
      "ETH address parsed");
    ignore (assert_string_equal "USDT" addr.currency "Currency is USDT");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "ERC20") addr.network
      "Network is ERC20")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: ETH ERC-20 deposit address should succeed: %s\n" msg

let test_deposit_address_empty_tag () =
  printf "\n=== Deposit Address: Empty Tag ===\n";

  let native_addr : V5.Deposit_address.address_info =
    { addr = "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"
    ; tag = ""
    ; memo = ""
    ; pmtId = ""
    ; ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; ctAddr = ""
    ; selected = true
    }
  in

  match normalize_deposit_address native_addr with
  | Ok addr ->
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      None addr.tag
      "Empty tag normalized to None")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Empty tag should be None: %s\n" msg

(* ============================================================ *)
(* Deposit Normalization *)
(* ============================================================ *)

(** Normalize OKX deposit record to Types.Deposit.t *)
let normalize_deposit (dep : V5.Deposit_history.deposit_record)
    : (Types.Deposit.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind amount = Fluxum.Normalize_common.Float_conv.of_string dep.amt in
  let%bind status = okx_deposit_status_to_transfer_status dep.state in
  let created_at =
    match Fluxum.Normalize_common.Float_conv.of_string dep.ts with
    | Ok ms -> Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms ms))
    | Error _ -> None
  in
  let address = match String.is_empty dep.to_ with
    | true -> None
    | false -> Some dep.to_
  in
  let tx_id = match String.is_empty dep.txId with
    | true -> None
    | false -> Some dep.txId
  in
  Ok (Types.Deposit.create
    ~venue:Types.Venue.Okx
    ~id:dep.depId
    ~currency:dep.ccy
    ~amount
    ~status
    ?address
    ?tx_id
    ?created_at
    ())

let test_deposit_completed () =
  printf "\n=== Deposit: Completed BTC Deposit ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; amt = "0.5"
    ; from = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    ; to_ = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; txId = "abc123def456789"
    ; ts = "1640000000000"
    ; state = "2"  (* deposit successful *)
    ; depId = "12345678"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_string_equal "12345678" deposit.id "Deposit ID parsed");
    ignore (assert_string_equal "BTC" deposit.currency "Currency is BTC");
    ignore (assert_float_equal 0.5 deposit.amount "Amount is 0.5");
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Completed deposit.status
      "Status is Completed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Valid deposit should succeed: %s\n" msg

let test_deposit_pending () =
  printf "\n=== Deposit: Pending ETH Deposit ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "ETH"
    ; chain = "ETH-ERC20"
    ; amt = "10.25"
    ; from = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; to_ = "0x123456789abcdef"
    ; txId = ""
    ; ts = "1640000000000"
    ; state = "0"  (* waiting for confirmation *)
    ; depId = "67890"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Pending deposit.status
      "Status is Pending");
    ignore (assert_float_equal 10.25 deposit.amount "Amount is 10.25");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      None deposit.tx_id
      "Empty txId normalized to None")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Pending deposit should succeed: %s\n" msg

let test_deposit_frozen () =
  printf "\n=== Deposit: Frozen Deposit (Status 12) ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "USDT"
    ; chain = "USDT-TRC20"
    ; amt = "1000.0"
    ; from = "TRX123"
    ; to_ = "TRX456"
    ; txId = "trxtx123"
    ; ts = "1640000000000"
    ; state = "12"  (* account/deposit frozen *)
    ; depId = "frozen_dep_001"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Failed deposit.status
      "Frozen status mapped to Failed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Frozen deposit should map to Failed: %s\n" msg

let test_deposit_high_precision () =
  printf "\n=== Deposit: High Precision Amount ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; amt = "0.00000001"
    ; from = ""
    ; to_ = "bc1qtest"
    ; txId = "satoshi_tx"
    ; ts = "1640000000000"
    ; state = "2"
    ; depId = "tiny_dep"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_float_equal 0.00000001 deposit.amount ~tolerance:0.000000001
      "High precision amount (1 satoshi)")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: High precision deposit should succeed: %s\n" msg

(* ============================================================ *)
(* Withdrawal Normalization *)
(* ============================================================ *)

(** Normalize OKX withdrawal record to Types.Withdrawal.t *)
let normalize_withdrawal_from_history (wd : V5.Withdrawal_history.withdrawal_record)
    : (Types.Withdrawal.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind amount = Fluxum.Normalize_common.Float_conv.of_string wd.amt in
  let%bind fee = Fluxum.Normalize_common.Float_conv.of_string wd.fee in
  let%bind status = okx_withdrawal_status_to_transfer_status wd.state in
  let created_at =
    match Fluxum.Normalize_common.Float_conv.of_string wd.ts with
    | Ok ms -> Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms ms))
    | Error _ -> None
  in
  let tx_id = match String.is_empty wd.txId with
    | true -> None
    | false -> Some wd.txId
  in
  Ok (Types.Withdrawal.create
    ~venue:Types.Venue.Okx
    ~id:wd.wdId
    ~currency:wd.ccy
    ~amount
    ~fee
    ~status
    ~address:wd.to_
    ?tx_id
    ?created_at
    ())

let test_withdrawal_completed () =
  printf "\n=== Withdrawal: Completed BTC Withdrawal ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; amt = "1.0"
    ; ts = "1640000000000"
    ; from = "okx_internal"
    ; to_ = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; txId = "txhash123456"
    ; fee = "0.0001"
    ; state = "2"  (* sent *)
    ; wdId = "wd_12345"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_string_equal "wd_12345" withdrawal.id "Withdrawal ID parsed");
    ignore (assert_string_equal "BTC" withdrawal.currency "Currency is BTC");
    ignore (assert_float_equal 1.0 withdrawal.amount "Amount is 1.0");
    ignore (assert_equal
      ~equal:[%equal: float option]
      ~sexp_of_t:[%sexp_of: float option]
      (Some 0.0001) withdrawal.fee
      "Fee is 0.0001");
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Completed withdrawal.status
      "Status is Completed");
    ignore (assert_string_equal "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh" withdrawal.address
      "Destination address parsed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Valid withdrawal should succeed: %s\n" msg

let test_withdrawal_pending () =
  printf "\n=== Withdrawal: Pending Withdrawal ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "ETH"
    ; chain = "ETH-ERC20"
    ; amt = "5.0"
    ; ts = "1640000000000"
    ; from = ""
    ; to_ = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; txId = ""
    ; fee = "0.001"
    ; state = "0"  (* pending *)
    ; wdId = "wd_pending_001"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Pending withdrawal.status
      "Status is Pending");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      None withdrawal.tx_id
      "Empty txId normalized to None")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Pending withdrawal should succeed: %s\n" msg

let test_withdrawal_cancelled () =
  printf "\n=== Withdrawal: Cancelled Withdrawal ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "USDT"
    ; chain = "USDT-TRC20"
    ; amt = "100.0"
    ; ts = "1640000000000"
    ; from = ""
    ; to_ = "TRx123"
    ; txId = ""
    ; fee = "1.0"
    ; state = "-2"  (* cancelled *)
    ; wdId = "wd_cancelled"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Cancelled withdrawal.status
      "Status is Cancelled")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Cancelled withdrawal should succeed: %s\n" msg

let test_withdrawal_failed () =
  printf "\n=== Withdrawal: Failed Withdrawal ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; amt = "0.5"
    ; ts = "1640000000000"
    ; from = ""
    ; to_ = "invalid_address"
    ; txId = ""
    ; fee = "0.0001"
    ; state = "-1"  (* failed *)
    ; wdId = "wd_failed"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Failed withdrawal.status
      "Status is Failed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Failed withdrawal should succeed: %s\n" msg

let test_withdrawal_awaiting_verification () =
  printf "\n=== Withdrawal: Awaiting Email Verification ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "ETH"
    ; chain = "ETH-ERC20"
    ; amt = "2.0"
    ; ts = "1640000000000"
    ; from = ""
    ; to_ = "0xverification_pending"
    ; txId = ""
    ; fee = "0.001"
    ; state = "3"  (* awaiting email verification *)
    ; wdId = "wd_email_verify"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Pending withdrawal.status
      "Awaiting verification maps to Pending")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Awaiting verification withdrawal should succeed: %s\n" msg

let test_withdrawal_zero_fee () =
  printf "\n=== Withdrawal: Zero Fee (Internal Transfer) ===\n";

  let native_wd : V5.Withdrawal_history.withdrawal_record =
    { ccy = "USDT"
    ; chain = "USDT-Internal"
    ; amt = "500.0"
    ; ts = "1640000000000"
    ; from = ""
    ; to_ = "okx_user@email.com"
    ; txId = ""
    ; fee = "0"
    ; state = "2"  (* sent *)
    ; wdId = "wd_internal"
    }
  in

  match normalize_withdrawal_from_history native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:[%equal: float option]
      ~sexp_of_t:[%sexp_of: float option]
      (Some 0.0) withdrawal.fee
      "Zero fee accepted")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Zero fee withdrawal should succeed: %s\n" msg

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_very_large_amount () =
  printf "\n=== Edge: Very Large Amount ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "USDT"
    ; chain = "USDT-ERC20"
    ; amt = "999999999999.99"
    ; from = ""
    ; to_ = "0xlarge"
    ; txId = "largetx"
    ; ts = "1640000000000"
    ; state = "2"
    ; depId = "large_dep"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_float_equal 999999999999.99 deposit.amount ~tolerance:0.01
      "Very large amount handled")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Large amount should succeed: %s\n" msg

let test_malformed_amount () =
  printf "\n=== Edge: Malformed Amount ===\n";

  let native_dep : V5.Deposit_history.deposit_record =
    { ccy = "BTC"
    ; chain = "BTC-Bitcoin"
    ; amt = "not_a_number"
    ; from = ""
    ; to_ = "bc1q"
    ; txId = ""
    ; ts = "1640000000000"
    ; state = "2"
    ; depId = "bad_dep"
    }
  in

  let _ = assert_error
    (normalize_deposit native_dep)
    "Malformed amount returns Error" in
  ()

let test_multiple_chains_usdt () =
  printf "\n=== Edge: Multiple Chains (USDT) ===\n";

  let chains =
    [ ("USDT-ERC20", "ERC20")
    ; ("USDT-TRC20", "TRC20")
    ; ("USDT-Polygon", "Polygon")
    ; ("USDT-Arbitrum One", "Arbitrum One")
    ; ("USDT-Optimism", "Optimism")
    ]
  in

  List.iter chains ~f:(fun (chain, expected_network) ->
    let native_addr : V5.Deposit_address.address_info =
      { addr = "0xtest"
      ; tag = ""
      ; memo = ""
      ; pmtId = ""
      ; ccy = "USDT"
      ; chain
      ; ctAddr = "0xcontract"
      ; selected = false
      }
    in
    match normalize_deposit_address native_addr with
    | Ok addr ->
      ignore (assert_equal
        ~equal:[%equal: string option]
        ~sexp_of_t:[%sexp_of: string option]
        (Some expected_network) addr.network
        (sprintf "Chain %s -> Network %s" chain expected_network))
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  FAIL: Chain %s should succeed: %s\n" chain msg)

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  OKX Account Operations - Unit Test Suite\n";
  printf "====================================================\n";

  (* Status mapping tests *)
  printf "\n=== Status Mapping Tests ===\n";
  test_okx_deposit_status_mapping ();
  test_okx_withdrawal_status_mapping ();

  (* Deposit address tests *)
  printf "\n=== Deposit Address Tests ===\n";
  test_deposit_address_basic ();
  test_deposit_address_with_tag ();
  test_deposit_address_with_memo ();
  test_deposit_address_eth_erc20 ();
  test_deposit_address_empty_tag ();

  (* Deposit tests *)
  printf "\n=== Deposit Tests ===\n";
  test_deposit_completed ();
  test_deposit_pending ();
  test_deposit_frozen ();
  test_deposit_high_precision ();

  (* Withdrawal tests *)
  printf "\n=== Withdrawal Tests ===\n";
  test_withdrawal_completed ();
  test_withdrawal_pending ();
  test_withdrawal_cancelled ();
  test_withdrawal_failed ();
  test_withdrawal_awaiting_verification ();
  test_withdrawal_zero_fee ();

  (* Edge cases *)
  printf "\n=== Edge Cases ===\n";
  test_very_large_amount ();
  test_malformed_amount ();
  test_multiple_chains_usdt ();

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

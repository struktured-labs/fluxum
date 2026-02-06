(** Unit tests for Bybit Account Operations

    Tests for deposit address, deposits, withdrawals normalization.
    Uses mock native types to test parsing and normalization logic.

    Test Categories:
    1. Deposit address parsing and normalization with chainType
    2. Deposit status mapping (0,1,2,3,4 -> Transfer_status.t)
    3. Withdrawal status mapping (SecurityCheck, Pending, success, etc.)
    4. Withdrawal normalization
    5. Edge cases: tag handling, multi-chain
*)

open Core

module Types = Fluxum.Types

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

let[@warning "-32"] assert_ok result msg =
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
(* Mock Bybit V5 Asset Types *)
(* These types mock the expected V5 Asset API response structures *)
(* ============================================================ *)

module Mock_v5 = struct
  (** Deposit address from GET /v5/asset/deposit/query-address *)
  module Deposit_address = struct
    type address_info =
      { coin : string            (** Currency, e.g., BTC, ETH *)
      ; chains : chain_info list (** List of supported chains *)
      }
    and chain_info =
      { chainType : string       (** Chain name, e.g., BTC, ETH, TRX *)
      ; addressDeposit : string  (** Deposit address *)
      ; tagDeposit : string      (** Tag/memo (empty if not needed) *)
      ; chain : string           (** Full chain name *)
      }
    [@@deriving sexp]
  end

  (** Deposit record from GET /v5/asset/deposit/query-record *)
  module Deposit_history = struct
    (** Bybit deposit statuses:
        0 = unknown
        1 = toBeConfirmed
        2 = processing (transferring to wallet)
        3 = success
        4 = deposit failed *)
    type deposit_record =
      { coin : string            (** Currency *)
      ; chain : string           (** Chain type *)
      ; amount : string          (** Deposit amount *)
      ; txID : string            (** Transaction hash *)
      ; status : int             (** Status code *)
      ; toAddress : string       (** Receiving address *)
      ; tag : string             (** Tag/memo *)
      ; depositFee : string      (** Deposit fee (if any) *)
      ; successAt : string       (** Success timestamp (ms) *)
      ; confirmations : string   (** Number of confirmations *)
      ; txIndex : string         (** Transaction index *)
      ; blockHash : string       (** Block hash *)
      ; depositId : string       (** Unique deposit ID *)
      }
    [@@deriving sexp]
  end

  (** Withdrawal record from GET /v5/asset/withdraw/query-record *)
  module Withdrawal_history = struct
    (** Bybit withdrawal statuses (string based):
        SecurityCheck - awaiting security check
        Pending - processing
        success - completed
        CancelByUser - cancelled by user
        Reject - rejected
        Fail - failed
        BlockchainConfirmed - confirmed on chain *)
    type withdrawal_record =
      { withdrawId : string      (** Unique withdrawal ID *)
      ; coin : string            (** Currency *)
      ; chain : string           (** Chain type *)
      ; amount : string          (** Withdrawal amount *)
      ; withdrawFee : string     (** Withdrawal fee *)
      ; status : string          (** Status string *)
      ; toAddress : string       (** Destination address *)
      ; tag : string             (** Tag/memo *)
      ; txID : string            (** Transaction hash *)
      ; createTime : string      (** Created timestamp (ms) *)
      ; updateTime : string      (** Updated timestamp (ms) *)
      }
    [@@deriving sexp]
  end

  (** Withdrawal response from POST /v5/asset/withdraw/create *)
  module Withdraw = struct
    type withdraw_response =
      { id : string              (** Withdrawal ID *)
      }
    [@@deriving sexp]
  end
end

(* ============================================================ *)
(* Bybit Deposit Status Mapping *)
(* ============================================================ *)

(** Bybit deposit statuses:
    0 = unknown
    1 = toBeConfirmed
    2 = processing
    3 = success
    4 = deposit failed *)

let bybit_deposit_status_to_transfer_status status_code =
  match status_code with
  | 0 -> Ok Types.Transfer_status.Pending       (* unknown -> treat as pending *)
  | 1 -> Ok Types.Transfer_status.Pending       (* toBeConfirmed *)
  | 2 -> Ok Types.Transfer_status.Processing    (* processing *)
  | 3 -> Ok Types.Transfer_status.Completed     (* success *)
  | 4 -> Ok Types.Transfer_status.Failed        (* deposit failed *)
  | s -> Error (sprintf "Unknown Bybit deposit status: %d" s)

let test_bybit_deposit_status_mapping () =
  printf "\n=== Bybit Deposit Status Mapping ===\n";

  let cases =
    [ (0, Types.Transfer_status.Pending, "unknown -> Pending")
    ; (1, Types.Transfer_status.Pending, "toBeConfirmed -> Pending")
    ; (2, Types.Transfer_status.Processing, "processing -> Processing")
    ; (3, Types.Transfer_status.Completed, "success -> Completed")
    ; (4, Types.Transfer_status.Failed, "deposit failed -> Failed")
    ]
  in
  List.iter cases ~f:(fun (bybit_status, expected, description) ->
    match bybit_deposit_status_to_transfer_status bybit_status with
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
    (bybit_deposit_status_to_transfer_status 999)
    "Unknown deposit status returns Error" in
  ()

(* ============================================================ *)
(* Bybit Withdrawal Status Mapping *)
(* ============================================================ *)

(** Bybit withdrawal statuses (string based):
    SecurityCheck - awaiting security check
    Pending - processing
    success - completed
    CancelByUser - cancelled by user
    Reject - rejected
    Fail - failed
    BlockchainConfirmed - confirmed on chain *)

let bybit_withdrawal_status_to_transfer_status status_str =
  match String.lowercase status_str with
  | "securitycheck" -> Ok Types.Transfer_status.Pending
  | "pending" -> Ok Types.Transfer_status.Processing
  | "success" -> Ok Types.Transfer_status.Completed
  | "blockchainconfirmed" -> Ok Types.Transfer_status.Completed
  | "cancelbyuser" -> Ok Types.Transfer_status.Cancelled
  | "reject" -> Ok Types.Transfer_status.Failed
  | "fail" -> Ok Types.Transfer_status.Failed
  | "unknown" -> Ok Types.Transfer_status.Pending
  | s -> Error (sprintf "Unknown Bybit withdrawal status: %s" s)

let test_bybit_withdrawal_status_mapping () =
  printf "\n=== Bybit Withdrawal Status Mapping ===\n";

  let cases =
    [ ("SecurityCheck", Types.Transfer_status.Pending, "SecurityCheck -> Pending")
    ; ("Pending", Types.Transfer_status.Processing, "Pending -> Processing")
    ; ("success", Types.Transfer_status.Completed, "success -> Completed")
    ; ("BlockchainConfirmed", Types.Transfer_status.Completed, "BlockchainConfirmed -> Completed")
    ; ("CancelByUser", Types.Transfer_status.Cancelled, "CancelByUser -> Cancelled")
    ; ("Reject", Types.Transfer_status.Failed, "Reject -> Failed")
    ; ("Fail", Types.Transfer_status.Failed, "Fail -> Failed")
    ; ("Unknown", Types.Transfer_status.Pending, "Unknown -> Pending")
    ]
  in
  List.iter cases ~f:(fun (bybit_status, expected, description) ->
    match bybit_withdrawal_status_to_transfer_status bybit_status with
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
    (bybit_withdrawal_status_to_transfer_status "SomeRandomStatus")
    "Unknown withdrawal status returns Error" in
  ()

(* ============================================================ *)
(* Deposit Address Normalization *)
(* ============================================================ *)

(** Normalize a single Bybit chain address to Types.Deposit_address.t *)
let normalize_deposit_address ~currency (chain_info : Mock_v5.Deposit_address.chain_info)
    : (Types.Deposit_address.t, string) Result.t =
  let tag = match String.is_empty chain_info.tagDeposit with
    | true -> None
    | false -> Some chain_info.tagDeposit
  in
  let network = Some chain_info.chainType in
  Ok (Types.Deposit_address.create
    ~venue:Types.Venue.Bybit
    ~currency
    ~address:chain_info.addressDeposit
    ?tag
    ?network
    ())

let test_deposit_address_btc () =
  printf "\n=== Deposit Address: Basic BTC Address ===\n";

  let chain_info : Mock_v5.Deposit_address.chain_info =
    { chainType = "BTC"
    ; addressDeposit = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; tagDeposit = ""
    ; chain = "BTC"
    }
  in

  match normalize_deposit_address ~currency:"BTC" chain_info with
  | Ok addr ->
    ignore (assert_string_equal "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh" addr.address
      "Address parsed correctly");
    ignore (assert_equal
      ~equal:Types.Venue.equal
      ~sexp_of_t:Types.Venue.sexp_of_t
      Types.Venue.Bybit addr.venue
      "Venue is Bybit");
    ignore (assert_string_equal "BTC" addr.currency "Currency is BTC");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "BTC") addr.network
      "Network is BTC")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Valid deposit address should succeed: %s\n" msg

let test_deposit_address_with_tag () =
  printf "\n=== Deposit Address: XRP with Tag ===\n";

  let chain_info : Mock_v5.Deposit_address.chain_info =
    { chainType = "XRP"
    ; addressDeposit = "rLW9gnQo7BQhU6igk5keqYnH3TVrCxGRzm"
    ; tagDeposit = "9876543210"
    ; chain = "XRP"
    }
  in

  match normalize_deposit_address ~currency:"XRP" chain_info with
  | Ok addr ->
    ignore (assert_string_equal "rLW9gnQo7BQhU6igk5keqYnH3TVrCxGRzm" addr.address
      "XRP address parsed");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "9876543210") addr.tag
      "Tag extracted correctly")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: XRP deposit address should succeed: %s\n" msg

let test_deposit_address_usdt_multi_chain () =
  printf "\n=== Deposit Address: USDT Multi-Chain ===\n";

  let chains : Mock_v5.Deposit_address.chain_info list =
    [ { chainType = "ETH"
      ; addressDeposit = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
      ; tagDeposit = ""
      ; chain = "ETH"
      }
    ; { chainType = "TRX"
      ; addressDeposit = "TRx123456789abcdef"
      ; tagDeposit = ""
      ; chain = "TRX"
      }
    ; { chainType = "ARBITRUMONE"
      ; addressDeposit = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
      ; tagDeposit = ""
      ; chain = "ARBITRUMONE"
      }
    ]
  in

  let expected_networks = ["ETH"; "TRX"; "ARBITRUMONE"] in

  List.iter2_exn chains expected_networks ~f:(fun chain_info expected_net ->
    match normalize_deposit_address ~currency:"USDT" chain_info with
    | Ok addr ->
      ignore (assert_equal
        ~equal:[%equal: string option]
        ~sexp_of_t:[%sexp_of: string option]
        (Some expected_net) addr.network
        (sprintf "ChainType %s -> Network %s" chain_info.chainType expected_net))
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  FAIL: Chain %s should succeed: %s\n" chain_info.chainType msg)

let test_deposit_address_empty_tag () =
  printf "\n=== Deposit Address: Empty Tag ===\n";

  let chain_info : Mock_v5.Deposit_address.chain_info =
    { chainType = "ETH"
    ; addressDeposit = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; tagDeposit = ""
    ; chain = "ETH"
    }
  in

  match normalize_deposit_address ~currency:"ETH" chain_info with
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

(** Normalize Bybit deposit record to Types.Deposit.t *)
let normalize_deposit (dep : Mock_v5.Deposit_history.deposit_record)
    : (Types.Deposit.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind amount = Fluxum.Normalize_common.Float_conv.of_string dep.amount in
  let%bind status = bybit_deposit_status_to_transfer_status dep.status in
  let created_at =
    match Fluxum.Normalize_common.Float_conv.of_string dep.successAt with
    | Ok ms when Float.(ms > 0.) ->
      Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms ms))
    | _ -> None
  in
  let address = match String.is_empty dep.toAddress with
    | true -> None
    | false -> Some dep.toAddress
  in
  let tx_id = match String.is_empty dep.txID with
    | true -> None
    | false -> Some dep.txID
  in
  Ok (Types.Deposit.create
    ~venue:Types.Venue.Bybit
    ~id:dep.depositId
    ~currency:dep.coin
    ~amount
    ~status
    ?address
    ?tx_id
    ?created_at
    ())

let test_deposit_success () =
  printf "\n=== Deposit: Successful BTC Deposit ===\n";

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "BTC"
    ; chain = "BTC"
    ; amount = "0.5"
    ; txID = "abc123def456789"
    ; status = 3  (* success *)
    ; toAddress = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = "1640000000000"
    ; confirmations = "6"
    ; txIndex = "0"
    ; blockHash = "blockhash123"
    ; depositId = "12345678"
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

let test_deposit_processing () =
  printf "\n=== Deposit: Processing ETH Deposit ===\n";

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "ETH"
    ; chain = "ETH"
    ; amount = "10.25"
    ; txID = "0xtxhash"
    ; status = 2  (* processing *)
    ; toAddress = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = ""
    ; confirmations = "3"
    ; txIndex = "0"
    ; blockHash = ""
    ; depositId = "67890"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Processing deposit.status
      "Status is Processing");
    ignore (assert_float_equal 10.25 deposit.amount "Amount is 10.25")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Processing deposit should succeed: %s\n" msg

let test_deposit_failed () =
  printf "\n=== Deposit: Failed Deposit (Status 4) ===\n";

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "USDT"
    ; chain = "TRX"
    ; amount = "1000.0"
    ; txID = "trxtx123"
    ; status = 4  (* deposit failed *)
    ; toAddress = "TRX456"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = ""
    ; confirmations = "0"
    ; txIndex = "0"
    ; blockHash = ""
    ; depositId = "failed_dep_001"
    }
  in

  match normalize_deposit native_dep with
  | Ok deposit ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Failed deposit.status
      "Failed status mapped correctly")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Failed deposit should map correctly: %s\n" msg

let test_deposit_high_precision () =
  printf "\n=== Deposit: High Precision Amount ===\n";

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "BTC"
    ; chain = "BTC"
    ; amount = "0.00000001"
    ; txID = "satoshi_tx"
    ; status = 3
    ; toAddress = "bc1qtest"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = "1640000000000"
    ; confirmations = "6"
    ; txIndex = "0"
    ; blockHash = ""
    ; depositId = "tiny_dep"
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

(** Normalize Bybit withdrawal record to Types.Withdrawal.t *)
let normalize_withdrawal (wd : Mock_v5.Withdrawal_history.withdrawal_record)
    : (Types.Withdrawal.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind amount = Fluxum.Normalize_common.Float_conv.of_string wd.amount in
  let%bind fee = Fluxum.Normalize_common.Float_conv.of_string wd.withdrawFee in
  let%bind status = bybit_withdrawal_status_to_transfer_status wd.status in
  let created_at =
    match Fluxum.Normalize_common.Float_conv.of_string wd.createTime with
    | Ok ms when Float.(ms > 0.) ->
      Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms ms))
    | _ -> None
  in
  let updated_at =
    match Fluxum.Normalize_common.Float_conv.of_string wd.updateTime with
    | Ok ms when Float.(ms > 0.) ->
      Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_ms ms))
    | _ -> None
  in
  let tx_id = match String.is_empty wd.txID with
    | true -> None
    | false -> Some wd.txID
  in
  let tag = match String.is_empty wd.tag with
    | true -> None
    | false -> Some wd.tag
  in
  Ok (Types.Withdrawal.create
    ~venue:Types.Venue.Bybit
    ~id:wd.withdrawId
    ~currency:wd.coin
    ~amount
    ~fee
    ~status
    ~address:wd.toAddress
    ?tag
    ?tx_id
    ?created_at
    ?updated_at
    ())

let test_withdrawal_success () =
  printf "\n=== Withdrawal: Successful BTC Withdrawal ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_12345"
    ; coin = "BTC"
    ; chain = "BTC"
    ; amount = "1.0"
    ; withdrawFee = "0.0001"
    ; status = "success"
    ; toAddress = "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
    ; tag = ""
    ; txID = "txhash123456"
    ; createTime = "1640000000000"
    ; updateTime = "1640000100000"
    }
  in

  match normalize_withdrawal native_wd with
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

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_pending_001"
    ; coin = "ETH"
    ; chain = "ETH"
    ; amount = "5.0"
    ; withdrawFee = "0.001"
    ; status = "Pending"
    ; toAddress = "0x742d35Cc6634C0532925a3b844Bc9e7595f3aEc2"
    ; tag = ""
    ; txID = ""
    ; createTime = "1640000000000"
    ; updateTime = ""
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Processing withdrawal.status
      "Pending status maps to Processing");
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      None withdrawal.tx_id
      "Empty txID normalized to None")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Pending withdrawal should succeed: %s\n" msg

let test_withdrawal_security_check () =
  printf "\n=== Withdrawal: Security Check ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_security"
    ; coin = "USDT"
    ; chain = "TRX"
    ; amount = "1000.0"
    ; withdrawFee = "1.0"
    ; status = "SecurityCheck"
    ; toAddress = "TRx123"
    ; tag = ""
    ; txID = ""
    ; createTime = "1640000000000"
    ; updateTime = ""
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Pending withdrawal.status
      "SecurityCheck maps to Pending")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Security check withdrawal should succeed: %s\n" msg

let test_withdrawal_cancelled () =
  printf "\n=== Withdrawal: Cancelled by User ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_cancelled"
    ; coin = "BTC"
    ; chain = "BTC"
    ; amount = "0.5"
    ; withdrawFee = "0.0001"
    ; status = "CancelByUser"
    ; toAddress = "bc1qcancel"
    ; tag = ""
    ; txID = ""
    ; createTime = "1640000000000"
    ; updateTime = "1640000100000"
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Cancelled withdrawal.status
      "CancelByUser maps to Cancelled")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Cancelled withdrawal should succeed: %s\n" msg

let test_withdrawal_rejected () =
  printf "\n=== Withdrawal: Rejected ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_rejected"
    ; coin = "ETH"
    ; chain = "ETH"
    ; amount = "10.0"
    ; withdrawFee = "0.001"
    ; status = "Reject"
    ; toAddress = "0xreject"
    ; tag = ""
    ; txID = ""
    ; createTime = "1640000000000"
    ; updateTime = "1640000100000"
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Failed withdrawal.status
      "Reject maps to Failed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Rejected withdrawal should succeed: %s\n" msg

let test_withdrawal_blockchain_confirmed () =
  printf "\n=== Withdrawal: Blockchain Confirmed ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_confirmed"
    ; coin = "BTC"
    ; chain = "BTC"
    ; amount = "2.0"
    ; withdrawFee = "0.0001"
    ; status = "BlockchainConfirmed"
    ; toAddress = "bc1qconfirmed"
    ; tag = ""
    ; txID = "confirmed_tx_hash"
    ; createTime = "1640000000000"
    ; updateTime = "1640001000000"
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:Types.Transfer_status.equal
      ~sexp_of_t:Types.Transfer_status.sexp_of_t
      Types.Transfer_status.Completed withdrawal.status
      "BlockchainConfirmed maps to Completed")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: Confirmed withdrawal should succeed: %s\n" msg

let test_withdrawal_with_tag () =
  printf "\n=== Withdrawal: XRP with Tag ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_xrp"
    ; coin = "XRP"
    ; chain = "XRP"
    ; amount = "100.0"
    ; withdrawFee = "0.25"
    ; status = "success"
    ; toAddress = "rLW9gnQo7BQhU6igk5keqYnH3TVrCxGRzm"
    ; tag = "1234567890"
    ; txID = "xrp_tx_hash"
    ; createTime = "1640000000000"
    ; updateTime = "1640000100000"
    }
  in

  match normalize_withdrawal native_wd with
  | Ok withdrawal ->
    ignore (assert_equal
      ~equal:[%equal: string option]
      ~sexp_of_t:[%sexp_of: string option]
      (Some "1234567890") withdrawal.tag
      "Tag extracted correctly")
  | Error msg ->
    incr tests_run;
    incr tests_failed;
    printf "  FAIL: XRP withdrawal with tag should succeed: %s\n" msg

let test_withdrawal_zero_fee () =
  printf "\n=== Withdrawal: Zero Fee (Internal Transfer) ===\n";

  let native_wd : Mock_v5.Withdrawal_history.withdrawal_record =
    { withdrawId = "wd_internal"
    ; coin = "USDT"
    ; chain = "Internal"
    ; amount = "500.0"
    ; withdrawFee = "0"
    ; status = "success"
    ; toAddress = "internal_uid_123"
    ; tag = ""
    ; txID = ""
    ; createTime = "1640000000000"
    ; updateTime = "1640000000100"
    }
  in

  match normalize_withdrawal native_wd with
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

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "USDT"
    ; chain = "ETH"
    ; amount = "999999999999.99"
    ; txID = "largetx"
    ; status = 3
    ; toAddress = "0xlarge"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = "1640000000000"
    ; confirmations = "12"
    ; txIndex = "0"
    ; blockHash = ""
    ; depositId = "large_dep"
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

  let native_dep : Mock_v5.Deposit_history.deposit_record =
    { coin = "BTC"
    ; chain = "BTC"
    ; amount = "not_a_number"
    ; txID = ""
    ; status = 3
    ; toAddress = "bc1q"
    ; tag = ""
    ; depositFee = "0"
    ; successAt = ""
    ; confirmations = "0"
    ; txIndex = "0"
    ; blockHash = ""
    ; depositId = "bad_dep"
    }
  in

  let _ = assert_error
    (normalize_deposit native_dep)
    "Malformed amount returns Error" in
  ()

let test_case_insensitive_withdrawal_status () =
  printf "\n=== Edge: Case Insensitive Withdrawal Status ===\n";

  let cases =
    [ ("SUCCESS", Types.Transfer_status.Completed)
    ; ("Success", Types.Transfer_status.Completed)
    ; ("pending", Types.Transfer_status.Processing)
    ; ("PENDING", Types.Transfer_status.Processing)
    ; ("fail", Types.Transfer_status.Failed)
    ; ("FAIL", Types.Transfer_status.Failed)
    ]
  in

  List.iter cases ~f:(fun (status_str, expected) ->
    match bybit_withdrawal_status_to_transfer_status status_str with
    | Ok result ->
      ignore (assert_equal
        ~equal:Types.Transfer_status.equal
        ~sexp_of_t:Types.Transfer_status.sexp_of_t
        expected result
        (sprintf "Status '%s' case insensitive" status_str))
    | Error msg ->
      incr tests_run;
      incr tests_failed;
      printf "  FAIL: Status '%s' should succeed: %s\n" status_str msg)

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let run_all_tests () =
  printf "\n";
  printf "====================================================\n";
  printf "  Bybit Account Operations - Unit Test Suite\n";
  printf "====================================================\n";

  (* Status mapping tests *)
  printf "\n=== Status Mapping Tests ===\n";
  test_bybit_deposit_status_mapping ();
  test_bybit_withdrawal_status_mapping ();

  (* Deposit address tests *)
  printf "\n=== Deposit Address Tests ===\n";
  test_deposit_address_btc ();
  test_deposit_address_with_tag ();
  test_deposit_address_usdt_multi_chain ();
  test_deposit_address_empty_tag ();

  (* Deposit tests *)
  printf "\n=== Deposit Tests ===\n";
  test_deposit_success ();
  test_deposit_processing ();
  test_deposit_failed ();
  test_deposit_high_precision ();

  (* Withdrawal tests *)
  printf "\n=== Withdrawal Tests ===\n";
  test_withdrawal_success ();
  test_withdrawal_pending ();
  test_withdrawal_security_check ();
  test_withdrawal_cancelled ();
  test_withdrawal_rejected ();
  test_withdrawal_blockchain_confirmed ();
  test_withdrawal_with_tag ();
  test_withdrawal_zero_fee ();

  (* Edge cases *)
  printf "\n=== Edge Cases ===\n";
  test_very_large_amount ();
  test_malformed_amount ();
  test_case_insensitive_withdrawal_status ();

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

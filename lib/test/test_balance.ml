(** Balance Module Tests

    Tests for balance retrieval and consolidated balance functionality.
    Uses mock data to test normalization and aggregation logic.
*)

open Core

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

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

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match not (equal expected actual) with
  | true ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | false ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

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

(** ========== BINANCE BALANCE NORMALIZATION TESTS ========== *)

let test_binance_balance_normalization () =
  printf "\n=== Binance Balance Normalization ===\n";

  let native_balance : Binance.V3.Account.T.balance =
    { asset = "BTC"
    ; free = "1.5"
    ; locked = "0.5"
    }
  in

  (match Binance.Fluxum_adapter.Adapter.Normalize.balance native_balance with
   | Ok normalized ->
     let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
       "BTC" normalized.currency "Currency is BTC" in
     let _ = assert_float_equal 2.0 normalized.total "Total = 2.0 (1.5 + 0.5)" in
     let _ = assert_float_equal 1.5 normalized.available "Available = 1.5" in
     let _ = assert_float_equal 0.5 normalized.locked "Locked = 0.5" in
     let _ = assert_equal
       ~equal:[%equal: Fluxum.Types.Venue.t]
       ~sexp_of_t:[%sexp_of: Fluxum.Types.Venue.t]
       Fluxum.Types.Venue.Binance normalized.venue "Venue is Binance" in
     ()
   | Error msg ->
     failwith (Printf.sprintf "Failed to normalize balance: %s" msg))

(** ========== MEXC BALANCE NORMALIZATION TESTS ========== *)

let test_mexc_balance_normalization () =
  printf "\n=== MEXC Balance Normalization ===\n";

  let native_balance : Mexc.V1.Account.balance =
    { asset = "ETH"
    ; free = "10.25"
    ; locked = "2.75"
    }
  in

  let normalized = match Mexc.Fluxum_adapter.Adapter.Normalize.balance native_balance with
    | Ok bal -> bal
    | Error msg -> failwith (sprintf "Failed to normalize balance: %s" msg)
  in

  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "ETH" normalized.currency "Currency is ETH" in
  let _ = assert_float_equal 13.0 normalized.total "Total = 13.0 (10.25 + 2.75)" in
  let _ = assert_float_equal 10.25 normalized.available "Available = 10.25" in
  let _ = assert_float_equal 2.75 normalized.locked "Locked = 2.75" in
  let _ = assert_equal
    ~equal:[%equal: Fluxum.Types.Venue.t]
    ~sexp_of_t:[%sexp_of: Fluxum.Types.Venue.t]
    Fluxum.Types.Venue.Mexc normalized.venue "Venue is MEXC" in
  ()

(** ========== COINBASE BALANCE NORMALIZATION TESTS ========== *)

let test_coinbase_balance_normalization () =
  printf "\n=== Coinbase Balance Normalization ===\n";

  let native_balance : Coinbase.Rest.Account.account =
    { uuid = "test-uuid"
    ; name = "BTC Wallet"
    ; currency = "BTC"
    ; available_balance = { value = "5.5"; currency = "BTC" }
    ; default = true
    ; active = true
    ; created_at = None
    ; updated_at = None
    ; deleted_at = None
    ; type_ = Some "ACCOUNT_TYPE_CRYPTO"
    ; ready = true
    ; hold = Some { value = "1.5"; currency = "BTC" }
    }
  in

  let normalized = match Coinbase.Fluxum_adapter.Adapter.Normalize.balance native_balance with
    | Ok bal -> bal
    | Error e -> failwith (sprintf "Balance normalization failed: %s" e)
  in

  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC" normalized.currency "Currency is BTC" in
  let _ = assert_float_equal 7.0 normalized.total "Total = 7.0 (5.5 + 1.5)" in
  let _ = assert_float_equal 5.5 normalized.available "Available = 5.5" in
  let _ = assert_float_equal 1.5 normalized.locked "Locked = 1.5" in
  let _ = assert_equal
    ~equal:[%equal: Fluxum.Types.Venue.t]
    ~sexp_of_t:[%sexp_of: Fluxum.Types.Venue.t]
    Fluxum.Types.Venue.Coinbase normalized.venue "Venue is Coinbase" in
  ()

(** ========== BITRUE BALANCE NORMALIZATION TESTS ========== *)

let test_bitrue_balance_normalization () =
  printf "\n=== Bitrue Balance Normalization ===\n";

  let native_balance : Bitrue.Rest.Account.balance =
    { asset = "USDT"
    ; free = "1000.0"
    ; locked = "250.0"
    }
  in

  let normalized = match Bitrue.Fluxum_adapter.Adapter.Normalize.balance native_balance with
    | Ok bal -> bal
    | Error msg -> failwith (sprintf "Failed to normalize balance: %s" msg)
  in

  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "USDT" normalized.currency "Currency is USDT" in
  let _ = assert_float_equal 1250.0 normalized.total "Total = 1250.0" in
  let _ = assert_float_equal 1000.0 normalized.available "Available = 1000.0" in
  let _ = assert_float_equal 250.0 normalized.locked "Locked = 250.0" in
  let _ = assert_equal
    ~equal:[%equal: Fluxum.Types.Venue.t]
    ~sexp_of_t:[%sexp_of: Fluxum.Types.Venue.t]
    Fluxum.Types.Venue.Bitrue normalized.venue "Venue is Bitrue" in
  ()

(** ========== CONSOLIDATED BALANCE TESTS ========== *)

let test_aggregated_balance () =
  printf "\n=== Aggregated Balance ===\n";

  let empty = Consolidated_balance.Aggregated_balance.empty "BTC" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC" empty.currency "Empty balance currency" in
  let _ = assert_float_equal 0.0 empty.total_across_exchanges "Empty total = 0" in

  (* Add first balance *)
  let bal1 : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Binance
    ; currency = "BTC"
    ; total = 1.5
    ; available = 1.0
    ; locked = 0.5
    }
  in
  let agg1 = Consolidated_balance.Aggregated_balance.add_balance empty bal1 in
  let _ = assert_float_equal 1.5 agg1.total_across_exchanges "After 1st: total = 1.5" in
  let _ = assert_float_equal 1.0 agg1.available_across_exchanges "After 1st: available = 1.0" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (List.length agg1.by_exchange) "After 1st: 1 exchange" in

  (* Add second balance from different exchange *)
  let bal2 : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Coinbase
    ; currency = "BTC"
    ; total = 2.5
    ; available = 2.0
    ; locked = 0.5
    }
  in
  let agg2 = Consolidated_balance.Aggregated_balance.add_balance agg1 bal2 in
  let _ = assert_float_equal 4.0 agg2.total_across_exchanges "After 2nd: total = 4.0" in
  let _ = assert_float_equal 3.0 agg2.available_across_exchanges "After 2nd: available = 3.0" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (List.length agg2.by_exchange) "After 2nd: 2 exchanges" in
  ()

let test_exchange_result () =
  printf "\n=== Exchange Result ===\n";

  let success = Consolidated_balance.Exchange_result.success
    ~venue:Fluxum.Types.Venue.Binance
    ~balances:[]
    ~latency_ms:100.0
  in
  let _ = assert_true (Option.is_none success.error) "Success has no error" in
  let _ = assert_float_equal 100.0 success.latency_ms "Latency = 100ms" in

  let failure = Consolidated_balance.Exchange_result.failure
    ~venue:Fluxum.Types.Venue.Kraken
    ~error:"Connection timeout"
    ~latency_ms:30000.0
  in
  let _ = assert_true (Option.is_some failure.error) "Failure has error" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "Connection timeout" (Option.value_exn failure.error) "Error message matches" in
  ()

let test_consolidated_view () =
  printf "\n=== Consolidated View ===\n";

  let btc_balance : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Binance
    ; currency = "BTC"
    ; total = 1.0
    ; available = 0.8
    ; locked = 0.2
    }
  in
  let eth_balance : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Binance
    ; currency = "ETH"
    ; total = 10.0
    ; available = 8.0
    ; locked = 2.0
    }
  in

  let result1 = Consolidated_balance.Exchange_result.success
    ~venue:Fluxum.Types.Venue.Binance
    ~balances:[btc_balance; eth_balance]
    ~latency_ms:50.0
  in

  let btc_balance2 : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Coinbase
    ; currency = "BTC"
    ; total = 2.0
    ; available = 1.5
    ; locked = 0.5
    }
  in

  let result2 = Consolidated_balance.Exchange_result.success
    ~venue:Fluxum.Types.Venue.Coinbase
    ~balances:[btc_balance2]
    ~latency_ms:75.0
  in

  let view = Consolidated_balance.Consolidated_view.create
    ~exchange_results:[result1; result2]
  in

  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (List.length view.exchange_results) "2 exchange results" in

  (* Check BTC aggregation *)
  let btc_agg = Map.find_exn view.aggregated "BTC" in
  let _ = assert_float_equal 3.0 btc_agg.total_across_exchanges "BTC total = 3.0" in
  let _ = assert_float_equal 2.3 btc_agg.available_across_exchanges "BTC available = 2.3" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (List.length btc_agg.by_exchange) "BTC from 2 exchanges" in

  (* Check ETH aggregation *)
  let eth_agg = Map.find_exn view.aggregated "ETH" in
  let _ = assert_float_equal 10.0 eth_agg.total_across_exchanges "ETH total = 10.0" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (List.length eth_agg.by_exchange) "ETH from 1 exchange" in

  (* Check summary *)
  let summary = Consolidated_balance.Consolidated_view.summary view in
  let _ = assert_true (String.is_substring summary ~substring:"2 ok") "Summary shows 2 ok" in
  let _ = assert_true (String.is_substring summary ~substring:"0 failed") "Summary shows 0 failed" in
  ()

let test_non_zero_balances () =
  printf "\n=== Non-Zero Balances Filter ===\n";

  let zero_balance : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Binance
    ; currency = "DUST"
    ; total = 0.00000001  (* Dust *)
    ; available = 0.00000001
    ; locked = 0.0
    }
  in
  let real_balance : Fluxum.Types.Balance.t =
    { venue = Fluxum.Types.Venue.Binance
    ; currency = "BTC"
    ; total = 1.0
    ; available = 1.0
    ; locked = 0.0
    }
  in

  let result = Consolidated_balance.Exchange_result.success
    ~venue:Fluxum.Types.Venue.Binance
    ~balances:[zero_balance; real_balance]
    ~latency_ms:50.0
  in

  let view = Consolidated_balance.Consolidated_view.create ~exchange_results:[result] in
  let non_zero = Consolidated_balance.Consolidated_view.non_zero_balances view in

  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (List.length non_zero) "Only 1 non-zero balance (dust filtered)" in

  (match List.hd non_zero with
   | None -> ignore (assert_true false "Expected non-empty non_zero list")
   | Some (currency, _) ->
     ignore (assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
       "BTC" currency "Non-zero currency is BTC"));
  ()

(** ========== TEST RUNNER ========== *)

let () =
  printf "\n";
  printf "====================================================\n";
  printf "  Balance Module - Test Suite\n";
  printf "====================================================\n";

  (* Run all tests *)
  test_binance_balance_normalization ();
  test_mexc_balance_normalization ();
  test_coinbase_balance_normalization ();
  test_bitrue_balance_normalization ();

  test_aggregated_balance ();
  test_exchange_result ();
  test_consolidated_view ();
  test_non_zero_balances ();

  (* Print summary *)
  printf "\n";
  printf "====================================================\n";
  printf "  Test Results Summary\n";
  printf "====================================================\n";
  printf "  Total:  %3d tests\n" !tests_run;
  printf "  Passed: %3d tests  *\n" !tests_passed;
  printf "  Failed: %3d tests  X\n" !tests_failed;
  printf "====================================================\n";
  printf "\n";

  match !tests_failed > 0 with
  | true ->
    printf "X TEST SUITE FAILED\n\n";
    exit 1
  | false ->
    printf "* ALL TESTS PASSED!\n\n";
    exit 0

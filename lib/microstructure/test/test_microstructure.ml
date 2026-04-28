(** Tests for microstructure primitives — emphasizing the PM-aware gates
    (sampling-aliased, discrete-tick, settlement-contamination, zero-volume)
    fire correctly on the inputs they're designed to reject. *)

open Core

let approx_eq ?(eps = 1e-9) a b = Float.( <= ) (Float.abs (a -. b)) eps

let assert_approx_eq ~name ?eps actual expected =
  match approx_eq ?eps actual expected with
  | true -> printf "OK   %s: %.6f\n" name actual
  | false ->
    eprintf "FAIL %s: got %.10f, expected %.10f\n" name actual expected;
    exit 1

(* ============================== Spread =============================== *)

let test_spread_continuous () =
  (* Synthetic continuous-tick price series with known half-spread.
     Bid-ask bounce: prices alternate by ~0.5 around a mean. *)
  let prices = [| 100.001; 100.501; 100.002; 100.502; 100.003; 100.503; 100.004 |] in
  let result = Microstructure.Spread.roll ~prices ~tick_size:0.0 () in
    (* tick_size=0 disables discrete-tick gate *)
    match result with
    | Spread s ->
      printf "OK   spread continuous: estimated=%.4f\n" s;
      (match Float.( > ) s 0. with
       | true -> ()
       | false ->
         eprintf "FAIL spread continuous: expected positive estimate, got %f\n" s;
         exit 1)
    | other ->
      eprintf "FAIL spread continuous: unexpected result %s\n"
        (Sexp.to_string (Microstructure.Spread.sexp_of_result other));
      exit 1

let test_spread_discrete_tick_gate () =
  (* Integer-cent prices: should trigger discrete-tick warning even on
     reasonable-looking input. *)
  let prices = [| 50.; 51.; 50.; 51.; 50.; 51.; 50. |] in
  let result = Microstructure.Spread.roll ~prices () in
    match result with
    | Discrete_tick_warning { tick_size; _ } ->
      printf "OK   discrete_tick_warning fired: tick_size=%.4f\n" tick_size
    | other ->
      eprintf
        "FAIL discrete-tick gate: should have fired warning, got %s\n"
        (Sexp.to_string (Microstructure.Spread.sexp_of_result other));
      exit 1

let test_spread_kalshi_cents () =
  (* Kalshi-style 0.01 tick prices *)
  let prices = [| 0.50; 0.51; 0.50; 0.51; 0.50; 0.51; 0.50 |] in
  let result = Microstructure.Spread.roll ~prices () in
    match result with
    | Discrete_tick_warning { tick_size; _ } ->
      printf "OK   kalshi cents → discrete warning: tick=%.4f\n" tick_size
    | other ->
      eprintf
        "FAIL kalshi cents: should have triggered discrete warning, got %s\n"
        (Sexp.to_string (Microstructure.Spread.sexp_of_result other));
      exit 1

let test_spread_insufficient () =
  let result = Microstructure.Spread.roll ~prices:[| 1.; 2. |] () in
    match result with
    | Insufficient_data 2 -> printf "OK   spread insufficient_data\n"
    | other ->
      eprintf "FAIL spread insufficient: %s\n"
        (Sexp.to_string (Microstructure.Spread.sexp_of_result other));
      exit 1

(* ============================ Liquidity ============================== *)

let test_kyle_basic () =
  (* Synthetic: returns positively correlated with signed flow → positive lambda *)
  let returns = [| 0.001; -0.002; 0.003; -0.001; 0.002 |] in
  let signed_volumes = [| 100.; -150.; 200.; -50.; 175. |] in
  let result =
    Microstructure.Liquidity.kyle_lambda ~returns ~signed_volumes ()
  in
    match result with
    | Lambda l ->
      printf "OK   kyle_lambda positive: %.6e\n" l;
      (match Float.( > ) l 0. with
       | true -> ()
       | false ->
         eprintf "FAIL kyle_lambda: expected positive, got %f\n" l;
         exit 1)
    | other ->
      eprintf "FAIL kyle: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_kyle_result other));
      exit 1

let test_kyle_settlement_gate () =
  (* Contract expires in 30 minutes; min_hours_to_expiry default is 1.0 →
     should reject *)
  let now = Time_float_unix.now () in
  let expiry = Time_float_unix.add now (Time_float_unix.Span.of_min 30.) in
  let returns = [| 0.001; -0.002; 0.003 |] in
  let signed_volumes = [| 100.; -150.; 200. |] in
  let result =
    Microstructure.Liquidity.kyle_lambda
      ~returns
      ~signed_volumes
      ~expiry_time:expiry
      ~as_of:now
      ()
  in
    match result with
    | Settlement_contamination { hours_to_expiry } ->
      printf "OK   kyle settlement gate fired: %.2fh to expiry\n" hours_to_expiry
    | other ->
      eprintf "FAIL kyle settlement gate: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_kyle_result other));
      exit 1

let test_kyle_settlement_passthrough () =
  (* Contract expires 5h out → above default 1h cutoff, should compute *)
  let now = Time_float_unix.now () in
  let expiry = Time_float_unix.add now (Time_float_unix.Span.of_hr 5.) in
  let returns = [| 0.001; -0.002; 0.003 |] in
  let signed_volumes = [| 100.; -150.; 200. |] in
  let result =
    Microstructure.Liquidity.kyle_lambda
      ~returns
      ~signed_volumes
      ~expiry_time:expiry
      ~as_of:now
      ()
  in
    match result with
    | Lambda _ -> printf "OK   kyle 5h-to-expiry passthrough computes\n"
    | other ->
      eprintf "FAIL kyle 5h passthrough: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_kyle_result other));
      exit 1

let test_amihud_basic () =
  let daily_returns = [| 0.01; -0.02; 0.03; -0.01 |] in
  let daily_volumes = [| 1000.; 1500.; 800.; 1200. |] in
  let result =
    Microstructure.Liquidity.amihud_illiquidity
      ~daily_returns
      ~daily_volumes
      ()
  in
    match result with
    | Illiquidity x ->
      printf "OK   amihud basic: %.6e\n" x;
      (match Float.( > ) x 0. with
       | true -> ()
       | false ->
         eprintf "FAIL amihud basic: expected positive, got %g\n" x;
         exit 1)
    | other ->
      eprintf "FAIL amihud basic: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_amihud_result other));
      exit 1

let test_amihud_zero_volume () =
  let daily_returns = [| 0.01; -0.02; 0.03 |] in
  let daily_volumes = [| 0.; 0.; 0. |] in
  let result =
    Microstructure.Liquidity.amihud_illiquidity
      ~daily_returns
      ~daily_volumes
      ()
  in
    match result with
    | All_zero_volume { days_observed } ->
      printf "OK   amihud all_zero_volume gate: %d days\n" days_observed
    | other ->
      eprintf "FAIL amihud zero volume: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_amihud_result other));
      exit 1

let test_amihud_min_volume_gate () =
  (* All days fail min_volume=2000 *)
  let daily_returns = [| 0.01; -0.02; 0.03 |] in
  let daily_volumes = [| 1000.; 1500.; 800. |] in
  let result =
    Microstructure.Liquidity.amihud_illiquidity
      ~daily_returns
      ~daily_volumes
      ~min_volume:2000.
      ()
  in
    match result with
    | All_zero_volume _ ->
      printf "OK   amihud min_volume gate excludes all → all_zero_volume\n"
    | other ->
      eprintf "FAIL amihud min_volume gate: %s\n"
        (Sexp.to_string (Microstructure.Liquidity.sexp_of_amihud_result other));
      exit 1

(* ============================ Order_flow ============================= *)

let make_snap ts bids asks : Microstructure.Order_flow.book_snapshot =
  { timestamp = ts
  ; bids = Array.of_list bids
  ; asks = Array.of_list asks
  }

let test_imbalance_basic () =
  let now = Time_float_unix.now () in
  let snaps =
    [| make_snap now [(99.5, 10.); (99.4, 5.)] [(100.5, 3.); (100.6, 2.)] |]
  in
  let result = Microstructure.Order_flow.orderbook_imbalance ~snapshots:snaps () in
    match result with
    | Imbalance x ->
      (* bids = 15, asks = 5, total = 20, imbalance = 0.5 *)
      assert_approx_eq ~name:"imbalance bid-heavy" x 0.5 ~eps:1e-9
    | other ->
      eprintf "FAIL imbalance basic: %s\n"
        (Sexp.to_string
           (Microstructure.Order_flow.sexp_of_imbalance_result other));
      exit 1

let test_imbalance_sampling_aliased () =
  (* 5 snapshots 30s apart; gate at 1.0s should reject *)
  let base = Time_float_unix.now () in
  let snaps =
    Array.init 5 ~f:(fun i ->
      let ts =
        Time_float_unix.add base (Time_float_unix.Span.of_sec (Float.of_int (i * 30)))
      in
        make_snap ts [(99.5, 10.)] [(100.5, 10.)])
  in
  let result =
    Microstructure.Order_flow.orderbook_imbalance
      ~snapshots:snaps
      ~max_inter_sample_seconds:1.0
      ()
  in
    match result with
    | Sampling_aliased { observed_dt; threshold } ->
      printf
        "OK   sampling_aliased gate fired: dt=%.1fs > threshold=%.1fs\n"
        observed_dt
        threshold
    | other ->
      eprintf "FAIL sampling gate: %s\n"
        (Sexp.to_string
           (Microstructure.Order_flow.sexp_of_imbalance_result other));
      exit 1

let test_imbalance_no_gate () =
  (* No max_inter_sample_seconds → should accept even coarse data *)
  let base = Time_float_unix.now () in
  let snaps =
    Array.init 3 ~f:(fun i ->
      let ts =
        Time_float_unix.add base (Time_float_unix.Span.of_sec (Float.of_int (i * 60)))
      in
        make_snap ts [(99.5, 10.)] [(100.5, 5.)])
  in
  let result = Microstructure.Order_flow.orderbook_imbalance ~snapshots:snaps () in
    match result with
    | Imbalance _ ->
      printf "OK   imbalance no-gate: accepts coarse data when caller asserts continuous\n"
    | other ->
      eprintf "FAIL no-gate: %s\n"
        (Sexp.to_string
           (Microstructure.Order_flow.sexp_of_imbalance_result other));
      exit 1

(* ================================ OFI ================================ *)

let make_trade ~tid ~ts ~price ~amount ~aggressor : Microstructure.Ofi.Trade.t =
  { tid = Int64.of_int tid
  ; timestamp = ts
  ; price
  ; amount
  ; aggressor
  }

let test_ofi_no_lookahead () =
  (* The trade at index i must NOT include itself in its own OFI window.
     Two trades at the same instant: the second's OFI window contains the
     first only. *)
  let ts0 = Time_ns_unix.now () in
  let trades =
    [| make_trade ~tid:1 ~ts:ts0 ~price:100. ~amount:1. ~aggressor:`Buy
     ; make_trade ~tid:2
         ~ts:(Time_ns_unix.add ts0 (Time_ns.Span.of_min 1.))
         ~price:100.
         ~amount:1.
         ~aggressor:`Sell
    |]
  in
  let tagged =
    Microstructure.Ofi.compute
      ~trades
      ~window:(Time_ns.Span.of_hr 1.)
      ~min_window_trades:1
      ()
  in
  let t0 = tagged.(0) in
  let t1 = tagged.(1) in
  (* t0 has nothing before it → No_data or Insufficient *)
  let t0_ok =
    match t0.ofi with
    | No_data | Insufficient_trades 0 -> true
    | _ -> false
  in
  (* t1's window contains only t0 (a buy of value 100); no sells → Saturated_buy *)
  let t1_ok =
    match t1.ofi with
    | Saturated_buy -> true
    | _ -> false
  in
    match t0_ok && t1_ok with
    | true -> printf "OK   ofi look-ahead-safe: t0=No_data, t1=Saturated_buy\n"
    | false ->
      eprintf "FAIL ofi look-ahead: t0=%s, t1=%s\n"
        (Sexp.to_string (Microstructure.Ofi.sexp_of_ofi_value t0.ofi))
        (Sexp.to_string (Microstructure.Ofi.sexp_of_ofi_value t1.ofi));
      exit 1

let test_ofi_basic_ratio () =
  let ts0 = Time_ns_unix.now () in
  let mins n =
    Time_ns_unix.add ts0 (Time_ns.Span.of_min (Float.of_int n))
  in
  let trades =
    [| make_trade ~tid:1 ~ts:(mins 0) ~price:100. ~amount:2. ~aggressor:`Buy
     ; make_trade ~tid:2 ~ts:(mins 5) ~price:100. ~amount:1. ~aggressor:`Buy
     ; make_trade ~tid:3 ~ts:(mins 10) ~price:100. ~amount:1. ~aggressor:`Sell
     (* The 11th-min trade's window contains all 3 above:
        buy_value = (100*2 + 100*1) = 300; sell_value = 100
        OFI = 3.0 *)
     ; make_trade ~tid:4 ~ts:(mins 11) ~price:100. ~amount:1. ~aggressor:`Buy
    |]
  in
  let tagged =
    Microstructure.Ofi.compute
      ~trades
      ~window:(Time_ns.Span.of_hr 1.)
      ~min_window_trades:3
      ()
  in
  let t3 = tagged.(3) in
    match t3.ofi with
    | Ofi v ->
      assert_approx_eq ~name:"ofi 3:1 ratio" v 3.0 ~eps:1e-9;
      assert_approx_eq
        ~name:"ofi window buy_value"
        t3.window_buy_value
        300.
        ~eps:1e-9;
      assert_approx_eq
        ~name:"ofi window sell_value"
        t3.window_sell_value
        100.
        ~eps:1e-9
    | other ->
      eprintf "FAIL ofi basic: %s\n"
        (Sexp.to_string (Microstructure.Ofi.sexp_of_ofi_value other));
      exit 1

let test_ofi_min_window_trades_gate () =
  let ts0 = Time_ns_unix.now () in
  let trades =
    [| make_trade ~tid:1 ~ts:ts0 ~price:100. ~amount:1. ~aggressor:`Buy
     ; make_trade ~tid:2
         ~ts:(Time_ns_unix.add ts0 (Time_ns.Span.of_sec 1.))
         ~price:100.
         ~amount:1.
         ~aggressor:`Buy
    |]
  in
  let tagged =
    Microstructure.Ofi.compute
      ~trades
      ~window:(Time_ns.Span.of_hr 1.)
      ~min_window_trades:5
      ()
  in
  let t1 = tagged.(1) in
    match t1.ofi with
    | Insufficient_trades 1 ->
      printf "OK   ofi min_window_trades gate fires: 1 < 5\n"
    | other ->
      eprintf "FAIL ofi min_window_trades: %s\n"
        (Sexp.to_string (Microstructure.Ofi.sexp_of_ofi_value other));
      exit 1

let test_ofi_default_bins_shape () =
  let bins = Microstructure.Ofi.default_bins in
  let n = List.length bins in
    match n = 5 with
    | true -> printf "OK   ofi default_bins has 5 entries\n"
    | false ->
      eprintf "FAIL ofi default_bins: expected 5, got %d\n" n;
      exit 1

let test_predictive_validity_seeded_reproducibility () =
  (* Two runs with same seed must produce bit-identical p-values.
     Two runs with different seeds should differ (with overwhelming
     probability) on at least one shuffle outcome. *)
  let ts0 = Time_ns_unix.now () in
  let mins n =
    Time_ns_unix.add ts0 (Time_ns.Span.of_min (Float.of_int n))
  in
  let trades =
    Array.init 50 ~f:(fun i ->
      let aggressor = match i % 3 with 0 -> `Buy | _ -> `Sell in
        make_trade ~tid:i ~ts:(mins i) ~price:100. ~amount:1. ~aggressor)
  in
  let tagged =
    Microstructure.Ofi.compute
      ~trades
      ~window:(Time_ns.Span.of_min 30.)
      ~min_window_trades:1
      ()
  in
  let mid_series =
    Array.init 100 ~f:(fun i ->
      (mins i, 100. +. (Float.of_int i *. 0.01)))
  in
  let run ~seed =
    Microstructure.Ofi.predictive_validity
      ~tagged_trades:tagged
      ~mid_series
      ~fwd_windows:[Time_ns.Span.of_min 5.]
      ~n_shuffles:200
      ~seed
      ()
  in
  let r1 = run ~seed:42 in
  let r2 = run ~seed:42 in
  let r3 = run ~seed:43 in
  let p1 = r1.shuffle_tests.(0).p_value in
  let p2 = r2.shuffle_tests.(0).p_value in
  let p3 = r3.shuffle_tests.(0).p_value in
    match Float.equal p1 p2 with
    | false ->
      eprintf "FAIL seeded reproducibility: seed=42 → p=%.6f vs p=%.6f\n" p1 p2;
      exit 1
    | true ->
      printf
        "OK   seeded reproducibility: seed=42 → p=%.4f (bit-identical), seed=43 → p=%.4f\n"
        p1
        p3

let test_predictive_validity_smoke () =
  (* Synthetic: 30 trades over 30 minutes, alternating buy/sell with
     mid drifting up. Expect non-NaN result; we don't assert specific
     p-values since 30 trades + 100 shuffles is too small for stable
     statistics. *)
  let ts0 = Time_ns_unix.now () in
  let mins n =
    Time_ns_unix.add ts0 (Time_ns.Span.of_min (Float.of_int n))
  in
  let trades =
    Array.init 30 ~f:(fun i ->
      let aggressor = match i % 2 with 0 -> `Buy | _ -> `Sell in
        make_trade ~tid:i ~ts:(mins i) ~price:100. ~amount:1. ~aggressor)
  in
  let tagged =
    Microstructure.Ofi.compute
      ~trades
      ~window:(Time_ns.Span.of_min 30.)
      ~min_window_trades:1
      ()
  in
  let mid_series =
    Array.init 60 ~f:(fun i ->
      (mins i, 100. +. (Float.of_int i *. 0.01)))
  in
  let result =
    Microstructure.Ofi.predictive_validity
      ~tagged_trades:tagged
      ~mid_series
      ~fwd_windows:[Time_ns.Span.of_min 5.]
      ~n_shuffles:100
      ()
  in
  let n_bins_per_window = List.length Microstructure.Ofi.default_bins in
    match Array.length result.per_bin_drift = n_bins_per_window with
    | true ->
      printf
        "OK   predictive_validity smoke: %d bin-drifts, %d shuffle tests\n"
        (Array.length result.per_bin_drift)
        (Array.length result.shuffle_tests)
    | false ->
      eprintf "FAIL predictive_validity smoke: bin count mismatch\n";
      exit 1

(* ========================= Pm_quality_gates ========================== *)

(** Test cases mirror bluxit's docs/gate_validation_v1.md named-cohort
    table — gates must fire on the markets the empirical work flagged
    as failures, and pass markets it flagged as clean. *)

let test_near_rail_kxfed_t125_4_23 () =
  (* KXFED-27APR-T1.25 (4/23): expected to fire Near_rail per bluxit memo *)
  match
    Microstructure.Pm_quality_gates.check_near_rail
      ~max_yes_bid:96
      ~min_yes_ask_nonzero:97
      ()
  with
  | Some _ -> printf "OK   near_rail fires on KXFED-T1.25 4/23 (yes_bid=96)\n"
  | None ->
    eprintf "FAIL near_rail should fire on yes_bid=96\n";
    exit 1

let test_near_rail_passes_clean () =
  (* mid-range bid/ask, clearly clean *)
  match
    Microstructure.Pm_quality_gates.check_near_rail
      ~max_yes_bid:46
      ~min_yes_ask_nonzero:29
      ()
  with
  | None -> printf "OK   near_rail passes on KXCAGOVPRIMARY1ST (yb=46, ya=29)\n"
  | Some _ ->
    eprintf "FAIL near_rail should pass on (46, 29)\n";
    exit 1

let test_near_rail_low_threshold () =
  (* Custom threshold of 10 → yes_bid=46 still passes (54-distance from 100) *)
  match
    Microstructure.Pm_quality_gates.check_near_rail
      ~threshold:10
      ~max_yes_bid:46
      ~min_yes_ask_nonzero:29
      ()
  with
  | None -> printf "OK   near_rail respects custom threshold\n"
  | Some _ ->
    eprintf "FAIL near_rail with threshold=10 should pass (yb=46)\n";
    exit 1

let test_near_rail_zero_ask_not_rail () =
  (* yes_ask=0 means no nonzero ask observed (One_sided_book case),
     not a near-rail case. check_near_rail should NOT fire just because
     ask=0. *)
  match
    Microstructure.Pm_quality_gates.check_near_rail
      ~max_yes_bid:30
      ~min_yes_ask_nonzero:0
      ()
  with
  | None -> printf "OK   near_rail correctly skips (yb=30, ya=0 → one-sided)\n"
  | Some _ ->
    eprintf "FAIL near_rail incorrectly fires on yes_ask=0 (one-sided case)\n";
    exit 1

let test_regime_break_kxalbumsales () =
  (* KXALBUMSALES 32→96 = 64c jump, fires both Near_rail and Regime_break *)
  match
    Microstructure.Pm_quality_gates.check_regime_break
      ~mid_start:32.
      ~mid_end:96.
      ()
  with
  | Some r ->
    assert_approx_eq ~name:"regime_break delta" r.delta 64. ~eps:1e-9
  | None ->
    eprintf "FAIL regime_break should fire on 32→96 jump\n";
    exit 1

let test_regime_break_kxcagovprimary_passes () =
  (* mid 51→29 = 22c < 40c default → passes regime_break. The clean miss
     bluxit's memo specifically called out — gates pass but ticker still
     drove 138% concentration. Per-market gate is correctly innocent;
     concentration is a cohort-level concern. *)
  match
    Microstructure.Pm_quality_gates.check_regime_break
      ~mid_start:51.
      ~mid_end:29.
      ()
  with
  | None ->
    printf "OK   regime_break passes on KXCAGOVPRIMARY1ST 51→29 (Δ=22 < 40)\n"
  | Some _ ->
    eprintf "FAIL regime_break should pass on 22c jump\n";
    exit 1

let test_one_sided_book_ask_empty () =
  (* Fed-strike T1.00 (4/25) per memo: One_sided_book(ask) *)
  match
    Microstructure.Pm_quality_gates.check_one_sided_book
      ~yes_bid_window:[ 30; 32; 31; 30 ]
      ~yes_ask_window:[ 0; 0; 0; 0 ]
  with
  | Some r ->
    (match r.side with
     | `Ask_empty ->
       printf "OK   one_sided_book fires (Ask_empty) on KXFED-T1.00 4/25\n"
     | `Bid_empty ->
       eprintf "FAIL expected Ask_empty, got Bid_empty\n";
       exit 1)
  | None ->
    eprintf "FAIL one_sided_book should fire when ask all zero\n";
    exit 1

let test_one_sided_book_passes_normal () =
  match
    Microstructure.Pm_quality_gates.check_one_sided_book
      ~yes_bid_window:[ 30; 32; 31; 30 ]
      ~yes_ask_window:[ 35; 36; 35; 34 ]
  with
  | None -> printf "OK   one_sided_book passes on two-sided book\n"
  | Some _ ->
    eprintf "FAIL one_sided_book should pass on two-sided book\n";
    exit 1

let test_check_all_per_market_pass () =
  (* KXCAGOVPRIMARY1ST-XBEC: passes ALL per-market gates per bluxit memo
     (this is the case that drove 138% concentration but per-market gates
     correctly mark clean — concentration is a separate level). *)
  let result =
    Microstructure.Pm_quality_gates.check_all_per_market
      ~max_yes_bid:46
      ~min_yes_ask_nonzero:29
      ~mid_start:(Some 51.)
      ~mid_end:(Some 29.)
      ~yes_bid_window:[ 50; 51; 49; 46; 30 ]
      ~yes_ask_window:[ 52; 53; 51; 48; 32 ]
      ()
  in
    match Microstructure.Pm_quality_gates.is_pass result with
    | true ->
      printf "OK   check_all_per_market: KXCAGOVPRIMARY1ST passes (cohort-level concern)\n"
    | false ->
      eprintf "FAIL KXCAGOVPRIMARY1ST should pass per-market gates\n";
      exit 1

let test_check_all_per_market_combined_fire () =
  (* KXFED-27APR-T1.25 4/25: fires both Near_rail AND One_sided_book *)
  let result =
    Microstructure.Pm_quality_gates.check_all_per_market
      ~max_yes_bid:97
      ~min_yes_ask_nonzero:0  (* one-sided *)
      ~mid_start:(Some 90.)
      ~mid_end:(Some 95.)
      ~yes_bid_window:[ 95; 96; 97; 96 ]
      ~yes_ask_window:[ 0; 0; 0; 0 ]  (* ask empty *)
      ()
  in
    match
      Option.is_some result.near_rail
      && Option.is_some result.one_sided_book
    with
    | true ->
      printf "OK   check_all_per_market: T1.25 4/25 fires Near_rail + One_sided_book\n"
    | false ->
      eprintf "FAIL T1.25 4/25 should fire BOTH Near_rail and One_sided_book\n";
      exit 1

(* =============================== Main ================================ *)

let () =
  test_spread_continuous ();
  test_spread_discrete_tick_gate ();
  test_spread_kalshi_cents ();
  test_spread_insufficient ();
  test_kyle_basic ();
  test_kyle_settlement_gate ();
  test_kyle_settlement_passthrough ();
  test_amihud_basic ();
  test_amihud_zero_volume ();
  test_amihud_min_volume_gate ();
  test_imbalance_basic ();
  test_imbalance_sampling_aliased ();
  test_imbalance_no_gate ();
  test_ofi_no_lookahead ();
  test_ofi_basic_ratio ();
  test_ofi_min_window_trades_gate ();
  test_ofi_default_bins_shape ();
  test_predictive_validity_smoke ();
  test_predictive_validity_seeded_reproducibility ();
  test_near_rail_kxfed_t125_4_23 ();
  test_near_rail_passes_clean ();
  test_near_rail_low_threshold ();
  test_near_rail_zero_ask_not_rail ();
  test_regime_break_kxalbumsales ();
  test_regime_break_kxcagovprimary_passes ();
  test_one_sided_book_ask_empty ();
  test_one_sided_book_passes_normal ();
  test_check_all_per_market_pass ();
  test_check_all_per_market_combined_fire ();
  printf "\nAll Microstructure tests passed.\n"

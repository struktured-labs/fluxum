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
  printf "\nAll Microstructure tests passed.\n"

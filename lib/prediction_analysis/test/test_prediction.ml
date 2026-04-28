(** Deterministic unit tests for prediction-market analytics. *)

open Core

let approx_eq ?(eps = 1e-9) a b =
  Float.( <= ) (Float.abs (a -. b)) eps

let assert_approx_eq ~name ?eps actual expected =
  match approx_eq ?eps actual expected with
  | true -> printf "OK   %s: %.6f\n" name actual
  | false ->
    eprintf "FAIL %s: got %.10f, expected %.10f\n" name actual expected;
    exit 1

let assert_int_eq ~name actual expected =
  match actual = expected with
  | true -> printf "OK   %s: %d\n" name actual
  | false ->
    eprintf "FAIL %s: got %d, expected %d\n" name actual expected;
    exit 1

(* ============================ Probability ============================ *)

let test_kalshi_round_trip () =
  let cents = 42 in
  let p = Prediction_analysis.Probability.from_kalshi_cents cents in
    assert_approx_eq ~name:"kalshi 42c → 0.42" p 0.42 ~eps:1e-9;
  let back = Prediction_analysis.Probability.to_kalshi_cents p in
    assert_int_eq ~name:"kalshi round-trip 42" back 42

let test_decimal_odds () =
  (* Decimal odds 2.0 = 50% prob *)
  assert_approx_eq
    ~name:"decimal 2.0 → 0.5"
    (Prediction_analysis.Probability.from_decimal_odds 2.0)
    0.5
    ~eps:1e-9;
  (* Decimal odds 5.0 = 20% prob *)
  assert_approx_eq
    ~name:"decimal 5.0 → 0.2"
    (Prediction_analysis.Probability.from_decimal_odds 5.0)
    0.2
    ~eps:1e-9;
  (* Round trip *)
  assert_approx_eq
    ~name:"decimal round-trip"
    (Prediction_analysis.Probability.to_decimal_odds 0.25)
    4.0
    ~eps:1e-9

let test_fractional_odds () =
  (* 5/2 odds = 2/(2+5) = 2/7 *)
  assert_approx_eq
    ~name:"fractional 5/2 → 2/7"
    (Prediction_analysis.Probability.from_fractional_odds ~num:5 ~denom:2)
    (2. /. 7.)
    ~eps:1e-9;
  (* Even odds: 1/1 → 0.5 *)
  assert_approx_eq
    ~name:"fractional 1/1 → 0.5"
    (Prediction_analysis.Probability.from_fractional_odds ~num:1 ~denom:1)
    0.5
    ~eps:1e-9

let test_american_odds () =
  (* +200 underdog: stake $100 to win $200, prob = 100/300 = 1/3 *)
  assert_approx_eq
    ~name:"american +200 → 1/3"
    (Prediction_analysis.Probability.from_american_odds 200)
    (1. /. 3.)
    ~eps:1e-9;
  (* -150 favorite: stake $150 to win $100, prob = 150/250 = 0.6 *)
  assert_approx_eq
    ~name:"american -150 → 0.6"
    (Prediction_analysis.Probability.from_american_odds (-150))
    0.6
    ~eps:1e-9;
  (* Even money +100: prob = 0.5 *)
  assert_approx_eq
    ~name:"american +100 → 0.5"
    (Prediction_analysis.Probability.from_american_odds 100)
    0.5
    ~eps:1e-9

let test_overround () =
  (* Fairly priced binary: yes 0.5, no 0.5 → overround 0 *)
  assert_approx_eq
    ~name:"fair binary overround"
    (Prediction_analysis.Probability.implied_overround
       ~probs:[| 0.5; 0.5 |])
    0.
    ~eps:1e-9;
  (* Vig'd binary: yes 0.55, no 0.50 → overround 0.05 *)
  assert_approx_eq
    ~name:"vig'd overround"
    (Prediction_analysis.Probability.implied_overround
       ~probs:[| 0.55; 0.50 |])
    0.05
    ~eps:1e-9;
  (* Cross-venue arb: yes 0.50, no 0.45 → overround -0.05 (arb!) *)
  assert_approx_eq
    ~name:"arb overround (negative)"
    (Prediction_analysis.Probability.implied_overround
       ~probs:[| 0.50; 0.45 |])
    (-0.05)
    ~eps:1e-9

let test_remove_overround () =
  let probs = [| 0.55; 0.50 |] in
  let normalized = Prediction_analysis.Probability.remove_overround ~probs in
  let sum = Array.fold normalized ~init:0. ~f:( +. ) in
    assert_approx_eq
      ~name:"remove_overround sums to 1"
      sum
      1.
      ~eps:1e-9;
    assert_approx_eq
      ~name:"remove_overround[0]"
      normalized.(0)
      (0.55 /. 1.05)
      ~eps:1e-9

(* ============================ Calibration ============================ *)

let test_brier () =
  (* Perfect prediction: pred=1, outcome=1 → brier 0 *)
  assert_approx_eq
    ~name:"brier perfect"
    (Prediction_analysis.Calibration.brier_score ~pred:1. ~outcome:1.)
    0.
    ~eps:1e-12;
  (* Worst prediction: pred=0, outcome=1 → brier 1 *)
  assert_approx_eq
    ~name:"brier worst"
    (Prediction_analysis.Calibration.brier_score ~pred:0. ~outcome:1.)
    1.
    ~eps:1e-12;
  (* Coin flip: pred=0.5, outcome=anything → brier 0.25 *)
  assert_approx_eq
    ~name:"brier coin flip (outcome=1)"
    (Prediction_analysis.Calibration.brier_score ~pred:0.5 ~outcome:1.)
    0.25
    ~eps:1e-12

let test_brier_avg () =
  let preds = [| 0.9; 0.1; 0.5 |] in
  let outcomes = [| 1.; 0.; 1. |] in
  let avg =
    Prediction_analysis.Calibration.brier_score_avg ~preds ~outcomes
  in
  (* (0.01 + 0.01 + 0.25) / 3 = 0.09 *)
    assert_approx_eq ~name:"brier_avg" avg 0.09 ~eps:1e-9

let test_log_loss () =
  (* Perfect: pred=1.0, outcome=1.0; clipped to 1-eps so loss is tiny *)
  let perfect =
    Prediction_analysis.Calibration.log_loss ~pred:1.0 ~outcome:1.0 ()
  in
    (* Should be very close to 0 *)
    match Float.( < ) perfect 1e-10 with
    | true -> printf "OK   log_loss perfect ~ 0: %g\n" perfect
    | false ->
      eprintf "FAIL log_loss perfect should be near 0, got %g\n" perfect;
      exit 1

let test_calibration_bins () =
  (* 10 predictions, 5 bins. Pred=0.1 outcome=0 (4x), pred=0.9 outcome=1 (4x),
     pred=0.5 outcome=1 (2x). Well-calibrated. *)
  let preds = [| 0.1; 0.1; 0.1; 0.1; 0.5; 0.5; 0.9; 0.9; 0.9; 0.9 |] in
  let outcomes = [| 0.; 0.; 0.; 0.; 1.; 1.; 1.; 1.; 1.; 1. |] in
  let bins =
    Prediction_analysis.Calibration.calibration_bins
      ~preds
      ~outcomes
      ~n_bins:5
  in
    assert_int_eq ~name:"bins length" (List.length bins) 5;
  let total =
    List.fold bins ~init:0 ~f:(fun acc (b : Prediction_analysis.Calibration.bin) -> acc + b.count)
  in
    assert_int_eq ~name:"bins total count" total 10

let test_ece () =
  (* Perfectly calibrated: pred=0.5 in bin [0.4,0.6); 50% outcome rate *)
  let preds = [| 0.5; 0.5; 0.5; 0.5 |] in
  let outcomes = [| 0.; 0.; 1.; 1. |] in
  let bins =
    Prediction_analysis.Calibration.calibration_bins
      ~preds
      ~outcomes
      ~n_bins:5
  in
  let ece =
    Prediction_analysis.Calibration.expected_calibration_error ~bins
  in
  (* All preds 0.5; outcomes avg 0.5; deviation = 0 *)
    assert_approx_eq ~name:"ECE perfect calibration" ece 0. ~eps:1e-9

(* ============================= Arbitrage ============================= *)

let test_arb_obvious () =
  (* Same event:
     Kalshi: yes at 50c (prob 0.50)
     Polymarket: no at 45c (prob 0.45)
     Sum = 0.95 → buy both, lock in 0.05 profit per unit *)
  let quotes : Prediction_analysis.Arbitrage.quote list =
    [ { venue = "kalshi"; side = `Yes; price = 0.50; size = 100. }
    ; { venue = "polymarket"; side = `No; price = 0.45; size = 50. }
    ]
  in
    match Prediction_analysis.Arbitrage.best_two_leg ~quotes with
    | None ->
      eprintf "FAIL arb_obvious: expected an opportunity\n";
      exit 1
    | Some op ->
      assert_approx_eq
        ~name:"arb cost"
        op.cost
        0.95
        ~eps:1e-9;
      assert_approx_eq
        ~name:"arb profit_per_unit"
        op.profit_per_unit
        0.05
        ~eps:1e-9;
      assert_approx_eq
        ~name:"arb max_size = min"
        op.max_size
        50.
        ~eps:1e-9

let test_arb_none () =
  (* Same venue both legs: should NOT count as cross-venue arb *)
  let quotes : Prediction_analysis.Arbitrage.quote list =
    [ { venue = "kalshi"; side = `Yes; price = 0.40; size = 100. }
    ; { venue = "kalshi"; side = `No; price = 0.50; size = 100. }
    ]
  in
    match Prediction_analysis.Arbitrage.best_two_leg ~quotes with
    | None -> printf "OK   arb_none: same-venue correctly excluded\n"
    | Some _ ->
      eprintf "FAIL arb_none: should not return same-venue arb\n";
      exit 1

let test_arb_all () =
  (* Three venues, multiple arb opportunities; check we sort by profit *)
  let quotes : Prediction_analysis.Arbitrage.quote list =
    [ { venue = "a"; side = `Yes; price = 0.45; size = 100. }
    ; { venue = "b"; side = `Yes; price = 0.40; size = 100. } (* cheaper *)
    ; { venue = "c"; side = `No; price = 0.50; size = 100. }  (* a+c = 0.95 *)
    ; { venue = "d"; side = `No; price = 0.45; size = 100. }  (* b+d = 0.85 *)
    ]
  in
  let all = Prediction_analysis.Arbitrage.all_two_leg ~quotes in
  let n = List.length all in
    match n >= 2 with
    | false ->
      eprintf "FAIL arb_all: expected >= 2 ops, got %d\n" n;
      exit 1
    | true ->
      let top = List.hd_exn all in
        (* Best should be the b+d combo, profit 0.15 *)
        assert_approx_eq
          ~name:"arb_all best profit"
          top.profit_per_unit
          0.15
          ~eps:1e-9

(* =============================== Main ================================ *)

let () =
  test_kalshi_round_trip ();
  test_decimal_odds ();
  test_fractional_odds ();
  test_american_odds ();
  test_overround ();
  test_remove_overround ();
  test_brier ();
  test_brier_avg ();
  test_log_loss ();
  test_calibration_bins ();
  test_ece ();
  test_arb_obvious ();
  test_arb_none ();
  test_arb_all ();
  printf "\nAll Prediction_analysis tests passed.\n"

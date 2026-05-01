(** Deterministic unit tests for [Analysis.{Returns,Stats,Bars}].

    No I/O, no network, fast. Each test prints OK or fails the executable. *)

open Core

let approx_eq ?(eps = 1e-9) a b =
  Float.( <= ) (Float.abs (a -. b)) eps

let assert_approx_eq ~name ?eps actual expected =
  match approx_eq ?eps actual expected with
  | true -> printf "OK   %s: %.6f\n" name actual
  | false ->
    eprintf "FAIL %s: got %.10f, expected %.10f (delta %.10g)\n"
      name actual expected (Float.abs (actual -. expected));
    exit 1

let assert_int_eq ~name actual expected =
  match actual = expected with
  | true -> printf "OK   %s: %d\n" name actual
  | false ->
    eprintf "FAIL %s: got %d, expected %d\n" name actual expected;
    exit 1

let assert_array_approx_eq ~name ?eps actual expected =
  let n = Array.length actual in
  let m = Array.length expected in
    match n <> m with
    | true ->
      eprintf "FAIL %s: length mismatch %d vs %d\n" name n m;
      exit 1
    | false ->
      let bad =
        Array.findi actual ~f:(fun i a ->
          not (approx_eq ?eps a expected.(i)))
      in
        match bad with
        | None -> printf "OK   %s: array of len %d matches\n" name n
        | Some (i, a) ->
          eprintf
            "FAIL %s: at index %d: got %.10f, expected %.10f\n"
            name i a expected.(i);
          exit 1

(* ============================== Returns ============================== *)

let test_simple_returns () =
  let prices = [| 100.; 110.; 99.; 121. |] in
  let r = Analysis.Returns.simple ~prices in
    assert_array_approx_eq
      ~name:"simple returns"
      r
      [| 0.10; -0.10; 22. /. 99. |]

let test_log_returns () =
  let prices = [| 100.; 100. *. Float.exp 0.1 |] in
  let r = Analysis.Returns.log ~prices in
    assert_array_approx_eq ~name:"log return = 0.1" r [| 0.1 |]

let test_cumulative () =
  let returns = [| 0.10; -0.10; 0.222222222222 |] in
  let c = Analysis.Returns.cumulative ~returns in
  let final = c.(Array.length c - 1) in
  let expected_final = (1.10 *. 0.90 *. 1.222222222222) -. 1. in
    assert_approx_eq
      ~name:"cumulative final"
      final
      expected_final
      ~eps:1e-9

let test_log_round_trip () =
  let r = 0.075 in
  let l = Analysis.Returns.to_log r in
  let r' = Analysis.Returns.of_log l in
    assert_approx_eq ~name:"to_log/of_log round-trip" r' r ~eps:1e-12

let test_annualize () =
  let daily = 0.001 in
  let annual = Analysis.Returns.annualize ~periods_per_year:365. daily in
    assert_approx_eq
      ~name:"annualize 0.1% daily"
      annual
      ((1.001 ** 365.) -. 1.)
      ~eps:1e-9

let test_returns_empty () =
  let r = Analysis.Returns.simple ~prices:[||] in
    assert_int_eq ~name:"empty prices → empty returns" (Array.length r) 0;
  let r = Analysis.Returns.simple ~prices:[| 100. |] in
    assert_int_eq ~name:"singleton prices → empty returns" (Array.length r) 0

(* =============================== Stats =============================== *)

let test_mean_std () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
    assert_approx_eq ~name:"mean [1..5]" (Analysis.Stats.mean xs) 3.;
    (* sample std of [1..5]: sqrt(10/4) = sqrt(2.5) *)
    assert_approx_eq
      ~name:"sample std [1..5]"
      (Analysis.Stats.std xs)
      (Float.sqrt 2.5)
      ~eps:1e-9;
    (* biased (population) std: sqrt(2) *)
    assert_approx_eq
      ~name:"biased std [1..5]"
      (Analysis.Stats.std ~biased:true xs)
      (Float.sqrt 2.)
      ~eps:1e-9

let test_percentile () =
  let xs = [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. |] in
    assert_approx_eq ~name:"median 1..10" (Analysis.Stats.median xs) 5.5 ~eps:1e-9;
    assert_approx_eq
      ~name:"p25 1..10"
      (Analysis.Stats.percentile xs ~p:25.)
      3.25
      ~eps:1e-9;
    assert_approx_eq
      ~name:"p100 1..10"
      (Analysis.Stats.percentile xs ~p:100.)
      10.
      ~eps:1e-9

let test_correlation () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
  let ys = [| 2.; 4.; 6.; 8.; 10. |] in
    assert_approx_eq
      ~name:"corr(linear) = 1.0"
      (Analysis.Stats.correlation xs ys)
      1.
      ~eps:1e-9;
  let zs = [| 5.; 4.; 3.; 2.; 1. |] in
    assert_approx_eq
      ~name:"corr(anti-linear) = -1.0"
      (Analysis.Stats.correlation xs zs)
      (-1.)
      ~eps:1e-9

let test_z_score () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
  let zs = Analysis.Stats.z_score xs in
  let mean_z = Analysis.Stats.mean zs in
  let std_z = Analysis.Stats.std zs in
    assert_approx_eq ~name:"z-score mean ≈ 0" mean_z 0. ~eps:1e-12;
    assert_approx_eq ~name:"z-score std ≈ 1" std_z 1. ~eps:1e-9

let test_rolling_mean () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
  let r = Analysis.Stats.rolling_mean ~window:3 xs in
  (* First two are NaN; then 2, 3, 4 *)
    assert_int_eq ~name:"rolling_mean length" (Array.length r) 5;
    let assert_nan_at i =
      match Float.is_nan r.(i) with
      | true -> printf "OK   rolling_mean.(%d) is NaN\n" i
      | false ->
        eprintf "FAIL rolling_mean.(%d) should be NaN, got %f\n" i r.(i);
        exit 1
    in
      assert_nan_at 0;
      assert_nan_at 1;
      assert_approx_eq ~name:"rolling_mean.(2)" r.(2) 2. ~eps:1e-12;
      assert_approx_eq ~name:"rolling_mean.(3)" r.(3) 3. ~eps:1e-12;
      assert_approx_eq ~name:"rolling_mean.(4)" r.(4) 4. ~eps:1e-12

let test_ewma () =
  let xs = [| 1.; 2.; 3.; 4.; 5. |] in
  let r = Analysis.Stats.ewma ~alpha:0.5 xs in
  (* Manual: r0=1, r1=0.5*2+0.5*1=1.5, r2=0.5*3+0.5*1.5=2.25, ... *)
    assert_array_approx_eq
      ~name:"ewma α=0.5"
      r
      [| 1.; 1.5; 2.25; 3.125; 4.0625 |]
      ~eps:1e-9

(* ================================ Bars =============================== *)

let make_trade ~symbol ~side ~price ~qty ~ts : Fluxum.Types.Trade.t =
  { venue = Fluxum.Types.Venue.Gemini
  ; symbol
  ; side
  ; price
  ; qty
  ; fee = None
  ; trade_id = None
  ; ts = Some ts
  }

let test_volume_bars () =
  let sym = Fluxum.Types.Symbol.of_string "BTCUSD" in
  let base_ts = Time_float_unix.now () in
  let t i =
    Time_float_unix.add base_ts (Time_float_unix.Span.of_sec (Float.of_int i))
  in
  let trades =
    [ make_trade ~symbol:sym ~side:Buy ~price:50_000. ~qty:0.4 ~ts:(t 0)
    ; make_trade ~symbol:sym ~side:Sell ~price:50_100. ~qty:0.4 ~ts:(t 1)
    ; make_trade ~symbol:sym ~side:Buy ~price:50_050. ~qty:0.5 ~ts:(t 2)
      (* triggers close of bar 1 at threshold 1.0; bar 1 = qty 1.3 *)
    ; make_trade ~symbol:sym ~side:Sell ~price:50_200. ~qty:0.6 ~ts:(t 3)
    ; make_trade ~symbol:sym ~side:Buy ~price:50_300. ~qty:0.5 ~ts:(t 4)
      (* bar 2 closes at qty 1.1 *)
    ]
  in
  let bars = Analysis.Bars.volume_bars ~threshold:1.0 ~trades in
    assert_int_eq ~name:"volume_bars count" (List.length bars) 2;
    let bar1 = List.nth_exn bars 0 in
    let bar2 = List.nth_exn bars 1 in
      assert_approx_eq
        ~name:"bar1 volume"
        bar1.volume
        1.3
        ~eps:1e-9;
      assert_approx_eq
        ~name:"bar1 buy_volume"
        bar1.buy_volume
        0.9
        ~eps:1e-9;
      assert_approx_eq
        ~name:"bar1 sell_volume"
        bar1.sell_volume
        0.4
        ~eps:1e-9;
      assert_approx_eq
        ~name:"bar1 vwap"
        bar1.vwap
        ((50_000. *. 0.4 +. 50_100. *. 0.4 +. 50_050. *. 0.5) /. 1.3)
        ~eps:1e-6;
      assert_approx_eq
        ~name:"bar2 volume"
        bar2.volume
        1.1
        ~eps:1e-9

let test_dollar_bars () =
  let sym = Fluxum.Types.Symbol.of_string "BTCUSD" in
  let base_ts = Time_float_unix.now () in
  let t i =
    Time_float_unix.add base_ts (Time_float_unix.Span.of_sec (Float.of_int i))
  in
  let trades =
    [ make_trade ~symbol:sym ~side:Buy ~price:100. ~qty:30. ~ts:(t 0)
      (* bar 1 closes at notional 3000 (threshold 2000) *)
    ; make_trade ~symbol:sym ~side:Buy ~price:100. ~qty:25. ~ts:(t 1)
      (* bar 2 closes at notional 2500 *)
    ]
  in
  let bars = Analysis.Bars.dollar_bars ~threshold:2_000. ~trades in
    assert_int_eq ~name:"dollar_bars count" (List.length bars) 2

let test_time_bars () =
  let sym = Fluxum.Types.Symbol.of_string "BTCUSD" in
  let base_ts = Time_float_unix.now () in
  let t secs =
    Time_float_unix.add base_ts (Time_float_unix.Span.of_sec secs)
  in
  let trades =
    [ make_trade ~symbol:sym ~side:Buy ~price:100. ~qty:1. ~ts:(t 0.)
    ; make_trade ~symbol:sym ~side:Buy ~price:101. ~qty:1. ~ts:(t 30.)
    (* both in same 60s bar *)
    ; make_trade ~symbol:sym ~side:Buy ~price:102. ~qty:1. ~ts:(t 70.)
    (* next 60s bar *)
    ]
  in
  let interval = Time_ns.Span.of_sec 60. in
  let bars = Analysis.Bars.time_bars ~interval ~trades in
    assert_int_eq ~name:"time_bars count" (List.length bars) 2;
    let bar1 = List.nth_exn bars 0 in
      assert_int_eq ~name:"bar1 trade_count" bar1.trade_count 2

let test_twap_vwap () =
  (* Two synthetic bars *)
  let now = Time_ns_unix.now () in
  let bar1 : Analysis.Bars.t =
    { symbol = "BTCUSD"
    ; start_ts = now
    ; end_ts = now
    ; open_ = 100.
    ; high = 105.
    ; low = 98.
    ; close = 102.
    ; volume = 10.
    ; notional = 1020.
    ; vwap = 102.
    ; trade_count = 5
    ; buy_volume = 6.
    ; sell_volume = 4.
    }
  in
  let bar2 = { bar1 with close = 110.; volume = 30.; notional = 3300.; vwap = 110. } in
  let twap = Analysis.Bars.twap [bar1; bar2] in
  let vwap = Analysis.Bars.vwap [bar1; bar2] in
    assert_approx_eq ~name:"twap of [102, 110]" twap 106. ~eps:1e-9;
    (* vwap = (1020 + 3300) / (10 + 30) = 4320/40 = 108 *)
    assert_approx_eq ~name:"vwap of weighted bars" vwap 108. ~eps:1e-9

(* =============================== Main ================================ *)

let () =
  test_simple_returns ();
  test_log_returns ();
  test_cumulative ();
  test_log_round_trip ();
  test_annualize ();
  test_returns_empty ();
  test_mean_std ();
  test_percentile ();
  test_correlation ();
  test_z_score ();
  test_rolling_mean ();
  test_ewma ();
  test_volume_bars ();
  test_dollar_bars ();
  test_time_bars ();
  test_twap_vwap ();
  printf "\nAll Analysis tests passed.\n"

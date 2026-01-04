(** Backtest Example - demonstrates the backtest harness *)

open Core
open Async

let () =
  don't_wait_for begin
    (* Generate synthetic test data *)
    let symbol = "BTCUSD" in
    let start = Backtest.Data_source.parse_time "2024-01-01" in
    let end_ = Backtest.Data_source.parse_time "2024-06-01" in
    let interval = Backtest.Interval.hour_1 in

    printf "=== Backtest Harness Demo ===\n\n";

    (* Generate synthetic candles with a slight upward trend *)
    printf "Generating synthetic price data...\n";
    let candles = Backtest.Data_source.Synthetic.trending
        ~symbol
        ~start
        ~end_
        ~interval
        ~initial_price:45000.
        ~trend_pct:0.0005  (* 0.05% daily trend *)
        ~volatility:0.01   (* 1% volatility *)
        ()
    in
    printf "Generated %d candles from %s to %s\n\n"
      (List.length candles)
      (Time_ns.to_string_utc start)
      (Time_ns.to_string_utc end_);

    (* Configure backtest *)
    let config = Backtest.Engine.Config.create
        ~initial_balance:10000.
        ~slippage_pct:0.001
        ~commission_pct:0.001
        ~history_size:50
        ()
    in

    (* Test 1: Buy and Hold Strategy *)
    printf "=== Strategy 1: Buy and Hold ===\n";
    let%bind result1 =
      Backtest.Engine.run
        (module Backtest_strategies.Buy_and_hold.Default)
        ~config
        ~candles
        ()
    in
    (match result1 with
     | Ok r ->
       printf "\n";
       Backtest.Result.print_summary r;
       printf "\n"
     | Error e -> printf "Error: %s\n" e);

    (* Test 2: SMA Crossover Strategy *)
    printf "\n=== Strategy 2: SMA Crossover ===\n";
    let%bind result2 =
      Backtest.Engine.run
        (module Backtest_strategies.Sma_crossover.Default)
        ~config
        ~candles
        ()
    in
    (match result2 with
     | Ok r ->
       printf "\n";
       Backtest.Result.print_summary r;
       printf "\n"
     | Error e -> printf "Error: %s\n" e);

    (* Test 3: Momentum Strategy *)
    printf "\n=== Strategy 3: Momentum ===\n";
    let%bind result3 =
      Backtest.Engine.run
        (module Backtest_strategies.Momentum.Default)
        ~config
        ~candles
        ()
    in
    (match result3 with
     | Ok r ->
       printf "\n";
       Backtest.Result.print_summary r;
       printf "\n"
     | Error e -> printf "Error: %s\n" e);

    (* List all registered strategies *)
    printf "\n=== Registered Strategies ===\n";
    List.iter (Backtest_strategies.list ()) ~f:(fun (name, desc) ->
      printf "  - %s: %s\n" name desc);

    printf "\n=== Demo Complete ===\n";
    return ()
  end;
  never_returns (Scheduler.go ())

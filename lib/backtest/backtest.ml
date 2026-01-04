(** Fluxum Backtest Harness

    A backtesting framework for trading strategies using historical candle data.

    Example usage:
    {[
      open Backtest

      (* Load data *)
      let%bind candles = Data_source.Csv.load_candles
        ~symbol:"BTCUSD"
        ~start:(Data_source.parse_time "2024-01-01")
        ~end_:(Data_source.parse_time "2024-06-01")
        ~interval:Data_source.Interval.hour_1
        ~path:"./data/btcusd.csv"

      (* Configure backtest *)
      let config = Engine.Config.create
        ~initial_balance:10000.
        ~slippage_pct:0.001
        ~commission_pct:0.001
        ()

      (* Run backtest *)
      let%bind result = Engine.run (module My_strategy) ~config ~candles ()

      (* Print results *)
      Result.print_summary result
    ]}
*)

(** {1 Core Types} *)

module Candle = Candle
module Metrics = Metrics
module Result = Backtest_result

(** {1 Strategy Interface} *)

module Strategy_intf = Strategy_intf

(** Signal type for strategy outputs *)
module Signal = Strategy_intf.Signal

(** Position information *)
module Position = Strategy_intf.Position

(** Context passed to strategies on each candle *)
module Context = Strategy_intf.Context

(** {1 Fill Simulation} *)

module Fill_model = Fill_model

(** {1 Data Sources} *)

module Data_source = Data_source

(** Common time intervals *)
module Interval = Data_source.Interval

(** {1 Backtest Engine} *)

module Engine = Engine

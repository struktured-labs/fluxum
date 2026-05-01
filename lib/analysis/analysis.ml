(** Analysis primitives for crypto and prediction-market data.

    General-purpose, dependency-light building blocks composed by higher-level
    library users (backtest engines, dashboards, research notebooks, fluxit's
    proprietary strategies). Pure functions over [float array] / typed records;
    no I/O.

    Modules:
    - {!Returns}: simple, log, cumulative, rolling
    - {!Stats}: rolling mean/std/percentile, EWMA, z-score, correlation
    - {!Bars}: trades → OHLCV bars (time / volume / dollar) with per-bar VWAP *)

module Returns = Returns
module Stats = Stats
module Bars = Bars

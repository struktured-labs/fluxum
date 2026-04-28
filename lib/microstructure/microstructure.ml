(** Market microstructure primitives — spread, liquidity, order-flow.

    Equity microstructure literature assumes continuous-priced random walks,
    unbounded distributions, and exogenous-only price drift. Prediction
    markets violate three of those assumptions: bounded [0, 1], discrete
    integer-tick (Kalshi cents), and exogenous settlement drift toward
    {0, 100}. Every metric in this library exposes PM-aware gates in the
    {b API shape} (return variants, required args) — not just docstrings —
    so misuse on PM data fails loud rather than silently producing
    statistical artifacts.

    Modules:
    - {!Spread}: Roll's spread estimator with discrete-tick detection
    - {!Liquidity}: Kyle's lambda, Amihud illiquidity with settlement-window
      contamination filters and zero-volume handling
    - {!Order_flow}: Orderbook imbalance with sampling-aliasing detection
    - {!Ofi}: Order Flow Imbalance from fluxit's empirically-validated
      Python prototype (signals at p<0.01 on 32h Gemini data)
    - {!Pm_quality_gates}: Per-market prediction-market quality gates
      (Near_rail, Regime_break, One_sided_book) per bluxit empirical
      validation on n=129 Kalshi cohort *)

module Spread = Spread
module Liquidity = Liquidity
module Order_flow = Order_flow
module Ofi = Ofi
module Pm_quality_gates = Pm_quality_gates

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
    - {!Order_flow}: Orderbook imbalance with sampling-aliasing detection *)

module Spread = Spread
module Liquidity = Liquidity
module Order_flow = Order_flow

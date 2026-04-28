(** Bid-ask spread estimators from trade-price series.

    {b Roll's estimator (1984)} infers the effective spread from negative
    serial covariance in price changes: under the assumption that prices
    bounce between bid and ask randomly, [Cov(Δp_t, Δp_{t-1})] should be
    negative, with magnitude [(spread/2)^2]. *)

(** Result of a spread estimation. *)
type result =
  | Spread of float
      (** Estimated spread in price units. *)
  | Discrete_tick_warning of
      { estimated_spread : float
      ; tick_size : float
      }
      (** Input prices appear integer-quantized (e.g. Kalshi cents). On
          discrete-tick markets, integer-tick random walks produce
          negative autocovariance from rounding alone — Roll returns
          [spread > 0] even on markets with no real trade-driven spread.
          Equity microstructure intuition does not transfer; treat the
          number as suspect. *)
  | Insufficient_data of int (** Fewer than 3 observations. *)
[@@deriving sexp]

(** Roll's effective-spread estimator.

    [s = 2 * sqrt(-Cov(Δp_t, Δp_{t-1}))] when the covariance is negative;
    when it's positive (theoretically unexpected), the convention is to
    return [Spread 0.0] (no inferable trade-driven spread).

    Auto-detects discrete-tick input by checking whether all prices are
    multiples of a small integer scale (default detection: prices land
    within [tolerance] of a common integer multiple). When detected,
    returns [Discrete_tick_warning] regardless of the numeric estimate.
    Pass [~tick_size:0.0] to disable the gate.

    @param prices Trade-price series in chronological order.
    @param tick_size Optional explicit tick size in price units. If
           [Some t], the detector confirms multiples of [t]; if [None],
           auto-detects. Pass [Some 0.0] to skip discrete-tick checking
           entirely (expressly assert the input is continuous). *)
val roll :
  prices:float array ->
  ?tick_size:float ->
  unit ->
  result

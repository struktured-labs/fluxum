(** OHLCV bar aggregation from a stream of trades.

    Three sampling regimes:
    - {!time_bars}: fixed time intervals (1m, 5m, 1h, 1d, ...)
    - {!volume_bars}: fixed cumulative volume threshold per bar
    - {!dollar_bars}: fixed cumulative notional (price × qty) threshold per bar

    Volume and dollar bars (per López de Prado, "Advances in Financial Machine
    Learning") often have better statistical properties for ML/research than
    time bars, since they sample more frequently during active periods. *)

open Core

type t =
  { symbol : string
  ; start_ts : Time_ns_unix.t
  ; end_ts : Time_ns_unix.t
  ; open_ : float
  ; high : float
  ; low : float
  ; close : float
  ; volume : float (** Sum of trade quantities. *)
  ; notional : float (** Sum of (price × qty). *)
  ; vwap : float (** notional / volume. *)
  ; trade_count : int
  ; buy_volume : float (** Sum of qty for [`Buy] side trades. *)
  ; sell_volume : float (** Sum of qty for [`Sell] side trades. *)
  }
[@@deriving sexp, compare, equal, fields]

(** Aggregate trades into fixed-time-interval bars.
    Trades with [ts = None] are silently skipped. Output bars are in
    chronological order, gaps in trade activity produce no bar (the next
    bar begins at the interval containing the next trade). *)
val time_bars :
  interval:Time_ns.Span.t ->
  trades:Fluxum.Types.Trade.t list ->
  t list

(** Aggregate trades into bars that close once cumulative volume reaches
    [threshold]. The final partial bar is emitted only if it has at least
    one trade. *)
val volume_bars :
  threshold:float ->
  trades:Fluxum.Types.Trade.t list ->
  t list

(** Aggregate trades into bars that close once cumulative notional
    (price × qty) reaches [threshold]. *)
val dollar_bars :
  threshold:float ->
  trades:Fluxum.Types.Trade.t list ->
  t list

(** TWAP across a list of bars: simple arithmetic mean of close prices. *)
val twap : t list -> float

(** VWAP across a list of bars (volume-weighted close, equivalent to
    aggregating each bar's notional/volume). *)
val vwap : t list -> float

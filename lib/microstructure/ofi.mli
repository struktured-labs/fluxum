(** Order Flow Imbalance (OFI) — USD-value-weighted buy/sell flow ratio
    over a rolling window.

    Ported from fluxit's Python prototype (`scripts/ofi_analyze.py`,
    documented in `fluxit/docs/ofi_port_spec.md` commit `fed8633`).
    Empirical validation: real predictive signals at p<0.01 on 32h of
    Gemini data — POLUSD inverted (heavy buy → −60bps drift) and
    RNDRUSD conventional (+159bps separating heavy-buy from heavy-sell).

    Both INFORMED-MM (high OFI → price rises) and EXIT-LIQUIDITY
    (high OFI → price falls) signs are real microstructural patterns.
    The right hypothesis emerges per-coin from data; the bootstrap test
    in {!predictive_validity} is sign-agnostic on |drift|. *)

(** Single trade event with explicit aggressor side. The [aggressor]
    field describes microstructure semantics (which side initiated by
    crossing the spread) rather than economic intent — the maker/taker
    ambiguity bit a fluxit consumer once. Per-venue adapters must
    normalize their native side encoding to this form upstream. *)
module Trade : sig
  type t =
    { tid : int64
    ; timestamp : Time_ns_unix.t
    ; price : float
    ; amount : float
    ; aggressor : [ `Buy | `Sell ]
    }
  [@@deriving sexp]
end

(** OFI value for a single trade. Saturated states preserved as variants
    rather than collapsed to magic numeric sentinels — downstream
    consumers can map them to a configurable cap at their layer. *)
type ofi_value =
  | Ofi of float
      (** [buy_value / sell_value] in [0, +inf), 1.0 = balanced. *)
  | Saturated_buy
      (** Window had zero sell-side volume but non-zero buy. Mathematical
          OFI is +∞. *)
  | Saturated_sell
      (** Window had zero buy-side volume but non-zero sell. Mathematical
          OFI is 0. (Distinguished from genuine 0 OFI which can't happen
          in this branch.) *)
  | No_data
      (** Window had zero trades on both sides — typically because the
          window predates the dataset start. *)
  | Insufficient_trades of int
      (** Total trades in the window was below
          [min_window_trades] — value is the observed count. *)
[@@deriving sexp]

(** A trade tagged with its OFI as observed at the moment the trade
    arrived (windowed strictly over PRECEDING trades — see {!compute}
    for look-ahead safety). *)
module Tagged_trade : sig
  type t =
    { trade : Trade.t
    ; ofi : ofi_value
    ; window_buy_value : float
    ; window_sell_value : float
    ; window_trade_count : int
    }
  [@@deriving sexp]
end

(** Compute OFI for every trade in [trades], windowed over the preceding
    [window] of time. The trade at index [i] is {b not} included in its
    own OFI window — that's the look-ahead-bias guard.

    @param trades Time-ordered trade stream. Caller must sort by
           timestamp ascending. Out-of-order input produces undefined
           OFI values; no validation performed for performance.
    @param window Lookback span. Default 60 minutes per fluxit
           prototype default.
    @param min_window_trades Minimum number of trades that must be in the
           window for the OFI value to be considered statistically
           meaningful. Below this, [Insufficient_trades n] is reported
           rather than a noisy ratio. Default 5.

    Time complexity: O(n × avg_window_size). For thin markets
    (≤ tens of trades per hour) effectively O(n). *)
val compute :
  trades:Trade.t array ->
  ?window:Time_ns.Span.t ->
  ?min_window_trades:int ->
  unit ->
  Tagged_trade.t array

(** Default OFI bin set per fluxit prototype. Asymmetric around 1.0
    because buy/sell ratios are asymmetric in raw form (0.4 ≈ 1/2.5
    in sell-skew terms); roughly symmetric in log-OFI space. *)
val default_bins : (float * float * string) list

(** {1 Predictive validity} *)

(** A bin definition: lower-bound (inclusive), upper-bound (exclusive),
    label. Use [Float.infinity] as upper-bound for the open-ended top
    bin. *)
type bin = float * float * string [@@deriving sexp]

(** Per-bin forward-drift summary. *)
type bin_drift =
  { bin_label : string
  ; fwd_window : Time_ns.Span.t
  ; mean_drift_bps : float
  ; n : int
  }
[@@deriving sexp]

(** Bootstrap-shuffle test result for one forward window. Compares the
    actual extreme-bin separation (heavy-buy minus heavy-sell mean
    drift) against the distribution of separations under random
    relabeling of OFI bins. Sign-agnostic: tests on [|diff|] so it
    catches both INFORMED-MM and EXIT-LIQUIDITY patterns. *)
type shuffle_test =
  { fwd_window : Time_ns.Span.t
  ; actual_extreme_diff_bps : float
  ; n_shuffles : int
  ; p_value : float
      (** Fraction of shuffled iterations whose [|diff|] meets or
          exceeds the actual [|diff|]. Lower = stronger signal. *)
  }
[@@deriving sexp]

type predictive_validity_result =
  { per_bin_drift : bin_drift array
  ; baseline_drift : (Time_ns.Span.t * float * int) array
      (** Mean drift across ALL trades (no OFI conditioning), per
          forward window. The "no-information" baseline that
          per-bin-drift should beat for the signal to be real. *)
  ; shuffle_tests : shuffle_test array
      (** One per [fwd_window]; uses the lowest-bin and highest-bin
          drift difference as the test statistic. *)
  }
[@@deriving sexp]

(** Test whether OFI predicts subsequent mid drift, conditioning on
    forward windows and OFI bins, with bootstrap-shuffle p-values.

    For each trade in [tagged_trades], find:
    - [mid_at]: most recent [mid_series] entry with timestamp ≤ trade
    - [mid_fwd]: first [mid_series] entry with timestamp ≥ (trade + fwd)

    Trades with missing anchors (data boundaries) are skipped. Drift =
    [10_000 * (mid_fwd / mid_at - 1)] in basis points.

    @param tagged_trades Output of {!compute}.
    @param mid_series Time-ordered (timestamp, mid) array.
    @param fwd_windows Forward-drift horizons to test. E.g.
           [[Time_ns.Span.of_min 5.; of_min 15.; of_min 60.]].
    @param bins OFI bin boundaries; default {!default_bins}.
    @param n_shuffles Bootstrap iteration count. Default 1000.
    @param seed Optional RNG seed for the bootstrap shuffle. When
           supplied, results are bit-reproducible across runs and across
           machines — use this for validation harnesses that diff against
           reference implementations. When omitted, an entropy-seeded
           [Random.State.t] is created (recommended for production use). *)
val predictive_validity :
  tagged_trades:Tagged_trade.t array ->
  mid_series:(Time_ns_unix.t * float) array ->
  fwd_windows:Time_ns.Span.t list ->
  ?bins:bin list ->
  ?n_shuffles:int ->
  ?seed:int ->
  unit ->
  predictive_validity_result

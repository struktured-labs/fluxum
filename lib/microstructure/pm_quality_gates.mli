(** Per-market prediction-market quality gates.

    Three independent gates flag distinct mechanisms by which a
    per-market microstructure metric becomes lying-prone on PM data.
    Each gate is a {b composable predicate} returning [Some failure_info]
    or [None] (passed), so callers can combine with metric computations
    naturally:

    {[
      match check_near_rail ~max_yes_bid ~min_yes_ask_nonzero () with
      | Some _ -> `Skipped_near_rail
      | None ->
        match check_regime_break ~mid_start ~mid_end () with
        | Some _ -> `Skipped_regime_break
        | None -> `Computed (Liquidity.kyle_lambda ...)
    ]}

    {b Important scope distinction}: these are PER-MARKET gates. They
    catch markets where the {i measurement framework} is invalid for
    that market specifically. They do {b not} catch cohort-level
    failure modes like per-ticker P&L concentration — that's a separate
    concern at a different aggregation level (proposed
    {!Cohort_quality_gates}; not yet shipped). Per bluxit empirical
    validation (n=129, KXCAGOVPRIMARY1ST drove 138% per-ticker
    concentration failure while passing every per-market gate),
    conflating the two levels would mask real failures.

    Empirical grounding: thresholds and gate structure validated against
    bluxit's TIGHT+LOOSE Kalshi cohort, see
    [bluxit/docs/gate_validation_v1.md] commit c45878d. Default
    thresholds match bluxit's hand-rolled heuristic; other venues may
    require different values. *)

(** Fired when the market sat near a price boundary during the
    observation window. Spread economics break down at the rail (1c is
    a huge fraction of a 5c price) — MM analysis is structurally
    invalid even with hours/days of life left. {b Distinct from
    settlement contamination} (which is purely time-based and would
    happily pass a market sitting at yes_bid=98 for 30 days). *)
module Near_rail : sig
  type t =
    { yes_bid : int (** Observed [max yes_bid] over the window. *)
    ; yes_ask : int (** Observed [min nonzero yes_ask] over the window. *)
    ; threshold : int (** Configured rail threshold (cents). *)
    }
  [@@deriving sexp]
end

(** Fired when mid moved more than the configured threshold during the
    window. Half the observed P&L on a 40c+ jump comes from being
    directionally correct, not from spread capture — the MM measurement
    framework attributes correctly only on stationary mid behavior.
    {b Distinct from Near_rail}: a market can jump 50→90 (large jump,
    no rail) or 50→98 (large jump that hits rail). Independent
    dimensions; per-bluxit-data the [(mid_market, jumped)] quadrant is
    empirically rare in Kalshi binary regime (catalysts that move 40c+
    usually near-resolve), but a future regime might populate it. *)
module Regime_break : sig
  type t =
    { mid_start : float
    ; mid_end : float
    ; delta : float (** [|mid_end -. mid_start|]. *)
    ; threshold : float (** Configured regime threshold (cents). *)
    }
  [@@deriving sexp]
end

(** Fired when one side of the book was structurally empty for the
    entire observation window. With no two-sided liquidity, MM spread
    capture is impossible regardless of quoted prices on the other
    side. Surfaced from bluxit's empirical validation as a real,
    distinct mechanism (10.1% of windows in the n=129 cohort) —
    rail-adjacent but not rail-equivalent: it's a {i liquidity}
    condition, not a {i price} condition. *)
module One_sided_book : sig
  type side = [ `Bid_empty | `Ask_empty ] [@@deriving sexp]

  type t =
    { side : side
    ; yes_bid_first : int
    ; yes_bid_last : int
    ; yes_ask_first : int
    ; yes_ask_last : int
    }
  [@@deriving sexp]
end

(** Check the rail gate. Pass [~max_yes_bid] = observed maximum and
    [~min_yes_ask_nonzero] = observed minimum non-zero ask over the
    window, both in cents (0-100).
    @param threshold Rail-distance in cents; default [5]. *)
val check_near_rail :
  ?threshold:int ->
  max_yes_bid:int ->
  min_yes_ask_nonzero:int ->
  unit ->
  Near_rail.t option

(** Check the regime-stability gate. Both [~mid_start] and [~mid_end]
    in cents (0-100 scale, can be fractional from a (bid+ask)/2 mid).
    @param threshold Regime-jump threshold in cents; default [40.0]. *)
val check_regime_break :
  ?threshold:float ->
  mid_start:float ->
  mid_end:float ->
  unit ->
  Regime_break.t option

(** Check the one-sided-book gate. Pass full per-snapshot windows of
    [yes_bid] and [yes_ask] values (in cents); fires if either side
    has only zeros for the entire window. *)
val check_one_sided_book :
  yes_bid_window:int list ->
  yes_ask_window:int list ->
  One_sided_book.t option

(** Aggregate per-market check result. All three sub-fields are
    [None] iff the market passes all gates ([is_pass = true]); any
    [Some] indicates a specific failure mechanism. The record carries
    all three independently because gates can fire in combination
    (per bluxit's data, KXFED-27APR-T1.25 4/25 fires both [Near_rail]
    AND [One_sided_book]). *)
type per_market_result =
  { near_rail : Near_rail.t option
  ; regime_break : Regime_break.t option
  ; one_sided_book : One_sided_book.t option
  }
[@@deriving sexp]

val is_pass : per_market_result -> bool

(** Run all three per-market gates in one call. Convenient for callers
    who want a single pass/fail decision rather than threading the
    composable predicates manually. *)
val check_all_per_market :
  ?rail_threshold:int ->
  ?regime_threshold:float ->
  max_yes_bid:int ->
  min_yes_ask_nonzero:int ->
  mid_start:float option ->
  mid_end:float option ->
  yes_bid_window:int list ->
  yes_ask_window:int list ->
  unit ->
  per_market_result

(** Cross-venue arbitrage detection for prediction markets.

    The canonical 2-leg arb in a binary market: if you can buy YES on
    venue A for [p_a] and buy NO on venue B for [p_b], and [p_a + p_b < 1.0],
    you've locked in a guaranteed profit of [1 - p_a - p_b] per unit stake.

    More generally, when the same event is quoted on N venues, arbitrage
    exists if you can construct a portfolio of YES/NO positions across
    venues whose total cost is less than 1.0 and whose payoff is exactly
    1.0 regardless of outcome. *)

(** A single venue's price for one side of a binary market. *)
type quote =
  { venue : string
  ; side : [ `Yes | `No ]
  ; price : float (** Price as a probability in [0, 1]. Convert via
                      [Probability.from_kalshi_cents] etc. if needed. *)
  ; size : float (** Available size at this price. Use [Float.infinity]
                     if size doesn't bind. *)
  }
[@@deriving sexp]

(** Result of arbitrage analysis. *)
type opportunity =
  { yes_quote : quote
  ; no_quote : quote
  ; cost : float (** [yes_quote.price + no_quote.price]; must be < 1.0 to be an arb. *)
  ; profit_per_unit : float (** [1.0 - cost]. Positive iff arb exists. *)
  ; max_size : float (** [min yes_quote.size no_quote.size]. *)
  }
[@@deriving sexp]

(** Find the best 2-leg cross-venue arbitrage from a list of quotes for
    the same event. Considers all (yes, no) pairs across distinct venues
    and returns the one with the highest [profit_per_unit], or [None] if
    no positive-profit pair exists.

    Filtering [yes_quote.venue <> no_quote.venue] is intentional — same-venue
    arb (the venue itself misquoting both sides) is theoretically possible
    but in practice the venue would catch it; this finds *cross*-venue
    arb where price discrepancies between independent venues create the
    opportunity. *)
val best_two_leg :
  quotes:quote list ->
  opportunity option

(** All positive-profit 2-leg cross-venue arbs, sorted by descending
    [profit_per_unit]. Use when you want to size across multiple legs
    rather than just the single best one. *)
val all_two_leg :
  quotes:quote list ->
  opportunity list

(** {1 Categorical (multi-outcome) arbitrage}

    For events with N > 2 outcomes — race winners, temperature bins,
    election outcomes with multiple candidates — the analog of "sum of
    probabilities < 1.0 = arb" requires that {b every outcome} is
    quoted. Without full coverage, the missing outcomes carry the
    missing probability mass and apparent arb is illusory.

    This API forces the caller to declare the total outcome count and
    gates on a coverage threshold, returning a three-way result so the
    caller can distinguish "real arb" from "can't tell — not enough
    quotes" from "all outcomes covered, no arb". *)

(** Result of a categorical arb check. *)
type categorical_result =
  | Arb of float
      (** Sum of observed prices < 1.0 with sufficient coverage; profit
          per unit if you bought all observed outcomes. *)
  | Insufficient_coverage of
      { observed_count : int
      ; expected_count : int
      ; observed_sum : float
      }
      (** Coverage threshold not met — verdict is "we don't know."
          Includes the partial sum so callers can log it for diagnosis
          but should not act on it as if it were real arb. *)
  | No_arb of float
      (** Full coverage, sum >= 1.0. Includes the sum (the venue's
          implied overround + 1). *)
[@@deriving sexp]

(** Check for arbitrage across all outcomes of a categorical event.

    @param outcome_prices Array of probability-prices for the outcomes
           that {b are} quoted. Must be in [[0, 1]].
    @param expected_outcomes Total number of possible outcomes for this
           event (e.g. 22 for an F1 race, 6 for a temperature bin
           market). Compared against [Array.length outcome_prices] to
           compute coverage.
    @param min_coverage_fraction Fraction of outcomes that must be
           observed for the arb verdict to be considered reliable.
           Default [1.0] (require all). Set [0.95] to allow 1 missing
           outcome on a 20-outcome event, etc. — but be aware that any
           less than 1.0 risks false positives proportional to the
           missing mass. *)
val categorical_check :
  outcome_prices:float array ->
  expected_outcomes:int ->
  ?min_coverage_fraction:float ->
  unit ->
  categorical_result

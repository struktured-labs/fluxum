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

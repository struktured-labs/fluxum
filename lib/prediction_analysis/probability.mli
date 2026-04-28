(** Convert between probability and various odds/price formats.

    All functions clamp output probabilities to [0, 1]. Invalid input
    (negative odds magnitudes, etc.) returns [Float.nan]. *)

(** [from_kalshi_cents 42] = [0.42]. Kalshi quotes binary contracts at
    0–100 cents; the cent price is the implied probability of YES. *)
val from_kalshi_cents : int -> float

(** [to_kalshi_cents 0.42] = [42]. Inverse of [from_kalshi_cents]; rounds
    to the nearest cent. *)
val to_kalshi_cents : float -> int

(** [from_decimal_odds 2.0] = [0.5]. Decimal odds are total payout per
    unit stake including the stake (European convention). *)
val from_decimal_odds : float -> float

(** [to_decimal_odds 0.5] = [2.0]. *)
val to_decimal_odds : float -> float

(** [from_fractional_odds ~num:5 ~denom:2] = [2/7 ≈ 0.2857]. UK
    bookmaker convention: profit/stake. *)
val from_fractional_odds : num:int -> denom:int -> float

(** [from_american_odds 200] = [1/3 ≈ 0.333] (positive: profit on $100
    stake). [from_american_odds (-150)] = [0.6] (negative: stake to win
    $100). *)
val from_american_odds : int -> float

(** [to_american_odds 0.333] = [201] (rounded). *)
val to_american_odds : float -> int

(** Sum of probabilities minus 1. For a fairly-priced binary market
    [\[p_yes; p_no\]] this is [0]; for a venue with vig it's positive
    (the venue's edge). For arbitrage candidates across venues, negative
    overround indicates a cross-venue arb opportunity. *)
val implied_overround : probs:float array -> float

(** Normalize an array of probabilities to remove the implied overround,
    so that [sum result = 1.0]. Each [p_i] is divided by [sum probs]. *)
val remove_overround : probs:float array -> float array

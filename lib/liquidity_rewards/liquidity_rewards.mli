(** Pure scoring functions for maker liquidity-reward programs.

    No I/O, no clocks, no order entry: callers supply book snapshots and get
    scores and payouts back. Formulas and their sources are documented per
    module. Where a venue does not publish an exact term the assumption is
    stated and parameterised. *)

(** Gemini Predictions Liquidity Rewards Program.
    Source: https://developer.gemini.com/prediction-markets/liquidity-rewards-program
    - snapshot score = spread weight x min(size, cap) x two-sided multiplier, per minute, per contract;
    - quotes beyond the max spread or below the min size score zero;
    - daily payout = pool x daily score / total qualifying score, for makers above the uptime
      threshold; payouts under the minimum are not paid; rounded down to the cent.
    Limitations (see README.md):
    - Gemini says only that the spread weight is a "quadratic curve"; the default
      [spread_weight] uses Polymarket's documented [((v - d) / v) ^ 2], which gives an
      81/1/18% split on Gemini's worked example versus Gemini's published 74/5/21%.
    - Competitor scores are unobservable (maker IDs hidden); shares computed from
      public books are bounds, not measurements.
    - How contract-level pools split across recurring instances is a caller assumption.
    - Gemini scores every minute; callers sampling more coarsely (the fluxit shadow
      recorder samples every ~5 minutes) miss intra-interval changes.
    - Research result 2026-10-16: the fluxit shadow scored -$28.03 over 4 days under
      this model; not evidence of a profitable strategy. *)
module Gemini : sig
  type config =
    { max_spread_cents : float
    ; min_size : float
    ; size_cap : float
    ; two_sided_multiplier : float
    ; uptime_threshold : float
    ; min_payout_usd : float
    }

  (** Values published on 2026-10-16: 10c, 10, 250, 1.5x, 50%, $1.00. *)
  val default_config : config

  type quote =
    { price : float
    ; size : float
    }

  (** [spread_weight config ~distance_cents] is in [0, 1]. *)
  val spread_weight : config -> distance_cents:float -> float

  (** Sum over one side of [spread_weight x min size cap] for qualifying quotes.
      [reference] is the midpoint, or the best price on this side when the book is one-sided. *)
  val side_score : config -> reference:float -> quote list -> float

  (** One maker's snapshot score on one contract. *)
  val snapshot_score : config -> reference:float -> bids:quote list -> asks:quote list -> float

  (** Daily USD payout. [uptime] is the fraction of eligible snapshots in which the maker qualified. *)
  val daily_payout
    :  config
    -> pool_usd:float
    -> own_score:float
    -> total_score:float
    -> uptime:float
    -> float
end

(** Polymarket Liquidity Rewards.
    Source: https://docs.polymarket.com/market-makers/liquidity-rewards *)
module Polymarket : sig
  (** [order_score ~max_spread ~spread ~multiplier] = ((v - s) / v)^2 * b, zero beyond [v]. *)
  val order_score : max_spread:float -> spread:float -> multiplier:float -> float

  (** Q for one book: sum of [order_score x size] over orders at or above [min_size]. *)
  val side_score
    :  max_spread:float
    -> min_size:float
    -> multiplier:float
    -> (float * float) list
    -> float

  (** Qmin. Midpoint in [0.10, 0.90]: max(min(q1, q2), max(q1 / c, q2 / c)); otherwise min(q1, q2).
      [c] is currently 3.0. *)
  val q_min : midpoint:float -> c:float -> q_one:float -> q_two:float -> float

  (** Epoch Q = sum of sample Qmin x uptime fraction. *)
  val epoch_score : sample_q_mins:float list -> uptime:float -> float

  (** reward x own / total; zero if total is not positive. *)
  val payout : reward:float -> own_epoch:float -> total_epoch:float -> float
end

(** Kalshi Liquidity Incentive Program.
    Sources: https://help.kalshi.com/en/articles/13823851-liquidity-incentive-program and the
    CFTC-filed rule text (2026-07-15 update, 2026-07-30 modification). Prices are integer ticks
    (cents); a yes ask is treated as a no bid. *)
module Kalshi : sig
  type order =
    { price_ticks : int
    ; size : float
    ; mine : bool
    }

  type side_result =
    { reference_ticks : int
    ; qualifying : order list
    }

  (** Walk bids from best down. Reference = first level where cumulative size reaches
      target / 5; stop once cumulative size reaches target. [None] if the target is never
      reached or the best bid is at or above [max_price_ticks]. *)
  val qualify_side : target_size:float -> max_price_ticks:int -> order list -> side_result option

  (** size x discount_factor ^ max 0 (reference - price). *)
  val order_score : discount_factor:float -> reference_ticks:int -> order -> float

  (** Own share of one side's qualifying score, in [0, 1]. *)
  val side_share : discount_factor:float -> side_result -> float

  (** Own snapshot score in [0, 2] = yes-side share + no-side share; zero unless both sides qualify. *)
  val snapshot_score
    :  target_size:float
    -> max_price_ticks:int
    -> discount_factor:float
    -> yes_bids:order list
    -> no_bids:order list
    -> float

  (** Period payout = reward x own / total snapshot scores, zero below [min_payout],
      rounded down to the cent. *)
  val payout : reward:float -> own:float -> total:float -> min_payout:float -> float
end

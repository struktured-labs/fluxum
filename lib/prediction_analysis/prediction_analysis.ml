(** Prediction-market analytics — venue-agnostic.

    Works with any binary or categorical prediction market regardless of
    whether prices are quoted in Kalshi cents (0–100), decimal odds,
    fractional odds, or American (moneyline) odds. Operates on probabilities
    in [0, 1] internally; conversion helpers in {!Probability}.

    Modules:
    - {!Probability}: convert between price formats and probabilities,
      compute vig (overround), normalize.
    - {!Calibration}: Brier score, log loss, calibration curves for assessing
      how well a forecaster's probabilities match observed outcomes.
    - {!Arbitrage}: detect cross-venue arbitrage when same-event quotes on
      different venues sum to less than 1.0 (buy yes on cheap, buy no on
      expensive — guaranteed profit). *)

module Probability = Probability
module Calibration = Calibration
module Arbitrage = Arbitrage

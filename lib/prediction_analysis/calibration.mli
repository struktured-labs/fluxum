(** Forecast accuracy metrics for prediction markets.

    Outcomes are encoded as [0.0] (didn't happen) or [1.0] (happened).
    Predictions are probabilities in [0, 1]. *)

(** Brier score for a single prediction: [(pred - outcome)^2].
    Lower is better. [0] = perfect prediction. *)
val brier_score : pred:float -> outcome:float -> float

(** Mean Brier score across an array of predictions and outcomes. *)
val brier_score_avg :
  preds:float array ->
  outcomes:float array ->
  float

(** Logarithmic (cross-entropy) loss for a single prediction:
    [-(outcome * log(pred) + (1 - outcome) * log(1 - pred))].
    Predictions are clipped to [[eps, 1 - eps]] (default [eps = 1e-15])
    to avoid [log 0] = [-infinity]. Lower is better; [0] = perfect. *)
val log_loss : ?eps:float -> pred:float -> outcome:float -> unit -> float

(** Mean log loss across an array of predictions and outcomes. *)
val log_loss_avg :
  ?eps:float ->
  preds:float array ->
  outcomes:float array ->
  unit ->
  float

(** Calibration bin: predictions falling into [[min_prob, max_prob)]
    are grouped, and we report the mean predicted probability vs the
    actual outcome rate (= mean outcome) in that bin.

    A perfectly-calibrated forecaster has [pred_avg ≈ outcome_rate]
    in every bin. *)
type bin =
  { min_prob : float
  ; max_prob : float
  ; pred_avg : float (** Mean prediction in this bin. *)
  ; outcome_rate : float (** Fraction of outcomes that = 1.0 in this bin. *)
  ; count : int (** Number of (pred, outcome) pairs in this bin. *)
  }
[@@deriving sexp]

(** Build [n_bins] equal-width calibration bins over [[0, 1]] from
    parallel arrays of predictions and outcomes. Empty bins are still
    emitted (with [pred_avg = bin midpoint], [outcome_rate = nan],
    [count = 0]) so the returned list always has length [n_bins]. *)
val calibration_bins :
  preds:float array ->
  outcomes:float array ->
  n_bins:int ->
  bin list

(** Expected Calibration Error: weighted absolute deviation between
    [pred_avg] and [outcome_rate] across all bins, weighted by [count].
    Returns [0] for a perfectly-calibrated forecaster. *)
val expected_calibration_error : bins:bin list -> float

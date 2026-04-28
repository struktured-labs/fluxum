(** Price-impact / illiquidity measures.

    Both metrics in this module are sensitive to {b settlement-window
    contamination} on prediction-market data: a binary contract within
    ~1h of resolution drifts toward {0, 1} based on time-to-expiry, not
    on flow. Computed on settlement-window data, both report huge "price
    impact per unit volume" that's endogenous to the clock, not the
    signal you want. The optional [~min_hours_to_expiry] arg lets callers
    declare a contamination cutoff. *)

(** Result of Kyle's lambda calculation. *)
type kyle_result =
  | Lambda of float
      (** Estimated price-impact per unit of signed volume. *)
  | Settlement_contamination of { hours_to_expiry : float }
      (** Within the [min_hours_to_expiry] window of resolution; metric
          rejected to avoid endogenous-clock drift artifacts. *)
  | Insufficient_data of int
[@@deriving sexp]

(** Kyle's (1985) lambda: regress log returns on signed order flow
    (signed by trade direction; positive = buyer-initiated). The slope
    is the per-unit price-impact coefficient. Higher = less liquid.

    Computed as [Cov(returns, signed_volumes) / Var(signed_volumes)].

    @param returns Period-over-period returns or log-returns, length N.
    @param signed_volumes Signed volume per period (same length as
           returns). Convention: positive = buyer-initiated (lifted
           offer), negative = seller-initiated (hit bid).
    @param expiry_time Optional contract expiry. If provided alongside
           [min_hours_to_expiry] and [as_of], the function checks whether
           the window straddles the contamination cutoff and returns
           [Settlement_contamination] rather than a contaminated lambda.
    @param min_hours_to_expiry Minimum time-to-expiry required for the
           result to be considered uncontaminated. Default 1.0 hour
           (matches bluxit-gemini project-wide convention).
    @param as_of Reference timestamp for the contamination check;
           defaults to the current wall clock. *)
val kyle_lambda :
  returns:float array ->
  signed_volumes:float array ->
  ?expiry_time:Time_float_unix.t ->
  ?min_hours_to_expiry:float ->
  ?as_of:Time_float_unix.t ->
  unit ->
  kyle_result

(** Result of Amihud illiquidity calculation. *)
type amihud_result =
  | Illiquidity of float
      (** Mean |return| / volume across days with sufficient volume. *)
  | All_zero_volume of { days_observed : int }
      (** Every day in the input had volume below [min_volume] (or zero).
          Standard Amihud is undefined here. *)
  | Settlement_contamination of { hours_to_expiry : float }
  | Insufficient_data of int
[@@deriving sexp]

(** Amihud (2002) illiquidity: mean of [|daily_return| / daily_volume]
    across days with non-zero volume. Higher = less liquid. Pure-text
    convention units depend on input scale (returns and volumes).

    Long-tail prediction-market contracts often have zero volume for
    entire days; the standard formula divides by zero. This function
    returns [All_zero_volume] when no day has volume above
    [min_volume], or computes the mean across only the days that meet
    the threshold.

    @param daily_returns Length-N daily returns.
    @param daily_volumes Length-N daily volumes (any unit, must match
           returns convention).
    @param min_volume Days with volume below this are excluded.
           Defaults to 0 (any non-zero volume). Set to a positive
           number to reject thinly-traded days entirely.
    @param expiry_time Optional contract expiry; same contamination-
           filter semantics as {!kyle_lambda}.
    @param min_hours_to_expiry Default 24.0 hours (Amihud is a daily
           metric so the cutoff is correspondingly larger).
    @param as_of Reference timestamp for contamination check. *)
val amihud_illiquidity :
  daily_returns:float array ->
  daily_volumes:float array ->
  ?min_volume:float ->
  ?expiry_time:Time_float_unix.t ->
  ?min_hours_to_expiry:float ->
  ?as_of:Time_float_unix.t ->
  unit ->
  amihud_result

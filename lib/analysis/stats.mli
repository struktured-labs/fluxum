(** Statistical primitives for time-series analysis.

    All operations take [float array]. Empty input returns [Float.nan] for
    aggregates and [[||]] for series-producing operations. *)

(** Arithmetic mean. [Float.nan] on empty input. *)
val mean : float array -> float

(** Sample variance (n-1 denominator) by default. Set [~biased:true] for
    population variance (n denominator). *)
val variance : ?biased:bool -> float array -> float

(** Sample standard deviation. *)
val std : ?biased:bool -> float array -> float

(** Linear-interpolated percentile. [p] in [0..100].
    [percentile xs ~p:50.] = median. *)
val percentile : float array -> p:float -> float

(** Median (= percentile 50). *)
val median : float array -> float

(** Min ignoring NaN. *)
val min : float array -> float

(** Max ignoring NaN. *)
val max : float array -> float

(** Pearson correlation coefficient. Arrays must be the same length;
    raises [Invalid_argument] otherwise. *)
val correlation : float array -> float array -> float

(** Covariance (sample, n-1). *)
val covariance : ?biased:bool -> float array -> float array -> float

(** Z-score normalization: [(x -. mean xs) /. std xs] for each element.
    Returns the original series if std is zero. *)
val z_score : float array -> float array

(** Rolling arithmetic mean over a fixed window. Output length matches input;
    the first [window - 1] elements are [Float.nan] (insufficient history). *)
val rolling_mean : window:int -> float array -> float array

(** Rolling standard deviation. Same windowing semantics as [rolling_mean]. *)
val rolling_std : ?biased:bool -> window:int -> float array -> float array

(** Rolling sum. *)
val rolling_sum : window:int -> float array -> float array

(** Exponentially weighted moving average.
    [s.(0) = xs.(0)]; [s.(i) = alpha *. xs.(i) +. (1 -. alpha) *. s.(i-1)].
    [alpha] should be in (0, 1]; higher = more weight on recent. *)
val ewma : alpha:float -> float array -> float array

(** Exponentially weighted standard deviation, matching [ewma] semantics. *)
val ewmstd : alpha:float -> float array -> float array

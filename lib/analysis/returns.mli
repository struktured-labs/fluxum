(** Return calculations from price series.

    All functions take [prices : float array] in chronological order.
    Output arrays are length [n - 1] for period-over-period operations. *)

(** Simple period returns: [r.(i) = (prices.(i+1) -. prices.(i)) /. prices.(i)].
    Length: [Array.length prices - 1].
    Returns empty array when input has fewer than 2 elements. *)
val simple : prices:float array -> float array

(** Log returns: [r.(i) = log (prices.(i+1) /. prices.(i))].
    Additive over time (sum of log returns over a window equals the log of
    the cumulative return), which makes them easier for time-series math.
    Length: [Array.length prices - 1]. *)
val log : prices:float array -> float array

(** Cumulative compounded return: [c.(i) = (∏ (1 + r.(j))) for j=0..i] minus 1.
    Length matches input. [c.(n-1)] is the total period return.
    For log returns, prefer [cumulative_log] which is just running sum. *)
val cumulative : returns:float array -> float array

(** Cumulative log return: running sum. *)
val cumulative_log : log_returns:float array -> float array

(** Convert simple return to log return: [log (1 +. r)]. *)
val to_log : float -> float

(** Convert log return to simple return: [exp r -. 1]. *)
val of_log : float -> float

(** Annualize a per-period return given periods-per-year (e.g. 365 for daily,
    24*365 for hourly, etc.). Uses geometric compounding:
    [(1 +. r) ** periods_per_year -. 1]. *)
val annualize : periods_per_year:float -> float -> float

(** Annualize a log return: [r *. periods_per_year]. *)
val annualize_log : periods_per_year:float -> float -> float

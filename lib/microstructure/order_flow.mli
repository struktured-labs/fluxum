(** Order-flow and order-book primitives.

    {b Important caveat for prediction-market data}: true order-flow
    metrics (OFI in particular) require {i per-event} order-book updates.
    REST-polled snapshots at 15-30 second cadence alias the underlying
    book dynamics — most updates between polls are missed, and what
    survives is a downsampled artifact of the real signal. Functions in
    this module accept a [?max_inter_sample_seconds] gate that returns
    [Sampling_aliased] when the snapshot stream is too coarse to be
    treated as event-stream-equivalent. *)

(** A single order-book snapshot at one moment. *)
type book_snapshot =
  { timestamp : Time_float_unix.t
  ; bids : (float * float) array
      (** Sorted descending by price; pairs of (price, size). *)
  ; asks : (float * float) array
      (** Sorted ascending by price; pairs of (price, size). *)
  }
[@@deriving sexp]

(** Result of orderbook-imbalance calculation. *)
type imbalance_result =
  | Imbalance of float
      (** [(bid_size_sum - ask_size_sum) / (bid_size_sum + ask_size_sum)]
          in [-1, 1]. Positive = bids dominate; negative = asks dominate. *)
  | Sampling_aliased of
      { observed_dt : float
      ; threshold : float
      }
      (** Average inter-snapshot gap exceeds [max_inter_sample_seconds];
          metric rejected to avoid HF-cadence interpretation of polled
          data. Returns the observed gap and the threshold for diagnosis. *)
  | Insufficient_data of int
[@@deriving sexp]

(** Top-of-book imbalance across the first [levels] price levels of each
    side, averaged over the snapshot stream.

    @param snapshots Time-ordered snapshot stream.
    @param levels Number of price levels per side to include. Default 5.
    @param max_inter_sample_seconds Maximum acceptable mean gap between
           consecutive snapshots. Default [Float.infinity] (no gate;
           caller asserts data is event-stream-quality). Set to e.g.
           [1.0] to reject anything coarser than 1-second polling. *)
val orderbook_imbalance :
  snapshots:book_snapshot array ->
  ?levels:int ->
  ?max_inter_sample_seconds:float ->
  unit ->
  imbalance_result

(** Per-snapshot imbalance series result. *)
type imbalance_series_result =
  | Series of float array
  | Series_sampling_aliased of
      { observed_dt : float
      ; threshold : float
      }
  | Series_insufficient_data of int
[@@deriving sexp]

(** Per-snapshot imbalance (for any consumer that wants the full series
    rather than the aggregate). Same sampling gate as
    {!orderbook_imbalance}. *)
val imbalance_series :
  snapshots:book_snapshot array ->
  ?levels:int ->
  ?max_inter_sample_seconds:float ->
  unit ->
  imbalance_series_result

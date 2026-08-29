(** Read-only measurement harness for Kraken Unified FIX market data. *)

module Fix = Fix
module Fix_session = Fix_session

type error =
  [ `Invalid_duration of float
  | `Invalid_sample_capacity of int
  | `Invalid_symbols
  | `Session of Fix_session.error ]
[@@deriving sexp_of]

module Metrics : sig
  type latency = {
    total_samples : int;
    retained_samples : int;
    overwritten_samples : int;
    invalid_samples : int;
    minimum_us : float option;
    p50_us : float option;
    p95_us : float option;
    p99_us : float option;
    maximum_us : float option;
  }
  [@@deriving sexp, equal]

  type snapshot = {
    connection_attempts : int;
    connections : int;
    disconnects : int;
    messages : int;
    messages_per_second : float;
    message_types : (string * int) list;
    possible_duplicates : int;
    non_monotonic_sequences : int;
    sequence_jumps : int;
    skipped_sequence_numbers : int;
    decode_latency : latency;
    delivery_latency : latency;
    interarrival : latency;
  }
  [@@deriving sexp, equal]

  type t

  val create : sample_capacity:int -> (t, error) Result.t
  (** Creates bounded, allocation-free-on-observation latency samplers. Once a
      sampler is full, newer observations replace the oldest retained values.
      Capacities from 1 through 1,000,000 are accepted. *)

  val observe : t -> Fix_session.Client.Timed_event.t -> unit
  val snapshot : t -> elapsed:Time_ns.Span.t -> snapshot
  val report : snapshot -> string
end

val run :
  environment:Fix.Endpoint.environment ->
  sender_comp_id:string ->
  symbols:string list ->
  depth:Fix.Market_data.depth ->
  state_path:string ->
  duration_seconds:float ->
  sample_capacity:int ->
  checkpoint_every:int ->
  reset_on_start:bool ->
  (Metrics.snapshot, error) Deferred.Result.t
(** Runs a bounded market-data-only probe. Every reconnect resubscribes to the
    requested book feed. This function cannot construct or send an order. *)

val command : string * Command.t

(** Stateful Async runner for Kraken Unified FIX sessions. *)

module Fix = Fix

type error =
  [ `Already_running
  | `Event_queue_full of int
  | `Event_stream_closed
  | `Fix of Fix.error
  | `Gap_buffer_full of int
  | `Io of Error.t
  | `Liveness_timeout of Time_ns.Span.t
  | `Logon_timeout of Time_ns.Span.t
  | `Not_connected
  | `Not_logged_on
  | `Replay_unavailable of int * int
  | `Sequence of Fix.Codec.Sequence.sequence_error
  | `Sent_but_not_checkpointed of int * Error.t
  | `State of Error.t
  | `Stopped
  | `Wrong_session_identity of string
  | `Wrong_session_type of string ]
[@@deriving sexp_of]

module Sequence_state : sig
  type t [@@deriving sexp, equal]

  val create : ?next_outgoing:int -> ?next_incoming:int -> unit -> t
  val next_outgoing : t -> int
  val next_incoming : t -> int
end

module State_store : sig
  val load : string -> Sequence_state.t Deferred.Or_error.t
  (** Missing files load as a fresh [1, 1] session. Saves use an fsynced
      same-directory temporary followed by atomic rename and containing-directory
      fsync. *)

  val save : string -> Sequence_state.t -> unit Deferred.Or_error.t
  val reset : string -> unit Deferred.Or_error.t
end

type authentication =
  | Market_data
  | Trading of {
      credentials : Fix.Credentials.t;
      cancel_on_disconnect : Fix.Session.cancel_on_disconnect;
      client_id : int option;
    }

module Config : sig
  type t

  val create :
    endpoint:Fix.Endpoint.t ->
    sender_comp_id:string ->
    authentication:authentication ->
    state_path:string ->
    ?heartbeat_interval:int ->
    ?reconnect_delay:Time_ns.Span.t ->
    ?connect_timeout:Time_ns.Span.t ->
    ?max_frame_length:int ->
    ?checkpoint_every:int ->
    ?event_capacity:int ->
    ?gap_buffer_capacity:int ->
    ?journal_capacity:int ->
    ?logon_timeout:Time_ns.Span.t ->
    ?liveness_timeout:Time_ns.Span.t ->
    ?reset_on_start:bool ->
    unit ->
    (t, error) Result.t
  (** [checkpoint_every] defaults to [1]. Higher values batch fsyncs for lower
      latency, at the cost of sequence rollback after a process or machine
      crash. Graceful disconnect and [stop] always checkpoint. Event, gap, and
      replay-journal capacities are hard bounds; crossing one fails closed. The
      replay journal survives reconnects within this process, but is not restored
      after a process restart. *)

  val endpoint : t -> Fix.Endpoint.t
  val sender_comp_id : t -> string
  val state_path : t -> string
end

module Outbound : sig
  type t =
    | Market_data_request of Fix.Market_data.request
    | New_order of Fix.Order.new_order
    | Cancel_order of Fix.Order.cancel_order
    | Heartbeat of string option
    | Test_request of string
    | Resend_request of {
        begin_sequence_number : int;
        end_sequence_number : int;
      }
    | Logout of string option
  [@@deriving sexp_of]
end

module Client : sig
  type t

  type event =
    | Connecting
    | Connected
    | Disconnected of Error.t
    | Message of Fix.Codec.Frame.t

  val create : Config.t -> (t, error) Deferred.Result.t
  val events : t -> event Pipe.Reader.t
  val state : t -> Sequence_state.t

  val run : t -> (unit, error) Deferred.Result.t
  (** Reconnects until [stop] is called. Connection failures are published as
      [Disconnected]; persistent-state failures terminate [run]. *)

  val send : t -> Outbound.t -> (unit, error) Deferred.Result.t
  (** Messages are serialized and assigned the next sequence number only after
      the socket flush succeeds. [`Sent_but_not_checkpointed] means the message
      reached the socket but its sequence state did not become durable; callers
      must reconcile and must not blindly retry. Application messages are
      rejected until Kraken's Logon response arrives. *)

  val stop : t -> unit

  module For_testing : sig
    type connection = {
      reader : Reader.t;
      writer : Writer.t;
      closed : unit Deferred.t;
      close : unit -> unit Deferred.t;
    }

    type connector =
      stop:unit Deferred.t ->
      Fix.Endpoint.t ->
      (connection, Error.t) Deferred.Result.t

    val create : Config.t -> connector:connector -> (t, error) Deferred.Result.t
    val tls_config : Fix.Endpoint.t -> Async_ssl.Config.Client.t
  end
end

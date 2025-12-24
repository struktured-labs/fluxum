(** Session Interface - Extracted from Gemini's auto-reconnecting stream implementation *)

open Core
open Async

(** Session state *)
module State = struct
  type t =
    | Disconnected
    | Connecting
    | Connected
    | Ready
    | Reconnecting of { attempt : int }
    | Failed of Error.t
  [@@deriving sexp, compare, equal]
end

(** Auto-restarting pipe wrapper (Gemini's killer feature!) *)
module Auto_restart = struct
  (** Create a pipe that automatically reconnects on EOF

      @param name Name for logging
      @param create_pipe Function to create a new pipe connection
      @return A pipe that never closes - automatically reconnects
  *)
  let pipe ~name ~create_pipe () =
    let reader, writer = Pipe.create () in
    let rec restart_loop () =
      Log.Global.info "auto_restart_pipe[%s]: connecting..." name;
      create_pipe () >>= fun source_pipe ->
      Log.Global.info "auto_restart_pipe[%s]: connected, relaying" name;
      Pipe.transfer source_pipe writer ~f:Fn.id >>= fun () ->
      Log.Global.info "auto_restart_pipe[%s]: EOF detected, restarting in 1s" name;
      after (Time_float_unix.Span.of_sec 1.0) >>= fun () ->
      restart_loop ()
    in
    don't_wait_for (restart_loop ());
    reader
end

(** Multi-stream event container *)
module type EVENTS = sig
  type balance
  type trade
  type market_event
  type book
  type ledger_entry
  type order_event
  type order_id

  type t

  val symbols : t -> Types.Symbol.t list
  val balance : t -> balance Pipe.Reader.t
  val trades : t -> trade Pipe.Reader.t Types.Symbol.Map.t
  val market_data : t -> market_event Pipe.Reader.t Types.Symbol.Map.t
  val order_books : t -> book Pipe.Reader.t Types.Symbol.Map.t
  val ledger : t -> ledger_entry Pipe.Reader.t Types.Symbol.Map.t
  val order_events : t -> order_event Pipe.Reader.t
end

(** Session management *)
module type S = sig
  type t

  module Events : EVENTS

  (** Get event streams *)
  val events : t -> Events.t

  (** Current state *)
  val state : t -> State.t

  (** State change notifications *)
  val state_changes : t -> State.t Pipe.Reader.t

  (** Close session *)
  val close : t -> unit Deferred.t
end

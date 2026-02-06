(** WebSocket health monitoring with proactive ping and pong timeout detection.

    This module provides configurable health monitoring for WebSocket connections:
    - Sends periodic ping frames at configurable intervals
    - Detects pong timeouts and triggers reconnection
    - Tracks connection health status
*)

open Core
open Async

(** Health monitoring configuration *)
module Config = struct
  type t = {
    ping_interval : Time_float_unix.Span.t;     (** How often to send pings *)
    pong_timeout : Time_float_unix.Span.t;      (** Max time to wait for pong *)
    max_reconnect_attempts : int;               (** Max consecutive reconnect attempts *)
  }
  [@@deriving sexp, compare, equal]

  let default = {
    ping_interval = Time_float_unix.Span.of_sec 30.0;
    pong_timeout = Time_float_unix.Span.of_sec 10.0;
    max_reconnect_attempts = 5;
  }

  let create
      ?(ping_interval = Time_float_unix.Span.of_sec 30.0)
      ?(pong_timeout = Time_float_unix.Span.of_sec 10.0)
      ?(max_reconnect_attempts = 5)
      ()
    =
    { ping_interval; pong_timeout; max_reconnect_attempts }

  (** Aggressive config for high-frequency trading *)
  let aggressive = {
    ping_interval = Time_float_unix.Span.of_sec 10.0;
    pong_timeout = Time_float_unix.Span.of_sec 5.0;
    max_reconnect_attempts = 10;
  }

  (** Relaxed config for low-frequency connections *)
  let relaxed = {
    ping_interval = Time_float_unix.Span.of_sec 60.0;
    pong_timeout = Time_float_unix.Span.of_sec 20.0;
    max_reconnect_attempts = 3;
  }
end

(** Connection health status *)
module Status = struct
  type t =
    | Healthy
    | Degraded of { last_pong_age : Time_float_unix.Span.t }
    | Unhealthy of { reason : string }
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Healthy -> "healthy"
    | Degraded { last_pong_age } ->
      sprintf "degraded (last pong %s ago)"
        (Time_float_unix.Span.to_short_string last_pong_age)
    | Unhealthy { reason } ->
      sprintf "unhealthy: %s" reason
end

(** Health monitor state *)
type t = {
  config : Config.t;
  mutable last_pong : Time_float_unix.t;
  mutable last_ping_sent : Time_float_unix.t option;
  mutable consecutive_timeouts : int;
  mutable running : bool;
  status_writer : Status.t Pipe.Writer.t;
  status_reader : Status.t Pipe.Reader.t;
}

let create ~config () =
  let status_reader, status_writer = Pipe.create () in
  {
    config;
    last_pong = Time_float_unix.now ();
    last_ping_sent = None;
    consecutive_timeouts = 0;
    running = false;
    status_writer;
    status_reader;
  }

(** Notify the monitor that a pong was received *)
let on_pong_received t =
  t.last_pong <- Time_float_unix.now ();
  t.consecutive_timeouts <- 0;
  don't_wait_for (Pipe.write_if_open t.status_writer Status.Healthy)

(** Get the current health status *)
let status t =
  let now = Time_float_unix.now () in
  let last_pong_age = Time_float_unix.diff now t.last_pong in
  match Time_float_unix.Span.(last_pong_age > t.config.pong_timeout) with
  | true ->
    Status.Unhealthy { reason = sprintf "pong timeout (%s)"
                                  (Time_float_unix.Span.to_short_string last_pong_age) }
  | false ->
    (* Degraded if we're past half the timeout threshold *)
    let half_timeout = Time_float_unix.Span.scale t.config.pong_timeout 0.5 in
    match Time_float_unix.Span.(last_pong_age > half_timeout) with
    | true -> Status.Degraded { last_pong_age }
    | false -> Status.Healthy

(** Get a pipe of status updates *)
let status_changes t = t.status_reader

(** Stop the health monitor *)
let stop t =
  t.running <- false;
  Pipe.close t.status_writer

(** Start the health monitoring loop.

    @param send_ping Function to send a ping frame (returns unit Or_error.t Deferred.t)
    @param on_timeout Function called when pong timeout is detected
    @return A deferred that resolves when the monitor stops
*)
let start t ~send_ping ~on_timeout =
  match t.running with
  | true -> Deferred.unit  (* Already running *)
  | false ->
    t.running <- true;
    t.last_pong <- Time_float_unix.now ();
    t.consecutive_timeouts <- 0;

    let rec loop () =
      match t.running with
      | false -> Deferred.unit
      | true ->
        (* Wait for ping interval *)
        let%bind () = after t.config.ping_interval in

        match t.running with
        | false -> Deferred.unit
        | true ->
          (* Send ping *)
          t.last_ping_sent <- Some (Time_float_unix.now ());
          let%bind ping_result = send_ping () in

          match ping_result with
          | Error err ->
            (* Ping send failed - connection likely dead *)
            Log.Global.error "ws_health: ping send failed: %s" (Error.to_string_hum err);
            t.running <- false;
            on_timeout ()
          | Ok () ->
            (* Wait for pong timeout period *)
            let%bind () = after t.config.pong_timeout in

            match t.running with
            | false -> Deferred.unit
            | true ->
              (* Check if we received a pong since the ping was sent *)
              let ping_time = Option.value_exn t.last_ping_sent in
              match Time_float_unix.(t.last_pong >= ping_time) with
              | true ->
                (* Pong received - continue healthy *)
                loop ()
              | false ->
                (* Pong timeout detected *)
                t.consecutive_timeouts <- t.consecutive_timeouts + 1;
                Log.Global.info "ws_health: pong timeout detected (attempt %d/%d)"
                  t.consecutive_timeouts t.config.max_reconnect_attempts;

                let current_status = status t in
                let%bind () = Pipe.write_if_open t.status_writer current_status in

                match t.consecutive_timeouts >= t.config.max_reconnect_attempts with
                | true ->
                  (* Max timeouts reached - trigger reconnect *)
                  Log.Global.error "ws_health: max reconnect attempts reached, triggering reconnect";
                  t.running <- false;
                  on_timeout ()
                | false ->
                  (* Try another ping cycle *)
                  loop ()
    in
    loop ()

(** Module type for WebSocket connections that support health monitoring *)
module type HEALTHABLE = sig
  type t

  (** Send a ping frame. Returns Ok () on success, Error on failure. *)
  val send_ping : t -> ?payload:string -> unit -> unit Or_error.t Deferred.t

  (** Close the connection *)
  val close : t -> unit Deferred.t
end

(** Functor to create a health-monitored WebSocket wrapper *)
module Make (Ws : HEALTHABLE) = struct
  type ws_with_health = {
    ws : Ws.t;
    health : t;
    mutable health_loop : unit Deferred.t option;
  }

  let create ~ws ~config ~on_timeout () =
    let health = create ~config () in
    let t = { ws; health; health_loop = None } in

    (* Start health monitoring *)
    let loop = start health
        ~send_ping:(fun () -> Ws.send_ping ws ())
        ~on_timeout:(fun () ->
          Log.Global.info "ws_health: triggering reconnect callback";
          on_timeout ())
    in
    t.health_loop <- Some loop;
    t

  let on_pong_received t = on_pong_received t.health
  let status t = status t.health
  let status_changes t = status_changes t.health

  let stop t =
    stop t.health;
    let%bind () =
      match t.health_loop with
      | Some loop -> loop
      | None -> Deferred.unit
    in
    Ws.close t.ws
end

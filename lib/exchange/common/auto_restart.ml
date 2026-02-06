(** Auto-restart utility for WebSocket and polling pipes.

    Creates pipes that automatically reconnect on EOF or health timeout,
    used across all exchange session implementations for resilient streaming.
*)

open Core
open Async

(** Health monitoring configuration for auto-restart pipes *)
module Health_config = struct
  type t = {
    ping_interval : Time_float_unix.Span.t;     (** How often to send pings *)
    pong_timeout : Time_float_unix.Span.t;      (** Max time to wait for pong *)
    max_reconnect_attempts : int;               (** Max consecutive reconnect attempts before giving up *)
    reconnect_delay : Time_float_unix.Span.t;   (** Delay between reconnect attempts *)
  }
  [@@deriving sexp, compare, equal]

  let default = {
    ping_interval = Time_float_unix.Span.of_sec 30.0;
    pong_timeout = Time_float_unix.Span.of_sec 10.0;
    max_reconnect_attempts = 5;
    reconnect_delay = Time_float_unix.Span.of_sec 1.0;
  }

  let create
      ?(ping_interval = Time_float_unix.Span.of_sec 30.0)
      ?(pong_timeout = Time_float_unix.Span.of_sec 10.0)
      ?(max_reconnect_attempts = 5)
      ?(reconnect_delay = Time_float_unix.Span.of_sec 1.0)
      ()
    =
    { ping_interval; pong_timeout; max_reconnect_attempts; reconnect_delay }
end

(** Health monitor state for tracking pong responses *)
module Health_monitor = struct
  type t = {
    config : Health_config.t;
    mutable last_pong : Time_float_unix.t;
    mutable last_ping_sent : Time_float_unix.t option;
    mutable consecutive_timeouts : int;
    mutable running : bool;
    mutable trigger_ivar : unit Ivar.t ref;  (** Filled when timeout triggers reconnect *)
  }

  let create ~config () = {
    config;
    last_pong = Time_float_unix.now ();
    last_ping_sent = None;
    consecutive_timeouts = 0;
    running = false;
    trigger_ivar = ref (Ivar.create ());
  }

  (** Called when a pong frame is received *)
  let on_pong_received t =
    t.last_pong <- Time_float_unix.now ();
    t.consecutive_timeouts <- 0

  (** Reset the monitor for a new connection *)
  let reset t =
    t.last_pong <- Time_float_unix.now ();
    t.last_ping_sent <- None;
    t.consecutive_timeouts <- 0;
    t.trigger_ivar <- ref (Ivar.create ())

  (** Stop the health monitor *)
  let stop t =
    t.running <- false

  (** Get a deferred that resolves when health timeout is detected *)
  let timeout_triggered t =
    Ivar.read !(t.trigger_ivar)

  (** Start the health monitoring loop.

      @param send_ping Function to send a ping frame
      @return A deferred that resolves when the monitor stops
  *)
  let start t ~send_ping =
    match t.running with
    | true -> Deferred.unit  (* Already running *)
    | false ->
      t.running <- true;
      reset t;

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
              Log.Global.error "health_monitor: ping send failed: %s" (Error.to_string_hum err);
              t.running <- false;
              Ivar.fill_if_empty !(t.trigger_ivar) ();
              Deferred.unit
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
                  Log.Global.info "health_monitor: pong timeout detected (attempt %d/%d)"
                    t.consecutive_timeouts t.config.max_reconnect_attempts;

                  match t.consecutive_timeouts >= t.config.max_reconnect_attempts with
                  | true ->
                    (* Max timeouts reached - trigger reconnect *)
                    Log.Global.error "health_monitor: max timeouts reached, triggering reconnect";
                    t.running <- false;
                    Ivar.fill_if_empty !(t.trigger_ivar) ();
                    Deferred.unit
                  | false ->
                    (* Try another ping cycle *)
                    loop ()
      in
      loop ()
end

(** Create a pipe that automatically reconnects on EOF.

    @param name Label for log messages (e.g. "kraken_balance_poll")
    @param create_pipe Factory that produces a fresh source pipe on each call
    @return A reader pipe that transparently restarts on upstream EOF
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

(** Create a pipe with health monitoring that reconnects on EOF or pong timeout.

    @param name Label for log messages
    @param config Health monitoring configuration
    @param create_pipe Factory that produces a fresh source pipe and ping function
    @return A reader pipe that transparently restarts on upstream EOF or health timeout
*)
let pipe_with_health ~name ~config ~create_pipe () =
  let reader, writer = Pipe.create () in
  let health_monitor = Health_monitor.create ~config () in

  let rec restart_loop ~attempt () =
    match attempt > config.max_reconnect_attempts with
    | true ->
      Log.Global.error "auto_restart_pipe[%s]: max reconnect attempts (%d) exceeded, giving up"
        name config.max_reconnect_attempts;
      Pipe.close writer;
      Deferred.unit
    | false ->
      Log.Global.info "auto_restart_pipe[%s]: connecting (attempt %d)..." name attempt;

      let%bind connection_result = create_pipe () in
      match connection_result with
      | Error err ->
        Log.Global.error "auto_restart_pipe[%s]: connection failed: %s" name (Error.to_string_hum err);
        let%bind () = after config.reconnect_delay in
        restart_loop ~attempt:(attempt + 1) ()
      | Ok (source_pipe, send_ping, on_pong_callback_setter) ->
        Log.Global.info "auto_restart_pipe[%s]: connected, starting health monitor" name;
        Health_monitor.reset health_monitor;

        (* Register pong callback *)
        on_pong_callback_setter (fun () -> Health_monitor.on_pong_received health_monitor);

        (* Start health monitoring *)
        don't_wait_for (Health_monitor.start health_monitor ~send_ping);

        (* Race between: EOF on source pipe, or health timeout *)
        let eof_deferred =
          let%map () = Pipe.transfer source_pipe writer ~f:Fn.id in
          `Eof
        in
        let timeout_deferred =
          let%map () = Health_monitor.timeout_triggered health_monitor in
          `Timeout
        in

        let%bind reason = Deferred.any [ eof_deferred; timeout_deferred ] in

        (* Stop health monitor *)
        Health_monitor.stop health_monitor;

        let reason_str =
          match reason with
          | `Eof -> "EOF"
          | `Timeout -> "health timeout"
        in
        Log.Global.info "auto_restart_pipe[%s]: %s detected, restarting in %s"
          name reason_str (Time_float_unix.Span.to_short_string config.reconnect_delay);

        let%bind () = after config.reconnect_delay in
        restart_loop ~attempt:1 ()  (* Reset attempt counter on successful connection *)
  in
  don't_wait_for (restart_loop ~attempt:1 ());
  reader

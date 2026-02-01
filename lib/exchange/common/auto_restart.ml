(** Auto-restart utility for WebSocket and polling pipes.

    Creates pipes that automatically reconnect on EOF, used across all
    exchange session implementations for resilient streaming.
*)

open Core
open Async

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

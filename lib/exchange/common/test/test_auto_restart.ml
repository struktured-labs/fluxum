(** Tests for auto_restart health monitoring *)

open Core
open Exchange_common.Auto_restart

(* ────────────────────────────────────────────────────────────────────────
   Tests: Health_config
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "Health_config" = (module struct
  let%test "default config has 30s ping interval" =
    let config = Health_config.default in
    Time_float_unix.Span.(equal config.ping_interval (of_sec 30.0))

  let%test "default config has 10s pong timeout" =
    let config = Health_config.default in
    Time_float_unix.Span.(equal config.pong_timeout (of_sec 10.0))

  let%test "default config has 5 max reconnect attempts" =
    Health_config.default.max_reconnect_attempts = 5

  let%test "default config has 1s reconnect delay" =
    let config = Health_config.default in
    Time_float_unix.Span.(equal config.reconnect_delay (of_sec 1.0))

  let%test "create with custom values" =
    let config = Health_config.create
        ~ping_interval:(Time_float_unix.Span.of_sec 15.0)
        ~pong_timeout:(Time_float_unix.Span.of_sec 5.0)
        ~max_reconnect_attempts:10
        ~reconnect_delay:(Time_float_unix.Span.of_sec 2.0)
        ()
    in
    Time_float_unix.Span.(equal config.ping_interval (of_sec 15.0)) &&
    Time_float_unix.Span.(equal config.pong_timeout (of_sec 5.0)) &&
    config.max_reconnect_attempts = 10 &&
    Time_float_unix.Span.(equal config.reconnect_delay (of_sec 2.0))

  let%test "create with partial custom values" =
    let config = Health_config.create
        ~ping_interval:(Time_float_unix.Span.of_sec 20.0)
        ()
    in
    Time_float_unix.Span.(equal config.ping_interval (of_sec 20.0)) &&
    Time_float_unix.Span.(equal config.pong_timeout (of_sec 10.0))  (* default *)
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: Health_monitor
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "Health_monitor" = (module struct
  let%test "create initializes with non-running state" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    not monitor.running

  let%test "create initializes consecutive_timeouts to 0" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    monitor.consecutive_timeouts = 0

  let%test "on_pong_received resets consecutive_timeouts" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    monitor.consecutive_timeouts <- 3;
    Health_monitor.on_pong_received monitor;
    monitor.consecutive_timeouts = 0

  let%test "on_pong_received updates last_pong time" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    let old_time = monitor.last_pong in
    Core_unix.sleep 1;  (* Small delay to ensure time changes *)
    Health_monitor.on_pong_received monitor;
    Time_float_unix.(monitor.last_pong > old_time)

  let%test "reset clears state" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    monitor.consecutive_timeouts <- 5;
    monitor.last_ping_sent <- Some (Time_float_unix.now ());
    Health_monitor.reset monitor;
    monitor.consecutive_timeouts = 0 && Option.is_none monitor.last_ping_sent

  let%test "stop sets running to false" =
    let config = Health_config.default in
    let monitor = Health_monitor.create ~config () in
    monitor.running <- true;
    Health_monitor.stop monitor;
    not monitor.running
end)

(** Print test summary *)
let () =
  printf "\n=== Auto Restart Health Monitoring Tests ===\n";
  printf "Health_config tests - default values and custom creation\n";
  printf "Health_monitor tests - state management and pong handling\n\n"

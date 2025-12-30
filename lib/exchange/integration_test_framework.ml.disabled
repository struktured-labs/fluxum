(** Integration Test Framework for Exchange APIs

    Tests REST and WebSocket endpoints against testnet/sandbox environments.
    Designed to run in CI with API keys from GitHub secrets.
*)

open Core
open Async

(** Test result *)
module Test_result = struct
  type t = {
    exchange: string;
    test_name: string;
    passed: bool;
    duration_ms: float;
    error: string option;
  } [@@deriving sexp, fields]

  let create ~exchange ~test_name ~passed ~duration_ms ?error () =
    { exchange; test_name; passed; duration_ms; error }

  let success ~exchange ~test_name ~duration_ms () =
    create ~exchange ~test_name ~passed:true ~duration_ms ()

  let failure ~exchange ~test_name ~duration_ms ~error () =
    { exchange; test_name; passed = false; duration_ms; error = Some error }
end

(** Test suite *)
module Suite = struct
  type t = {
    name: string;
    results: Test_result.t list ref;
    start_time: Time_float_unix.t;
  }

  let create ~name () = {
    name;
    results = ref [];
    start_time = Time_float_unix.now ();
  }

  let add_result t result =
    t.results := result :: !(t.results)

  let total_tests t = List.length !(t.results)
  let passed_tests t = List.count !(t.results) ~f:(fun r -> r.Test_result.passed)
  let failed_tests t = total_tests t - passed_tests t

  let duration t =
    Time_float_unix.diff (Time_float_unix.now ()) t.start_time
    |> Time_float_unix.Span.to_sec

  let print_summary t =
    let total = total_tests t in
    let passed = passed_tests t in
    let failed = failed_tests t in
    let duration = duration t in

    printf "\n╔════════════════════════════════════════════════════════╗\n";
    printf "║  %s - Test Results\n" t.name;
    printf "╠════════════════════════════════════════════════════════╣\n";
    printf "║  Total:   %3d tests                                     ║\n" total;
    printf "║  Passed:  %3d tests  ✓                                  ║\n" passed;
    printf "║  Failed:  %3d tests  ✗                                  ║\n" failed;
    printf "║  Duration: %.2fs                                        ║\n" duration;
    printf "╚════════════════════════════════════════════════════════╝\n";

    if failed > 0 then (
      printf "\nFailed Tests:\n";
      List.iter !(t.results) ~f:(fun r ->
        if not r.Test_result.passed then
          printf "  ✗ %s/%s: %s\n"
            r.Test_result.exchange
            r.Test_result.test_name
            (Option.value r.Test_result.error ~default:"Unknown error")
      )
    );

    printf "\n";
    passed = total
end

(** Rate limiter to avoid hitting API limits *)
module Rate_limiter = struct
  type t = {
    min_interval_ms: float;
    mutable last_request: Time_float_unix.t option;
  }

  let create ~requests_per_second =
    let min_interval_ms = 1000. /. Float.of_int requests_per_second in
    { min_interval_ms; last_request = None }

  let wait t =
    match t.last_request with
    | None ->
      t.last_request <- Some (Time_float_unix.now ());
      Deferred.unit
    | Some last ->
      let elapsed =
        Time_float_unix.diff (Time_float_unix.now ()) last
        |> Time_float_unix.Span.to_ms
      in
      let wait_ms = Float.max 0. (t.min_interval_ms -. elapsed) in
      t.last_request <- Some (Time_float_unix.now ());
      if Float.(wait_ms > 0.) then
        after (Time_float_unix.Span.of_ms wait_ms)
      else
        Deferred.unit
end

(** Test runner *)
module Runner = struct
  type test_fn = unit -> (unit, string) Result.t Deferred.t

  let run_test ~suite ~exchange ~name ~f ~rate_limiter =
    let start = Time_float_unix.now () in
    Rate_limiter.wait rate_limiter >>= fun () ->
    Monitor.try_with (fun () -> f ())
    >>| fun result ->
    let duration_ms =
      Time_float_unix.diff (Time_float_unix.now ()) start
      |> Time_float_unix.Span.to_ms
    in
    match result with
    | Ok (Ok ()) ->
      let r = Test_result.success ~exchange ~test_name:name ~duration_ms () in
      Suite.add_result suite r;
      printf "  ✓ %s (%0.1fms)\n%!" name duration_ms
    | Ok (Error err) ->
      let r = Test_result.failure ~exchange ~test_name:name ~duration_ms ~error:err () in
      Suite.add_result suite r;
      printf "  ✗ %s: %s (%0.1fms)\n%!" name err duration_ms
    | Error exn ->
      let err = Exn.to_string exn in
      let r = Test_result.failure ~exchange ~test_name:name ~duration_ms ~error:err () in
      Suite.add_result suite r;
      printf "  ✗ %s: %s (%0.1fms)\n%!" name err duration_ms

  let run_tests ~suite ~exchange ~tests ~rate_limiter =
    printf "\n=== Testing %s ===\n%!" exchange;
    Deferred.List.iter tests ~how:`Sequential ~f:(fun (name, f) ->
      run_test ~suite ~exchange ~name ~f ~rate_limiter
    )
end

(** Helper functions for common test patterns *)
module Helpers = struct
  (** Test that endpoint returns successfully *)
  let test_endpoint ~name ~f =
    (name, fun () ->
      f () >>| function
      | Ok _ -> Ok ()
      | Error err -> Error (Sexp.to_string_hum err))

  (** Test that value matches expected *)
  let assert_equal ~equal ~sexp_of_t ~expected actual =
    if equal expected actual then
      Ok ()
    else
      Error (sprintf "Expected %s but got %s"
        (Sexp.to_string_hum (sexp_of_t expected))
        (Sexp.to_string_hum (sexp_of_t actual)))

  (** Test that value is Some *)
  let assert_some ~sexp_of_t:_ = function
    | Some v -> Ok v
    | None -> Error "Expected Some but got None"

  (** Test that operation succeeds *)
  let assert_ok = function
    | Ok v -> Ok v
    | Error err -> Error (Sexp.to_string_hum err)

  (** Cleanup helper - cancel all open orders for a symbol *)
  let cleanup_orders ~cancel_fn ~symbol =
    Monitor.try_with (fun () -> cancel_fn ~symbol ())
    >>| function
    | Ok _ -> ()
    | Error _ -> ()  (* Ignore cleanup errors *)
end

(** Configuration from environment variables *)
module Config = struct
  type t = {
    exchange: string;
    api_key: string;
    api_secret: string;
    testnet: bool;
  }

  let from_env ~exchange ~testnet =
    let prefix = String.uppercase exchange in
    let suffix = if testnet then "_TESTNET" else "" in
    let api_key_var = sprintf "%s%s_API_KEY" prefix suffix in
    let api_secret_var = sprintf "%s%s_API_SECRET" prefix suffix in

    match Sys.getenv api_key_var, Sys.getenv api_secret_var with
    | Some api_key, Some api_secret ->
      Some { exchange; api_key; api_secret; testnet }
    | _ ->
      printf "⚠ Skipping %s tests - %s and %s not set\n%!"
        exchange api_key_var api_secret_var;
      None

  let is_available ~exchange ~testnet =
    Option.is_some (from_env ~exchange ~testnet)
end

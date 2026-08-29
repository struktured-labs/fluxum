open Core
open Async
module Fix = Kraken.Fix
module Probe = Kraken.Fix_probe
module Timed_event = Kraken.Fix_session.Client.Timed_event

let result_exn result ~sexp_of_error =
  match result with
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (sexp_of_error error))

let frame ~sequence ~msg_type =
  Fix.Codec.Encoder.message ~sender_comp_id:"KRAKEN-MD" ~target_comp_id:"CLIENT"
    ~msg_type ~msg_seq_num:sequence ~sending_time:"20260826-12:34:56.123"
    ~body_fields:[]
  |> result_exn ~sexp_of_error:Fix.Codec.sexp_of_error
  |> Fix.Codec.Frame.decode
  |> result_exn ~sexp_of_error:Fix.Codec.sexp_of_error

let possible_duplicate_frame ~sequence ~msg_type =
  Fix.Codec.Encoder.message_poss_dup ~sender_comp_id:"KRAKEN-MD"
    ~target_comp_id:"CLIENT" ~msg_type ~msg_seq_num:sequence
    ~sending_time:"20260826-12:34:57.123"
    ~orig_sending_time:"20260826-12:34:56.123" ~body_fields:[]
  |> result_exn ~sexp_of_error:Fix.Codec.sexp_of_error
  |> Fix.Codec.Frame.decode
  |> result_exn ~sexp_of_error:Fix.Codec.sexp_of_error

let at microseconds =
  Time_ns.add Time_ns.epoch (Time_ns.Span.of_us microseconds)

let message frame ~received_us ~decode_us ~delivery_us =
  let received_at = at received_us in
  Kraken.Fix_session.Client.
    {
      frame;
      received_at;
      decoded_at = Time_ns.add received_at (Time_ns.Span.of_us decode_us);
      delivered_at = Time_ns.add received_at (Time_ns.Span.of_us delivery_us);
    }

let assert_some_float option expected =
  match option with
  | Some actual -> assert (Float.equal actual expected)
  | None -> failwith "expected a latency value"

let test_invalid_capacity () =
  match Probe.Metrics.create ~sample_capacity:0 with
  | Error (`Invalid_sample_capacity 0) -> (
      match Probe.Metrics.create ~sample_capacity:1_000_001 with
      | Error (`Invalid_sample_capacity 1_000_001) -> ()
      | Error error -> failwith (Sexp.to_string_hum (Probe.sexp_of_error error))
      | Ok _ -> failwith "oversized sample capacity was accepted")
  | Error error -> failwith (Sexp.to_string_hum (Probe.sexp_of_error error))
  | Ok _ -> failwith "zero sample capacity was accepted"

let test_metrics () =
  let metrics =
    Probe.Metrics.create ~sample_capacity:2
    |> result_exn ~sexp_of_error:Probe.sexp_of_error
  in
  Probe.Metrics.observe metrics Timed_event.Connecting;
  Probe.Metrics.observe metrics Timed_event.Connected;
  Probe.Metrics.observe metrics
    (Timed_event.Message
       (message
          (frame ~sequence:1 ~msg_type:"A")
          ~received_us:1_000. ~decode_us:10. ~delivery_us:30.));
  Probe.Metrics.observe metrics
    (Timed_event.Message
       (message
          (frame ~sequence:3 ~msg_type:"X")
          ~received_us:1_100. ~decode_us:20. ~delivery_us:60.));
  Probe.Metrics.observe metrics
    (Timed_event.Message
       (message
          (possible_duplicate_frame ~sequence:2 ~msg_type:"X")
          ~received_us:1_500. ~decode_us:40. ~delivery_us:80.));
  Probe.Metrics.observe metrics
    (Timed_event.Disconnected (Error.of_string "test disconnect"));
  let snapshot =
    Probe.Metrics.snapshot metrics ~elapsed:(Time_ns.Span.of_sec 2.)
  in
  assert (snapshot.connection_attempts = 1);
  assert (snapshot.connections = 1);
  assert (snapshot.disconnects = 1);
  assert (snapshot.messages = 3);
  assert (Float.equal snapshot.messages_per_second 1.5);
  (match snapshot.message_types with
  | [ ("A", 1); ("X", 2) ] -> ()
  | message_types ->
      failwith
        (Sexp.to_string_hum ([%sexp_of: (string * int) list] message_types)));
  assert (snapshot.possible_duplicates = 1);
  assert (snapshot.sequence_jumps = 1);
  assert (snapshot.skipped_sequence_numbers = 1);
  assert (snapshot.non_monotonic_sequences = 1);
  assert (snapshot.decode_latency.total_samples = 3);
  assert (snapshot.decode_latency.retained_samples = 2);
  assert (snapshot.decode_latency.overwritten_samples = 1);
  assert_some_float snapshot.decode_latency.minimum_us 20.;
  assert_some_float snapshot.decode_latency.p50_us 20.;
  assert_some_float snapshot.decode_latency.p95_us 40.;
  assert_some_float snapshot.decode_latency.p99_us 40.;
  assert_some_float snapshot.decode_latency.maximum_us 40.;
  assert_some_float snapshot.delivery_latency.minimum_us 60.;
  assert_some_float snapshot.delivery_latency.maximum_us 80.;
  assert_some_float snapshot.interarrival.minimum_us 100.;
  assert_some_float snapshot.interarrival.maximum_us 400.;
  let report = Probe.Metrics.report snapshot in
  assert (String.is_substring report ~substring:"rate=1.50/s");
  assert (String.is_substring report ~substring:"types=[A=1, X=2]")

let run_probe ?(duration_seconds = 1.) ?(symbols = [ "BTC/USD" ]) () =
  Probe.run ~environment:Fix.Endpoint.Uat ~sender_comp_id:"CLIENT" ~symbols
    ~depth:Fix.Market_data.Top ~state_path:"/tmp/fluxum-invalid-probe.sexp"
    ~duration_seconds ~sample_capacity:2 ~checkpoint_every:1
    ~reset_on_start:false

let expect_invalid_duration duration_seconds =
  let%map result = run_probe ~duration_seconds () in
  match result with
  | Error (`Invalid_duration actual) ->
      assert (
        match Float.is_nan duration_seconds with
        | true -> Float.is_nan actual
        | false -> Float.equal actual duration_seconds)
  | Error error -> failwith (Sexp.to_string_hum (Probe.sexp_of_error error))
  | Ok _ -> failwith "invalid probe duration was accepted"

let expect_invalid_symbols symbols =
  let%map result = run_probe ~symbols () in
  match result with
  | Error `Invalid_symbols -> ()
  | Error error -> failwith (Sexp.to_string_hum (Probe.sexp_of_error error))
  | Ok _ -> failwith "invalid probe symbols were accepted"

let run_tests () =
  test_invalid_capacity ();
  test_metrics ();
  let%bind () = expect_invalid_duration 0. in
  let%bind () = expect_invalid_duration Float.nan in
  let%bind () = expect_invalid_symbols [] in
  let%bind () = expect_invalid_symbols [ "   " ] in
  print_endline "Kraken FIX probe tests passed";
  Shutdown.exit 0

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

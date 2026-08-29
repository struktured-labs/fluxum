open Core
open Async
module Fix = Fix
module Fix_session = Fix_session

type error =
  [ `Invalid_duration of float
  | `Invalid_sample_capacity of int
  | `Invalid_symbols
  | `Session of Fix_session.error ]
[@@deriving sexp_of]

module Samples = struct
  (* Each probe owns its samplers on Async's scheduler thread. The circular
     arrays keep observation allocation-free and memory usage hard-bounded. *)
  type t = {
    values_ns : float array;
    mutable next : int;
    mutable retained : int;
    mutable observed : int;
    mutable invalid : int;
  }

  let create capacity =
    {
      values_ns = Array.create ~len:capacity 0.;
      next = 0;
      retained = 0;
      observed = 0;
      invalid = 0;
    }

  let add t span =
    let value_ns = Time_ns.Span.to_ns span in
    match Float.is_finite value_ns && Float.(value_ns >= 0.) with
    | false -> t.invalid <- t.invalid + 1
    | true -> (
        t.values_ns.(t.next) <- value_ns;
        t.next <- (t.next + 1) mod Array.length t.values_ns;
        t.observed <- t.observed + 1;
        match t.retained < Array.length t.values_ns with
        | true -> t.retained <- t.retained + 1
        | false -> ())

  let retained_values t = Array.sub t.values_ns ~pos:0 ~len:t.retained
end

module Metrics = struct
  let maximum_sample_capacity = 1_000_000

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

  type t = {
    decode_latency : Samples.t;
    delivery_latency : Samples.t;
    interarrival : Samples.t;
    mutable connection_attempts : int;
    mutable connections : int;
    mutable disconnects : int;
    mutable messages : int;
    mutable message_types : int String.Map.t;
    mutable possible_duplicates : int;
    mutable non_monotonic_sequences : int;
    mutable sequence_jumps : int;
    mutable skipped_sequence_numbers : int;
    mutable previous_received_at : Time_ns.t option;
    mutable previous_sequence_number : int option;
  }

  let create ~sample_capacity =
    match sample_capacity > 0 && sample_capacity <= maximum_sample_capacity with
    | false -> Error (`Invalid_sample_capacity sample_capacity)
    | true ->
        Ok
          {
            decode_latency = Samples.create sample_capacity;
            delivery_latency = Samples.create sample_capacity;
            interarrival = Samples.create sample_capacity;
            connection_attempts = 0;
            connections = 0;
            disconnects = 0;
            messages = 0;
            message_types = String.Map.empty;
            possible_duplicates = 0;
            non_monotonic_sequences = 0;
            sequence_jumps = 0;
            skipped_sequence_numbers = 0;
            previous_received_at = None;
            previous_sequence_number = None;
          }

  let observe_sequence t frame =
    let sequence_number = Fix.Codec.Frame.sequence_number frame in
    (match t.previous_sequence_number with
    | None -> ()
    | Some previous -> (
        match Int.compare sequence_number (previous + 1) with
        | 0 -> ()
        | comparison when comparison < 0 ->
            t.non_monotonic_sequences <- t.non_monotonic_sequences + 1
        | _ ->
            t.sequence_jumps <- t.sequence_jumps + 1;
            t.skipped_sequence_numbers <-
              t.skipped_sequence_numbers + sequence_number - previous - 1));
    t.previous_sequence_number <- Some sequence_number

  let observe_message t (message : Fix_session.Client.message) =
    let frame = message.frame in
    t.messages <- t.messages + 1;
    let msg_type = Fix.Codec.Frame.msg_type frame in
    t.message_types <-
      Map.update t.message_types msg_type ~f:(function
        | None -> 1
        | Some count -> count + 1);
    (match Fix.Codec.Frame.value frame 43 with
    | Some "Y" -> t.possible_duplicates <- t.possible_duplicates + 1
    | Some _ | None -> ());
    Samples.add t.decode_latency
      (Time_ns.diff message.decoded_at message.received_at);
    Samples.add t.delivery_latency
      (Time_ns.diff message.delivered_at message.received_at);
    (match t.previous_received_at with
    | None -> ()
    | Some previous ->
        Samples.add t.interarrival (Time_ns.diff message.received_at previous));
    t.previous_received_at <- Some message.received_at;
    observe_sequence t frame

  let observe t = function
    | Fix_session.Client.Timed_event.Connecting ->
        t.connection_attempts <- t.connection_attempts + 1;
        t.previous_received_at <- None;
        t.previous_sequence_number <- None
    | Connected -> t.connections <- t.connections + 1
    | Disconnected _ ->
        t.disconnects <- t.disconnects + 1;
        t.previous_received_at <- None;
        t.previous_sequence_number <- None
    | Message message -> observe_message t message

  let percentile values probability =
    let length = Array.length values in
    let rank = Float.iround_up_exn (probability *. Float.of_int length) in
    let index = Int.min (length - 1) (Int.max 0 (rank - 1)) in
    values.(index) /. 1_000.

  let latency samples =
    let values = Samples.retained_values samples in
    Array.sort values ~compare:Float.compare;
    let retained_samples = Array.length values in
    let total_samples = samples.Samples.observed in
    let empty = Int.equal retained_samples 0 in
    {
      total_samples;
      retained_samples;
      overwritten_samples = total_samples - retained_samples;
      invalid_samples = samples.invalid;
      minimum_us =
        (match empty with true -> None | false -> Some (values.(0) /. 1_000.));
      p50_us =
        (match empty with
        | true -> None
        | false -> Some (percentile values 0.50));
      p95_us =
        (match empty with
        | true -> None
        | false -> Some (percentile values 0.95));
      p99_us =
        (match empty with
        | true -> None
        | false -> Some (percentile values 0.99));
      maximum_us =
        (match empty with
        | true -> None
        | false -> Some (values.(retained_samples - 1) /. 1_000.));
    }

  let snapshot t ~elapsed =
    let elapsed_seconds = Time_ns.Span.to_sec elapsed in
    let messages_per_second =
      match Float.is_finite elapsed_seconds && Float.(elapsed_seconds > 0.) with
      | true -> Float.of_int t.messages /. elapsed_seconds
      | false -> 0.
    in
    {
      connection_attempts = t.connection_attempts;
      connections = t.connections;
      disconnects = t.disconnects;
      messages = t.messages;
      messages_per_second;
      message_types = Map.to_alist t.message_types;
      possible_duplicates = t.possible_duplicates;
      non_monotonic_sequences = t.non_monotonic_sequences;
      sequence_jumps = t.sequence_jumps;
      skipped_sequence_numbers = t.skipped_sequence_numbers;
      decode_latency = latency t.decode_latency;
      delivery_latency = latency t.delivery_latency;
      interarrival = latency t.interarrival;
    }

  let latency_report label (latency : latency) =
    let value = Option.value_map ~default:"n/a" ~f:(sprintf "%.3f") in
    sprintf
      ("%s (us): min=%s p50=%s p95=%s p99=%s max=%s samples=%d/%d"
     ^^ " overwritten=%d invalid=%d")
      label (value latency.minimum_us) (value latency.p50_us)
      (value latency.p95_us) (value latency.p99_us) (value latency.maximum_us)
      latency.retained_samples latency.total_samples latency.overwritten_samples
      latency.invalid_samples

  let report (snapshot : snapshot) =
    let message_types =
      match snapshot.message_types with
      | [] -> "none"
      | counts ->
          List.map counts ~f:(fun (msg_type, count) ->
              sprintf "%s=%d" msg_type count)
          |> String.concat ~sep:", "
    in
    String.concat ~sep:"\n"
      [
        "Kraken FIX probe";
        sprintf "connections: attempts=%d established=%d disconnected=%d"
          snapshot.connection_attempts snapshot.connections snapshot.disconnects;
        sprintf "messages: total=%d rate=%.2f/s types=[%s]" snapshot.messages
          snapshot.messages_per_second message_types;
        sprintf "sequence: poss_dup=%d non_monotonic=%d jumps=%d skipped=%d"
          snapshot.possible_duplicates snapshot.non_monotonic_sequences
          snapshot.sequence_jumps snapshot.skipped_sequence_numbers;
        latency_report "read -> decoded" snapshot.decode_latency;
        latency_report "read -> delivered" snapshot.delivery_latency;
        latency_report "interarrival" snapshot.interarrival;
      ]
end

let normalize_symbols symbols =
  let normalized = List.map symbols ~f:String.strip in
  match normalized with
  | [] -> Error `Invalid_symbols
  | _ -> (
      match
        List.for_all normalized ~f:(fun symbol ->
            match String.is_empty symbol with true -> false | false -> true)
      with
      | false -> Error `Invalid_symbols
      | true -> Ok normalized)

let validate_duration_seconds duration_seconds =
  match Float.is_finite duration_seconds && Float.(duration_seconds > 0.) with
  | true -> Ok ()
  | false -> Error (`Invalid_duration duration_seconds)

let duration_span duration_seconds =
  let open Result.Let_syntax in
  let%bind () = validate_duration_seconds duration_seconds in
  Or_error.try_with (fun () -> Time_ns.Span.of_sec duration_seconds)
  |> Result.map_error ~f:(fun _error -> `Invalid_duration duration_seconds)

let consume_until_deadline ~client ~events ~metrics ~request ~deadline =
  let rec loop () =
    let%bind next =
      Deferred.any
        [
          (Pipe.read events >>| fun event -> `Event event);
          (Clock_ns.at deadline >>| fun () -> `Deadline);
        ]
    in
    match next with
    | `Deadline -> return (Ok ())
    | `Event `Eof -> return (Ok ())
    | `Event (`Ok event) -> (
        Metrics.observe metrics event;
        match event with
        | Fix_session.Client.Timed_event.Connected -> (
            let%bind sent =
              Fix_session.Client.send client
                (Fix_session.Outbound.Market_data_request request)
            in
            match sent with
            | Error error -> return (Error (`Session error))
            | Ok () -> loop ())
        | Connecting | Disconnected _ | Message _ -> loop ())
  in
  loop ()

let run ~environment ~sender_comp_id ~symbols ~depth ~state_path
    ~duration_seconds ~sample_capacity ~checkpoint_every ~reset_on_start =
  let validated =
    let open Result.Let_syntax in
    let%bind duration = duration_span duration_seconds in
    let%bind symbols = normalize_symbols symbols in
    let%map metrics = Metrics.create ~sample_capacity in
    (duration, symbols, metrics)
  in
  match validated with
  | Error _ as error -> return error
  | Ok (duration, symbols, metrics) -> (
      let endpoint =
        Fix.Endpoint.create ~environment ~service:Spot_market_data_l2
      in
      let config =
        Fix_session.Config.create ~endpoint ~sender_comp_id
          ~authentication:Market_data ~state_path ~checkpoint_every
          ~reset_on_start ~capture_timing:true ()
        |> Result.map_error ~f:(fun error -> `Session error)
      in
      match config with
      | Error _ as error -> return error
      | Ok config -> (
          let%bind created = Fix_session.Client.create config in
          match created with
          | Error error -> return (Error (`Session error))
          | Ok client -> (
              let events = Fix_session.Client.timed_events client in
              match events with
              | Error error -> return (Error (`Session error))
              | Ok events -> (
                  let started_at = Time_ns.now () in
                  let deadline = Time_ns.add started_at duration in
                  let request =
                    Fix.Market_data.
                      {
                        request_id =
                          "fluxum-probe-"
                          ^ (started_at |> Time_ns.to_int63_ns_since_epoch
                           |> Int63.to_string);
                        action = Subscribe;
                        depth;
                        entries = [ Book ];
                        symbols;
                      }
                  in
                  let legacy_events_finished =
                    Pipe.iter_without_pushback
                      (Fix_session.Client.events client) ~f:(fun _event -> ())
                  in
                  let run_finished = Fix_session.Client.run client in
                  let%bind consumed =
                    consume_until_deadline ~client ~events ~metrics ~request
                      ~deadline
                  in
                  Fix_session.Client.stop client;
                  let%bind session = run_finished in
                  let%bind () = legacy_events_finished in
                  let elapsed = Time_ns.diff (Time_ns.now ()) started_at in
                  let snapshot = Metrics.snapshot metrics ~elapsed in
                  match (consumed, session) with
                  | (Error _ as error), _ -> return error
                  | Ok (), Error error -> return (Error (`Session error))
                  | Ok (), Ok () -> return (Ok snapshot)))))

let environment_arg =
  Command.Arg_type.of_alist_exn
    [ ("uat", Fix.Endpoint.Uat); ("production", Production) ]

let depth_arg =
  Command.Arg_type.of_alist_exn
    [
      ("full", Fix.Market_data.Full);
      ("top", Top);
      ("10", Levels_10);
      ("25", Levels_25);
      ("100", Levels_100);
      ("500", Levels_500);
      ("1000", Levels_1000);
    ]

let probe_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"Measure the read-only Kraken FIX L2 path"
    [%map_open
      let sender_comp_id =
        flag "--sender-comp-id" (required string)
          ~doc:"STRING Kraken-assigned FIX SenderCompID"
      and symbols =
        flag "--symbols"
          (required (Command.Arg_type.comma_separated string))
          ~doc:"SYMBOLS comma-separated Spot symbols, for example BTC/USD"
      and environment =
        flag "--environment"
          (optional_with_default Fix.Endpoint.Uat environment_arg)
          ~doc:"ENV uat or production (default uat)"
      and depth =
        flag "--depth"
          (optional_with_default Fix.Market_data.Levels_10 depth_arg)
          ~doc:"DEPTH full, top, 10, 25, 100, 500, or 1000 (default 10)"
      and duration_seconds =
        flag "--duration"
          (optional_with_default 60. float)
          ~doc:"SECONDS measurement duration (default 60)"
      and state_path =
        flag "--state-path" (required string)
          ~doc:"PATH dedicated durable FIX sequence state"
      and sample_capacity =
        flag "--sample-capacity"
          (optional_with_default 250_000 int)
          ~doc:"INT retained samples per latency metric (default 250000)"
      and checkpoint_every =
        flag "--checkpoint-every"
          (optional_with_default 1 int)
          ~doc:"INT sequence-state checkpoint interval (default 1)"
      and reset_on_start =
        flag "--reset-on-start" no_arg
          ~doc:"Coordinate a fresh sequence reset on the next Logon"
      in
      fun () ->
        match validate_duration_seconds duration_seconds with
        | Error error ->
            Deferred.return (Or_error.error_s (sexp_of_error error))
        | Ok () ->
            Deferred.map
              (run ~environment ~sender_comp_id ~symbols ~depth ~state_path
                 ~duration_seconds ~sample_capacity ~checkpoint_every
                 ~reset_on_start) ~f:(fun result ->
                Result.map result ~f:(fun snapshot ->
                    print_endline (Metrics.report snapshot))
                |> Result.map_error ~f:(fun error ->
                    Error.create_s (sexp_of_error error)))]

let command =
  ( "fix",
    Command.group ~summary:"Kraken Unified FIX commands"
      [ ("probe", probe_command) ] )

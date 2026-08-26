open Core
open Async
module Fix = Kraken.Fix
module Session = Kraken.Fix_session

let or_error_exn = function
  | Ok value -> value
  | Error error -> Error.raise error

let session_exn = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (Session.sexp_of_error error))

let state_path () =
  let pid = Unix.getpid () |> Pid.to_int in
  Filename.concat "/tmp" [%string "fluxum-fix-session-%{pid#Int}.sexp"]

let remove_if_present path =
  let%bind exists = Sys.file_exists path in
  match exists with `Yes -> Unix.unlink path | `No | `Unknown -> return ()

let test_state_store path =
  let state =
    Session.Sequence_state.create ~next_outgoing:41 ~next_incoming:92 ()
  in
  let%bind () = Session.State_store.save path state >>| or_error_exn in
  let%map loaded = Session.State_store.load path >>| or_error_exn in
  assert (Session.Sequence_state.equal state loaded)

type server_connection = {
  reader : Reader.t;
  writer : Writer.t;
  framer : Fix.Codec.Framer.t;
  ready : Fix.Codec.Frame.t Queue.t;
}

let in_memory_connection () =
  let client_input, server_output = Pipe.create () in
  let server_input, client_output = Pipe.create () in
  let%bind client_reader =
    Reader.of_pipe (Info.of_string "client input") client_input
  in
  let%bind client_writer, _ =
    Writer.of_pipe (Info.of_string "client output") client_output
  in
  let%bind server_reader =
    Reader.of_pipe (Info.of_string "server input") server_input
  in
  let%map server_writer, _ =
    Writer.of_pipe (Info.of_string "server output") server_output
  in
  let closed =
    Deferred.any_unit
      [
        Reader.close_finished client_reader; Writer.close_finished client_writer;
      ]
  in
  let close () =
    let%bind () = Writer.close client_writer in
    Reader.close client_reader
  in
  ( Session.Client.For_testing.
      { reader = client_reader; writer = client_writer; closed; close },
    {
      reader = server_reader;
      writer = server_writer;
      framer = Fix.Codec.Framer.create ();
      ready = Queue.create ();
    } )

let read_frame server =
  let buffer = Bytes.create 4096 in
  let rec loop () =
    match Queue.dequeue server.ready with
    | Some frame -> return frame
    | None -> (
        let%bind result = Reader.read server.reader buffer in
        match result with
        | `Eof -> failwith "unexpected EOF from client"
        | `Ok length -> (
            let bytes = Bytes.To_string.sub buffer ~pos:0 ~len:length in
            match Fix.Codec.Framer.feed server.framer bytes with
            | Error error ->
                failwith (Sexp.to_string_hum (Fix.Codec.sexp_of_error error))
            | Ok frames ->
                Queue.enqueue_all server.ready frames;
                loop ()))
  in
  loop ()

let server_message ?(sender_comp_id = "KRAKEN-MD") ~sequence ~msg_type
    ~body_fields () =
  Fix.Codec.Encoder.message ~sender_comp_id ~target_comp_id:"CLIENT"
    ~msg_type ~msg_seq_num:sequence ~sending_time:"20260825-12:34:56.123"
    ~body_fields
  |> Result.map_error ~f:(fun error -> (error :> Fix.error))
  |> Result.map_error ~f:(fun error -> Error.create_s (Fix.sexp_of_error error))
  |> or_error_exn

let wait_connected events =
  let rec loop () =
    let%bind result = Pipe.read events in
    match result with
    | `Eof -> failwith "session events closed before Logon"
    | `Ok Session.Client.Connected -> return ()
    | `Ok _ -> loop ()
  in
  loop ()

let wait_disconnected events =
  let rec loop () =
    let%bind result = Pipe.read events in
    match result with
    | `Eof -> failwith "session events closed before disconnect"
    | `Ok (Session.Client.Disconnected error) -> return error
    | `Ok _ -> loop ()
  in
  loop ()

let rec take_message_sequences events remaining sequences =
  match remaining with
  | 0 -> return (List.rev sequences)
  | _ -> (
      let%bind result = Pipe.read events in
      match result with
      | `Eof -> failwith "session events closed before expected messages"
      | `Ok (Session.Client.Message frame) ->
          take_message_sequences events (remaining - 1)
            (Fix.Codec.Frame.sequence_number frame :: sequences)
      | `Ok _ -> take_message_sequences events remaining sequences)

let run_client client =
  let finished = Ivar.create () in
  don't_wait_for
    (let%map result = Session.Client.run client in
     Ivar.fill_if_empty finished result);
  finished

let test_reconnect path =
  let endpoint =
    Fix.Endpoint.create ~environment:Uat ~service:Spot_market_data_l2
  in
  let config =
    Session.Config.create ~endpoint ~sender_comp_id:"CLIENT"
      ~authentication:Market_data ~state_path:path
      ~reconnect_delay:(Time_ns.Span.of_ms 1.) ~checkpoint_every:1 ()
    |> session_exn
  in
  let connection_number = ref 0 in
  let first_request = Ivar.create () in
  let test_response = Ivar.create () in
  let connector ~stop:_ _endpoint =
    Int.incr connection_number;
    let number = !connection_number in
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind logon = read_frame server in
       let expected = match number with 1 -> 1 | _ -> 3 in
       assert (Fix.Codec.Frame.sequence_number logon = expected);
       assert (String.equal (Fix.Codec.Frame.msg_type logon) "A");
       Writer.write server.writer
         (server_message ~sequence:number ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       let%bind () = Writer.flushed server.writer in
       match number with
       | 1 ->
           let%bind request = read_frame server in
           assert (Fix.Codec.Frame.sequence_number request = 2);
           assert (String.equal (Fix.Codec.Frame.msg_type request) "V");
           Ivar.fill_if_empty first_request ();
           Writer.close server.writer
       | _ ->
           Writer.write server.writer
             (server_message ~sequence:3 ~msg_type:"1"
                ~body_fields:[ (112, "health-check") ] ());
           let%bind () = Writer.flushed server.writer in
           let%map heartbeat = read_frame server in
           assert (Fix.Codec.Frame.sequence_number heartbeat = 4);
           assert (String.equal (Fix.Codec.Frame.msg_type heartbeat) "0");
           (match Fix.Codec.Frame.value heartbeat 112 with
           | Some "health-check" -> ()
           | _ ->
               failwith
                 [%string
                   "missing TestReqID echo: %{Fix.Codec.Frame.raw heartbeat}"]);
           Ivar.fill_if_empty test_response ());
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let events = Session.Client.events client in
  let%bind () = wait_connected events in
  let request =
    Fix.Market_data.
      {
        request_id = "BTC-BOOK";
        action = Subscribe;
        depth = Levels_10;
        entries = [ Book ];
        symbols = [ "BTC/USD" ];
      }
  in
  let%bind () =
    Session.Client.send client (Market_data_request request) >>| session_exn
  in
  let%bind () = Ivar.read first_request in
  let%bind () = wait_connected events in
  assert (!connection_number = 2);
  let%bind () = Ivar.read test_response in
  Session.Client.stop client;
  let%bind run_result = Ivar.read run_finished in
  session_exn run_result;
  let%map persisted = Session.State_store.load path >>| or_error_exn in
  assert (Session.Sequence_state.next_outgoing persisted = 5);
  assert (Session.Sequence_state.next_incoming persisted = 4)

let market_data_config path ?(event_capacity = 4_096)
    ?(gap_buffer_capacity = 4_096) ?(journal_capacity = 65_536)
    ?(logon_timeout_ms = 500.) ?(liveness_timeout_ms = 180_000.) () =
  let endpoint =
    Fix.Endpoint.create ~environment:Uat ~service:Spot_market_data_l2
  in
  Session.Config.create ~endpoint ~sender_comp_id:"CLIENT"
    ~authentication:Market_data ~state_path:path ~checkpoint_every:1
    ~reconnect_delay:(Time_ns.Span.of_ms 1.) ~event_capacity
    ~gap_buffer_capacity ~journal_capacity
    ~logon_timeout:(Time_ns.Span.of_ms logon_timeout_ms)
    ~liveness_timeout:(Time_ns.Span.of_ms liveness_timeout_ms)
    ()
  |> session_exn

let test_gap_buffer path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       Writer.write server.writer
         (server_message ~sequence:3 ~msg_type:"0" ~body_fields:[] ());
       let%bind () = Writer.flushed server.writer in
       let%bind resend = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type resend) "2");
       assert (
         Option.equal String.equal (Fix.Codec.Frame.value resend 7) (Some "2"));
       assert (
         Option.equal String.equal (Fix.Codec.Frame.value resend 16) (Some "2"));
       Writer.write server.writer
         (server_message ~sequence:2 ~msg_type:"0" ~body_fields:[] ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let sequences = take_message_sequences (Session.Client.events client) 3 [] in
  let%bind sequences = sequences in
  assert (List.equal Int.equal sequences [ 1; 2; 3 ]);
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let test_sequence_reset_discards_buffered path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path ~gap_buffer_capacity:1 () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       Writer.write server.writer
         (server_message ~sequence:3 ~msg_type:"0" ~body_fields:[] ());
       let%bind () = Writer.flushed server.writer in
       let%bind first_resend = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type first_resend) "2");
       Writer.write server.writer
         (server_message ~sequence:2 ~msg_type:"4"
            ~body_fields:[ (123, "Y"); (36, "4") ] ());
       Writer.write server.writer
         (server_message ~sequence:5 ~msg_type:"0" ~body_fields:[] ());
       let%bind () = Writer.flushed server.writer in
       let%bind second_resend = read_frame server in
       assert (
         Option.equal String.equal
           (Fix.Codec.Frame.value second_resend 7)
           (Some "4"));
       assert (
         Option.equal String.equal
           (Fix.Codec.Frame.value second_resend 16)
           (Some "4"));
       Writer.write server.writer
         (server_message ~sequence:4 ~msg_type:"0" ~body_fields:[] ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind sequences =
    take_message_sequences (Session.Client.events client) 4 []
  in
  assert (List.equal Int.equal sequences [ 1; 2; 4; 5 ]);
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let test_outbound_replay path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path () in
  let replayed = Ivar.create () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       let%bind () = Writer.flushed server.writer in
       let%bind original_request = read_frame server in
       assert (Fix.Codec.Frame.sequence_number original_request = 2);
       Writer.write server.writer
         (server_message ~sequence:2 ~msg_type:"2"
            ~body_fields:[ (7, "1"); (16, "2") ] ());
       let%bind () = Writer.flushed server.writer in
       let%bind gap_fill = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type gap_fill) "4");
       assert (Fix.Codec.Frame.sequence_number gap_fill = 1);
       assert (
         Option.equal String.equal
           (Fix.Codec.Frame.value gap_fill 36)
           (Some "2"));
       assert (
         Option.equal String.equal
           (Fix.Codec.Frame.value gap_fill 43)
           (Some "Y"));
       let%map request = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type request) "V");
       assert (Fix.Codec.Frame.sequence_number request = 2);
       assert (
         Option.equal String.equal (Fix.Codec.Frame.value request 43) (Some "Y"));
       assert (Option.is_some (Fix.Codec.Frame.value request 122));
       Ivar.fill_if_empty replayed ());
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind () = wait_connected (Session.Client.events client) in
  let request =
    Fix.Market_data.
      {
        request_id = "REPLAY-BOOK";
        action = Subscribe;
        depth = Top;
        entries = [ Book ];
        symbols = [ "BTC/USD" ];
      }
  in
  let%bind () =
    Session.Client.send client (Market_data_request request) >>| session_exn
  in
  let%bind () = Ivar.read replayed in
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let test_journal_eviction path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path ~journal_capacity:1 () in
  let request_received = Ivar.create () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       let%bind () = Writer.flushed server.writer in
       let%bind request = read_frame server in
       assert (Fix.Codec.Frame.sequence_number request = 2);
       Ivar.fill_if_empty request_received ();
       Writer.write server.writer
         (server_message ~sequence:2 ~msg_type:"2"
            ~body_fields:[ (7, "1"); (16, "2") ] ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind () = wait_connected (Session.Client.events client) in
  let request =
    Fix.Market_data.
      {
        request_id = "EVICT-BOOK";
        action = Subscribe;
        depth = Top;
        entries = [ Book ];
        symbols = [ "BTC/USD" ];
      }
  in
  let%bind () =
    Session.Client.send client (Market_data_request request) >>| session_exn
  in
  let%bind () = Ivar.read request_received in
  let%map result = Ivar.read run_finished in
  match result with
  | Error (`Replay_unavailable (1, 2)) -> ()
  | _ -> failwith "evicted replay history did not fail closed"

let test_logon_timeout path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path ~logon_timeout_ms:5. () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for (read_frame server >>| ignore);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind disconnected = wait_disconnected (Session.Client.events client) in
  assert (
    String.is_substring
      (Error.to_string_hum disconnected)
      ~substring:"Logon_timeout");
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let test_event_bound path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path ~event_capacity:1 () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let%map result = Session.Client.run client in
  match result with
  | Error (`Event_queue_full 1) -> ()
  | _ -> failwith "expected the bounded event queue to fail closed"

let test_corrupt_state path =
  let%bind () = Writer.save path ~contents:"not a sequence-state sexp" in
  let%map loaded = Session.State_store.load path in
  match loaded with
  | Error _ -> ()
  | Ok _ -> failwith "corrupt sequence state was accepted"

let test_tls_defaults () =
  let endpoint =
    Fix.Endpoint.create ~environment:Uat ~service:Spot_market_data_l2
  in
  let config = Session.Client.For_testing.tls_config endpoint in
  assert (
    Option.equal String.equal
      (Async_ssl.Config.Client.remote_hostname config)
      (Some "fix.uat.kraken.com"));
  assert (
    List.exists (Async_ssl.Config.Client.verify_modes config) ~f:(function
      | Async_ssl.Ssl.Verify_mode.Verify_peer -> true
      | Verify_none | Verify_fail_if_no_peer_cert | Verify_client_once -> false))

let test_replay_unavailable path ~begin_sequence_number ~end_sequence_number
    ~expected =
  let state =
    Session.Sequence_state.create ~next_outgoing:3 ~next_incoming:1 ()
  in
  let%bind () = Session.State_store.save path state >>| or_error_exn in
  let config = market_data_config path () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind logon = read_frame server in
       assert (Fix.Codec.Frame.sequence_number logon = 3);
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       Writer.write server.writer
         (server_message ~sequence:2 ~msg_type:"2"
            ~body_fields:
              [
                (7, Int.to_string begin_sequence_number);
                (16, Int.to_string end_sequence_number);
              ]
            ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let%map result = Session.Client.run client in
  match result with
  | Error (`Replay_unavailable (actual_begin, actual_end)) ->
      let expected_begin, expected_end = expected in
      assert (actual_begin = expected_begin);
      assert (actual_end = expected_end)
  | _ -> failwith "missing replay history did not fail closed"

let test_sent_but_not_checkpointed () =
  let pid = Unix.getpid () |> Pid.to_int in
  let directory =
    Filename.concat "/tmp" [%string "fluxum-fix-state-failure-%{pid#Int}"]
  in
  let path = Filename.concat directory "state.sexp" in
  let%bind () = Unix.mkdir ~p:() ~perm:0o700 directory in
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let config = market_data_config path () in
  let request_received = Ivar.create () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       let%bind () = Writer.flushed server.writer in
       let%map request = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type request) "V");
       Ivar.fill_if_empty request_received ());
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind () = wait_connected (Session.Client.events client) in
  let%bind () = Unix.unlink path in
  let%bind () = Unix.rmdir directory in
  let%bind () = Writer.save directory ~contents:"blocks the state directory" in
  let request =
    Fix.Market_data.
      { request_id = "AMBIGUOUS-BOOK"
      ; action = Subscribe
      ; depth = Top
      ; entries = [ Book ]
      ; symbols = [ "BTC/USD" ]
      }
  in
  let%bind sent = Session.Client.send client (Market_data_request request) in
  (match sent with
  | Error (`Sent_but_not_checkpointed (2, _)) -> ()
  | _ -> failwith "post-flush checkpoint failure was not explicit");
  let%bind second_send =
    Session.Client.send client (Market_data_request request)
  in
  (match second_send with
  | Error (`Sent_but_not_checkpointed (2, _))
  | Error `Not_connected
  | Error `Stopped -> ()
  | _ -> failwith "send was accepted after a terminal checkpoint failure");
  let%bind () = Ivar.read request_received in
  let%bind run_result = Ivar.read run_finished in
  (match run_result with
  | Error (`Sent_but_not_checkpointed (2, _)) -> ()
  | _ -> failwith "checkpoint ambiguity did not terminate the session");
  assert (Session.Sequence_state.next_outgoing (Session.Client.state client) = 3);
  Unix.unlink directory

let decimal value =
  match Fix.Decimal.of_string value with
  | Ok decimal -> decimal
  | Error error -> failwith error

let test_trading_session path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let credentials =
    Fix.Credentials.create ~api_key:"APIKEY" ~api_secret_base64:"c2VjcmV0"
    |> Result.map_error ~f:(fun error -> `Fix error)
    |> session_exn
  in
  let endpoint =
    Fix.Endpoint.create ~environment:Uat ~service:Spot_trading
  in
  let config =
    Session.Config.create ~endpoint ~sender_comp_id:"CLIENT"
      ~authentication:
        (Trading
           { credentials
           ; cancel_on_disconnect = Fix.Session.Cancel
           ; client_id = None
           })
      ~state_path:path ~checkpoint_every:1 ()
    |> session_exn
  in
  let order_received = Ivar.create () in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind logon = read_frame server in
       assert (Option.equal String.equal (Fix.Codec.Frame.value logon 553) (Some "APIKEY"));
       assert (Option.is_some (Fix.Codec.Frame.value logon 554));
       assert (Option.is_some (Fix.Codec.Frame.value logon 5025));
       Writer.write server.writer
         (server_message ~sender_comp_id:"KRAKEN-TRD" ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ] ());
       let%bind () = Writer.flushed server.writer in
       let%map order = read_frame server in
       assert (String.equal (Fix.Codec.Frame.msg_type order) "D");
       Ivar.fill_if_empty order_received ());
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind () = wait_connected (Session.Client.events client) in
  let client_order_id = Fix.Client_order_id.create "1744036325000001" in
  let client_order_id =
    match client_order_id with
    | Ok id -> id
    | Error error ->
        failwith (Sexp.to_string_hum (Fix.sexp_of_error error))
  in
  let order =
    Fix.Order.
      { client_order_id
      ; kind = Limit { price = decimal "84000.00"; post_only = true }
      ; quantity = decimal "0.00100000"
      ; side = Buy
      ; symbol = "BTC/USD"
      ; time_in_force = Gtc
      ; self_trade_prevention = Some Cancel_newest
      }
  in
  let%bind () = Session.Client.send client (New_order order) >>| session_exn in
  let%bind () = Ivar.read order_received in
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let test_liveness_timeout path =
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let endpoint =
    Fix.Endpoint.create ~environment:Uat ~service:Spot_market_data_l2
  in
  let config =
    Session.Config.create ~endpoint ~sender_comp_id:"CLIENT"
      ~authentication:Market_data ~state_path:path ~heartbeat_interval:1
      ~liveness_timeout:(Time_ns.Span.of_ms 10.)
      ~reconnect_delay:(Time_ns.Span.of_ms 1.) ()
    |> session_exn
  in
  let connector ~stop:_ _endpoint =
    let%map client, server = in_memory_connection () in
    don't_wait_for
      (let%bind _logon = read_frame server in
       Writer.write server.writer
         (server_message ~sequence:1 ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "1") ] ());
       Writer.flushed server.writer);
    Ok client
  in
  let%bind client =
    Session.Client.For_testing.create config ~connector >>| session_exn
  in
  let run_finished = run_client client in
  let%bind disconnected = wait_disconnected (Session.Client.events client) in
  assert (
    String.is_substring (Error.to_string_hum disconnected)
      ~substring:"Liveness_timeout");
  Session.Client.stop client;
  let%map result = Ivar.read run_finished in
  session_exn result

let run () =
  let path = state_path () in
  let%bind () = remove_if_present path in
  let%bind () = test_state_store path in
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let%bind () = test_reconnect path in
  let%bind () = test_gap_buffer path in
  let%bind () = test_sequence_reset_discards_buffered path in
  let%bind () = test_outbound_replay path in
  let%bind () = test_journal_eviction path in
  let%bind () =
    test_replay_unavailable path ~begin_sequence_number:1
      ~end_sequence_number:2 ~expected:(1, 2)
  in
  let%bind () =
    test_replay_unavailable path ~begin_sequence_number:5
      ~end_sequence_number:0 ~expected:(5, 5)
  in
  let%bind () = test_logon_timeout path in
  let%bind () = test_liveness_timeout path in
  let%bind () = test_event_bound path in
  let%bind () = test_sent_but_not_checkpointed () in
  let%bind () = test_trading_session path in
  let%bind () = test_corrupt_state path in
  test_tls_defaults ();
  let%bind () = remove_if_present path in
  print_endline "Kraken FIX session tests passed";
  Shutdown.exit 0

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())

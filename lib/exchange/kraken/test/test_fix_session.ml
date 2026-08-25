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

type server_connection = { reader : Reader.t; writer : Writer.t }

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
    { reader = server_reader; writer = server_writer } )

let read_frame reader =
  let framer = Fix.Codec.Framer.create () in
  let buffer = Bytes.create 4096 in
  let rec loop () =
    let%bind result = Reader.read reader buffer in
    match result with
    | `Eof -> failwith "unexpected EOF from client"
    | `Ok length -> (
        let bytes = Stdlib.Bytes.sub_string buffer 0 length in
        match Fix.Codec.Framer.feed framer bytes with
        | Error error ->
            failwith (Sexp.to_string_hum (Fix.Codec.sexp_of_error error))
        | Ok [] -> loop ()
        | Ok (frame :: _) -> return frame)
  in
  loop ()

let server_message ~sequence ~msg_type ~body_fields =
  Fix.Codec.Encoder.message ~sender_comp_id:"KRAKEN-MD" ~target_comp_id:"CLIENT"
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
      (let%bind logon = read_frame server.reader in
       let expected = match number with 1 -> 1 | _ -> 3 in
       assert (Fix.Codec.Frame.sequence_number logon = expected);
       assert (String.equal (Fix.Codec.Frame.msg_type logon) "A");
       Writer.write server.writer
         (server_message ~sequence:number ~msg_type:"A"
            ~body_fields:[ (98, "0"); (108, "60") ]);
       let%bind () = Writer.flushed server.writer in
       match number with
       | 1 ->
           let%bind request = read_frame server.reader in
           assert (Fix.Codec.Frame.sequence_number request = 2);
           assert (String.equal (Fix.Codec.Frame.msg_type request) "V");
           Ivar.fill_if_empty first_request ();
           Writer.close server.writer
       | _ ->
           Writer.write server.writer
             (server_message ~sequence:3 ~msg_type:"1"
                ~body_fields:[ (112, "health-check") ]);
           let%bind () = Writer.flushed server.writer in
           let%map heartbeat = read_frame server.reader in
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
  let run_finished = Ivar.create () in
  don't_wait_for
    (let%map result = Session.Client.run client in
     Ivar.fill_if_empty run_finished result);
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

let run () =
  let path = state_path () in
  let%bind () = remove_if_present path in
  let%bind () = test_state_store path in
  let%bind () = Session.State_store.reset path >>| or_error_exn in
  let%bind () = test_reconnect path in
  let%bind () = remove_if_present path in
  print_endline "Kraken FIX session tests passed";
  Shutdown.exit 0

let () =
  don't_wait_for (run ());
  never_returns (Scheduler.go ())

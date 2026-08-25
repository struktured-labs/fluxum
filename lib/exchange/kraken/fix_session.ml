open Core
open Async
module Fix = Fix

type error =
  [ `Already_running
  | `Fix of Fix.error
  | `Io of Error.t
  | `Not_connected
  | `Not_logged_on
  | `Sequence of Fix.Codec.Sequence.sequence_error
  | `State of Error.t
  | `Stopped
  | `Wrong_session_identity of string
  | `Wrong_session_type of string ]
[@@deriving sexp_of]

module Sequence_state = struct
  type t = { next_outgoing : int; next_incoming : int } [@@deriving sexp, equal]

  let create ?(next_outgoing = 1) ?(next_incoming = 1) () =
    { next_outgoing; next_incoming }

  let next_outgoing t = t.next_outgoing
  let next_incoming t = t.next_incoming

  let validate t =
    match t.next_outgoing > 0 && t.next_incoming > 0 with
    | true -> Ok t
    | false ->
        Or_error.error_s
          [%message
            "FIX sequence numbers must be positive"
              (t.next_outgoing : int)
              (t.next_incoming : int)]
end

module State_store = struct
  let load path =
    Monitor.try_with_or_error (fun () ->
        let%bind exists = Sys.file_exists path in
        match exists with
        | `No -> return (Sequence_state.create ())
        | `Yes ->
            let%map contents = Reader.file_contents path in
            Sexp.of_string contents |> Sequence_state.t_of_sexp
            |> Sequence_state.validate |> Or_error.ok_exn
        | `Unknown ->
            raise_s [%message "unable to inspect FIX sequence state" path])

  let temporary_path path =
    let pid = Unix.getpid () |> Pid.to_int in
    let timestamp =
      Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string
    in
    [%string "%{path}.tmp.%{pid#Int}.%{timestamp}"]

  let save path state =
    match Sequence_state.validate state with
    | Error error -> return (Error error)
    | Ok state -> (
        let temporary = temporary_path path in
        let%bind saved =
          Monitor.try_with_or_error (fun () ->
              let directory = Filename.dirname path in
              let%bind () = Unix.mkdir ~p:() ~perm:0o700 directory in
              let%bind () =
                Writer.with_file ~perm:0o600 temporary ~f:(fun writer ->
                    Writer.write_line writer
                      (Sexp.to_string_mach (Sequence_state.sexp_of_t state));
                    Writer.fsync writer)
              in
              Unix.rename ~src:temporary ~dst:path)
        in
        match saved with
        | Ok () -> return (Ok ())
        | Error _ as error ->
            let%map _ = Monitor.try_with (fun () -> Unix.unlink temporary) in
            error)

  let reset path = save path (Sequence_state.create ())
end

type authentication =
  | Market_data
  | Trading of {
      credentials : Fix.Credentials.t;
      cancel_on_disconnect : Fix.Session.cancel_on_disconnect;
      client_id : int option;
    }

module Config = struct
  type t = {
    endpoint : Fix.Endpoint.t;
    sender_comp_id : string;
    authentication : authentication;
    state_path : string;
    heartbeat_interval : int;
    reconnect_delay : Time_ns.Span.t;
    connect_timeout : Time_ns.Span.t;
    max_frame_length : int;
    checkpoint_every : int;
    reset_on_start : bool;
  }

  let create ~endpoint ~sender_comp_id ~authentication ~state_path
      ?(heartbeat_interval = 60) ?(reconnect_delay = Time_ns.Span.of_sec 1.)
      ?(connect_timeout = Time_ns.Span.of_sec 10.)
      ?(max_frame_length = 1024 * 1024) ?(checkpoint_every = 1)
      ?(reset_on_start = false) () =
    let open Result.Let_syntax in
    let%bind () =
      Fix.Header.create ~sender_comp_id ~msg_seq_num:1
        ~sending_time:"19700101-00:00:00.000"
      |> Result.map ~f:ignore
      |> Result.map_error ~f:(fun error -> `Fix error)
    in
    let%bind () =
      match String.is_empty state_path with
      | true -> Error (`State (Error.of_string "state_path must be non-empty"))
      | false -> Ok ()
    in
    let%bind () =
      match (Fix.Endpoint.service endpoint, authentication) with
      | Fix.Endpoint.Spot_trading, Trading _ -> Ok ()
      | (Fix.Endpoint.Spot_market_data_l2 | Spot_market_data_l3), Market_data ->
          Ok ()
      | Spot_trading, Market_data ->
          Error (`Wrong_session_type "trading endpoint requires credentials")
      | (Spot_market_data_l2 | Spot_market_data_l3), Trading _ ->
          Error
            (`Wrong_session_type "market-data endpoint rejects trading logon")
    in
    let%bind () =
      match heartbeat_interval > 0 with
      | true -> Ok ()
      | false -> Error (`Fix (`Invalid_heartbeat_interval heartbeat_interval))
    in
    let%bind () =
      match
        Time_ns.Span.(reconnect_delay >= zero)
        && Time_ns.Span.(connect_timeout > zero)
        && max_frame_length > 0 && checkpoint_every > 0
      with
      | true -> Ok ()
      | false -> Error (`State (Error.of_string "invalid FIX session limits"))
    in
    Ok
      {
        endpoint;
        sender_comp_id;
        authentication;
        state_path;
        heartbeat_interval;
        reconnect_delay;
        connect_timeout;
        max_frame_length;
        checkpoint_every;
        reset_on_start;
      }

  let endpoint t = t.endpoint
  let sender_comp_id t = t.sender_comp_id
  let state_path t = t.state_path
end

module Outbound = struct
  type t =
    | Market_data_request of Fix.Market_data.request
    | New_order of Fix.Order.new_order
    | Cancel_order of Fix.Order.cancel_order
    | Heartbeat of string option
    | Test_request of string
    | Resend_request of {
        begin_sequence_number : int;
        end_sequence_number : int;
      }
    | Logout of string option
  [@@deriving sexp_of]
end

module Client = struct
  type connection = {
    reader : Reader.t;
    writer : Writer.t;
    closed : unit Deferred.t;
    close : unit -> unit Deferred.t;
  }

  type live = { connection : connection; logged_on : unit Ivar.t }

  type event =
    | Connecting
    | Connected
    | Disconnected of Error.t
    | Message of Fix.Codec.Frame.t

  type connector =
    stop:unit Deferred.t ->
    Fix.Endpoint.t ->
    (connection, Error.t) Deferred.Result.t

  type t = {
    config : Config.t;
    connector : connector;
    mutable sequence_state : Sequence_state.t;
    sequencer : unit Throttle.Sequencer.t;
    mutable messages_since_checkpoint : int;
    mutable live : live option;
    mutable running : bool;
    mutable reset_logon : bool;
    stop : unit Ivar.t;
    events_reader : event Pipe.Reader.t;
    events_writer : event Pipe.Writer.t;
  }

  let error_to_error error = Error.create_s (sexp_of_error error)

  let publish t event =
    Pipe.write_without_pushback_if_open t.events_writer event;
    return ()

  let create_with_connector config ~connector =
    let%bind state =
      (match config.Config.reset_on_start with
        | false -> State_store.load config.state_path
        | true ->
            let%map result = State_store.reset config.state_path in
            Result.map result ~f:(fun () -> Sequence_state.create ()))
      |> Deferred.map ~f:(Result.map_error ~f:(fun error -> `State error))
    in
    match state with
    | Error _ as error -> return error
    | Ok state ->
        let events_reader, events_writer = Pipe.create () in
        return
          (Ok
             {
               config;
               connector;
               sequence_state = state;
               sequencer = Throttle.Sequencer.create ~continue_on_error:true ();
               messages_since_checkpoint = 0;
               live = None;
               running = false;
               reset_logon = config.reset_on_start;
               stop = Ivar.create ();
               events_reader;
               events_writer;
             })

  let tls_connector ~connect_timeout ~stop endpoint =
    let hostname = Fix.Endpoint.hostname endpoint in
    let port = Fix.Endpoint.port endpoint in
    let verify_callback connection =
      Async_ssl.Ssl.Connection.check_peer_certificate_host connection hostname
      |> return
    in
    let tls_config =
      Async_ssl.Config.Client.create ~remote_hostname:(Some hostname)
        ~ca_file:None ~ca_path:None ~verify_callback ()
    in
    let where_to_connect =
      Host_and_port.create ~host:hostname ~port
      |> Tcp.Where_to_connect.of_host_and_port
    in
    Monitor.try_with_or_error (fun () ->
        let%map _socket, tls, reader, writer =
          Async_ssl.Tls.Expert.connect ~interrupt:stop ~timeout:connect_timeout
            tls_config where_to_connect
        in
        let closed =
          Deferred.any_unit
            [
              Reader.close_finished reader;
              Writer.close_finished writer;
              Async_ssl.Ssl.Connection.closed tls |> Deferred.ignore_m;
            ]
        in
        let close () =
          Async_ssl.Ssl.Connection.close tls;
          Monitor.protect
            (fun () -> Writer.close writer)
            ~finally:(fun () -> Reader.close reader)
        in
        { reader; writer; closed; close })

  let create config =
    create_with_connector config
      ~connector:(tls_connector ~connect_timeout:config.Config.connect_timeout)

  let events t = t.events_reader
  let state t = t.sequence_state

  let session_target t =
    match t.config.authentication with
    | Market_data -> Fix.Session.Market_data
    | Trading _ -> Fix.Session.Trading

  let fix_timestamp now =
    let date, ofday =
      Time_ns_unix.to_date_ofday now ~zone:Time_ns_unix.Zone.utc
    in
    let date = Date.to_string date |> String.filter ~f:Char.is_digit in
    let ofday = Time_ns.Ofday.to_string ofday in
    let whole, fraction =
      match String.lsplit2 ofday ~on:'.' with
      | Some parts -> parts
      | None -> (ofday, "")
    in
    let milliseconds = String.prefix (fraction ^ "000") 3 in
    [%string "%{date}-%{whole}.%{milliseconds}"]

  let nonce now =
    Time_ns.to_int63_ns_since_epoch now |> Int63.to_int64 |> fun nanoseconds ->
    Int64.(nanoseconds / 1_000_000L)

  let checkpoint_unlocked t =
    let%map result = State_store.save t.config.state_path t.sequence_state in
    match result with
    | Error error -> Error (`State error)
    | Ok () ->
        t.messages_since_checkpoint <- 0;
        Ok ()

  let checkpoint t =
    Throttle.enqueue t.sequencer (fun () -> checkpoint_unlocked t)

  let maybe_checkpoint_unlocked t =
    match t.messages_since_checkpoint >= t.config.checkpoint_every with
    | true -> checkpoint_unlocked t
    | false -> return (Ok ())

  let encode_outbound t ~header outbound =
    let target = session_target t in
    let fix result = Result.map_error result ~f:(fun error -> `Fix error) in
    match (outbound, t.config.authentication) with
    | Outbound.Market_data_request request, Market_data ->
        fix (Fix.Market_data.request ~header request)
    | New_order order, Trading _ -> fix (Fix.Order.new_single ~header order)
    | Cancel_order order, Trading _ ->
        fix (Fix.Order.cancel_single ~header order)
    | Heartbeat test_request_id, _ ->
        fix (Fix.Session.heartbeat ~header ~target ?test_request_id ())
    | Test_request test_request_id, _ ->
        fix (Fix.Session.test_request ~header ~target ~test_request_id)
    | Resend_request { begin_sequence_number; end_sequence_number }, _ ->
        fix
          (Fix.Session.resend_request ~header ~target ~begin_sequence_number
             ~end_sequence_number)
    | Logout text, _ -> fix (Fix.Session.logout ~header ~target ?text ())
    | Market_data_request _, Trading _ ->
        Error (`Wrong_session_type "market-data request on trading session")
    | (New_order _ | Cancel_order _), Market_data ->
        Error (`Wrong_session_type "order request on market-data session")

  let write_encoded t live ~encode =
    Throttle.enqueue t.sequencer (fun () ->
        match t.live with
        | None -> return (Error `Not_connected)
        | Some current when not (phys_equal current live) ->
            return (Error `Not_connected)
        | Some _ -> (
            let sequence_number = t.sequence_state.next_outgoing in
            let now = Time_ns.now () in
            let result =
              let open Result.Let_syntax in
              let%bind header =
                Fix.Header.create ~sender_comp_id:t.config.sender_comp_id
                  ~msg_seq_num:sequence_number ~sending_time:(fix_timestamp now)
                |> Result.map_error ~f:(fun error -> `Fix error)
              in
              encode ~now ~header
            in
            match result with
            | Error _ as error -> return error
            | Ok wire -> (
                let%bind written =
                  Monitor.try_with_or_error (fun () ->
                      Writer.write live.connection.writer wire;
                      Writer.flushed live.connection.writer)
                in
                match written with
                | Error error -> return (Error (`Io error))
                | Ok () ->
                    t.sequence_state <-
                      {
                        t.sequence_state with
                        next_outgoing = sequence_number + 1;
                      };
                    t.messages_since_checkpoint <-
                      t.messages_since_checkpoint + 1;
                    maybe_checkpoint_unlocked t)))

  let send_internal t live outbound =
    write_encoded t live ~encode:(fun ~now:_ ~header ->
        encode_outbound t ~header outbound)

  let send_logon t live =
    let reset_sequence_numbers = t.reset_logon in
    let%map result =
      write_encoded t live ~encode:(fun ~now ~header ->
          match t.config.authentication with
          | Market_data ->
              Fix.Session.market_data_logon ~header
                ~heartbeat_interval:t.config.heartbeat_interval
                ~reset_sequence_numbers
              |> Result.map_error ~f:(fun error -> `Fix error)
          | Trading { credentials; cancel_on_disconnect; client_id } ->
              Fix.Session.trading_logon ~header ~credentials ~nonce:(nonce now)
                ~heartbeat_interval:t.config.heartbeat_interval
                ~reset_sequence_numbers ~cancel_on_disconnect ?client_id ()
              |> Result.map_error ~f:(fun error -> `Fix error))
    in
    Result.iter result ~f:(fun () -> t.reset_logon <- false);
    result

  let update_incoming_sequence t frame =
    let sequence =
      Fix.Codec.Sequence.create ~outgoing:t.sequence_state.next_outgoing
        ~incoming:t.sequence_state.next_incoming ()
    in
    match Fix.Codec.Sequence.accept_incoming sequence frame with
    | Error error -> Error (`Sequence error)
    | Ok (_, `Possible_duplicate) -> Ok false
    | Ok (_, `Accept) ->
        let accepted_next = Fix.Codec.Frame.sequence_number frame + 1 in
        let incoming =
          match Fix.Codec.Frame.msg_type frame with
          | "4" -> (
              match Fix.Codec.Frame.int_value frame 36 with
              | Ok value when value >= accepted_next -> Ok value
              | Ok value -> Error (`Fix (`Invalid_sequence_number value))
              | Error error -> Error (`Fix (error :> Fix.error)))
          | _ -> Ok accepted_next
        in
        Result.map incoming ~f:(fun incoming ->
            t.sequence_state <-
              { t.sequence_state with next_incoming = incoming };
            t.messages_since_checkpoint <- t.messages_since_checkpoint + 1;
            true)

  let process_frame t live frame =
    let identity =
      match
        (Fix.Codec.Frame.value frame 49, Fix.Codec.Frame.value frame 56)
      with
      | Some sender, Some target
        when String.equal sender (Fix.Endpoint.target_comp_id t.config.endpoint)
             && String.equal target t.config.sender_comp_id ->
          Ok ()
      | _ ->
          Error
            (`Wrong_session_identity
               "SenderCompID or TargetCompID does not match the configured \
                session")
    in
    match
      Result.bind identity ~f:(fun () -> update_incoming_sequence t frame)
    with
    | Error (`Sequence (`Gap (expected, received))) ->
        let%map result =
          send_internal t live
            (Outbound.Resend_request
               {
                 begin_sequence_number = expected;
                 end_sequence_number = received - 1;
               })
        in
        Result.map result ~f:(fun () -> `Continue)
    | Error _ as error -> return error
    | Ok accepted -> (
        let%bind checkpointed =
          match accepted with
          | true ->
              Throttle.enqueue t.sequencer (fun () ->
                  maybe_checkpoint_unlocked t)
          | false -> return (Ok ())
        in
        match checkpointed with
        | Error _ as error -> return error
        | Ok () -> (
            let%bind () =
              match Fix.Codec.Frame.msg_type frame with
              | "A" when Ivar.is_empty live.logged_on ->
                  Ivar.fill live.logged_on ();
                  publish t Connected
              | _ -> return ()
            in
            let%bind response =
              match Fix.Codec.Frame.msg_type frame with
              | "1" -> (
                  match Fix.Codec.Frame.value frame 112 with
                  | Some test_request_id ->
                      send_internal t live (Heartbeat (Some test_request_id))
                  | None ->
                      return
                        (Error (`Fix (`Missing_required_field 112 : Fix.error)))
                  )
              | _ -> return (Ok ())
            in
            match response with
            | Error _ as error -> return error
            | Ok () ->
                let%map () = publish t (Message frame) in
                Ok `Continue))

  let read_loop t live =
    let framer =
      Fix.Codec.Framer.create ~max_frame_length:t.config.max_frame_length ()
    in
    let buffer = Bytes.create 65_536 in
    let rec process = function
      | [] -> return (Ok ())
      | frame :: frames -> (
          let%bind result = process_frame t live frame in
          match result with
          | Ok `Continue -> process frames
          | Error _ as error -> return error)
    in
    let rec loop () =
      let%bind read = Reader.read live.connection.reader buffer in
      match read with
      | `Eof -> return (Error (`Io (Error.of_string "Kraken FIX EOF")))
      | `Ok length -> (
          let chunk = Stdlib.Bytes.sub_string buffer 0 length in
          match Fix.Codec.Framer.feed framer chunk with
          | Error error -> return (Error (`Fix (error :> Fix.error)))
          | Ok frames -> (
              let%bind result = process frames in
              match result with
              | Ok () -> loop ()
              | Error _ as error -> return error))
    in
    loop ()

  let heartbeat_loop t live =
    let%bind ready =
      Deferred.any
        [
          (Ivar.read live.logged_on >>| fun () -> `Ready);
          (live.connection.closed >>| fun () -> `Closed);
          (Ivar.read t.stop >>| fun () -> `Stopped);
        ]
    in
    match ready with
    | `Closed -> return (Error (`Io (Error.of_string "Kraken FIX closed")))
    | `Stopped -> return (Error `Stopped)
    | `Ready ->
        let rec loop () =
          let%bind wake =
            Deferred.any
              [
                ( Clock_ns.after
                    (Time_ns.Span.of_sec
                       (Float.of_int t.config.heartbeat_interval))
                >>| fun () -> `Heartbeat );
                (live.connection.closed >>| fun () -> `Closed);
                (Ivar.read t.stop >>| fun () -> `Stopped);
              ]
          in
          match wake with
          | `Closed ->
              return (Error (`Io (Error.of_string "Kraken FIX closed")))
          | `Stopped -> return (Error `Stopped)
          | `Heartbeat -> (
              let%bind result = send_internal t live (Heartbeat None) in
              match result with
              | Ok () -> loop ()
              | Error _ as error -> return error)
        in
        loop ()

  let close_connection t live =
    t.live <- None;
    let%bind checkpointed = checkpoint t in
    let%map closed = Monitor.try_with_or_error live.connection.close in
    match (checkpointed, closed) with
    | (Error _ as error), _ -> error
    | Ok (), Error error -> Error (`Io error)
    | Ok (), Ok () -> Ok ()

  let run_connection t connection =
    let live = { connection; logged_on = Ivar.create () } in
    t.live <- Some live;
    let%bind logon = send_logon t live in
    match logon with
    | Error _ as error ->
        let%map _ = close_connection t live in
        error
    | Ok () -> (
        let guarded_read =
          Monitor.try_with_or_error (fun () -> read_loop t live) >>| function
          | Ok result -> result
          | Error error -> Error (`Io error)
        in
        let%bind result =
          Deferred.any [ guarded_read; heartbeat_loop t live ]
        in
        let%map closed = close_connection t live in
        match (result, closed) with
        | (Error _ as error), _ -> error
        | Ok (), (Error _ as error) -> error
        | Ok (), Ok () -> Ok ())

  let rec reconnect_loop t =
    match Ivar.is_full t.stop with
    | true -> return (Ok ())
    | false -> (
        let%bind () = publish t Connecting in
        let%bind connected =
          t.connector ~stop:(Ivar.read t.stop) t.config.endpoint
        in
        match connected with
        | Error error ->
            let%bind () = publish t (Disconnected error) in
            let%bind () =
              Deferred.any_unit
                [ Clock_ns.after t.config.reconnect_delay; Ivar.read t.stop ]
            in
            reconnect_loop t
        | Ok connection -> (
            let%bind result = run_connection t connection in
            match result with
            | Error (`State _ as error) -> return (Error error)
            | Error `Stopped -> return (Ok ())
            | Error error ->
                let%bind () = publish t (Disconnected (error_to_error error)) in
                let%bind () =
                  Deferred.any_unit
                    [
                      Clock_ns.after t.config.reconnect_delay; Ivar.read t.stop;
                    ]
                in
                reconnect_loop t
            | Ok () -> reconnect_loop t))

  let run t =
    match t.running with
    | true -> return (Error `Already_running)
    | false ->
        t.running <- true;
        let%map result = reconnect_loop t in
        t.running <- false;
        Pipe.close t.events_writer;
        result

  let send t outbound =
    match t.live with
    | None -> return (Error `Not_connected)
    | Some live when Ivar.is_empty live.logged_on ->
        return (Error `Not_logged_on)
    | Some live -> send_internal t live outbound

  let stop t = Ivar.fill_if_empty t.stop ()

  module For_testing = struct
    type nonrec connection = connection = {
      reader : Reader.t;
      writer : Writer.t;
      closed : unit Deferred.t;
      close : unit -> unit Deferred.t;
    }

    type nonrec connector = connector

    let create = create_with_connector
  end
end

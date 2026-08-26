open Core
open Async
module Fix = Fix

type error =
  [ `Already_running
  | `Event_queue_full of int
  | `Event_stream_closed
  | `Fix of Fix.error
  | `Gap_buffer_full of int
  | `Io of Error.t
  | `Liveness_timeout of Time_ns.Span.t
  | `Logon_timeout of Time_ns.Span.t
  | `Not_connected
  | `Not_logged_on
  | `Replay_unavailable of int * int
  | `Sequence of Fix.Codec.Sequence.sequence_error
  | `Sent_but_not_checkpointed of int * Error.t
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
    let%bind inspected =
      Monitor.try_with_or_error (fun () -> Sys.file_exists path)
    in
    match inspected with
    | Error _ as error -> return error
    | Ok `No -> return (Ok (Sequence_state.create ()))
    | Ok `Unknown ->
        return
          (Or_error.error_s
             [%message "unable to inspect FIX sequence state" path])
    | Ok `Yes -> (
        let%bind contents =
          Monitor.try_with_or_error (fun () -> Reader.file_contents path)
        in
        match contents with
        | Error _ as error -> return error
        | Ok contents -> (
            match
              Or_error.try_with (fun () ->
                  Sexp.of_string_conv_exn contents Sequence_state.t_of_sexp)
            with
            | Error _ as error -> return error
            | Ok state -> Sequence_state.validate state |> return))

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
              let%bind () = Unix.rename ~src:temporary ~dst:path in
              let%bind directory_fd =
                Unix.openfile directory ~mode:[ `Rdonly ]
              in
              Monitor.protect
                (fun () -> Unix.fsync directory_fd)
                ~finally:(fun () -> Fd.close directory_fd))
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
    event_capacity : int;
    gap_buffer_capacity : int;
    journal_capacity : int;
    logon_timeout : Time_ns.Span.t;
    liveness_timeout : Time_ns.Span.t;
    reset_on_start : bool;
  }

  let create ~endpoint ~sender_comp_id ~authentication ~state_path
      ?(heartbeat_interval = 60) ?(reconnect_delay = Time_ns.Span.of_sec 1.)
      ?(connect_timeout = Time_ns.Span.of_sec 10.)
      ?(max_frame_length = 1024 * 1024) ?(checkpoint_every = 1)
      ?(event_capacity = 4_096) ?(gap_buffer_capacity = 4_096)
      ?(journal_capacity = 65_536) ?(logon_timeout = Time_ns.Span.of_sec 10.)
      ?liveness_timeout ?(reset_on_start = false) () =
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
    let liveness_timeout =
      Option.value liveness_timeout
        ~default:(Time_ns.Span.of_sec (Float.of_int heartbeat_interval *. 3.))
    in
    let%bind () =
      match
        Time_ns.Span.(reconnect_delay >= zero)
        && Time_ns.Span.(connect_timeout > zero)
        && Time_ns.Span.(logon_timeout > zero)
        && Time_ns.Span.(liveness_timeout > zero)
        && max_frame_length > 0 && checkpoint_every > 0 && event_capacity > 0
        && gap_buffer_capacity > 0 && journal_capacity > 0
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
        event_capacity;
        gap_buffer_capacity;
        journal_capacity;
        logon_timeout;
        liveness_timeout;
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

  type live = {
    connection : connection;
    logged_on : unit Ivar.t;
    failed : error Ivar.t;
    (* Read by the watchdog and updated by the sequenced inbound path. Async's
       scheduler is single-threaded, so this does not cross execution domains. *)
    mutable last_inbound : Time_ns.t;
  }

  type journal_entry =
    [ `Administrative of string
    | `Application of string
    ]
  (** Administrative entries retain only their original SendingTime; this is
      enough for a gap fill and avoids journaling Logon credentials. *)

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
    (* FIX sequence transitions and journal changes are mutable for the hot path,
       but every access that can yield is serialized by [sequencer]. *)
    mutable sequence_state : Sequence_state.t;
    sequencer : unit Throttle.Sequencer.t;
    mutable messages_since_checkpoint : int;
    mutable journal : journal_entry Int.Map.t;
    mutable pending_incoming : Fix.Codec.Frame.t Int.Map.t;
    mutable resend_requested_through : int option;
    (* Connection lifecycle mutation is confined to [run_connection]. *)
    mutable live : live option;
    mutable running : bool;
    mutable reset_logon : bool;
    stop : unit Ivar.t;
    events_reader : event Pipe.Reader.t;
    events_writer : event Pipe.Writer.t;
  }

  let error_to_error error = Error.create_s (sexp_of_error error)

  let publish t event =
    match Pipe.is_closed t.events_writer with
    | true -> Error `Event_stream_closed
    | false -> (
        match Pipe.length t.events_writer >= t.config.event_capacity with
        | true -> Error (`Event_queue_full t.config.event_capacity)
        | false ->
            Pipe.write_without_pushback t.events_writer event;
            Ok ())

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
        let events_reader, events_writer =
          Pipe.create ~size_budget:config.event_capacity ()
        in
        return
          (Ok
             {
               config;
               connector;
               sequence_state = state;
               sequencer = Throttle.Sequencer.create ~continue_on_error:true ();
               messages_since_checkpoint = 0;
               journal = Int.Map.empty;
               pending_incoming = Int.Map.empty;
               resend_requested_through = None;
               live = None;
               running = false;
               reset_logon = config.reset_on_start;
               stop = Ivar.create ();
               events_reader;
               events_writer;
             })

  let tls_config endpoint =
    let hostname = Fix.Endpoint.hostname endpoint in
    let verify_callback connection =
      Async_ssl.Ssl.Connection.check_peer_certificate_host connection hostname
      |> return
    in
    Async_ssl.Config.Client.create ~remote_hostname:(Some hostname)
      ~ca_file:None ~ca_path:None ~verify_callback ()

  let tls_connector ~connect_timeout ~stop endpoint =
    let hostname = Fix.Endpoint.hostname endpoint in
    let port = Fix.Endpoint.port endpoint in
    let tls_config = tls_config endpoint in
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

  let fail_live live error =
    Ivar.fill_if_empty live.failed error;
    error

  let terminal_error t live =
    match Ivar.peek t.stop with
    | Some () -> Some `Stopped
    | None -> Ivar.peek live.failed

  let add_to_journal t ~sequence_number ~wire ~orig_sending_time ~replay_kind =
    let entry =
      match replay_kind with
      | `Administrative -> `Administrative orig_sending_time
      | `Application -> `Application wire
    in
    let journal =
      Map.set t.journal ~key:sequence_number ~data:entry
    in
    t.journal <-
      (match Map.length journal > t.config.journal_capacity with
      | false -> journal
      | true -> (
          match Map.min_elt journal with
          | None -> journal
          | Some (oldest, _) -> Map.remove journal oldest))

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

  let write_encoded_unlocked t live ~replay_kind ~encode =
    match terminal_error t live with
    | Some error -> return (Error error)
    | None -> (
        match t.live with
        | None -> return (Error `Not_connected)
        | Some current when not (phys_equal current live) ->
            return (Error `Not_connected)
        | Some _ -> (
            let sequence_number = t.sequence_state.next_outgoing in
            let now = Time_ns.now () in
            let sending_time = fix_timestamp now in
            let result =
              let open Result.Let_syntax in
              let%bind header =
                Fix.Header.create ~sender_comp_id:t.config.sender_comp_id
                  ~msg_seq_num:sequence_number ~sending_time
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
                | Ok () -> (
                    t.sequence_state <-
                      {
                        t.sequence_state with
                        next_outgoing = sequence_number + 1;
                      };
                    t.messages_since_checkpoint <-
                      t.messages_since_checkpoint + 1;
                    add_to_journal t ~sequence_number ~wire
                      ~orig_sending_time:sending_time ~replay_kind;
                    let%map checkpointed = maybe_checkpoint_unlocked t in
                    match checkpointed with
                    | Ok () -> Ok ()
                    | Error (`State cause) ->
                        Error
                          (fail_live live
                             (`Sent_but_not_checkpointed
                               (sequence_number, cause)))
                    | Error error -> Error error))))

  let write_encoded t live ~replay_kind ~encode =
    Throttle.enqueue t.sequencer (fun () ->
        write_encoded_unlocked t live ~replay_kind ~encode)

  let replay_kind_of_outbound = function
    | Outbound.Market_data_request _ | New_order _ | Cancel_order _ ->
        `Application
    | Heartbeat _ | Test_request _ | Resend_request _ | Logout _ ->
        `Administrative

  let send_internal_unlocked t live outbound =
    write_encoded_unlocked t live
      ~replay_kind:(replay_kind_of_outbound outbound)
      ~encode:(fun ~now:_ ~header -> encode_outbound t ~header outbound)

  let send_internal t live outbound =
    write_encoded t live ~replay_kind:(replay_kind_of_outbound outbound)
      ~encode:(fun ~now:_ ~header -> encode_outbound t ~header outbound)

  let send_logon t live =
    let reset_sequence_numbers = t.reset_logon in
    let%map result =
      write_encoded t live ~replay_kind:`Administrative
        ~encode:(fun ~now ~header ->
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
    (match result with
    | Ok () -> t.reset_logon <- false
    | Error _ -> ());
    result

  let update_incoming_sequence_unlocked t frame =
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

  let write_replay_unlocked live wire =
    let%map result =
      Monitor.try_with_or_error (fun () ->
          Writer.write live.connection.writer wire;
          Writer.flushed live.connection.writer)
    in
    Result.map_error result ~f:(fun error -> `Io error)

  let replay_frame_unlocked live original_wire =
    let result =
      let open Result.Let_syntax in
      let%bind frame =
        Fix.Codec.Frame.decode original_wire
        |> Result.map_error ~f:(fun error -> `Fix (error :> Fix.error))
      in
      Fix.Codec.Encoder.replay frame
        ~sending_time:(fix_timestamp (Time_ns.now ()))
      |> Result.map_error ~f:(fun error -> `Fix (error :> Fix.error))
    in
    match result with
    | Error _ as error -> return error
    | Ok wire -> write_replay_unlocked live wire

  let gap_fill_unlocked t live ~begin_sequence_number ~new_sequence_number
      ~orig_sending_time =
    let result =
      let open Result.Let_syntax in
      let%bind header =
        Fix.Header.create ~sender_comp_id:t.config.sender_comp_id
          ~msg_seq_num:begin_sequence_number
          ~sending_time:(fix_timestamp (Time_ns.now ()))
        |> Result.map_error ~f:(fun error -> `Fix error)
      in
      Fix.Session.sequence_reset_gap_fill ~header ~target:(session_target t)
        ~orig_sending_time ~new_sequence_number
      |> Result.map_error ~f:(fun error -> `Fix error)
    in
    match result with
    | Error _ as error -> return error
    | Ok wire -> write_replay_unlocked live wire

  let replay_range_unlocked t live ~begin_sequence_number ~end_sequence_number =
    let last_sent = t.sequence_state.next_outgoing - 1 in
    let requested_end =
      match end_sequence_number with
      | 0 -> last_sent
      | explicit -> Int.min explicit last_sent
    in
    let rec all_available sequence_number =
      match sequence_number > requested_end with
      | true -> true
      | false -> (
          match Map.mem t.journal sequence_number with
          | false -> false
          | true -> all_available (sequence_number + 1))
    in
    match
      begin_sequence_number > 0
      && begin_sequence_number <= requested_end
      && all_available begin_sequence_number
    with
    | false ->
        return
          (Error (`Replay_unavailable (begin_sequence_number, requested_end)))
    | true ->
        let rec administrative_end sequence_number =
          match sequence_number > requested_end with
          | true -> sequence_number
          | false -> (
              match Map.find t.journal sequence_number with
              | Some (`Administrative _) ->
                  administrative_end (sequence_number + 1)
              | Some (`Application _) | None -> sequence_number)
        in
        let rec loop sequence_number =
          match sequence_number > requested_end with
          | true -> return (Ok ())
          | false -> (
              match Map.find t.journal sequence_number with
              | None ->
                  return
                    (Error
                       (`Replay_unavailable
                          (begin_sequence_number, requested_end)))
              | Some (`Application original_wire) -> (
                  let%bind replayed =
                    replay_frame_unlocked live original_wire
                  in
                  match replayed with
                  | Error _ as error -> return error
                  | Ok () -> loop (sequence_number + 1))
              | Some (`Administrative orig_sending_time) -> (
                  let new_sequence_number =
                    administrative_end (sequence_number + 1)
                  in
                  let%bind filled =
                    gap_fill_unlocked t live
                      ~begin_sequence_number:sequence_number
                      ~new_sequence_number ~orig_sending_time
                  in
                  match filled with
                  | Error _ as error -> return error
                  | Ok () -> loop new_sequence_number))
        in
        loop begin_sequence_number

  let handle_resend_request_unlocked t live frame =
    let result =
      let open Result.Let_syntax in
      let%bind begin_sequence_number =
        Fix.Codec.Frame.int_value frame 7
        |> Result.map_error ~f:(fun error -> `Fix (error :> Fix.error))
      in
      let%map end_sequence_number =
        Fix.Codec.Frame.int_value frame 16
        |> Result.map_error ~f:(fun error -> `Fix (error :> Fix.error))
      in
      (begin_sequence_number, end_sequence_number)
    in
    match result with
    | Error _ as error -> return error
    | Ok (begin_sequence_number, end_sequence_number) ->
        replay_range_unlocked t live ~begin_sequence_number ~end_sequence_number

  let handle_inbound_unlocked t live frame =
    match Fix.Codec.Frame.msg_type frame with
    | "1" -> (
        match Fix.Codec.Frame.value frame 112 with
        | Some test_request_id ->
            send_internal_unlocked t live (Heartbeat (Some test_request_id))
        | None ->
            return (Error (`Fix (`Missing_required_field 112 : Fix.error))))
    | "2" -> handle_resend_request_unlocked t live frame
    | _ -> return (Ok ())

  let process_accepted_unlocked t live frame accepted =
    let%bind checkpointed =
      match accepted with
      | true -> maybe_checkpoint_unlocked t
      | false -> return (Ok ())
    in
    match checkpointed with
    | Error _ as error -> return error
    | Ok () -> (
        let connected =
          match Fix.Codec.Frame.msg_type frame with
          | "A" when Ivar.is_empty live.logged_on ->
              Ivar.fill live.logged_on ();
              publish t Connected
          | _ -> Ok ()
        in
        match connected with
        | Error _ as error -> return error
        | Ok () -> (
            let%bind response = handle_inbound_unlocked t live frame in
            match response with
            | Error _ as error -> return error
            | Ok () -> publish t (Message frame) |> return))

  let request_gap_unlocked t live ~expected ~received =
    let requested_through = received - 1 in
    match t.resend_requested_through with
    | Some through when through >= requested_through -> return (Ok ())
    | Some _ | None ->
        let%map result =
          send_internal_unlocked t live
            (Outbound.Resend_request
               {
                 begin_sequence_number = expected;
                 end_sequence_number = requested_through;
               })
        in
        (match result with
        | Ok () -> t.resend_requested_through <- Some requested_through
        | Error _ -> ());
        result

  let buffer_gap_unlocked t live frame ~expected ~received =
    let already_buffered = Map.mem t.pending_incoming received in
    match
      ( already_buffered,
        Map.length t.pending_incoming >= t.config.gap_buffer_capacity )
    with
    | false, true ->
        return (Error (`Gap_buffer_full t.config.gap_buffer_capacity))
    | true, _ -> request_gap_unlocked t live ~expected ~received
    | false, false ->
        t.pending_incoming <-
          Map.set t.pending_incoming ~key:received ~data:frame;
        request_gap_unlocked t live ~expected ~received

  let clear_completed_resend t =
    match t.resend_requested_through with
    | Some through when t.sequence_state.next_incoming > through ->
        t.resend_requested_through <- None
    | Some _ | None -> ()

  let rec drain_pending_unlocked t live =
    clear_completed_resend t;
    let expected = t.sequence_state.next_incoming in
    t.pending_incoming <-
      Map.filter_keys t.pending_incoming ~f:(fun sequence_number ->
          sequence_number >= expected);
    match Map.find t.pending_incoming expected with
    | Some frame -> (
        t.pending_incoming <- Map.remove t.pending_incoming expected;
        let accepted = update_incoming_sequence_unlocked t frame in
        match accepted with
        | Error _ as error -> return error
        | Ok accepted -> (
            let%bind processed =
              process_accepted_unlocked t live frame accepted
            in
            match processed with
            | Error _ as error -> return error
            | Ok () -> drain_pending_unlocked t live))
    | None -> (
        match Map.min_elt t.pending_incoming with
        | Some (received, _) when received > expected ->
            request_gap_unlocked t live ~expected ~received
        | Some _ | None -> return (Ok ()))

  let valid_identity t frame =
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
    identity

  let process_frame_unlocked t live frame =
    match t.live with
    | None -> return (Error `Not_connected)
    | Some current when not (phys_equal current live) ->
        return (Error `Not_connected)
    | Some _ -> (
        live.last_inbound <- Time_ns.now ();
        match valid_identity t frame with
        | Error _ as error -> return error
        | Ok () -> (
            let expected = t.sequence_state.next_incoming in
            let received = Fix.Codec.Frame.sequence_number frame in
            match Int.compare received expected with
            | comparison when comparison > 0 ->
                buffer_gap_unlocked t live frame ~expected ~received
            | _ -> (
                let accepted = update_incoming_sequence_unlocked t frame in
                match accepted with
                | Error _ as error -> return error
                | Ok accepted -> (
                    let%bind processed =
                      process_accepted_unlocked t live frame accepted
                    in
                    match processed with
                    | Error _ as error -> return error
                    | Ok () -> drain_pending_unlocked t live))))

  let process_frame t live frame =
    Throttle.enqueue t.sequencer (fun () -> process_frame_unlocked t live frame)

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
          | Ok () -> process frames
          | Error _ as error -> return error)
    in
    let rec loop () =
      let%bind read = Reader.read live.connection.reader buffer in
      match read with
      | `Eof -> return (Error (`Io (Error.of_string "Kraken FIX EOF")))
      | `Ok length -> (
          let chunk = Bytes.To_string.sub buffer ~pos:0 ~len:length in
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
        let heartbeat_interval =
          Time_ns.Span.of_sec (Float.of_int t.config.heartbeat_interval)
        in
        let rec loop next_heartbeat =
          let liveness_deadline =
            Time_ns.add live.last_inbound t.config.liveness_timeout
          in
          let wake_at =
            match Time_ns.(liveness_deadline <= next_heartbeat) with
            | true -> liveness_deadline
            | false -> next_heartbeat
          in
          let%bind wake =
            Deferred.any
              [
                (Clock_ns.at wake_at >>| fun () -> `Timer);
                (live.connection.closed >>| fun () -> `Closed);
                (Ivar.read t.stop >>| fun () -> `Stopped);
              ]
          in
          match wake with
          | `Closed ->
              return (Error (`Io (Error.of_string "Kraken FIX closed")))
          | `Stopped -> return (Error `Stopped)
          | `Timer -> (
              let current_time = Time_ns.now () in
              let silence = Time_ns.diff current_time live.last_inbound in
              match Time_ns.Span.(silence >= t.config.liveness_timeout) with
              | true -> return (Error (`Liveness_timeout silence))
              | false -> (
                  match Time_ns.(current_time >= next_heartbeat) with
                  | false -> loop next_heartbeat
                  | true -> (
                      let outbound =
                        match Time_ns.Span.(silence >= heartbeat_interval) with
                        | true ->
                            let request_id =
                              current_time |> Time_ns.to_int63_ns_since_epoch
                              |> Int63.to_string
                            in
                            Outbound.Test_request request_id
                        | false -> Outbound.Heartbeat None
                      in
                      let%bind result = send_internal t live outbound in
                      match result with
                      | Ok () ->
                          loop (Time_ns.add current_time heartbeat_interval)
                      | Error _ as error -> return error)))
        in
        loop (Time_ns.add (Time_ns.now ()) heartbeat_interval)

  let prefer_connection_error result closed =
    match (result, closed) with
    | Error ((`Sent_but_not_checkpointed _) as error), _ -> Error error
    | _, (Error _ as error) -> error
    | _, Ok () -> result

  let close_connection t live =
    t.live <- None;
    let%bind checkpointed = checkpoint t in
    let%map closed = Monitor.try_with_or_error live.connection.close in
    match (checkpointed, closed) with
    | (Error _ as error), _ -> error
    | Ok (), Error error -> Error (`Io error)
    | Ok (), Ok () -> Ok ()

  let run_connection t connection =
    let live =
      {
        connection;
        logged_on = Ivar.create ();
        failed = Ivar.create ();
        last_inbound = Time_ns.now ();
      }
    in
    t.pending_incoming <- Int.Map.empty;
    t.resend_requested_through <- None;
    t.live <- Some live;
    let%bind logon = send_logon t live in
    match logon with
    | Error _ as error -> (
        let%map closed = close_connection t live in
        prefer_connection_error error closed)
    | Ok () -> (
        let guarded_read =
          Monitor.try_with_or_error (fun () -> read_loop t live) >>| function
          | Ok result -> result
          | Error error -> Error (`Io error)
        in
        let logon_timeout =
          let%bind () = Clock_ns.after t.config.logon_timeout in
          match Ivar.is_full live.logged_on with
          | true -> Deferred.never ()
          | false -> return (Error (`Logon_timeout t.config.logon_timeout))
        in
        let%bind result =
          Deferred.any
            [
              guarded_read;
              heartbeat_loop t live;
              logon_timeout;
              (Ivar.read live.failed >>| fun error -> Error error);
            ]
        in
        let%map closed = close_connection t live in
        prefer_connection_error result closed)

  let rec reconnect_loop t =
    match Ivar.is_full t.stop with
    | true -> return (Ok ())
    | false -> (
        let connecting = publish t Connecting in
        match connecting with
        | Error _ as error -> return error
        | Ok () -> (
            let%bind connected =
              t.connector ~stop:(Ivar.read t.stop) t.config.endpoint
            in
            match connected with
            | Error error -> (
                match publish t (Disconnected error) with
                | Error _ as publish_error -> return publish_error
                | Ok () ->
                    let%bind () =
                      Deferred.any_unit
                        [
                          Clock_ns.after t.config.reconnect_delay;
                          Ivar.read t.stop;
                        ]
                    in
                    reconnect_loop t)
            | Ok connection -> (
                let%bind result = run_connection t connection in
                match result with
                | Error
                    (( `Event_queue_full _ | `Event_stream_closed
                     | `Replay_unavailable _ | `Sent_but_not_checkpointed _
                     | `State _ | `Wrong_session_identity _ ) as error) ->
                    return (Error error)
                | Error `Stopped -> return (Ok ())
                | Error error -> (
                    match publish t (Disconnected (error_to_error error)) with
                    | Error _ as publish_error -> return publish_error
                    | Ok () ->
                        let%bind () =
                          Deferred.any_unit
                            [
                              Clock_ns.after t.config.reconnect_delay;
                              Ivar.read t.stop;
                            ]
                        in
                        reconnect_loop t)
                | Ok () -> reconnect_loop t)))

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
    match Ivar.peek t.stop with
    | Some () -> return (Error `Stopped)
    | None -> (
        match t.live with
        | None -> return (Error `Not_connected)
        | Some live -> (
            match Ivar.peek live.failed with
            | Some error -> return (Error error)
            | None -> (
                match Ivar.is_empty live.logged_on with
                | true -> return (Error `Not_logged_on)
                | false -> send_internal t live outbound)))

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
    let tls_config = tls_config
  end
end

(** Websocket api support for the gemini trading exchange. *)
module type EVENT_TYPE = Csv_support.EVENT_TYPE
module type CSV_OF_EVENTS = Csv_support.CSV_OF_EVENTS
(** Specification for a websocket channel. *)
module type CHANNEL = sig
  (** The name of the channel *)
  val name : string

  (** The channel protocol version *)
  val version : string

  (** The uri path for this channel *)
  val path : string list

  (** Authentication syle for this channel. One of [`Private] or [`Public] *)
  val authentication : [ `Private | `Public ]

  (** Uri arguments which are appended to the end of the path segment *)
  type uri_args [@@deriving sexp, enumerate]

  (** Encder from well typed uri arguments to a string suitable for a uri. *)
  val encode_uri_args : uri_args -> string

  (** Defaut uri arguments. Optional for some channels. *)
  val default_uri_args : uri_args option

  (** Respone type of the channel. Must have sexp converters and a yojson parser *)
  type response [@@deriving sexp, of_yojson]

  module Event_type : EVENT_TYPE

  module Csv_of_event : CSV_OF_EVENTS with module Event_type = Event_type

  (** Given a response value produce csvable events modularized by event type. *)
  val events_of_response : response -> Csv_of_event.t

  (** Query parameters for the channel *)
  type query [@@deriving sexp]

  (** Encodes queries as an http header key value pair *)
  val encode_query : query -> string * string
end

module Error = struct
  type t =
    [ `Json_parse_error of string
    | `Channel_parse_error of string
    ]
  [@@deriving sexp, yojson]
end

module type RESULT = sig
  type response [@@deriving sexp, of_yojson]

  type ok = [ `Ok of response ] [@@deriving sexp, of_yojson]

  type t =
    [ ok
    | Error.t
    ]
  [@@deriving sexp, of_yojson]
end

module type CHANNEL_CLIENT_BASE = sig
  include CHANNEL

  module Error : module type of Error

  val command : string * Command.t
end

module type CHANNEL_CLIENT_INTERNAL = sig
  include CHANNEL_CLIENT_BASE

  val client :
    (module Cfg.S) ->
    ?query:Sexp.t list ->
    ?uri_args:uri_args ->
    ?nonce:int Inf_pipe.Reader.t ->
    unit ->
    [ `Ok of response | Error.t ] Pipe.Reader.t Deferred.t
end

module type CHANNEL_CLIENT_NO_REQUEST = sig
  include CHANNEL_CLIENT_BASE

  val client :
    (module Cfg.S) ->
    ?query:Sexp.t list ->
    ?uri_args:uri_args ->
    unit ->
    [ `Ok of response | Error.t ] Pipe.Reader.t Deferred.t
end
 
module type CHANNEL_CLIENT = sig
  include CHANNEL_CLIENT_BASE

  module Error : module type of Error

  val command : string * Command.t

  val client :
    (module Cfg.S) ->
    nonce:Nonce.reader ->
    ?query:Sexp.t list ->
    ?uri_args:uri_args ->
    unit ->
    [ `Ok of response | Error.t ] Pipe.Reader.t Deferred.t
end

(** Creates a websocket implementation given a [Channel] *)
module Impl (Channel : CHANNEL) :
  CHANNEL_CLIENT_INTERNAL
    with type response := Channel.response
     and type uri_args = Channel.uri_args
     and module Event_type = Channel.Event_type
     and type query = Channel.query
     and module Error = Error = struct
  (** Establishes a web socket client given configuration [Cfg] and optional
      [query], [uri_args] and [nonce] parameters.

      Produces a pipe of [(Channel.response, string) result] instances. *)

  include Channel
  module Error = Error

  let client (module Cfg : Cfg.S) ?query ?uri_args ?nonce () :
      [ `Ok of response | Error.t ] Pipe.Reader.t Deferred.t =
    let query =
      Option.map query ~f:(fun l ->
          List.fold ~init:String.Map.empty l ~f:(fun map q ->
              let key, data = Channel.encode_query (Channel.query_of_sexp q) in
              Map.add_multi map ~key ~data )
          |> fun map ->
          let keys = Map.keys map in
          List.map keys ~f:(fun k -> (k, Map.find_multi map k)) )
    in
    let uri =
      Uri.make ~host:Cfg.api_host ~scheme:"wss" ?query
        ~path:
          (String.concat ~sep:"/"
             ( Channel.path
             @ Option.(map ~f:Channel.encode_uri_args uri_args |> to_list) ) )
        ()
    in
    Log.Global.info "Ws.client: uri=%s" (Uri.to_string uri);
    let payload = `Null in
    let path = Path.to_string Channel.path in
    let%bind payload =
      match nonce with
      | None -> return None
      | Some nonce ->
        Nonce.Request.(make ~nonce ~request:path ~payload () >>| to_yojson)
        >>| fun s -> Yojson.Safe.to_string s |> Option.some
    in
    let _headers =
      (* TODO: websocket_curl doesn't support custom headers yet.
         This means authenticated WebSocket connections will fail.
         Need to extend websocket_curl or use a different approach. *)
      ( match Channel.authentication with
      | `Private ->
        Option.map
          ~f:(fun p -> Auth.(to_headers (module Cfg) (of_payload p)))
          payload
      | `Public -> None )
      |> Option.value ~default:(Cohttp.Header.init ())
    in
    let url = Uri.to_string uri in
    let%bind ws_result = Websocket_curl.connect ~url in
    match ws_result with
    | Error _err ->
      (* Return a pipe with a single error *)
      return (Pipe.of_list [`Json_parse_error "WebSocket connection failed"])
    | Ok ws ->
      let r, w = Pipe.create () in
      (* Background task to receive messages with JSON buffering *)
      don't_wait_for (
        let buffer = ref "" in
        let rec receive_loop () =
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            (if String.length !buffer > 0 then
              Log.Global.error "WebSocket closed with incomplete JSON buffer: %d bytes"
                (String.length !buffer));
            Pipe.close w;
            return ()
          | Some s ->
            (* Append to buffer *)
            buffer := !buffer ^ s;

            (* Try to extract complete JSON messages from buffer *)
            let rec process_buffer () =
              match String.length !buffer with
              | 0 -> return ()
              | _ ->
                (* Check if we have a complete JSON message *)
                let complete_json_opt =
                  try
                    (* Attempt to parse from start of buffer *)
                    let json = Yojson.Safe.from_string !buffer in
                    (* If successful, we have complete JSON *)
                    Some (Yojson.Safe.to_string json, String.length (Yojson.Safe.to_string json))
                  with
                  | Yojson.Json_error _ ->
                    (* Try to find first complete JSON object/array *)
                    let rec find_complete idx depth in_string escape =
                      if idx >= String.length !buffer then None
                      else
                        let c = !buffer.[idx] in
                        match in_string, escape, c with
                        | true, true, _ ->
                          (* In string, was escaped, consume and continue *)
                          find_complete (idx + 1) depth true false
                        | true, false, '\\' ->
                          (* In string, found escape char *)
                          find_complete (idx + 1) depth true true
                        | true, false, '"' ->
                          (* In string, found closing quote *)
                          find_complete (idx + 1) depth false false
                        | false, _, '"' ->
                          (* Not in string, found opening quote *)
                          find_complete (idx + 1) depth true false
                        | false, _, '{' | false, _, '[' ->
                          (* Opening brace/bracket *)
                          find_complete (idx + 1) (depth + 1) false false
                        | false, _, '}' | false, _, ']' ->
                          (* Closing brace/bracket *)
                          let new_depth = depth - 1 in
                          if new_depth = 0 then
                            (* Found complete JSON *)
                            let json_str = String.sub !buffer ~pos:0 ~len:(idx + 1) in
                            Some (json_str, idx + 1)
                          else
                            find_complete (idx + 1) new_depth false false
                        | _, _, _ ->
                          (* Other character *)
                          find_complete (idx + 1) depth in_string false
                    in
                    find_complete 0 0 false false
                in

                match complete_json_opt with
                | None ->
                  (* No complete JSON yet, wait for more data *)
                  Log.Global.debug "Buffering incomplete JSON: %d bytes" (String.length !buffer);
                  return ()
                | Some (json_str, consumed_len) ->
                  (* We have complete JSON, parse and emit it *)
                  Log.Global.debug "Complete JSON message: %d bytes" consumed_len;
                  let parsed_msg =
                    ( try `Ok (Yojson.Safe.from_string json_str) with
                    | Yojson.Json_error e -> `Json_parse_error e )
                    |> function
                    | #Error.t as e -> e
                    | `Ok json -> (
                      match Channel.response_of_yojson json with
                      | Ok response -> `Ok response
                      | Error e -> `Channel_parse_error e )
                  in
                  (* Remove consumed JSON from buffer *)
                  buffer := String.sub !buffer ~pos:consumed_len
                    ~len:(String.length !buffer - consumed_len);
                  (* Emit parsed message *)
                  let%bind () = Pipe.write w parsed_msg in
                  (* Process remaining buffer *)
                  process_buffer ()
            in
            process_buffer () >>= fun () ->
            receive_loop ()
        in
        receive_loop ()
      );
      return r

  let command =
    let spec : (_, _) Command.Spec.t =
      let open Command.Spec in
      empty +> Cfg.param
      +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
      +> flag "-query" (listed sexp) ~doc:"QUERY query parameters"
      +> flag "-csv-dir" (optional string)
           ~doc:
             "PATH output each event type to a separate csv file at PATH. \
              Defaults to current directory."
      +> Command.Spec.flag "--no-csv" no_arg ~doc:"Disable csv generation."
      +> anon (maybe ("uri_args" %: sexp))
    in
    let set_loglevel = function
      | 2 ->
        Log.Global.set_level `Info;
        Logs.set_level @@ Some Logs.Info
      | e when e > 2 ->
        Log.Global.set_level `Debug;
        Logs.set_level @@ Some Logs.Debug
      | _ -> ()
    in
    let run cfg loglevel query (csv_dir : string option) no_csv uri_args () =
      let cfg = Cfg.or_default cfg in
      let module Cfg = (val cfg : Cfg.S) in
      Option.iter loglevel ~f:set_loglevel;
      let uri_args =
        Option.first_some
          (Option.map ~f:Channel.uri_args_of_sexp uri_args)
          Channel.default_uri_args
      in
      let query =
        match List.is_empty query with
        | true -> None
        | false -> Some query
      in
      let%bind nonce = Nonce.File.(pipe ~init:default_filename) () in
      Log.Global.info "Initiating channel %s with path %s" Channel.name
        (Path.to_string Channel.path);
      let channel_to_sexp_str response =
        Channel.sexp_of_response response
        |> Sexp.to_string_hum |> sprintf "%s\n"
      in
      let append_to_csv =
        match no_csv with
        | true -> fun _ -> ()
        | false -> fun response ->
        let events = Channel.events_of_response response in
        let all = Channel.Csv_of_event.write_all ?dir:csv_dir events in
        let tags =
          List.map all ~f:(fun (k, v) ->
              (Channel.Event_type.to_string k, Int.to_string v) )
        in
        Log.Global.debug ~tags "wrote csv response events";
        ()
      in
      let ok_pipe_reader response =
        append_to_csv response;
        channel_to_sexp_str response
      in
      client (module Cfg) ?query ?uri_args ~nonce () >>= fun pipe ->
      Log.Global.debug "Broadcasting channel %s to stderr..." Channel.name;
      let pipe =
        Pipe.filter_map pipe ~f:(function
          | `Ok ok -> Some ok
          | #Error.t as e ->
            Log.Global.error "Failed to parse last event: %s"
              (Error.sexp_of_t e |> Sexp.to_string);
            None )
      in
      Pipe.transfer pipe Writer.(pipe (Lazy.force stderr)) ~f:ok_pipe_reader
    in
    ( Channel.name,
      Command.async_spec
        ~summary:
          (sprintf "Gemini %s %s Websocket Command" Channel.version Channel.name)
        spec run )
end

(** Create a websocket interface that has no request parameters *)
module Make_no_request (Channel : CHANNEL with type query = unit) :
  CHANNEL_CLIENT_NO_REQUEST
    with type response := Channel.response
     and type uri_args := Channel.uri_args
     and module Event_type := Channel.Event_type
     and type query := unit = struct
  include Channel
  include Impl (Channel)

  let client = client ?nonce:None
end

(** Create a websocket interface with request parameters *)
module Make (Channel : CHANNEL) :
  CHANNEL_CLIENT
    with type response := Channel.response
     and type uri_args := Channel.uri_args
     and module Event_type := Channel.Event_type
     and type query := Channel.query = struct
  include Channel
  include Impl (Channel)

  let client (module Cfg : Cfg.S) ~nonce = client (module Cfg) ~nonce
end

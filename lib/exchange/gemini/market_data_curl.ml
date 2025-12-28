(** Gemini Market Data WebSocket client using libcurl *)

open Core
open Async
open Common

(** Represents the market data stream *)
type t = {
  uri : Uri.t;
  ws : Websocket_curl.t;
  symbol : Symbol.t;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
  mutable active : bool;
}

(** Connect to Gemini public market data WebSocket *)
let connect (module Cfg : Cfg.S) ~(symbol : Symbol.t) () : (t, string) Result.t Deferred.t =
  let open Deferred.Let_syntax in

  let symbol_str = Symbol.to_string symbol in
  let url = sprintf "wss://%s/v1/marketdata/%s" Cfg.api_host symbol_str in
  let uri = Uri.of_string url in

  Log.Global.info "Gemini libcurl connecting to: %s" url;

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url in

  match ws_result with
  | Error err ->
    return (Error (Error.to_string_hum err))
  | Ok ws ->
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      ws;
      symbol;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages with proper JSON object boundary detection *)
    Log.Global.info "Gemini: Starting receive loop in background";
    let receive_count = ref 0 in
    let parsed_count = ref 0 in
    don't_wait_for (
      let buffer = Buffer.create 4096 in
      let rec receive_loop () =
        if not t.active then (
          Log.Global.info "Gemini: Receive loop ending (inactive)";
          Pipe.close t.message_writer;
          return ()
        ) else (
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            Log.Global.info "Gemini: Connection closed by server";
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            receive_count := !receive_count + 1;
            if !receive_count mod 50 = 0 then
              Log.Global.info "Gemini: Received %d chunks so far" !receive_count;
            (* Append to buffer *)
            Buffer.add_string buffer payload;

            (* Extract complete JSON objects - try parsing incrementally *)
            let rec extract_complete_json () =
              let content = Buffer.contents buffer in
              if String.length content = 0 then
                return ()
              else
                (* Try to parse as JSON - if successful, we have a complete object *)
                try
                  let _json = Yojson.Safe.from_string content in
                  (* Successfully parsed - entire buffer is one JSON object *)
                  parsed_count := !parsed_count + 1;
                  if String.length content > 100000 then
                    Log.Global.info "Gemini: Parsed large JSON (%d bytes, msg #%d), writing to pipe"
                      (String.length content) !parsed_count
                  else if !parsed_count mod 100 = 0 then
                    Log.Global.info "Gemini: Parsed %d messages so far" !parsed_count;
                  let json_str = content in
                  Buffer.clear buffer;
                  let%bind () = Pipe.write t.message_writer json_str in
                  extract_complete_json ()
                with
                | Yojson.Json_error _ ->
                  (* Not a complete JSON yet, wait for more data *)
                  return ()
            in
            let%bind () = extract_complete_json () in
            receive_loop ()
        )
      in
      receive_loop ()
    );

    return (Ok t)

(** Get the raw message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Parse and filter for successful market data responses *)
let response_pipe t : [ `Ok of Market_data.response | Ws.Error.t ] Pipe.Reader.t =
  Pipe.map t.message_pipe ~f:(fun s ->
    Log.Global.debug "Gemini message: %s" s;
    try
      let json = Yojson.Safe.from_string s in
      match Market_data.response_of_yojson json with
      | Ok response -> `Ok response
      | Error e -> `Channel_parse_error e
    with
    | Yojson.Json_error e -> `Json_parse_error e
  )

(** Close the WebSocket connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

(** Get connection endpoint *)
let endpoint t = Uri.to_string t.uri

(** Get symbol *)
let symbol t = t.symbol

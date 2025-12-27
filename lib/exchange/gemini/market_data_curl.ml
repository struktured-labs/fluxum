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
    don't_wait_for (
      let buffer = Buffer.create 4096 in
      let rec receive_loop () =
        if not t.active then (
          Pipe.close t.message_writer;
          return ()
        ) else
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            (* Debug: log received data *)
            if String.length payload > 0 then
              Log.Global.debug "Gemini libcurl received %d bytes" (String.length payload);
            (* Append to buffer *)
            Buffer.add_string buffer payload;

            (* Extract complete JSON objects by counting braces *)
            let rec extract_complete_json () =
              let content = Buffer.contents buffer in
              if String.length content = 0 then
                return ()
              else
                (* Find a complete JSON object by matching braces *)
                let rec find_json_end pos depth in_string escaped =
                  if pos >= String.length content then
                    None (* Incomplete JSON *)
                  else
                    let c = content.[pos] in
                    match c, in_string, escaped with
                    | '\\', true, false -> find_json_end (pos + 1) depth true true
                    | '"', false, _ -> find_json_end (pos + 1) depth true false
                    | '"', true, false -> find_json_end (pos + 1) depth false false
                    | '{', false, _ -> find_json_end (pos + 1) (depth + 1) false false
                    | '}', false, _ ->
                      let new_depth = depth - 1 in
                      if new_depth = 0 then
                        Some (pos + 1) (* Found complete JSON *)
                      else
                        find_json_end (pos + 1) new_depth false false
                    | _, _, _ -> find_json_end (pos + 1) depth in_string false
                in

                match find_json_end 0 0 false false with
                | None ->
                  (* No complete JSON yet, wait for more data *)
                  return ()
                | Some end_pos ->
                  (* Extract the complete JSON *)
                  let json_str = String.sub content ~pos:0 ~len:end_pos in
                  (* Remove it from buffer *)
                  Buffer.clear buffer;
                  Buffer.add_string buffer (String.drop_prefix content end_pos);
                  (* Send the complete JSON message *)
                  let%bind () = Pipe.write t.message_writer json_str in
                  (* Check for more complete objects in buffer *)
                  extract_complete_json ()
            in
            let%bind () = extract_complete_json () in
            receive_loop ()
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

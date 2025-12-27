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

    (* Start background task to receive messages with buffering for fragmented JSON *)
    don't_wait_for (
      let buffer = ref "" in
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
            (* Append to buffer *)
            buffer := !buffer ^ payload;
            (* Try to extract complete JSON messages *)
            let rec extract_messages buf =
              try
                let json = Yojson.Safe.from_string buf in
                (* Successfully parsed - write it and clear buffer *)
                let json_str = Yojson.Safe.to_string json in
                buffer := String.drop_prefix buf (String.length json_str);
                (* Trim any whitespace before next message *)
                buffer := String.lstrip !buffer;
                let%bind () = Pipe.write t.message_writer json_str in
                (* Check if there's more data to process *)
                if String.length !buffer > 0 then
                  extract_messages !buffer
                else
                  return ()
              with
              | _ ->
                (* Incomplete JSON, wait for more data *)
                return ()
            in
            let%bind () = extract_messages !buffer in
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

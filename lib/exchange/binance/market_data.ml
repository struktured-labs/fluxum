(** Market data WebSocket client for Binance using curl-based WebSocket *)

open Core
open Async

(** Represents the market data stream *)
type t = {
  uri : Uri.t;
  ws : Websocket_curl.t;
  streams : Ws.Stream.t list ref;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
  mutable active : bool;
}

(** Connect to Binance WebSocket and subscribe to streams *)
let connect ~(streams : Ws.Stream.t list) ?(url = Ws.Endpoint.data_stream) () : (t, string) Result.t Deferred.t =
  let open Deferred.Let_syntax in

  (* Build WebSocket URL with stream names *)
  let stream_names = List.map streams ~f:Ws.Stream.to_stream_name in
  let full_url =
    match stream_names with
    | [single] -> sprintf "%s/ws/%s" url single
    | multiple -> sprintf "%s/stream?streams=%s" url (String.concat ~sep:"/" multiple)
  in

  let uri = Uri.of_string full_url in

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url:full_url () in

  match ws_result with
  | Error err ->
    return (Error (Error.to_string_hum err))
  | Ok ws ->
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      ws;
      streams = ref streams;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages *)
    Log.Global.info "Binance: Starting receive loop in background";
    let receive_count = ref 0 in
    don't_wait_for (
      let rec receive_loop () =
        match t.active with
        | false ->
          Log.Global.info "Binance: Receive loop ending (inactive)";
          Pipe.close t.message_writer;
          return ()
        | true ->
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            Log.Global.info "Binance: Connection closed by server";
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            receive_count := !receive_count + 1;
            (match !receive_count mod 100 = 0 with
             | true -> Log.Global.info "Binance: Received %d messages so far" !receive_count
             | false -> ());
            let%bind () = Pipe.write t.message_writer payload in
            receive_loop ()
      in
      receive_loop ()
    );

    (* Binance doesn't require explicit ping messages - the server sends pings
       and expects pongs, which websocket_curl handles automatically *)

    return (Ok t)

(** Get the message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Close the connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

(** Market data WebSocket client for Hyperliquid using curl-based WebSocket *)

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

(** Connect to Hyperliquid WebSocket and subscribe to streams *)
let connect ~(streams : Ws.Stream.t list) ?(url = Ws.Endpoint.mainnet) () : (t, string) Result.t Deferred.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

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
      streams = ref streams;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages *)
    don't_wait_for (
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
            let%bind () = Pipe.write t.message_writer payload in
            receive_loop ()
      in
      receive_loop ()
    );

    (* Send subscription messages for each stream *)
    let%bind () =
      Deferred.List.iter ~how:`Sequential streams ~f:(fun stream ->
        let msg = Ws.Stream.to_subscribe_message stream in
        let msg_str = Yojson.Safe.to_string msg in
        Websocket_curl.send ws msg_str
      )
    in

    return (Ok t)

(** Get the message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Subscribe to an additional stream *)
let subscribe t (stream : Ws.Stream.t) : unit Deferred.t =
  t.streams := stream :: !(t.streams);

  if not t.active then
    Deferred.unit
  else
    let msg = Ws.Stream.to_subscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Unsubscribe from a stream *)
let unsubscribe t (stream : Ws.Stream.t) : unit Deferred.t =
  t.streams := List.filter !(t.streams) ~f:(fun s -> not (phys_equal s stream));

  if not t.active then
    Deferred.unit
  else
    let msg = Ws.Stream.to_unsubscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Send ping *)
let ping t : unit Deferred.t =
  if not t.active then
    Deferred.unit
  else
    Websocket_curl.send t.ws "{\"method\":\"ping\"}"

(** Close the WebSocket connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

(** Get current streams *)
let streams t = !(t.streams)

(** Get connection endpoint *)
let endpoint t = Uri.to_string t.uri

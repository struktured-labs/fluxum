(** Market data WebSocket client for Kraken using curl-based WebSocket *)

open Core
open Async

(** Represents an active WebSocket subscription *)
type subscription = {
  channel: string;
  pairs: string list;
  interval: int option;
  depth: int option;
}

(** Represents the market data stream *)
type t = {
  uri : Uri.t;
  ws : Websocket_curl.t;
  subscriptions : subscription list ref;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
  mutable active : bool;
}

(** Connect to Kraken public WebSocket and subscribe to market data *)
let connect ~(subscriptions : subscription list) ?(url = Ws.Endpoint.public_url) () : (t, Ws.Error.t) Result.t Deferred.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url () in

  match ws_result with
  | Error err ->
    return (Error (`Connection_error (Error.to_string_hum err)))
  | Ok ws ->
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      ws;
      subscriptions = ref subscriptions;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages *)
    don't_wait_for (
      let rec receive_loop () =
        match t.active with
        | false ->
          Pipe.close t.message_writer;
          return ()
        | true ->
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

    (* Send subscription messages for each subscription *)
    let%bind () =
      Deferred.List.iter ~how:`Sequential subscriptions ~f:(fun sub ->
        let msg = match sub.channel with
          | "ticker" -> Ws.Public.subscribe_ticker sub.pairs
          | "trade" -> Ws.Public.subscribe_trade sub.pairs
          | "ohlc" ->
              let interval = Option.value sub.interval ~default:60 in
              Ws.Public.subscribe_ohlc ~interval sub.pairs
          | "book" ->
              let depth = Option.value sub.depth ~default:10 in
              Ws.Public.subscribe_book ~depth sub.pairs
          | _ -> ""
        in
        match String.is_empty msg with
        | true -> Deferred.unit
        | false -> Websocket_curl.send ws msg)
    in

    return (Ok t)

(** Get the message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Subscribe to an additional channel *)
let subscribe t ~channel ~pairs : unit Deferred.t =
  let sub = { channel; pairs; interval = None; depth = None } in
  t.subscriptions := sub :: !(t.subscriptions);
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = match channel with
      | "ticker" -> Ws.Public.subscribe_ticker pairs
      | "trade" -> Ws.Public.subscribe_trade pairs
      | "ohlc" -> Ws.Public.subscribe_ohlc ~interval:60 pairs
      | "book" -> Ws.Public.subscribe_book ~depth:10 pairs
      | _ -> ""
    in
    match String.is_empty msg with
    | true -> Deferred.unit
    | false -> Websocket_curl.send t.ws msg

(** Unsubscribe from a channel *)
let unsubscribe t ~channel ~pairs : unit Deferred.t =
  t.subscriptions :=
    List.filter !(t.subscriptions) ~f:(fun sub ->
      not (String.equal sub.channel channel && List.equal String.equal sub.pairs pairs));
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = Ws.Public.unsubscribe channel pairs in
    Websocket_curl.send t.ws msg

(** Close the WebSocket connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

(** Get current subscriptions *)
let subscriptions t = !(t.subscriptions)

(** Get connection endpoint *)
let endpoint t = Uri.to_string t.uri

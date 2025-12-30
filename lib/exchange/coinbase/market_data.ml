(** Market data WebSocket client for Coinbase using curl-based WebSocket *)

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

(** Connect to Coinbase WebSocket and subscribe to streams *)
let connect ~(streams : Ws.Stream.t list) ?(url = Ws.Endpoint.exchange) () : (t, string) Result.t Deferred.t =
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
    Log.Global.info "Coinbase: Starting receive loop in background";
    let receive_count = ref 0 in
    don't_wait_for (
      let rec receive_loop () =
        if not t.active then (
          Log.Global.info "Coinbase: Receive loop ending (inactive)";
          Pipe.close t.message_writer;
          return ()
        ) else
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            Log.Global.info "Coinbase: Connection closed by server";
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            receive_count := !receive_count + 1;
            (* Log first few messages for debugging *)
            if !receive_count <= 5 then
              Log.Global.info "Coinbase: Message #%d: %s" !receive_count payload
            else if !receive_count mod 100 = 0 then
              Log.Global.info "Coinbase: Received %d messages so far" !receive_count;
            let%bind () = Pipe.write t.message_writer payload in
            receive_loop ()
      in
      receive_loop ()
    );

    (* Coinbase doesn't require explicit ping messages - handled by server *)

    (* Send subscription messages for each stream *)
    let%bind () =
      (* Check if API credentials are available for authentication *)
      match Sys.getenv "COINBASE_API_KEY", Sys.getenv "COINBASE_API_SECRET" with
      | Some api_key, Some api_secret ->
        (* Generate authentication signature *)
        let timestamp = Int63.to_string (Time_ns.to_int63_ns_since_epoch (Time_ns.now ())) in
        let channel = List.hd_exn streams |> Ws.Stream.channel_name in
        let product_ids_str =
          List.hd_exn streams
          |> Ws.Stream.product_ids
          |> String.concat ~sep:","
        in
        let signature_result = Signature.coinbase_ws_signature
          ~api_secret
          ~timestamp
          ~channel
          ~product_ids:product_ids_str
        in
        let signature =
          match signature_result with
          | Ok s -> s
          | Error (`Msg msg) -> failwith msg
        in
        let subscribe_msg = Ws.Stream.to_subscribe_message_authenticated
          ~api_key
          ~signature
          ~timestamp
          streams
        in
        let msg_str = Yojson.Safe.to_string subscribe_msg in
        Log.Global.info "Coinbase: Sending authenticated subscription: %s" msg_str;
        Websocket_curl.send ws msg_str
      | _ ->
        (* No credentials, try unauthenticated subscription (will fail for level2) *)
        let subscribe_msg = Ws.Stream.to_subscribe_message streams in
        let msg_str = Yojson.Safe.to_string subscribe_msg in
        Log.Global.info "Coinbase: Sending unauthenticated subscription: %s" msg_str;
        Websocket_curl.send ws msg_str
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
    let msg = Ws.Stream.to_subscribe_message [stream] in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Unsubscribe from a stream *)
let unsubscribe t (stream : Ws.Stream.t) : unit Deferred.t =
  t.streams := List.filter !(t.streams) ~f:(fun s -> not (phys_equal s stream));

  if not t.active then
    Deferred.unit
  else
    let msg = Ws.Stream.to_unsubscribe_message [stream] in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Close the connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

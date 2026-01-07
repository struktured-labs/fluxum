(** Bitrue Market Data WebSocket client using libcurl *)

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

(** Connect to Bitrue WebSocket and subscribe to streams *)
let connect ~(streams : Ws.Stream.t list) ?(url = Ws.Endpoint.market) () : (t, string) Result.t Deferred.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

  Log.Global.info "Bitrue libcurl connecting to: %s" url;

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url in

  match ws_result with
  | Error err ->
    Log.Global.error "Bitrue connection failed: %s" (Error.to_string_hum err);
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

    (* Start background task to receive messages and handle ping/pong *)
    Log.Global.info "Bitrue: Starting receive loop in background";
    let receive_count = ref 0 in
    let pong_count = ref 0 in
    don't_wait_for (
      let rec receive_loop () =
        match t.active with
        | false ->
          Log.Global.info "Bitrue: Receive loop ending (inactive)";
          Pipe.close t.message_writer;
          return ()
        | true ->
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            Log.Global.info "Bitrue: Connection closed by server";
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            receive_count := !receive_count + 1;
            (match !receive_count mod 50 = 0 with
             | true -> Log.Global.info "Bitrue: Received %d messages so far" !receive_count
             | false -> ());

            (* Decompress gzip-compressed messages *)
            let%bind decompressed_payload =
              match String.length payload >= 2 &&
                 Char.(payload.[0] = '\x1F') &&
                 Char.(payload.[1] = '\x8B') with
              | true ->
                (* Gzip-compressed message - decompress using gunzip in thread pool *)
                In_thread.run (fun () ->
                  try
                    let (ic, oc) = Core_unix.open_process "gunzip -c" in
                    Out_channel.output_string oc payload;
                    Out_channel.flush oc;
                    Out_channel.close oc;
                    let result = In_channel.input_all ic in
                    ignore (Core_unix.close_process (ic, oc));
                    match String.length result > 0 with true -> result | false -> payload
                  with exn ->
                    Core.eprintf "Bitrue: Decompression failed: %s\n%!" (Exn.to_string exn);
                    payload
                )
              | false ->
                return payload
            in

            (* Check if this is a ping message - if so, respond with pong *)
            (try
              let json = Yojson.Safe.from_string decompressed_payload in
              match Yojson.Safe.Util.member "ping" json with
              | `Int ts ->
                pong_count := !pong_count + 1;
                Log.Global.info "Bitrue: Received ping #%d, responding with pong" !pong_count;
                let pong_msg = sprintf "{\"pong\":%d}" ts in
                don't_wait_for (Websocket_curl.send ws pong_msg);
                receive_loop ()  (* Don't forward ping to application *)
              | `Intlit ts ->
                pong_count := !pong_count + 1;
                Log.Global.info "Bitrue: Received ping #%d, responding with pong" !pong_count;
                let pong_msg = sprintf "{\"pong\":%s}" ts in
                don't_wait_for (Websocket_curl.send ws pong_msg);
                receive_loop ()  (* Don't forward ping to application *)
              | _ ->
                (* Not a ping, forward to application *)
                let%bind () = Pipe.write t.message_writer decompressed_payload in
                receive_loop ()
            with _ ->
              (* Parse error, forward raw message *)
              let%bind () = Pipe.write t.message_writer decompressed_payload in
              receive_loop ()
            )
      in
      receive_loop ()
    );

    (* Send subscription messages for each stream *)
    Log.Global.info "Bitrue: Subscribing to %d stream(s)" (List.length streams);
    let%bind () =
      Deferred.List.iter ~how:`Sequential streams ~f:(fun stream ->
        let msg = Ws.Stream.to_subscribe_message stream in
        let msg_str = Yojson.Safe.to_string msg in
        Log.Global.info "Bitrue: Subscribing to %s" (Ws.Stream.to_channel stream);
        Websocket_curl.send ws msg_str
      )
    in

    Log.Global.info "Bitrue: Connection established and subscribed";
    return (Ok t)

(** Get the message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Subscribe to an additional stream *)
let subscribe t (stream : Ws.Stream.t) : unit Deferred.t =
  t.streams := stream :: !(t.streams);
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = Ws.Stream.to_subscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Unsubscribe from a stream *)
let unsubscribe t (stream : Ws.Stream.t) : unit Deferred.t =
  t.streams := List.filter !(t.streams) ~f:(fun s -> not (phys_equal s stream));
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = Ws.Stream.to_unsubscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

(** Close the WebSocket connection *)
let close t : unit Deferred.t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

(** Get connection endpoint *)
let endpoint t = Uri.to_string t.uri

(** Get subscribed streams *)
let streams t = !(t.streams)

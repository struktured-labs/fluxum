(** OKX Private WebSocket - Authenticated order/account streams
    Connects to wss://ws.okx.com:8443/ws/v5/private, authenticates via
    HMAC-SHA256 login, and subscribes to orders and account channels. *)

open Core
open Async

(** Connect to OKX private WebSocket and return a pipe of parsed messages.
    Sends login message, waits for success, then subscribes to orders channel. *)
let connect ~cfg:(module Cfg : Cfg.S) ()
  : (Ws.Private.message Pipe.Reader.t, [> `Auth_error of string | `Connection_error of string ]) Deferred.Result.t =
  let url = Ws.Endpoint.private_production in
  Websocket_curl.connect ~url >>= function
  | Error err ->
    return (Error (`Connection_error (Error.to_string_hum err)))
  | Ok ws ->
    let reader, writer = Pipe.create () in

    (* Step 1: Send login message *)
    let login_msg = Ws.Auth.login_message
      ~api_key:Cfg.api_key
      ~api_secret:Cfg.api_secret
      ~passphrase:Cfg.api_passphrase
    in
    let%bind () = Websocket_curl.send ws login_msg in
    Log.Global.info "OKX Private WS: login message sent";

    (* Step 2: Wait for login response *)
    let%bind login_result =
      Websocket_curl.receive ws >>| function
      | None -> Error (`Auth_error "Connection closed before login response")
      | Some payload ->
        match Ws.Private.parse_message payload with
        | Ws.Private.LoginSuccess ->
          Log.Global.info "OKX Private WS: login successful";
          Ok ()
        | Ws.Private.LoginError err ->
          Log.Global.error "OKX Private WS: login failed: %s" err;
          Error (`Auth_error err)
        | _ ->
          (* Might be a subscription confirmation or other message, treat as ok *)
          Log.Global.info "OKX Private WS: unexpected first message, continuing";
          Ok ()
    in

    match login_result with
    | Error _ as err ->
      let%bind () = Websocket_curl.close ws in
      return err
    | Ok () ->
      (* Step 3: Subscribe to orders channel *)
      let%bind () = Websocket_curl.send ws (Ws.Private.subscribe_orders ()) in
      Log.Global.info "OKX Private WS: subscribed to orders";

      (* Step 4: Receive loop - parse and dispatch *)
      don't_wait_for (
        let rec receive_loop () =
          Websocket_curl.receive ws >>= function
          | None ->
            Log.Global.info "OKX Private WS: connection closed";
            Pipe.close writer;
            return ()
          | Some payload ->
            let msg = Ws.Private.parse_message payload in
            (match msg with
            | Ws.Private.Orders _ | Ws.Private.Account _ ->
              Pipe.write writer msg >>= receive_loop
            | Ws.Private.LoginSuccess | Ws.Private.LoginError _ ->
              receive_loop ()
            | Ws.Private.Unknown _ ->
              receive_loop ())
        in
        receive_loop ()
      );

      return (Ok reader)

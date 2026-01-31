(** Kraken Private WebSocket - Authenticated order/trade streams
    Connects to wss://ws-auth.kraken.com/, authenticates with a token obtained
    via the GetWebSocketsToken REST endpoint, and subscribes to openOrders
    and ownTrades channels. *)

open Core
open Async

(** Connect to Kraken private WebSocket and return a pipe of parsed messages.
    Obtains a WS token via REST, connects to the auth endpoint, subscribes
    to openOrders and ownTrades, and dispatches parsed events. *)
let connect ~cfg:(module Cfg : Cfg.S) ()
  : (Ws.Private.message Pipe.Reader.t, [> `Token_error of string | `Connection_error of string ]) Deferred.Result.t =
  (* Step 1: Get WebSocket token from REST API *)
  V1.Websocket_token.post (module Cfg) () >>= function
  | `Ok { token } ->
    Log.Global.info "Kraken Private WS: obtained token";
    (* Step 2: Connect to private WS endpoint *)
    let url = Ws.Endpoint.private_url in
    Websocket_curl.connect ~url >>= (function
    | Error err ->
      return (Error (`Connection_error (Error.to_string_hum err)))
    | Ok ws ->
      let reader, writer = Pipe.create () in

      (* Step 3: Subscribe to openOrders and ownTrades *)
      let%bind () = Websocket_curl.send ws (Ws.Private.auth_message ~signature:token) in
      let%bind () = Websocket_curl.send ws (Ws.Private.auth_trades_message ~signature:token) in
      Log.Global.info "Kraken Private WS: subscribed to openOrders and ownTrades";

      (* Step 4: Receive loop - parse and dispatch *)
      don't_wait_for (
        let rec receive_loop () =
          Websocket_curl.receive ws >>= function
          | None ->
            Log.Global.info "Kraken Private WS: connection closed";
            Pipe.close writer;
            return ()
          | Some payload ->
            (match Ws.parse_message payload with
            | Ok (Ws.Private msg) ->
              Pipe.write writer msg >>= receive_loop
            | Ok (Ws.System (Ws.System.Heartbeat)) ->
              receive_loop ()
            | Ok (Ws.System _) ->
              receive_loop ()
            | Ok _ ->
              receive_loop ()
            | Error _ ->
              receive_loop ())
        in
        receive_loop ()
      );

      return (Ok reader))
  | #Rest.Error.post as err ->
    let msg = sprintf "Kraken Private WS: token request failed: %s"
      (Sexp.to_string (Rest.Error.sexp_of_post err)) in
    Log.Global.error "%s" msg;
    return (Error (`Token_error msg))

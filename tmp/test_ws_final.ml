open Core
open Async

let test () =
  let open Deferred.Let_syntax in
  printf "Testing WebSocket with libcurl to Kraken...\n%!";
  match%bind Websocket_curl.connect ~url:"wss://ws.kraken.com/" with
  | Error e ->
    printf "Connection failed: %s\n" (Error.to_string_hum e);
    exit 1
  | Ok ws ->
    printf "✓ SUCCESS! Connected to Kraken via libcurl\n";
    printf "✓ Bypassed Cloudflare's TLS fingerprinting!\n%!";

    (* Send subscribe message *)
    let subscribe = {|{"event":"subscribe","pair":["BTC/USD"],"subscription":{"name":"ticker"}}|} in
    printf "\nSending subscription: %s\n%!" subscribe;
    let%bind () = Websocket_curl.send ws subscribe in

    (* Receive a few messages *)
    let rec receive_loop count =
      if count >= 3 then return ()
      else
        match%bind Websocket_curl.receive ws with
        | None ->
          printf "Connection closed\n";
          return ()
        | Some msg ->
          printf "Received [%d]: %s\n%!" count msg;
          receive_loop (count + 1)
    in
    let%bind () = receive_loop 0 in

    Websocket_curl.close ws

let () =
  Command.async
    ~summary:"Test curl WebSocket with Kraken"
    (Command.Param.return test)
  |> Command_unix.run

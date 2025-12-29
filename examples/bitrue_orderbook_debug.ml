open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Testing Bitrue WebSocket (raw messages)...\\n%!";

  let symbol_str = "BTCUSDT" in
  let streams = [Bitrue.Ws.Stream.Depth symbol_str] in
  let%bind md_result = Bitrue.Market_data_curl.connect ~streams () in

  match md_result with
  | Error err ->
    eprintf "Connection failed: %s\\n%!" err;
    return ()
  | Ok md ->
    printf "✓ Connected to Bitrue\\n%!";

    let msg_count = ref 0 in
    let%bind () =
      Pipe.iter (Bitrue.Market_data_curl.messages md) ~f:(fun msg_str ->
        msg_count := !msg_count + 1;
        printf "\\n[Message #%d]\\n%s\\n%!" !msg_count msg_str;

        (* Try to parse *)
        let parsed = Bitrue.Ws.parse_message msg_str in
        printf "Parsed as: %s\\n%!" (Sexp.to_string_hum (Bitrue.Ws.Message.sexp_of_t parsed));

        return ()
      )
    in

    printf "Stream ended\\n%!";
    return ()

let () =
  Command.async
    ~summary:"Bitrue WebSocket debug"
    (Command.Param.return test)
  |> Command_unix.run

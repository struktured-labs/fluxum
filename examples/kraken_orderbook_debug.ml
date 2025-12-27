open Core
open Async
open Kraken

let test () =
  let open Deferred.Let_syntax in

  printf "Connecting to Kraken WebSocket for order book...\n%!";

  let%bind md_result =
    Market_data.connect
      ~subscriptions:[{
        channel = "book";
        pairs = ["XBT/USD"];
        interval = None;
        depth = Some 10;
      }]
      ()
  in

  match md_result with
  | Error err ->
    printf "Connection error: %s\n%!" (Sexp.to_string (Ws.Error.sexp_of_t err));
    return ()
  | Ok md ->
    printf "Connected! Waiting for messages...\n%!";

    let messages = Market_data.messages md in
    let count = ref 0 in

    let%bind () =
      Pipe.iter messages ~f:(fun msg ->
        count := !count + 1;
        printf "\n=== Message %d ===\n%s\n%!" !count msg;

        (* Stop after 5 messages *)
        if !count >= 5 then (
          printf "\nClosing connection...\n%!";
          Market_data.close md
        ) else
          return ()
      )
    in

    printf "Done!\n%!";
    return ()

let () =
  Command.async
    ~summary:"Debug Kraken order book WebSocket"
    (Command.Param.return test)
  |> Command_unix.run

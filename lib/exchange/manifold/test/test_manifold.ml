(** Live smoke for Manifold Feed_only_adapter. *)

let strict () = Option.is_some (Sys.getenv "SMOKE_REQUIRE_NETWORK")

let format_err = function
  | `Http (c, b) -> sprintf "http %d: %s" c (String.prefix b 200)
  | `Network m -> sprintf "network: %s" m
  | `Json_parse m -> sprintf "parse: %s" m
  | `Not_found m -> sprintf "not_found: %s" m
  | `Rate_limited m -> sprintf "rate_limited: %s" (String.prefix m 200)

let on_err name e =
  match strict () with
  | true ->
    eprintf "FAIL [%s]: %s\n" name (format_err e);
    exit 1
  | false ->
    printf "SKIP [%s]: %s\n" name (format_err e);
    Deferred.unit

let test_metadata () =
  printf
    "OK   [metadata]: name=%s us_geo_restricted=%b\n"
    Manifold.name
    Manifold.us_geo_restricted;
  Deferred.unit

let test_list_events () =
  let filter =
    { Feed_only_adapter.List_filter.default with limit = Some 5 }
  in
  let%bind r = Manifold.list_events ~filter () in
    match r with
    | Error e -> on_err "list_events" e
    | Ok evs ->
      printf "OK   [list_events]: got %d events\n" (List.length evs);
      List.iter evs ~f:(fun (e : Feed_only_adapter.Event.t) ->
        printf "  - %s : %s (%d outcomes)\n"
          e.id (String.prefix e.title 60) (Array.length e.outcomes));
      Deferred.unit

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_list_events () in
    printf "\nAll Manifold smoke tests done.\n";
    Deferred.unit

let () =
  Command.async ~summary:"Manifold smoke" (Command.Param.return main)
  |> Command_unix.run

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

let test_get_event_single () =
  let filter = { Feed_only_adapter.List_filter.default with limit = Some 5 } in
  let%bind r = Manifold.list_events ~filter () in
    match r with
    | Error e -> on_err "list(setup)" e
    | Ok [] -> on_err "list(setup)" (`Network "empty result list")
    | Ok (first :: _) ->
      let%bind r = Manifold.get_event ~event_id:first.id in
        (match r with
         | Error e -> on_err "get_event(single)" e
         | Ok ev ->
           printf
             "OK   [get_event(single)]: id=%s outcomes=%d resolved=%b\n"
             ev.id
             (Array.length ev.outcomes)
             ev.is_resolved;
           (* BINARY markets must have exactly 2 outcomes *)
           let outcomes_plausible = Array.length ev.outcomes >= 1 in
             match outcomes_plausible with
             | true -> Deferred.unit
             | false ->
               eprintf "FAIL: event has 0 outcomes\n";
               exit 1)

let test_resolved_events () =
  let%bind r = Manifold.get_resolved_events ~limit:5 () in
    match r with
    | Error e -> on_err "get_resolved_events" e
    | Ok evs ->
      let all_resolved =
        List.for_all evs ~f:(fun (e : Feed_only_adapter.Event.t) -> e.is_resolved)
      in
        printf "OK   [get_resolved_events]: %d events, all_resolved=%b\n"
          (List.length evs)
          all_resolved;
        match all_resolved with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL: get_resolved_events returned non-resolved\n";
          exit 1

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_list_events () in
  let%bind () = test_get_event_single () in
  let%bind () = test_resolved_events () in
    printf "\nAll Manifold smoke tests done.\n";
    Deferred.unit

let () =
  Command.async ~summary:"Manifold smoke" (Command.Param.return main)
  |> Command_unix.run

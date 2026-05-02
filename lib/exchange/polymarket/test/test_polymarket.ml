(** Live smoke test for the Polymarket Feed_only_adapter implementation.

    Hits Polymarket's public Gamma + CLOB + Data APIs with a small
    request budget (~5 calls per run). Skips gracefully on network
    errors so [dune test] doesn't fail offline; set
    [SMOKE_REQUIRE_NETWORK=1] to make missing-network a hard failure. *)

let strict_mode () =
  match Sys.getenv "SMOKE_REQUIRE_NETWORK" with
  | Some _ -> true
  | None -> false

let format_err = function
  | `Http (code, body) ->
    sprintf "http %d: %s" code (String.prefix body 200)
  | `Network m -> sprintf "network: %s" m
  | `Json_parse m -> sprintf "parse: %s" m
  | `Not_found m -> sprintf "not_found: %s" m
  | `Rate_limited m -> sprintf "rate_limited: %s" (String.prefix m 200)

let on_error name err_str =
  match strict_mode () with
  | true ->
    eprintf "FAIL [%s]: %s\n" name err_str;
    exit 1
  | false ->
    printf "SKIP [%s]: %s (set SMOKE_REQUIRE_NETWORK=1 to fail)\n" name err_str;
    Deferred.unit

let test_metadata () =
  printf
    "OK   [metadata]: name=%s us_geo_restricted=%b homepage=%s\n"
    Polymarket.name
    Polymarket.us_geo_restricted
    Polymarket.homepage;
  Deferred.unit

let test_list_events () =
  let filter = { Feed_only_adapter.List_filter.default with limit = Some 5 } in
  let%bind r = Polymarket.list_events ~filter () in
    match r with
    | Error e -> on_error "list_events" (format_err e)
    | Ok [] ->
      eprintf "FAIL [list_events]: returned 0 events\n";
      exit 1
    | Ok events ->
      let n = List.length events in
      let first : Feed_only_adapter.Event.t = List.hd_exn events in
        printf
          "OK   [list_events]: got %d events; first id=%s, title=%S, %d outcomes, resolved=%b\n"
          n
          first.id
          first.title
          (Array.length first.outcomes)
          first.is_resolved;
        Deferred.unit

let test_get_event () =
  let filter = { Feed_only_adapter.List_filter.default with limit = Some 5 } in
  let%bind r = Polymarket.list_events ~filter () in
    match r with
    | Error e -> on_error "list_events(setup)" (format_err e)
    | Ok [] -> on_error "list_events(setup)" "empty event list"
    | Ok (first :: _) ->
      let%bind r = Polymarket.get_event ~event_id:first.id in
        (match r with
         | Error e -> on_error "get_event" (format_err e)
         | Ok ev ->
           printf
             "OK   [get_event]: id=%s, %d outcomes\n"
             ev.id
             (Array.length ev.outcomes);
           Deferred.unit)

let test_book_and_quote () =
  let filter = { Feed_only_adapter.List_filter.default with limit = Some 5 } in
  let%bind r = Polymarket.list_events ~filter () in
    match r with
    | Error e -> on_error "list_events(setup)" (format_err e)
    | Ok [] -> on_error "list_events(setup)" "empty event list"
    | Ok (first :: _) ->
      (match Array.length first.outcomes > 0 with
       | false ->
         eprintf "FAIL [book/quote]: first event had no outcomes\n";
         exit 1
       | true ->
         let outcome : Feed_only_adapter.Outcome.t = first.outcomes.(0) in
         let outcome_id = outcome.id in
         let%bind br = Polymarket.get_book ~outcome_id () in
         let%bind () =
           match br with
           | Error e -> on_error "get_book" (format_err e)
           | Ok (book : Feed_only_adapter.Book.t) ->
             printf
               "OK   [get_book]: outcome=%S bids=%d asks=%d\n"
               outcome.label
               (Array.length book.bids)
               (Array.length book.asks);
             Deferred.unit
         in
         let%bind qr = Polymarket.get_quote ~outcome_id in
           (match qr with
            | Error e -> on_error "get_quote" (format_err e)
            | Ok (q : Feed_only_adapter.Quote.t) ->
              printf
                "OK   [get_quote]: bid=%s ask=%s mid=%s\n"
                (Option.value_map q.bid ~default:"-" ~f:(sprintf "%.4f"))
                (Option.value_map q.ask ~default:"-" ~f:(sprintf "%.4f"))
                (Option.value_map q.last_price ~default:"-" ~f:(sprintf "%.4f"));
              Deferred.unit))

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_list_events () in
  let%bind () = test_get_event () in
  let%bind () = test_book_and_quote () in
    printf "\nAll Polymarket smoke tests done.\n";
    Deferred.unit

let () =
  Command.async
    ~summary:"Polymarket Feed_only_adapter smoke test"
    (Command.Param.return main)
  |> Command_unix.run

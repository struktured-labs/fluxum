(** Live smoke for DefiLlama Aggregate_feed adapter. *)

let strict () = Option.is_some (Sys.getenv "SMOKE_REQUIRE_NETWORK")

let format_err = function
  | `Http (c, b) -> sprintf "http %d: %s" c (String.prefix b 200)
  | `Network m -> sprintf "network: %s" m
  | `Json_parse m -> sprintf "parse: %s" m
  | `Not_found m -> sprintf "not_found: %s" m
  | `Rate_limited m -> sprintf "rate_limited: %s" (String.prefix m 200)
  | `Entity_unknown s -> sprintf "entity_unknown: %s" s

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
    Defillama.name
    Defillama.us_geo_restricted;
  Deferred.unit

let test_list_top_dex () =
  let%bind r = Defillama.list_entities ~category:"DEX" ~limit:5 () in
    match r with
    | Error e -> on_err "list_entities(DEX)" e
    | Ok protocols ->
      printf "OK   [list_entities(DEX)]: top %d by TVL\n" (List.length protocols);
      List.iter protocols ~f:(fun (p : Feed_only_adapter.Aggregate_feed.Stats.t) ->
        printf "  - %s : $%s TVL\n"
          p.name
          (Option.value_map p.tvl_usd ~default:"?" ~f:(sprintf "%.0f")));
      Deferred.unit

let test_get_uniswap () =
  let%bind r = Defillama.get_entity ~slug:"uniswap" in
    match r with
    | Error e -> on_err "get_entity(uniswap)" e
    | Ok (s : Feed_only_adapter.Aggregate_feed.Stats.t) ->
      printf
        "OK   [get_entity(uniswap)]: %s, TVL=$%s\n"
        s.name
        (Option.value_map s.tvl_usd ~default:"?" ~f:(sprintf "%.0f"));
      Deferred.unit

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_list_top_dex () in
  let%bind () = test_get_uniswap () in
    printf "\nAll DefiLlama smoke tests done.\n";
    Deferred.unit

let () =
  Command.async ~summary:"DefiLlama smoke" (Command.Param.return main)
  |> Command_unix.run

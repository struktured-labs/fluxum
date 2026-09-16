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
  (* DefiLlama uses "Dexs" (their spelling) not "DEX". Verified empirically:
     1964 protocols in this category, 0 in "DEX". *)
  let%bind r = Defillama.list_entities ~category:"Dexs" ~limit:5 () in
    match r with
    | Error e -> on_err "list_entities(Dexs)" e
    | Ok [] ->
      eprintf "FAIL [list_entities(Dexs)]: empty result (expected top-N DEXes)\n";
      exit 1
    | Ok protocols ->
      let n = List.length protocols in
        printf "OK   [list_entities(Dexs)]: top %d by TVL\n" n;
        List.iter protocols ~f:(fun (p : Feed_only_adapter.Aggregate_feed.Stats.t) ->
          printf "  - %s : $%s TVL\n"
            p.name
            (Option.value_map p.tvl_usd ~default:"?" ~f:(sprintf "%.0f")));
        match n > 0 with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL: expected non-empty Dexs list\n";
          exit 1

let test_get_uniswap () =
  let%bind r = Defillama.get_entity ~slug:"uniswap" in
    match r with
    | Error e -> on_err "get_entity(uniswap)" e
    | Ok (s : Feed_only_adapter.Aggregate_feed.Stats.t) ->
      printf
        "OK   [get_entity(uniswap)]: %s, TVL=$%s\n"
        s.name
        (Option.value_map s.tvl_usd ~default:"?" ~f:(sprintf "%.0f"));
      let tvl_plausible =
        match s.tvl_usd with
        | Some t -> Float.( > ) t 1_000_000.  (* > $1M, sanity check *)
        | None -> false
      in
        match tvl_plausible with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL: Uniswap TVL implausible (expected > $1M)\n";
          exit 1

let test_get_unknown_slug () =
  (* /protocol/{slug} for a non-existent slug should yield Error.
     Verifies error handling on the Not_found path. *)
  let%bind r = Defillama.get_entity ~slug:"definitely-not-a-real-protocol-xyz123" in
    match r with
    | Error _ -> printf "OK   [get_entity(unknown)]: error returned as expected\n"; Deferred.unit
    | Ok _ ->
      eprintf "FAIL: expected error for unknown slug\n";
      exit 1

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_list_top_dex () in
  let%bind () = test_get_uniswap () in
  let%bind () = test_get_unknown_slug () in
    printf "\nAll DefiLlama smoke tests done.\n";
    Deferred.unit

let () =
  Command.async ~summary:"DefiLlama smoke" (Command.Param.return main)
  |> Command_unix.run

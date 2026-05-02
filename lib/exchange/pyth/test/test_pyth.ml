(** Live smoke test for Pyth Spot_feed adapter.
    Hits Hermes mainnet API; ~3 calls per run. *)

let strict () =
  match Sys.getenv "SMOKE_REQUIRE_NETWORK" with
  | Some _ -> true
  | None -> false

let format_err = function
  | `Http (code, body) -> sprintf "http %d: %s" code (String.prefix body 200)
  | `Network m -> sprintf "network: %s" m
  | `Json_parse m -> sprintf "parse: %s" m
  | `Not_found m -> sprintf "not_found: %s" m
  | `Rate_limited m -> sprintf "rate_limited: %s" (String.prefix m 200)
  | `Symbol_unknown s -> sprintf "symbol_unknown: %s" s

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
    Pyth.name
    Pyth.us_geo_restricted;
  Deferred.unit

let btc_usd_feed_id =
  "e62df6c8b4a85fe1a67db44dc12de5db330f7ac66b72dc658afedf0f4a415b43"

let test_get_btc_price () =
  let%bind r = Pyth.get_price ~symbol:btc_usd_feed_id in
    match r with
    | Error e -> on_err "get_price(BTC/USD)" e
    | Ok (p : Feed_only_adapter.Spot_feed.Price.t) ->
      let plausible = Float.( > ) p.price 1000. && Float.( < ) p.price 10_000_000. in
        printf
          "OK   [get_price(BTC/USD)]: $%.2f (conf=%s)\n"
          p.price
          (Option.value_map p.confidence ~default:"-" ~f:(sprintf "%.2f"));
        match plausible with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL price implausible: %f\n" p.price;
          exit 1

let test_list_symbols () =
  let%bind r = Pyth.list_symbols ~query:"BTC" ~limit:5 () in
    match r with
    | Error e -> on_err "list_symbols(BTC)" e
    | Ok syms ->
      printf "OK   [list_symbols(BTC)]: %d results\n" (List.length syms);
      List.iter syms ~f:(fun (s : Feed_only_adapter.Spot_feed.Symbol_info.t) ->
        printf "  - %s [%s]\n"
          s.symbol
          (Option.value s.asset_class ~default:"?"));
      Deferred.unit

let main () =
  let%bind () = test_metadata () in
  let%bind () = test_get_btc_price () in
  let%bind () = test_list_symbols () in
    printf "\nAll Pyth smoke tests done.\n";
    Deferred.unit

let () =
  Command.async ~summary:"Pyth smoke" (Command.Param.return main)
  |> Command_unix.run

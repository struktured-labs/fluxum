(** DefiLlama — read-only TVL/volume/fees aggregator for DeFi protocols.

    Implements {!Feed_only_adapter.Aggregate_feed.S} against
    [api.llama.fi] (free tier). Public no-auth.

    Coverage: 7000+ protocols across 200+ chains as of 2026-05.
    Categories: DEX, Lending, CDP, Yield, CEX, Bridge, Liquid Staking,
    Perps, etc.

    Use cases:
    - Cross-chain protocol discovery (which chains is X live on?)
    - Protocol-revenue benchmarking (compare fees / TVL ratios)
    - DEX volume aggregation (alternative to per-DEX-adapter polling)
    - Sanity checks ("is the TVL bluxit-gemini sees on-chain consistent
      with DefiLlama's number?") *)

module Agg = Feed_only_adapter.Aggregate_feed

let name = "defillama"
let homepage = "https://defillama.com"
let us_geo_restricted = false

let api_host = "api.llama.fi"

(* --- HTTP --- *)

let http_get ~path ?query () =
  let uri = Uri.make ~scheme:"https" ~host:api_host ~path ?query () in
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
    |> Deferred.bind ~f:(function
      | Error err -> return (Error (`Network (Core.Error.to_string_hum err)))
      | Ok (response, body) ->
        let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
        Cohttp_async.Body.to_string body
        >>| fun body_str ->
          (match code with
           | 200 ->
             (try Ok (Yojson.Safe.from_string body_str) with
              | Yojson.Json_error msg ->
                Error (`Json_parse (sprintf "JSON parse error: %s" msg)))
           | 404 -> Error (`Not_found body_str)
           | 429 -> Error (`Rate_limited body_str)
           | _ -> Error (`Http (code, body_str))))

let field name j =
  match j with
  | `Assoc fs -> List.Assoc.find fs ~equal:String.equal name
  | _ -> None

let str_field n j =
  match field n j with
  | Some (`String s) -> Some s
  | _ -> None

let float_field n j =
  match field n j with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (Float.of_int i)
  | Some `Null -> None
  | _ -> None

(** Parse one /protocols entry into Stats.t. *)
let parse_protocol (json : Yojson.Safe.t) : Agg.Stats.t option =
  let slug =
    match str_field "slug" json with
    | Some s -> Some s
    | None -> str_field "id" json
  in
    match slug with
    | None -> None
    | Some slug ->
      let name = Option.value (str_field "name" json) ~default:slug in
      let category = str_field "category" json in
      let chain = str_field "chain" json in
      let tvl_usd = float_field "tvl" json in
      let venue_metadata =
        List.filter_opt
          [ Option.map (str_field "symbol" json) ~f:(fun s -> "symbol", s)
          ; Option.map (str_field "url" json) ~f:(fun s -> "url", s)
          ; (match field "chains" json with
             | Some (`List items) ->
               let names =
                 List.filter_map items ~f:(function
                   | `String s -> Some s
                   | _ -> None)
                 |> String.concat ~sep:","
               in
                 Some ("chains", names)
             | _ -> None)
          ]
      in
        Some
          { Agg.Stats.slug
          ; name
          ; category
          ; chain
          ; tvl_usd
          ; volume_24h_usd = None
          ; fees_24h_usd = None
          ; revenue_24h_usd = None
          ; ts = Time_float_unix.now ()
          ; venue_metadata
          }

(* --- Public API --- *)

let list_entities ?category ?(limit = 50) () =
  http_get ~path:"/protocols" ()
  >>| function
  | Error e -> Error e
  | Ok (`List items) ->
    let parsed = List.filter_map items ~f:parse_protocol in
    let filtered =
      match category with
      | None -> parsed
      | Some cat ->
        List.filter parsed ~f:(fun (s : Agg.Stats.t) ->
          match s.category with
          | Some c -> String.equal c cat
          | None -> false)
    in
    (* Sort by TVL descending, take top N *)
    let sorted =
      List.sort filtered ~compare:(fun (a : Agg.Stats.t) b ->
        match a.tvl_usd, b.tvl_usd with
        | Some x, Some y -> Float.compare y x
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0)
    in
      Ok (List.take sorted limit)
  | Ok _ -> Error (`Json_parse "Expected JSON array from /protocols")

let get_entity ~slug =
  http_get ~path:(sprintf "/protocol/%s" slug) ()
  >>| function
  | Error e -> Error e
  | Ok json ->
    (* /protocol/{slug} returns a richer object with currentChainTvls etc. *)
    let name = Option.value (str_field "name" json) ~default:slug in
    let category = str_field "category" json in
    let chain = str_field "chain" json in
    let tvl_usd =
      (* /protocol returns currentChainTvls; sum all chains *)
      match field "currentChainTvls" json with
      | Some (`Assoc entries) ->
        let total =
          List.fold entries ~init:0. ~f:(fun acc (_, v) ->
            match v with
            | `Float f -> acc +. f
            | `Int i -> acc +. Float.of_int i
            | _ -> acc)
        in
          Some total
      | _ -> float_field "tvl" json
    in
      Ok
        { Agg.Stats.slug
        ; name
        ; category
        ; chain
        ; tvl_usd
        ; volume_24h_usd = None
        ; fees_24h_usd = None
        ; revenue_24h_usd = None
        ; ts = Time_float_unix.now ()
        ; venue_metadata = []
        }

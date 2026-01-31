(** Uniswap V3 Subgraph API Client

    @see <https://docs.uniswap.org/api/subgraph/guides/examples>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
  | `GraphQL of string
] [@@deriving sexp]

let make_graphql_request ~cfg ~query =
  let uri = Uri.of_string cfg.Cfg.subgraph_url in
  let headers = Cohttp.Header.init () in
  let headers = Cohttp.Header.add headers "Content-Type" "application/json" in
  let headers = match cfg.Cfg.api_key with
    | Some key -> Cohttp.Header.add headers "Authorization" (sprintf "Bearer %s" key)
    | None -> headers
  in
  let body = `Assoc [("query", `String query)] |> Yojson.Safe.to_string in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post
        ~body:(Cohttp_async.Body.of_string body)
        ~headers
        uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    match code with
    | 200 ->
      (try
        let json = Yojson.Safe.from_string body_str in
        match json with
        | `Assoc fields ->
          (match List.Assoc.find fields ~equal:String.equal "errors" with
           | Some errors -> return (Error (`GraphQL (Yojson.Safe.to_string errors)))
           | None ->
             (match List.Assoc.find fields ~equal:String.equal "data" with
              | Some data -> return (Ok data)
              | None -> return (Error (`Json_parse "No data field in response"))))
        | _ -> return (Error (`Json_parse "Expected object at root"))
      with ex -> return (Error (`Json_parse (Exn.to_string ex))))
    | _ -> return (Error (`Http (code, body_str)))

let pool_by_id ~cfg ~pool_id =
  let query = sprintf {|
    {
      pool(id: "%s") {
        id
        token0 {
          id
          symbol
          name
          decimals
        }
        token1 {
          id
          symbol
          name
          decimals
        }
        feeTier
        liquidity
        sqrtPrice
        tick
        volumeUSD
        txCount
      }
    }
  |} pool_id in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "pool" with
         | Some pool_json ->
           (match Types.pool_of_yojson pool_json with
            | Ok pool -> return (Ok pool)
            | Error msg -> return (Error (`Json_parse msg)))
         | None -> return (Error (`Json_parse "No pool field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let pools ~cfg ?(first = 100) ?(skip = 0) () =
  let query = sprintf {|
    {
      pools(first: %d, skip: %d, orderBy: volumeUSD, orderDirection: desc) {
        id
        token0 {
          id
          symbol
          name
          decimals
        }
        token1 {
          id
          symbol
          name
          decimals
        }
        feeTier
        liquidity
        sqrtPrice
        tick
        volumeUSD
        txCount
      }
    }
  |} first skip in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "pools" with
         | Some (`List pools_json) ->
           let pool_results = List.map pools_json ~f:Types.pool_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok pool :: rest -> collect_results (pool :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] pool_results)
         | Some _ -> return (Error (`Json_parse "Expected pools array"))
         | None -> return (Error (`Json_parse "No pools field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let token ~cfg ~token_address =
  let query = sprintf {|
    {
      token(id: "%s") {
        id
        symbol
        name
        decimals
      }
    }
  |} token_address in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "token" with
         | Some token_json ->
           (match Types.token_of_yojson token_json with
            | Ok token -> return (Ok token)
            | Error msg -> return (Error (`Json_parse msg)))
         | None -> return (Error (`Json_parse "No token field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let recent_swaps ~cfg ~pool_id ?(first = 100) () =
  let query = sprintf {|
    {
      swaps(first: %d, where: { pool: "%s" }, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        amount0
        amount1
        amountUSD
        sender
        recipient
      }
    }
  |} first pool_id in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "swaps" with
         | Some (`List swaps_json) ->
           let swap_results = List.map swaps_json ~f:Types.swap_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok swap :: rest -> collect_results (swap :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] swap_results)
         | Some _ -> return (Error (`Json_parse "Expected swaps array"))
         | None -> return (Error (`Json_parse "No swaps field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let pool_ticks ~cfg ~pool_id ?(first = 1000) () =
  let query = sprintf {|
    {
      ticks(first: %d, where: { pool: "%s" }, orderBy: tickIdx) {
        tickIdx
        liquidityGross
        liquidityNet
        price0
        price1
      }
    }
  |} first pool_id in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "ticks" with
         | Some (`List ticks_json) ->
           let tick_results = List.map ticks_json ~f:Types.tick_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok tick :: rest -> collect_results (tick :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] tick_results)
         | Some _ -> return (Error (`Json_parse "Expected ticks array"))
         | None -> return (Error (`Json_parse "No ticks field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let pools_for_pair ~cfg ~token0 ~token1 ?(first = 10) () =
  let token0_lower = String.lowercase token0 in
  let token1_lower = String.lowercase token1 in
  let query = sprintf {|
    {
      pools(first: %d, where: {
        or: [
          { token0_: { symbol_contains_nocase: "%s" }, token1_: { symbol_contains_nocase: "%s" } },
          { token0_: { symbol_contains_nocase: "%s" }, token1_: { symbol_contains_nocase: "%s" } }
        ]
      }, orderBy: volumeUSD, orderDirection: desc) {
        id
        token0 {
          id
          symbol
          name
          decimals
        }
        token1 {
          id
          symbol
          name
          decimals
        }
        feeTier
        liquidity
        sqrtPrice
        tick
        volumeUSD
        txCount
      }
    }
  |} first token0_lower token1_lower token1_lower token0_lower in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "pools" with
         | Some (`List pools_json) ->
           let pool_results = List.map pools_json ~f:Types.pool_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok pool :: rest -> collect_results (pool :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] pool_results)
         | Some _ -> return (Error (`Json_parse "Expected pools array"))
         | None -> return (Error (`Json_parse "No pools field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

type pool_day_datum = {
  date : int;
  volumeUSD : float;
  tvlUSD : float;
  open_ : float [@key "open"];
  high : float;
  low : float;
  close : float;
} [@@deriving yojson { strict = false }]

let pool_day_data ~cfg ~pool_id ?(days = 7) () =
  let query = sprintf {|
    {
      poolDayDatas(first: %d, where: { pool: "%s" }, orderBy: date, orderDirection: desc) {
        date
        volumeUSD
        tvlUSD
        open
        high
        low
        close
      }
    }
  |} days pool_id in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "poolDayDatas" with
         | Some (`List day_json) ->
           let day_results = List.map day_json ~f:pool_day_datum_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok d :: rest -> collect_results (d :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] day_results)
         | Some _ -> return (Error (`Json_parse "Expected poolDayDatas array"))
         | None -> return (Error (`Json_parse "No poolDayDatas field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let swaps_for_sender ~cfg ~sender ?(first = 100) () =
  let query = sprintf {|
    {
      swaps(first: %d, where: { sender: "%s" }, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        amount0
        amount1
        amountUSD
        sender
        recipient
      }
    }
  |} first sender in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "swaps" with
         | Some (`List swaps_json) ->
           let swap_results = List.map swaps_json ~f:Types.swap_of_yojson in
           let rec collect_results acc = function
             | [] -> Ok (List.rev acc)
             | Ok swap :: rest -> collect_results (swap :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect_results [] swap_results)
         | Some _ -> return (Error (`Json_parse "Expected swaps array"))
         | None -> return (Error (`Json_parse "No swaps field")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

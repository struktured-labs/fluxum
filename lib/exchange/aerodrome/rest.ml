(** Aerodrome DEX Subgraph API Client *)

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
  let body = `Assoc [("query", `String query)] |> Yojson.Safe.to_string in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post ~body:(Cohttp_async.Body.of_string body) ~headers uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err -> `Network (Core.Error.to_string_hum err)))
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
              | None -> return (Error (`Json_parse "No data field"))))
        | _ -> return (Error (`Json_parse "Expected object"))
      with ex -> return (Error (`Json_parse (Exn.to_string ex))))
    | _ -> return (Error (`Http (code, body_str)))

let get_pools ~cfg ?(first = 100) () =
  let query = sprintf {|
    {
      pools(first: %d, orderBy: liquidity, orderDirection: desc) {
        id
        token0 { id symbol decimals }
        token1 { id symbol decimals }
        liquidity
        sqrtPrice
        tick
      }
    }
  |} first in
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
           let rec collect acc = function
             | [] -> Ok (List.rev acc)
             | Ok p :: rest -> collect (p :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect [] pool_results)
         | _ -> return (Error (`Json_parse "Expected pools array")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let get_pool ~cfg ~pool_id =
  let query = sprintf {|
    {
      pool(id: "%s") {
        id
        token0 { id symbol decimals }
        token1 { id symbol decimals }
        liquidity
        sqrtPrice
        tick
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

let get_swaps ~cfg ~pool_id ?(first = 100) () =
  let query = sprintf {|
    {
      swaps(first: %d, where: { pool: "%s" }, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        amount0
        amount1
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
           let rec collect acc = function
             | [] -> Ok (List.rev acc)
             | Ok s :: rest -> collect (s :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect [] swap_results)
         | _ -> return (Error (`Json_parse "Expected swaps array")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

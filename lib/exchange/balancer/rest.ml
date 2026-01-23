(** Balancer DEX Subgraph API Client

    @see <https://docs.balancer.fi/reference/subgraph/>
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

let get_pools ~cfg ?(first = 100) ?(skip = 0) () =
  let query = sprintf {|
    {
      pools(first: %d, skip: %d, orderBy: totalLiquidity, orderDirection: desc) {
        id
        address
        poolType
        swapFee
        totalLiquidity
        totalShares
        tokens {
          address
          symbol
          name
          decimals
          balance
          weight
        }
        swapsCount
        holdersCount
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

let get_pool ~cfg ~pool_id =
  let query = sprintf {|
    {
      pool(id: "%s") {
        id
        address
        poolType
        swapFee
        totalLiquidity
        totalShares
        tokens {
          address
          symbol
          name
          decimals
          balance
          weight
        }
        swapsCount
        holdersCount
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
      swaps(first: %d, where: { poolId: "%s" }, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        tokenIn
        tokenOut
        tokenAmountIn
        tokenAmountOut
        valueUSD
        userAddress {
          id
        }
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
           let swaps_json = List.map swaps_json ~f:(fun swap ->
             match swap with
             | `Assoc fields ->
               let fields = List.map fields ~f:(fun (k, v) ->
                 match k with
                 | "userAddress" ->
                   (match v with
                    | `Assoc user_fields ->
                      (match List.Assoc.find user_fields ~equal:String.equal "id" with
                       | Some addr -> (k, addr)
                       | None -> (k, `String ""))
                    | _ -> (k, `String ""))
                 | _ -> (k, v))
               in
               `Assoc fields
             | _ -> swap)
           in
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

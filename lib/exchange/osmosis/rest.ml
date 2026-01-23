(** Osmosis DEX LCD API Client

    @see <https://docs.osmosis.zone/apis/rest>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
] [@@deriving sexp]

let make_request ~cfg ~endpoint =
  let url = sprintf "%s%s" cfg.Cfg.api_url endpoint in
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.init () in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.get ~headers uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err -> `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    match code with
    | 200 -> return (Ok body_str)
    | _ -> return (Error (`Http (code, body_str)))

let get_pools ~cfg =
  let%bind json_str = make_request ~cfg ~endpoint:"/osmosis/gamm/v1beta1/pools" in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
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
     | _ -> return (Error (`Json_parse "Expected object")))
  | Error e -> return (Error e)

let get_pool ~cfg ~pool_id =
  let endpoint = sprintf "/osmosis/gamm/v1beta1/pools/%s" pool_id in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `Assoc fields ->
       (match List.Assoc.find fields ~equal:String.equal "pool" with
        | Some pool_json ->
          (match Types.pool_of_yojson pool_json with
           | Ok pool -> return (Ok pool)
           | Error msg -> return (Error (`Json_parse msg)))
        | None -> return (Error (`Json_parse "No pool field")))
     | _ -> return (Error (`Json_parse "Expected object")))
  | Error e -> return (Error e)

let get_spot_price ~cfg ~pool_id ~base_denom ~quote_denom =
  let endpoint = sprintf "/osmosis/gamm/v1beta1/pools/%s/prices?base_asset_denom=%s&quote_asset_denom=%s"
    pool_id base_denom quote_denom in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `Assoc fields ->
       (match List.Assoc.find fields ~equal:String.equal "spot_price" with
        | Some (`String price) -> return (Ok price)
        | _ -> return (Error (`Json_parse "Expected spot_price string")))
     | _ -> return (Error (`Json_parse "Expected object")))
  | Error e -> return (Error e)

let get_token_prices ~cfg =
  let%bind json_str = make_request ~cfg ~endpoint:"/osmosis/gamm/v1beta1/estimated_prices" in
  match json_str with
  | Ok _ -> return (Ok ())
  | Error e -> return (Error e)

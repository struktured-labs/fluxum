(** Curve Finance DEX REST API Client

    @see <https://curve.readthedocs.io/ref-api.html>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
  | `Api_error of string
] [@@deriving sexp]

let make_request ~cfg ~endpoint =
  let url = sprintf "%s/%s" cfg.Cfg.api_url endpoint in
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.init () in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.get ~headers uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
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
  let endpoint = sprintf "getPools/%s/main" cfg.Cfg.network in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.pool_list_response_of_yojson json with
     | Ok response ->
       (match response.success with
        | true -> return (Ok response.data)
        | false -> return (Error (`Api_error "API returned success=false")))
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let get_pool ~cfg ~pool_address =
  let endpoint = sprintf "getPool/%s/%s" cfg.Cfg.network pool_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.pool_detail_response_of_yojson json with
     | Ok response ->
       (match response.success with
        | true -> return (Ok response.data)
        | false -> return (Error (`Api_error "API returned success=false")))
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let get_volume ~cfg ~pool_address =
  let endpoint = sprintf "getVolume/%s/%s" cfg.Cfg.network pool_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.volume_response_of_yojson json with
     | Ok response ->
       (match response.success with
        | true -> return (Ok response.data)
        | false -> return (Error (`Api_error "API returned success=false")))
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let get_trades ~cfg ~pool_address =
  let endpoint = sprintf "getTrades/%s/%s" cfg.Cfg.network pool_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.trades_response_of_yojson json with
     | Ok response ->
       (match response.success with
        | true -> return (Ok response.data)
        | false -> return (Error (`Api_error "API returned success=false")))
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

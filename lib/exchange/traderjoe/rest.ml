(** Trader Joe DEX REST API Client

    @see <https://docs.traderjoexyz.com/>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
] [@@deriving sexp]

let make_request ~cfg ~endpoint =
  let url = sprintf "%s/%s/%s" cfg.Cfg.api_url cfg.Cfg.network endpoint in
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

let get_pairs ~cfg =
  let%bind json_str = make_request ~cfg ~endpoint:"v1/pairs" in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `List pairs_json ->
       let pair_results = List.map pairs_json ~f:Types.pair_of_yojson in
       let rec collect_results acc = function
         | [] -> Ok (List.rev acc)
         | Ok pair :: rest -> collect_results (pair :: acc) rest
         | Error msg :: _ -> Error (`Json_parse msg)
       in
       return (collect_results [] pair_results)
     | _ -> return (Error (`Json_parse "Expected array of pairs")))
  | Error e -> return (Error e)

let get_pair ~cfg ~pair_address =
  let endpoint = sprintf "v1/pairs/%s" pair_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.pair_of_yojson json with
     | Ok pair -> return (Ok pair)
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let get_lb_pairs ~cfg =
  let%bind json_str = make_request ~cfg ~endpoint:"v2/lb-pairs" in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `List pairs_json ->
       let pair_results = List.map pairs_json ~f:Types.lb_pair_of_yojson in
       let rec collect_results acc = function
         | [] -> Ok (List.rev acc)
         | Ok pair :: rest -> collect_results (pair :: acc) rest
         | Error msg :: _ -> Error (`Json_parse msg)
       in
       return (collect_results [] pair_results)
     | _ -> return (Error (`Json_parse "Expected array of LB pairs")))
  | Error e -> return (Error e)

let get_lb_pair ~cfg ~pair_address =
  let endpoint = sprintf "v2/lb-pairs/%s" pair_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.lb_pair_of_yojson json with
     | Ok pair -> return (Ok pair)
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let get_swaps ~cfg ~pair_address =
  let endpoint = sprintf "v1/pairs/%s/swaps" pair_address in
  let%bind json_str = make_request ~cfg ~endpoint in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `List swaps_json ->
       let swap_results = List.map swaps_json ~f:Types.swap_of_yojson in
       let rec collect_results acc = function
         | [] -> Ok (List.rev acc)
         | Ok swap :: rest -> collect_results (swap :: acc) rest
         | Error msg :: _ -> Error (`Json_parse msg)
       in
       return (collect_results [] swap_results)
     | _ -> return (Error (`Json_parse "Expected array of swaps")))
  | Error e -> return (Error e)

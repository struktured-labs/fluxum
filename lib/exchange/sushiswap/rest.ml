(** SushiSwap DEX REST API Client (Multi-chain)

    @see <https://docs.sushi.com/api/examples/swap>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
] [@@deriving sexp]

let make_request ~cfg ~endpoint ~params =
  let uri = Uri.of_string (cfg.Cfg.api_url ^ endpoint) in
  let query_params = List.map params ~f:(fun (k, v) -> (k, [v])) in
  let uri = Uri.add_query_params uri query_params in
  let headers = Cohttp.Header.init () in
  let headers = match cfg.Cfg.api_key with
    | Some key -> Cohttp.Header.add headers "X-API-KEY" key
    | None -> headers
  in
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

let swap_quote ~cfg ~chain_id ~token_in ~token_out ~amount ~slippage ~max_price_impact =
  let params = [
    ("chainId", Int.to_string chain_id);
    ("tokenIn", token_in);
    ("tokenOut", token_out);
    ("amount", amount);
    ("maxSlippage", Float.to_string slippage);
    ("maxPriceImpact", Float.to_string max_price_impact);
  ] in
  let%bind json_str = make_request ~cfg ~endpoint:"/swap" ~params in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.swap_quote_of_yojson json with
     | Ok quote -> return (Ok quote)
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let pool_info ~cfg ~chain_id ~address =
  let params = [
    ("chainId", Int.to_string chain_id);
    ("address", address);
  ] in
  let%bind json_str = make_request ~cfg ~endpoint:"/pool" ~params in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match Types.pool_info_of_yojson json with
     | Ok pool -> return (Ok pool)
     | Error msg -> return (Error (`Json_parse msg)))
  | Error e -> return (Error e)

let pools_for_token ~cfg ~chain_id ~token =
  let params = [
    ("chainId", Int.to_string chain_id);
    ("token", token);
  ] in
  let%bind json_str = make_request ~cfg ~endpoint:"/pools" ~params in
  match Result.bind json_str ~f:(fun s ->
    try Ok (Yojson.Safe.from_string s)
    with ex -> Error (`Json_parse (Exn.to_string ex))) with
  | Ok json ->
    (match json with
     | `List pools ->
       let pool_results = List.map pools ~f:Types.pool_info_of_yojson in
       let rec collect_results acc = function
         | [] -> Ok (List.rev acc)
         | Ok pool :: rest -> collect_results (pool :: acc) rest
         | Error msg :: _ -> Error (`Json_parse msg)
       in
       return (collect_results [] pool_results)
     | _ -> return (Error (`Json_parse "Expected list of pools")))
  | Error e -> return (Error e)

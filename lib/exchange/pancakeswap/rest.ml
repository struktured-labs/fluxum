(** PancakeSwap DEX REST API Client (BSC)

    Public API for PancakeSwap token and pair data.
    No authentication required.

    @see <https://docs.pancakeswap.finance>
*)

open Core
open Async

module Error = struct
  type t = [
    | `Http of int * string
    | `Json_parse of string
    | `Network of string
  ] [@@deriving sexp]

  let to_string = function
    | `Http (code, msg) -> sprintf "HTTP %d: %s" code msg
    | `Json_parse msg -> sprintf "JSON parse error: %s" msg
    | `Network msg -> sprintf "Network error: %s" msg
end

let make_request ~cfg ~endpoint =
  let open Deferred.Result.Let_syntax in
  let url = sprintf "%s%s" cfg.Cfg.api_url endpoint in

  let%bind response, body =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.get (Uri.of_string url))
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in

  let%bind body_string =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Body.to_string body)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in

  let status_code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  match status_code with
  | 200 -> return body_string
  | code -> Deferred.Result.fail (`Http (code, body_string))

let tokens ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:"/tokens" in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("updated_at", _); ("data", `Assoc tokens)] ->
    let token_list = List.map tokens ~f:(fun (_addr, data) ->
      Types.token_data_of_yojson data) in
    (match Result.all token_list with
     | Ok tokens -> return tokens
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected tokens format")

let pairs ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:"/pairs" in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("updated_at", _); ("data", `Assoc pairs)] ->
    let pair_list = List.map pairs ~f:(fun (_addr, data) ->
      Types.pair_data_of_yojson data) in
    (match Result.all pair_list with
     | Ok pairs -> return pairs
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected pairs format")

(** Orca DEX REST API Client (Solana)

    Public API for Orca whirlpools and prices.
    No authentication required.

    @see <https://docs.orca.so/>
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

(** Get all whirlpools *)
let whirlpools ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:"/whirlpools" in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("whirlpools", `List pools)] ->
    (match Result.all (List.map pools ~f:Types.whirlpool_of_yojson) with
     | Ok pools -> return pools
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected whirlpools array")

(** Get token price *)
let token_price ~cfg ~mint =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:(sprintf "/token/%s" mint) in
  match Yojson.Safe.from_string json_str |> Types.token_price_of_yojson with
  | Ok price -> return price
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

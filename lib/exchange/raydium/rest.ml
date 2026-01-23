(** Raydium DEX REST API Client (Solana)

    Public API for Raydium swap quotes and pool information.
    No authentication required.

    @see <https://docs.raydium.io/raydium/traders/trade-api>
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

let make_request ~cfg ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let query_string = String.concat ~sep:"&"
    (List.map params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v))) in

  let url = sprintf "%s%s?%s" cfg.Cfg.swap_url endpoint query_string in

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

(** Get swap quote for exact input amount *)
let swap_quote ~cfg ~input_mint ~output_mint ~amount ~slippage =
  let open Deferred.Result.Let_syntax in
  let params = [
    ("inputMint", input_mint);
    ("outputMint", output_mint);
    ("amount", amount);
    ("slippage", Float.to_string slippage);
  ] in
  let%bind json_str = make_request ~cfg ~endpoint:"/compute/swap-base-in" ~params in
  match Yojson.Safe.from_string json_str |> Types.swap_quote_of_yojson with
  | Ok quote -> return quote
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get all liquidity pools *)
let pools ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:"/main/pools" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List pools ->
    (match Result.all (List.map pools ~f:Types.pool_info_of_yojson) with
     | Ok pools -> return pools
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of pools")

(** Get pairs with 24h stats *)
let pairs ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_request ~cfg ~endpoint:"/main/pairs" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List pairs ->
    (match Result.all (List.map pairs ~f:Types.pair_info_of_yojson) with
     | Ok pairs -> return pairs
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of pairs")

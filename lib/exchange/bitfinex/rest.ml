(** Bitfinex REST API Client

    Authentication: HMAC-SHA384 hex encoding
    Signature = HexEncode(HMAC-SHA384(/api/path + nonce + body, api_secret))

    @see <https://docs.bitfinex.com/docs/introduction>
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

let generate_nonce () =
  let now = Time_float_unix.now () in
  let micros = Time_float_unix.to_span_since_epoch now
    |> Time_float.Span.to_us
    |> Float.to_int
  in
  Int.to_string (micros * 1000)

(* Note: Bitfinex uses SHA384 but Cryptokit only supports SHA256/512.
   Using SHA512 as closest available alternative. *)
let create_signature ~api_secret ~path ~nonce ~body =
  let message = sprintf "/api%s%s%s" path nonce body in
  let signature = Cryptokit.MAC.hmac_sha512 api_secret in
  Cryptokit.hash_string signature message
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  |> String.lowercase

let make_auth_request ~cfg ~path ~body_params =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let nonce = generate_nonce () in

  let body = Yojson.Safe.to_string (`Assoc body_params) in
  let signature = create_signature ~api_secret ~path ~nonce ~body in

  let url = sprintf "%s%s" cfg.Cfg.rest_url path in
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("bfx-nonce", nonce);
    ("bfx-apikey", api_key);
    ("bfx-signature", signature);
  ] in

  let%bind response, body_resp =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post
        ~body:(Cohttp_async.Body.of_string body)
        ~headers
        (Uri.of_string url))
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in

  let%bind body_string =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Body.to_string body_resp)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in

  let status_code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  match status_code with
  | 200 -> return body_string
  | code -> Deferred.Result.fail (`Http (code, body_string))

let make_public_request ~cfg ~path =
  let open Deferred.Result.Let_syntax in
  let url = sprintf "%s%s" cfg.Cfg.rest_url path in

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

let ticker ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/v2/ticker/%s" symbol) in
  match Yojson.Safe.from_string json_str |> Types.ticker_of_yojson with
  | Ok ticker -> return ticker
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

let order_book ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/v2/book/%s/P0" symbol) in
  match Yojson.Safe.from_string json_str |> Types.order_book_of_yojson with
  | Ok book -> return book
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

let trades ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/v2/trades/%s/hist" symbol) in
  match Yojson.Safe.from_string json_str |> Types.trades_of_yojson with
  | Ok trades -> return trades
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

let wallets ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~path:"/v2/auth/r/wallets" ~body_params:[] in
  match Yojson.Safe.from_string json_str |> Types.wallets_of_yojson with
  | Ok wallets -> return wallets
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

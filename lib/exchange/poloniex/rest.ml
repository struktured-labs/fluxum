(** Poloniex REST API Client

    Authentication: HMAC-SHA256 + Base64
    Signature = Base64(HMAC-SHA256(request_string, api_secret))

    @see <https://api-docs.poloniex.com/spot/api/>
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

let generate_timestamp () =
  let now = Time_float_unix.now () in
  Time_float_unix.to_span_since_epoch now
  |> Time_float.Span.to_ms
  |> Float.to_int
  |> Int.to_string

let create_signature ~api_secret ~request_string =
  let signature = Cryptokit.MAC.hmac_sha256 api_secret in
  Cryptokit.hash_string signature request_string
  |> Cryptokit.transform_string (Cryptokit.Base64.encode_compact ())

let make_auth_request ~cfg ~method_ ~path ~body =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let timestamp = generate_timestamp () in

  let request_string = sprintf "%s\n%s\nrequestBody=%s&signTimestamp=%s"
    method_ path body timestamp in
  let signature = create_signature ~api_secret ~request_string in

  let url = sprintf "%s%s" cfg.Cfg.rest_url path in
  let headers = Cohttp.Header.of_list [
    ("key", api_key);
    ("signTimestamp", timestamp);
    ("signature", signature);
    ("Content-Type", "application/json");
  ] in

  let%bind response, body_resp =
    Deferred.Or_error.try_with (fun () ->
      match method_ with
      | "GET" -> Cohttp_async.Client.get ~headers (Uri.of_string url)
      | "POST" -> Cohttp_async.Client.post
          ~body:(Cohttp_async.Body.of_string body)
          ~headers
          (Uri.of_string url)
      | _ -> failwith "Unsupported method")
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
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/markets/%s/ticker24h" symbol) in
  match Yojson.Safe.from_string json_str |> Types.ticker_of_yojson with
  | Ok ticker -> return ticker
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

let order_book ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/markets/%s/orderBook" symbol) in
  match Yojson.Safe.from_string json_str |> Types.order_book_of_yojson with
  | Ok book -> return book
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

let trades ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~path:(sprintf "/markets/%s/trades" symbol) in
  match Yojson.Safe.from_string json_str with
  | `List trades ->
    (match Result.all (List.map trades ~f:Types.trade_of_yojson) with
     | Ok trades -> return trades
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of trades")

let balances ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"GET" ~path:"/accounts/balances" ~body:"" in
  match Yojson.Safe.from_string json_str with
  | `List balances ->
    (match Result.all (List.map balances ~f:Types.balance_of_yojson) with
     | Ok balances -> return balances
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of balances")

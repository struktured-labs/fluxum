(** KuCoin REST API Client

    Implements both public and private REST endpoints.

    Authentication uses HMAC-SHA256 signatures with:
    - API key
    - API secret
    - Passphrase (encrypted for v2+)
    - Timestamp (milliseconds)
    - Signature = Base64(HMAC-SHA256(timestamp + method + endpoint + body, api_secret))

    @see <https://www.kucoin.com/docs/basic-info/connection-method/authentication>
*)

open Core
open Async

(** Error types *)
module Error = struct
  type t = [
    | `Http of int * string
    | `Json_parse of string
    | `Api_error of string
    | `Network of string
  ] [@@deriving sexp]

  let to_string = function
    | `Http (code, msg) -> sprintf "HTTP %d: %s" code msg
    | `Json_parse msg -> sprintf "JSON parse error: %s" msg
    | `Api_error msg -> sprintf "API error: %s" msg
    | `Network msg -> sprintf "Network error: %s" msg
end

(** Generate timestamp in milliseconds *)
let generate_timestamp () =
  let now = Time_float_unix.now () in
  let ms = Time_float_unix.to_span_since_epoch now
    |> Time_float.Span.to_ms
    |> Float.to_int
  in
  Int.to_string ms

(** Create HMAC-SHA256 signature *)
let create_signature ~api_secret ~message =
  let signature = Cryptokit.MAC.hmac_sha256 api_secret in
  Cryptokit.hash_string signature message
  |> Cryptokit.transform_string (Cryptokit.Base64.encode_compact ())

(** Encrypt passphrase for v2+ API keys *)
let encrypt_passphrase ~api_secret ~passphrase =
  create_signature ~api_secret ~message:passphrase

(** Make authenticated request *)
let make_auth_request ~cfg ~method_ ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let passphrase = Cfg.passphrase_exn cfg in

  let timestamp = generate_timestamp () in

  (* Encrypt passphrase for v2+ *)
  let encrypted_passphrase = match cfg.Cfg.api_version with
    | 1 -> passphrase
    | _ -> encrypt_passphrase ~api_secret ~passphrase
  in

  (* Build request body *)
  let body = match params with
    | [] -> ""
    | _ -> Yojson.Safe.to_string (`Assoc params)
  in

  (* Create signature message: timestamp + method + endpoint + body *)
  let message = timestamp ^ method_ ^ endpoint ^ body in
  let signature = create_signature ~api_secret ~message in

  let url = sprintf "%s%s" cfg.Cfg.rest_url endpoint in
  let headers = Cohttp.Header.of_list [
    ("KC-API-KEY", api_key);
    ("KC-API-SIGN", signature);
    ("KC-API-TIMESTAMP", timestamp);
    ("KC-API-PASSPHRASE", encrypted_passphrase);
    ("KC-API-KEY-VERSION", Int.to_string cfg.Cfg.api_version);
    ("Content-Type", "application/json");
  ] in

  let%bind response, body =
    Deferred.Or_error.try_with (fun () ->
      match method_ with
      | "GET" -> Cohttp_async.Client.get ~headers (Uri.of_string url)
      | "POST" -> Cohttp_async.Client.post
          ~body:(Cohttp_async.Body.of_string body)
          ~headers
          (Uri.of_string url)
      | "DELETE" -> Cohttp_async.Client.delete ~headers (Uri.of_string url)
      | _ -> failwith "Unsupported HTTP method")
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

(** Make public request *)
let make_public_request ~cfg ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let query_params = match params with
    | [] -> ""
    | _ -> "?" ^ (String.concat ~sep:"&"
        (List.map params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v))))
  in

  let url = sprintf "%s%s%s" cfg.Cfg.rest_url endpoint query_params in

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

(** {1 Public Endpoints} *)

(** Get 24hr ticker *)
let ticker ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/api/v1/market/stats?symbol=%s" symbol) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", data)] ->
    (match Types.ticker_of_yojson data with
     | Ok ticker -> return ticker
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected ticker format")

(** Get order book *)
let order_book ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/api/v1/market/orderbook/level2_20?symbol=%s" symbol) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", data)] ->
    (match Types.order_book_of_yojson data with
     | Ok book -> return book
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected order book format")

(** Get recent trades *)
let trades ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/api/v1/market/histories?symbol=%s" symbol) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", `List trades)] ->
    (match Result.all (List.map trades ~f:Types.trade_of_yojson) with
     | Ok trades -> return trades
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of trades")

(** Get symbol list *)
let symbols ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~endpoint:"/api/v1/symbols" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", `List symbols)] ->
    (match Result.all (List.map symbols ~f:Types.symbol_info_of_yojson) with
     | Ok symbols -> return symbols
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of symbols")

(** {1 Private Endpoints} *)

(** Get account list *)
let accounts ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"GET" ~endpoint:"/api/v1/accounts" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", `List accounts)] ->
    (match Result.all (List.map accounts ~f:Types.balance_of_yojson) with
     | Ok balances -> return balances
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of accounts")

(** Place limit order *)
let place_limit_order ~cfg ~symbol ~side ~price ~size =
  let open Deferred.Result.Let_syntax in
  let params = [
    ("clientOid", `String (Uuid_unix.create () |> Uuid.to_string));
    ("side", `String side);
    ("symbol", `String symbol);
    ("type", `String "limit");
    ("price", `String price);
    ("size", `String size);
  ] in
  let%bind json_str = make_auth_request ~cfg ~method_:"POST" ~endpoint:"/api/v1/orders" ~params in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", data)] ->
    (match Types.order_response_of_yojson data with
     | Ok response -> return response
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected order response format")

(** Place market order *)
let place_market_order ~cfg ~symbol ~side ~size =
  let open Deferred.Result.Let_syntax in
  let params = [
    ("clientOid", `String (Uuid_unix.create () |> Uuid.to_string));
    ("side", `String side);
    ("symbol", `String symbol);
    ("type", `String "market");
    ("size", `String size);
  ] in
  let%bind json_str = make_auth_request ~cfg ~method_:"POST" ~endpoint:"/api/v1/orders" ~params in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", data)] ->
    (match Types.order_response_of_yojson data with
     | Ok response -> return response
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected order response format")

(** Cancel order *)
let cancel_order ~cfg ~order_id =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"DELETE"
    ~endpoint:(sprintf "/api/v1/orders/%s" order_id) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", `Assoc [("cancelledOrderIds", `List _)])] -> return order_id
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected cancel response")

(** Get order details *)
let order_details ~cfg ~order_id =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"GET"
    ~endpoint:(sprintf "/api/v1/orders/%s" order_id) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", data)] ->
    (match Types.order_of_yojson data with
     | Ok order -> return order
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected order format")

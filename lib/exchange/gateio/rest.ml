(** Gate.io REST API Client

    Authentication uses HMAC-SHA512:
    - Signature = HexEncode(HMAC-SHA512(secret, signature_string))
    - signature_string = method + "\n" + url_path + "\n" + query_string + "\n" + hex_hash_body + "\n" + timestamp

    @see <https://www.gate.com/docs/developers/apiv4/>
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
  |> Time_float.Span.to_sec
  |> Float.to_int
  |> Int.to_string

let sha512_hash str =
  let hash = Cryptokit.Hash.sha512 () in
  Cryptokit.hash_string hash str
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  |> String.lowercase

let create_signature ~api_secret ~method_ ~url_path ~query_string ~body ~timestamp =
  let body_hash = sha512_hash body in
  let signature_string = String.concat ~sep:"\n" [
    method_;
    url_path;
    query_string;
    body_hash;
    timestamp;
  ] in
  let signature = Cryptokit.MAC.hmac_sha512 api_secret in
  Cryptokit.hash_string signature signature_string
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  |> String.lowercase

let make_auth_request ~cfg ~method_ ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let timestamp = generate_timestamp () in

  let body = match params with
    | [] -> ""
    | _ -> Yojson.Safe.to_string (`Assoc params)
  in

  let signature = create_signature
    ~api_secret
    ~method_
    ~url_path:endpoint
    ~query_string:""
    ~body
    ~timestamp
  in

  let url = sprintf "%s%s" cfg.Cfg.rest_url endpoint in
  let headers = Cohttp.Header.of_list [
    ("KEY", api_key);
    ("SIGN", signature);
    ("Timestamp", timestamp);
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
      | _ -> failwith "Unsupported method")
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

let make_public_request ~cfg ~endpoint =
  let open Deferred.Result.Let_syntax in
  let url = sprintf "%s%s" cfg.Cfg.rest_url endpoint in

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

(** Get ticker *)
let ticker ~cfg ~currency_pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/spot/tickers?currency_pair=%s" currency_pair) in
  match Yojson.Safe.from_string json_str with
  | `List [ticker] ->
    (match Types.ticker_of_yojson ticker with
     | Ok ticker -> return ticker
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected single ticker")

(** Get order book *)
let order_book ~cfg ~currency_pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/spot/order_book?currency_pair=%s" currency_pair) in
  match Yojson.Safe.from_string json_str |> Types.order_book_of_yojson with
  | Ok book -> return book
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get recent trades *)
let trades ~cfg ~currency_pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/spot/trades?currency_pair=%s" currency_pair) in
  match Yojson.Safe.from_string json_str with
  | `List trades ->
    (match Result.all (List.map trades ~f:Types.trade_of_yojson) with
     | Ok trades -> return trades
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of trades")

(** Get spot accounts (balances) *)
let balances ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"GET" ~endpoint:"/spot/accounts" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List balances ->
    (match Result.all (List.map balances ~f:Types.balance_of_yojson) with
     | Ok balances -> return balances
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of balances")

(** Place limit order *)
let place_limit_order ~cfg ~currency_pair ~side ~amount ~price =
  let open Deferred.Result.Let_syntax in
  let params = [
    ("currency_pair", `String currency_pair);
    ("side", `String side);
    ("amount", `String amount);
    ("price", `String price);
    ("type", `String "limit");
  ] in
  let%bind json_str = make_auth_request ~cfg ~method_:"POST" ~endpoint:"/spot/orders" ~params in
  match Yojson.Safe.from_string json_str |> Types.order_response_of_yojson with
  | Ok response -> return response
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** HTX (Huobi) REST API Client

    Authentication: HMAC-SHA256 + Base64 encoding
    Signature = Base64(HmacSHA256(message, secret_key))

    @see <https://huobiapi.github.io/docs/spot/v1/en/>
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
  Time_float_unix.format now ~zone:Time_float_unix.Zone.utc "%Y-%m-%dT%H:%M:%S"

let create_signature ~api_secret ~message =
  let signature = Cryptokit.MAC.hmac_sha256 api_secret in
  Cryptokit.hash_string signature message
  |> Cryptokit.transform_string (Cryptokit.Base64.encode_compact ())

let make_auth_request ~cfg ~method_ ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let timestamp = generate_timestamp () in

  let sorted_params = List.sort params ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) in
  let all_params = ("AccessKeyId", api_key) :: ("SignatureMethod", "HmacSHA256") ::
                   ("SignatureVersion", "2") :: ("Timestamp", timestamp) :: sorted_params in

  let query_string = String.concat ~sep:"&"
    (List.map all_params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v))) in

  let signature_payload = String.concat ~sep:"\n" [
    method_;
    "api.htx.com";
    endpoint;
    query_string;
  ] in

  let signature = create_signature ~api_secret ~message:signature_payload in
  let final_query = sprintf "%s&Signature=%s" query_string (Uri.pct_encode signature) in

  let url = sprintf "%s%s?%s" cfg.Cfg.rest_url endpoint final_query in

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

let ticker ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/market/detail/merged?symbol=%s" symbol) in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("status", `String "ok"); ("tick", tick)] ->
    let tick_with_symbol = match tick with
      | `Assoc fields -> `Assoc (("symbol", `String symbol) :: fields)
      | _ -> tick
    in
    (match Types.ticker_of_yojson tick_with_symbol with
     | Ok ticker -> return ticker
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected ticker format")

let order_book ~cfg ~symbol =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg
    ~endpoint:(sprintf "/market/depth?symbol=%s&type=step0" symbol) in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("status", `String "ok"); ("tick", tick)] ->
    (match Types.order_book_of_yojson tick with
     | Ok book -> return book
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected order book format")

let balances ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~method_:"GET" ~endpoint:"/v1/account/accounts" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("status", `String "ok"); ("data", `List accounts)] ->
    return (sprintf "Found %d accounts" (List.length accounts))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected balances format")

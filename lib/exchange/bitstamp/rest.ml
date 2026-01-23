(** Bitstamp REST API Client

    Implements both public and private REST endpoints.

    Authentication uses HMAC-SHA256 signatures with:
    - Customer ID
    - API key
    - Nonce (timestamp in microseconds)
    - Signature = HMAC-SHA256(nonce + customer_id + api_key, api_secret)

    @see <https://www.bitstamp.net/api/>
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

(** Generate nonce (microseconds since epoch) *)
let generate_nonce () =
  let now = Time_float_unix.now () in
  let micros = Time_float_unix.to_span_since_epoch now
    |> Time_float.Span.to_us
    |> Float.to_int
  in
  Int.to_string micros

(** Create HMAC-SHA256 signature *)
let create_signature ~api_secret ~nonce ~customer_id ~api_key =
  let message = nonce ^ customer_id ^ api_key in
  let signature = Cryptokit.MAC.hmac_sha256 api_secret in
  Cryptokit.hash_string signature message
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  |> String.uppercase

(** Make authenticated request *)
let make_auth_request ~cfg ~endpoint ~params =
  let open Deferred.Result.Let_syntax in

  let api_key = Cfg.api_key_exn cfg in
  let api_secret = Cfg.api_secret_exn cfg in
  let customer_id = Cfg.customer_id_exn cfg in

  let nonce = generate_nonce () in
  let signature = create_signature ~api_secret ~nonce ~customer_id ~api_key in

  let auth_params = [
    ("key", api_key);
    ("signature", signature);
    ("nonce", nonce);
  ] in

  let all_params = auth_params @ params in
  let body = String.concat ~sep:"&"
    (List.map all_params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v)))
  in

  let url = sprintf "%s/%s/" cfg.Cfg.rest_url endpoint in
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in

  let%bind response, body =
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

  let url = sprintf "%s/%s/%s" cfg.Cfg.rest_url endpoint query_params in

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

(** Get ticker for a currency pair *)
let ticker ~cfg ~pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~endpoint:(sprintf "ticker/%s" pair) ~params:[] in
  match Yojson.Safe.from_string json_str |> Types.ticker_of_yojson with
  | Ok ticker -> return ticker
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get hourly ticker *)
let ticker_hour ~cfg ~pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~endpoint:(sprintf "ticker_hour/%s" pair) ~params:[] in
  match Yojson.Safe.from_string json_str |> Types.ticker_of_yojson with
  | Ok ticker -> return ticker
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get order book *)
let order_book ~cfg ~pair ?(group = 1) () =
  let open Deferred.Result.Let_syntax in
  let params = [("group", Int.to_string group)] in
  let%bind json_str = make_public_request ~cfg ~endpoint:(sprintf "order_book/%s" pair) ~params in
  match Yojson.Safe.from_string json_str |> Types.order_book_of_yojson with
  | Ok book -> return book
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get recent transactions *)
let transactions ~cfg ~pair ?(time = `Hour) () =
  let open Deferred.Result.Let_syntax in
  let time_param = match time with
    | `Minute -> "minute"
    | `Hour -> "hour"
    | `Day -> "day"
  in
  let params = [("time", time_param)] in
  let%bind json_str = make_public_request ~cfg ~endpoint:(sprintf "transactions/%s" pair) ~params in
  match Yojson.Safe.from_string json_str with
  | `List trades ->
    (match Result.all (List.map trades ~f:Types.trade_of_yojson) with
     | Ok trades -> return trades
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of trades")

(** Get trading pairs info *)
let trading_pairs_info ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_public_request ~cfg ~endpoint:"trading-pairs-info" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List pairs ->
    (match Result.all (List.map pairs ~f:Types.trading_pair_info_of_yojson) with
     | Ok pairs -> return pairs
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of trading pairs")

(** Get OHLC data *)
let ohlc ~cfg ~pair ~step ~limit ?(start = 0) ?(end_ = 0) () =
  let open Deferred.Result.Let_syntax in
  let params = [
    ("step", Int.to_string step);
    ("limit", Int.to_string limit);
    ("start", Int.to_string start);
    ("end", Int.to_string end_);
  ] in
  let%bind json_str = make_public_request ~cfg ~endpoint:(sprintf "ohlc/%s" pair) ~params in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("data", `Assoc [("ohlc", `List candles)])] ->
    (match Result.all (List.map candles ~f:Types.ohlc_of_yojson) with
     | Ok candles -> return candles
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected OHLC response format")

(** {1 Private Endpoints} *)

(** Get account balance *)
let balance ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~endpoint:"balance" ~params:[] in
  match Yojson.Safe.from_string json_str |> Types.balance_of_yojson with
  | Ok balance -> return balance
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Get user transactions *)
let user_transactions ~cfg ~pair ?(offset = 0) ?(limit = 100) ?(sort = `Desc) () =
  let open Deferred.Result.Let_syntax in
  let sort_str = match sort with `Asc -> "asc" | `Desc -> "desc" in
  let params = [
    ("offset", Int.to_string offset);
    ("limit", Int.to_string limit);
    ("sort", sort_str);
  ] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "user_transactions/%s" pair) ~params in
  match Yojson.Safe.from_string json_str with
  | `List txs ->
    (match Result.all (List.map txs ~f:Types.user_transaction_of_yojson) with
     | Ok txs -> return txs
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of transactions")

(** Get open orders *)
let open_orders ~cfg ~pair =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "open_orders/%s" pair) ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List orders ->
    (match Result.all (List.map orders ~f:Types.open_order_of_yojson) with
     | Ok orders -> return orders
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of orders")

(** Get all open orders *)
let open_orders_all ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~endpoint:"open_orders/all" ~params:[] in
  match Yojson.Safe.from_string json_str with
  | `List orders ->
    (match Result.all (List.map orders ~f:Types.open_order_of_yojson) with
     | Ok orders -> return orders
     | Error msg -> Deferred.Result.fail (`Json_parse msg))
  | _ -> Deferred.Result.fail (`Json_parse "Expected array of orders")

(** Place buy limit order *)
let buy_limit_order ~cfg ~pair ~amount ~price ?limit_price ?daily_order () =
  let open Deferred.Result.Let_syntax in
  let params = List.filter_opt [
    Some ("amount", Float.to_string amount);
    Some ("price", Float.to_string price);
    Option.map limit_price ~f:(fun p -> ("limit_price", Float.to_string p));
    Option.map daily_order ~f:(fun d -> ("daily_order", if d then "True" else "False"));
  ] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "buy/%s" pair) ~params in
  match Yojson.Safe.from_string json_str |> Types.order_response_of_yojson with
  | Ok order -> return order
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Place sell limit order *)
let sell_limit_order ~cfg ~pair ~amount ~price ?limit_price ?daily_order () =
  let open Deferred.Result.Let_syntax in
  let params = List.filter_opt [
    Some ("amount", Float.to_string amount);
    Some ("price", Float.to_string price);
    Option.map limit_price ~f:(fun p -> ("limit_price", Float.to_string p));
    Option.map daily_order ~f:(fun d -> ("daily_order", if d then "True" else "False"));
  ] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "sell/%s" pair) ~params in
  match Yojson.Safe.from_string json_str |> Types.order_response_of_yojson with
  | Ok order -> return order
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Place buy market order *)
let buy_market_order ~cfg ~pair ~amount =
  let open Deferred.Result.Let_syntax in
  let params = [("amount", Float.to_string amount)] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "buy/market/%s" pair) ~params in
  match Yojson.Safe.from_string json_str |> Types.order_response_of_yojson with
  | Ok order -> return order
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Place sell market order *)
let sell_market_order ~cfg ~pair ~amount =
  let open Deferred.Result.Let_syntax in
  let params = [("amount", Float.to_string amount)] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:(sprintf "sell/market/%s" pair) ~params in
  match Yojson.Safe.from_string json_str |> Types.order_response_of_yojson with
  | Ok order -> return order
  | Error msg -> Deferred.Result.fail (`Json_parse msg)

(** Cancel order *)
let cancel_order ~cfg ~order_id =
  let open Deferred.Result.Let_syntax in
  let params = [("id", order_id)] in
  let%bind json_str = make_auth_request ~cfg ~endpoint:"cancel_order" ~params in
  match Yojson.Safe.from_string json_str with
  | `Assoc [("id", `String id)] -> return id
  | _ -> Deferred.Result.fail (`Json_parse "Unexpected cancel response")

(** Cancel all orders *)
let cancel_all_orders ~cfg =
  let open Deferred.Result.Let_syntax in
  let%bind json_str = make_auth_request ~cfg ~endpoint:"cancel_all_orders" ~params:[] in
  return json_str  (* Returns success message *)

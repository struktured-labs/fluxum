(** OKX REST API Client - V5 API

    Implements OKX's unified V5 API for spot, derivatives, and options trading.

    Key features:
    - HMAC-SHA256 authentication with Base64 encoding
    - JSON POST bodies (not form-encoded)
    - Result-wrapped responses: {"code": "0", "msg": "success", "data": {...}}
    - Configurable base URL (production/AWS/testnet)

    Authentication headers:
    - OK-ACCESS-KEY: API key
    - OK-ACCESS-SIGN: HMAC-SHA256 signature (Base64 encoded)
    - OK-ACCESS-TIMESTAMP: ISO timestamp (e.g., 2020-12-08T09:08:57.715Z)
    - OK-ACCESS-PASSPHRASE: API passphrase

    Signing string format: timestamp + method + requestPath + body
    - For GET: requestPath includes query string
    - For POST: body is JSON string
*)

open Core
open Async

module Error = struct
  type http =
    [ `Bad_request of string
    | `Not_found
    | `Service_unavailable of string
    | `Unauthorized of string
    | `Too_many_requests of string
    | `Forbidden of string
    ]
  [@@deriving sexp]

  type json_error =
    { message : string
    ; body : string
    }
  [@@deriving sexp]

  type json = [ `Json_parse_error of json_error ] [@@deriving sexp]

  type api_error =
    { code : string
    ; msg : string
    }
  [@@deriving sexp]

  type response = [ `Api_error of api_error ] [@@deriving sexp]

  type t =
    [ http
    | json
    | response
    ]
  [@@deriving sexp]
end

module Operation = struct
  module type S = sig
    val name : string
    val endpoint : string
    val http_method : [ `GET | `POST | `DELETE ]
    val requires_auth : bool

    type request [@@deriving sexp]

    val request_to_params : request -> (string * string) list

    type response [@@deriving sexp, of_yojson]
  end

  module type S_NO_ARG = sig
    include S with type request = unit
  end
end

module Response = struct
  (** Parse OKX V5 response format

      OKX returns:
      - Success: {"code": "0", "msg": "success", "data": {...}}
      - Error: {"code": "50000", "msg": "error description"}
  *)
  let parse json result_of_yojson =
    match json with
    | `Assoc fields -> (
      (* Check code for errors *)
      match List.Assoc.find fields ~equal:String.equal "code" with
      | Some (`String code) when not (String.equal code "0") ->
        let msg =
          List.Assoc.find fields ~equal:String.equal "msg"
          |> Option.value_map ~default:"Unknown error" ~f:(function
            | `String s -> s
            | j -> Yojson.Safe.to_string j)
        in
        `Api_error Error.{ code; msg }
      | _ ->
        (* Extract data field *)
        let result_json =
          List.Assoc.find fields ~equal:String.equal "data"
          |> Option.value ~default:`Null
        in
        (match result_of_yojson result_json with
        | Result.Ok x -> `Ok x
        | Result.Error e ->
          `Json_parse_error
            Error.{ message = e; body = Yojson.Safe.to_string json }))
    | _ ->
      `Json_parse_error
        Error.{ message = "Expected object"; body = Yojson.Safe.to_string json }
end

module Signature = struct
  (** Generate ISO timestamp

      OKX expects ISO format: 2020-12-08T09:08:57.715Z
  *)
  let generate_timestamp () =
    let now = Time_float_unix.now () in
    Time_float_unix.to_string_abs ~zone:Time_float_unix.Zone.utc now
    |> String.substr_replace_all ~pattern:" " ~with_:"T"
    |> fun s -> s ^ "Z"

  (** Build query string from parameters *)
  let build_query_string params =
    match params with
    | [] -> ""
    | _ ->
      "?" ^ String.concat ~sep:"&"
        (List.map params ~f:(fun (k, v) ->
          sprintf "%s=%s" k (Uri.pct_encode v)))

  (** HMAC-SHA256 implementation using Digestif *)
  let hmac_sha256 ~secret ~message =
    let block_size = 64 in
    let secret_key =
      match String.length secret > block_size with
      | true -> Digestif.SHA256.digest_string secret |> Digestif.SHA256.to_raw_string
      | false -> secret ^ String.make (block_size - String.length secret) '\x00'
    in
    let ipad = String.init block_size ~f:(fun i ->
      Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x36))
    in
    let opad = String.init block_size ~f:(fun i ->
      Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x5c))
    in
    let inner_msg = ipad ^ message in
    let inner_hash = Digestif.SHA256.digest_string inner_msg |> Digestif.SHA256.to_raw_string in
    let outer_msg = opad ^ inner_hash in
    Digestif.SHA256.digest_string outer_msg

  (** Sign OKX V5 request

      Signing string: timestamp + method + requestPath + body
      - method: UPPERCASE (GET, POST, DELETE)
      - requestPath: /api/v5/... including query string for GET
      - body: JSON string for POST/DELETE, empty for GET

      Returns Base64-encoded signature
  *)
  let sign ~api_secret ~timestamp ~method_ ~request_path ~body =
    let message = timestamp ^ method_ ^ request_path ^ body in
    let hash = hmac_sha256 ~secret:api_secret ~message in
    Base64.encode_exn (Digestif.SHA256.to_raw_string hash)
end

module Request (Operation : Operation.S) = struct
  let request (module Cfg : Cfg.S) (req : Operation.request) =
    let base_params = Operation.request_to_params req in

    (* Build URI, headers, and body based on auth requirement and HTTP method *)
    let uri, headers, body =
      match Operation.requires_auth with
      | true ->
        let timestamp = Signature.generate_timestamp () in
        let method_str =
          match Operation.http_method with
          | `GET -> "GET"
          | `POST -> "POST"
          | `DELETE -> "DELETE"
        in
        let request_path, json_body =
          match Operation.http_method with
          | `GET ->
            let query_string = Signature.build_query_string base_params in
            (sprintf "/api/v5/%s%s" Operation.endpoint query_string, "")
          | `POST | `DELETE ->
            (* For POST/DELETE, params become JSON body *)
            let json = `Assoc (List.map base_params ~f:(fun (k, v) -> (k, `String v))) in
            let body_str = Yojson.Safe.to_string json in
            (sprintf "/api/v5/%s" Operation.endpoint, body_str)
        in
        let signature =
          Signature.sign
            ~api_secret:Cfg.api_secret
            ~timestamp
            ~method_:method_str
            ~request_path
            ~body:json_body
        in
        let headers =
          Cohttp.Header.of_list
            [ ("OK-ACCESS-KEY", Cfg.api_key)
            ; ("OK-ACCESS-SIGN", signature)
            ; ("OK-ACCESS-TIMESTAMP", timestamp)
            ; ("OK-ACCESS-PASSPHRASE", Cfg.api_passphrase)
            ; ("Content-Type", "application/json")
            ]
        in
        let uri =
          match Operation.http_method with
          | `GET ->
            Uri.make
              ~scheme:"https"
              ~host:Cfg.base_url
              ~path:(sprintf "/api/v5/%s" Operation.endpoint)
              ~query:(List.map base_params ~f:(fun (k, v) -> (k, [ v ])))
              ()
          | `POST | `DELETE ->
            Uri.make
              ~scheme:"https"
              ~host:Cfg.base_url
              ~path:(sprintf "/api/v5/%s" Operation.endpoint)
              ()
        in
        let cohttp_body =
          match json_body with
          | "" -> None
          | s -> Some (Cohttp_async.Body.of_string s)
        in
        (uri, headers, cohttp_body)
      | false ->
        (* Public endpoint - no authentication *)
        let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ] in
        let uri =
          Uri.make
            ~scheme:"https"
            ~host:Cfg.base_url
            ~path:(sprintf "/api/v5/%s" Operation.endpoint)
            ~query:(List.map base_params ~f:(fun (k, v) -> (k, [ v ])))
            ()
        in
        (uri, headers, None)
    in

    Log.Global.debug "OKX API call: %s %s"
      (match Operation.http_method with `GET -> "GET" | `POST -> "POST" | `DELETE -> "DELETE")
      Operation.endpoint;

    (* Execute HTTP request *)
    let make_request () =
      match Operation.http_method, body with
      | `GET, _ -> Cohttp_async.Client.get ~headers uri
      | `POST, Some b -> Cohttp_async.Client.post ~headers ~body:b uri
      | `POST, None -> Cohttp_async.Client.post ~headers uri
      | `DELETE, Some b -> Cohttp_async.Client.delete ~headers ~body:b uri
      | `DELETE, None -> Cohttp_async.Client.delete ~headers uri
    in

    make_request () >>= fun (response, resp_body) ->
    match Cohttp.Response.status response with
    | `OK | `Created ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      (try
        let json = Yojson.Safe.from_string s in
        Response.parse json Operation.response_of_yojson
      with exn ->
        `Json_parse_error Error.{ message = Exn.to_string exn; body = s })
    | `Bad_request ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Bad_request s
    | `Not_found ->
      return `Not_found
    | `Unauthorized ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Unauthorized s
    | `Forbidden ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Forbidden s
    | `Too_many_requests ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Too_many_requests s
    | `Service_unavailable | `Internal_server_error | `Gateway_timeout ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Service_unavailable s
    | status ->
      Cohttp_async.Body.to_string resp_body >>| fun s ->
      `Bad_request (sprintf "Unexpected status %s: %s"
        (Cohttp.Code.string_of_status status) s)
end

module Make (Operation : Operation.S) = struct
  include Request (Operation)

  let command =
    Command.async ~summary:Operation.name
      (let%map_open.Command cfg = Cfg.param
       and req_sexp = anon ("REQUEST" %: sexp_conv [%of_sexp: Operation.request])
       in
       fun () ->
         let cfg = Cfg.or_default cfg in
         let%bind result = request cfg req_sexp in
         printf !"%{sexp#hum:[ `Ok of Operation.response | Error.t ]}\n" result;
         return ())
end

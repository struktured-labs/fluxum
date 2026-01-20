(** Bybit REST API Client - V5 API

    Implements Bybit's unified V5 API for spot, derivatives, and options trading.

    Key features:
    - HMAC-SHA256 authentication
    - JSON POST bodies (not form-encoded)
    - Result-wrapped responses: {"retCode": 0, "retMsg": "OK", "result": {...}}
    - Configurable base URL (production/testnet)

    Authentication headers:
    - X-BAPI-API-KEY: API key
    - X-BAPI-TIMESTAMP: Request timestamp (milliseconds)
    - X-BAPI-SIGN: HMAC-SHA256 signature
    - X-BAPI-RECV-WINDOW: Validity window (5000ms default)

    Signing string format: timestamp + api_key + recv_window + params
    - For GET: params is query string (NOT URL-encoded in signature)
    - For POST: params is JSON body string
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
    { retCode : int
    ; retMsg : string
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
  (** Parse Bybit V5 response format

      Bybit returns:
      - Success: {"retCode": 0, "retMsg": "OK", "result": {...}, "time": 1234567890}
      - Error: {"retCode": 10001, "retMsg": "error description"}
  *)
  let parse json result_of_yojson =
    match json with
    | `Assoc fields -> (
      (* Check retCode for errors *)
      match List.Assoc.find fields ~equal:String.equal "retCode" with
      | Some (`Int retCode) when retCode <> 0 ->
        let retMsg =
          List.Assoc.find fields ~equal:String.equal "retMsg"
          |> Option.value_map ~default:"Unknown error" ~f:(function
            | `String s -> s
            | j -> Yojson.Safe.to_string j)
        in
        `Api_error Error.{ retCode; retMsg }
      | _ ->
        (* Extract result field *)
        let result_json =
          List.Assoc.find fields ~equal:String.equal "result"
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
  (** Generate timestamp in milliseconds *)
  let generate_timestamp () =
    Int63.to_string (Int63.of_float (Core_unix.gettimeofday () *. 1000.0))

  (** Recv window - validity window in milliseconds *)
  let recv_window = "5000"

  (** Build query string from parameters

      Note: Bybit expects NO URL encoding in the signature!
      The actual HTTP request will URL-encode, but signature uses plain values.
  *)
  let build_query_string params =
    String.concat ~sep:"&"
      (List.map params ~f:(fun (k, v) ->
        sprintf "%s=%s" k v))

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

  (** Sign Bybit V5 request

      Signing string: timestamp + api_key + recv_window + params
      - For GET: params is query string (NO URL encoding!)
      - For POST: params is JSON body string
  *)
  let sign ~api_secret ~timestamp ~api_key ~params =
    let message = timestamp ^ api_key ^ recv_window ^ params in
    let hash = hmac_sha256 ~secret:api_secret ~message in
    Digestif.SHA256.to_hex hash
end

module Request (Operation : Operation.S) = struct
  let request (module Cfg : Cfg.S) (req : Operation.request) =
    let base_params = Operation.request_to_params req in

    (* Build URI, headers, and body based on auth requirement and HTTP method *)
    let uri, headers, body =
      match Operation.requires_auth with
      | true ->
        let timestamp = Signature.generate_timestamp () in
        let params_string, json_body =
          match Operation.http_method with
          | `GET ->
            let qs = Signature.build_query_string base_params in
            (qs, None)
          | `POST | `DELETE ->
            (* For POST/DELETE, params become JSON body *)
            let json = `Assoc (List.map base_params ~f:(fun (k, v) -> (k, `String v))) in
            let body_str = Yojson.Safe.to_string json in
            (body_str, Some body_str)
        in
        let signature =
          Signature.sign
            ~api_secret:Cfg.api_secret
            ~timestamp
            ~api_key:Cfg.api_key
            ~params:params_string
        in
        let headers =
          Cohttp.Header.of_list
            [ ("X-BAPI-API-KEY", Cfg.api_key)
            ; ("X-BAPI-TIMESTAMP", timestamp)
            ; ("X-BAPI-SIGN", signature)
            ; ("X-BAPI-RECV-WINDOW", Signature.recv_window)
            ; ("Content-Type", "application/json")
            ]
        in
        let uri =
          match Operation.http_method with
          | `GET ->
            Uri.make
              ~scheme:"https"
              ~host:Cfg.base_url
              ~path:(sprintf "/v5/%s" Operation.endpoint)
              ~query:(List.map base_params ~f:(fun (k, v) -> (k, [ v ])))
              ()
          | `POST | `DELETE ->
            Uri.make
              ~scheme:"https"
              ~host:Cfg.base_url
              ~path:(sprintf "/v5/%s" Operation.endpoint)
              ()
        in
        let cohttp_body = Option.map json_body ~f:Cohttp_async.Body.of_string in
        (uri, headers, cohttp_body)
      | false ->
        (* Public endpoint - no authentication *)
        let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ] in
        let uri =
          Uri.make
            ~scheme:"https"
            ~host:Cfg.base_url
            ~path:(sprintf "/v5/%s" Operation.endpoint)
            ~query:(List.map base_params ~f:(fun (k, v) -> (k, [ v ])))
            ()
        in
        (uri, headers, None)
    in

    Log.Global.debug "Bybit API call: %s %s"
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
      Log.Global.debug "Response: %s" s;
      (try
        let json = Yojson.Safe.from_string s in
        Response.parse json Operation.response_of_yojson
      with e ->
        `Json_parse_error Error.{ message = Exn.to_string e; body = s })
    | `Bad_request ->
      Cohttp_async.Body.to_string resp_body >>| fun b -> `Bad_request b
    | `Not_found -> return `Not_found
    | `Unauthorized ->
      Cohttp_async.Body.to_string resp_body >>| fun b -> `Unauthorized b
    | `Forbidden ->
      Cohttp_async.Body.to_string resp_body >>| fun b -> `Forbidden b
    | `Too_many_requests ->
      Cohttp_async.Body.to_string resp_body >>| fun b -> `Too_many_requests b
    | `Service_unavailable ->
      Cohttp_async.Body.to_string resp_body >>| fun b -> `Service_unavailable b
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string resp_body >>| fun b ->
      failwiths
        ~here:[%here]
        (sprintf "Unexpected Bybit API status code (body=%S)" b)
        code
        Cohttp.Code.sexp_of_status_code
end

module Make (Operation : Operation.S) = struct
  include Request (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name
    , Command.async
        ~summary:(sprintf "Bybit %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param
          and req = anon ("request" %: sexp) in
          fun () ->
            let req = Operation.request_of_sexp req in
            let config = Cfg.or_default config in
            Deferred.return () >>= fun () ->
            request config req >>= function
            | `Ok response ->
              Log.Global.info
                "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths
                ~here:[%here]
                (sprintf "Bybit %s failed" Operation.endpoint)
                err
                Error.sexp_of_t] )
end

module Make_no_arg (Operation : Operation.S_NO_ARG) = struct
  include Request (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name
    , Command.async
        ~summary:(sprintf "Bybit %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param in
          fun () ->
            let config = Cfg.or_default config in
            Deferred.return () >>= fun () ->
            request config () >>= function
            | `Ok response ->
              Log.Global.info
                "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths
                ~here:[%here]
                (sprintf "Bybit %s failed" Operation.endpoint)
                err
                Error.sexp_of_t] )
end

module Make_with_params
    (Operation : Operation.S)
    (Params : sig
       val params : Operation.request Command.Param.t
     end) =
struct
  include Request (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name
    , Command.async
        ~summary:(sprintf "Bybit %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param
          and req = Params.params
          and sexp_request = anon (maybe ("request-sexp" %: sexp)) in
          fun () ->
            let req =
              match sexp_request with
              | Some sexp -> Operation.request_of_sexp sexp
              | None -> req
            in
            Log.Global.info
              "Request: %s"
              (Operation.sexp_of_request req |> Sexp.to_string);
            let config = Cfg.or_default config in
            request config req >>= function
            | `Ok response ->
              Log.Global.info
                "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths
                ~here:[%here]
                (sprintf "Bybit %s failed" Operation.endpoint)
                err
                Error.sexp_of_t] )
end

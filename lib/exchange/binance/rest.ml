(** Binance REST API Client *)

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
    { code : int
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
  (** Parse Binance response - handles both success and error formats

      Binance can return:
      - Success: Direct JSON object or array
      - Error: {"code": <int>, "msg": "<string>"}
  *)
  let parse json result_of_yojson =
    match json with
    | `Assoc fields -> (
      (* Check for error response with code field *)
      match List.Assoc.find fields ~equal:String.equal "code" with
      | Some (`Int code) ->
        let msg =
          List.Assoc.find fields ~equal:String.equal "msg"
          |> Option.value_map ~default:"Unknown error" ~f:(function
            | `String s -> s
            | j -> Yojson.Safe.to_string j)
        in
        `Api_error Error.{ code; msg }
      | _ ->
        (* Success response - parse directly *)
        (match result_of_yojson json with
        | Result.Ok x -> `Ok x
        | Result.Error e ->
          `Json_parse_error Error.{ message = e; body = Yojson.Safe.to_string json }))
    | `List _ ->
      (* Direct array response *)
      (match result_of_yojson json with
      | Result.Ok x -> `Ok x
      | Result.Error e ->
        `Json_parse_error Error.{ message = e; body = Yojson.Safe.to_string json })
    | _ ->
      `Json_parse_error
        Error.{ message = "Unexpected response format"; body = Yojson.Safe.to_string json }
end

module Request (Operation : Operation.S) = struct
  let request (module Cfg : Cfg.S) (req : Operation.request) =
    let base_params = Operation.request_to_params req in

    (* Add timestamp and signature for authenticated endpoints *)
    let params, headers =
      match Operation.requires_auth with
      | true ->
        let timestamp = Signature.generate_timestamp () in
        let params_with_ts = base_params @ [ ("timestamp", timestamp) ] in
        let signature =
          Signature.sign ~api_secret:Cfg.api_secret ~params:params_with_ts
        in
        let final_params = params_with_ts @ [ ("signature", signature) ] in
        let headers =
          Cohttp.Header.of_list
            [ ("X-MBX-APIKEY", Cfg.api_key)
            ; ("Content-Type", "application/json")
            ]
        in
        (final_params, headers)
      | false ->
        let headers =
          Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
        in
        (base_params, headers)
    in

    let path = sprintf "/api/v3/%s" Operation.endpoint in
    let uri =
      Uri.make
        ~scheme:"https"
        ~host:Cfg.base_url
        ~path
        ~query:(List.map params ~f:(fun (k, v) -> (k, [ v ])))
        ()
    in

    Log.Global.debug
      "Binance API call: %s %s"
      (match Operation.http_method with
      | `GET -> "GET"
      | `POST -> "POST"
      | `DELETE -> "DELETE")
      Operation.endpoint;

    let make_request () =
      match Operation.http_method with
      | `GET ->
        if Operation.requires_auth
        then Cohttp_async.Client.get ~headers ?interrupt:None ?ssl_config:None uri
        else Cohttp_async.Client.get ?interrupt:None ?ssl_config:None uri
      | `POST ->
        (* For POST, include params in both query string and body *)
        let query_string = Signature.build_query_string params in
        let body = Cohttp_async.Body.of_string query_string in
        let headers =
          Cohttp.Header.replace headers "Content-Type" "application/x-www-form-urlencoded"
        in
        Cohttp_async.Client.post ~headers ~body ?chunked:None ?interrupt:None ?ssl_config:None uri
      | `DELETE -> Cohttp_async.Client.delete ~headers ?chunked:None ?interrupt:None ?ssl_config:None uri
    in

    make_request () >>= fun (response, body) ->
    match Cohttp.Response.status response with
    | `OK | `Created ->
      Cohttp_async.Body.to_string body >>| fun s ->
      Log.Global.debug "Response: %s" s;
      (try
        let json = Yojson.Safe.from_string s in
        Response.parse json Operation.response_of_yojson
      with e -> `Json_parse_error Error.{ message = Exn.to_string e; body = s })
    | `Bad_request ->
      Cohttp_async.Body.to_string body >>| fun b -> `Bad_request b
    | `Not_found -> return `Not_found
    | `Unauthorized ->
      Cohttp_async.Body.to_string body >>| fun b -> `Unauthorized b
    | `Forbidden ->
      Cohttp_async.Body.to_string body >>| fun b -> `Forbidden b
    | `Too_many_requests ->
      Cohttp_async.Body.to_string body >>| fun b -> `Too_many_requests b
    | `Service_unavailable ->
      Cohttp_async.Body.to_string body >>| fun b -> `Service_unavailable b
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string body >>| fun b ->
      failwiths ~here:[%here]
        (sprintf "Unexpected Binance API status code (body=%S)" b)
        code Cohttp.Code.sexp_of_status_code
end

module Make (Operation : Operation.S) = struct
  include Request (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name
    , Command.async
        ~summary:(sprintf "Binance %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param
          and req = anon ("request" %: sexp) in
          fun () ->
            let req = Operation.request_of_sexp req in
            let config = Cfg.or_default config in
            (* CRITICAL: Add intermediate deferred to initialize async scheduler *)
            Deferred.return () >>= fun () ->
            request config req >>= function
            | `Ok response ->
              Log.Global.info "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths ~here:[%here]
                (sprintf "Binance %s failed" Operation.endpoint)
                err Error.sexp_of_t] )
end

module Make_no_arg (Operation : Operation.S_NO_ARG) = struct
  include Request (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name
    , Command.async
        ~summary:(sprintf "Binance %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param in
          fun () ->
            let config = Cfg.or_default config in
            (* CRITICAL: Add intermediate deferred to initialize async scheduler *)
            Deferred.return () >>= fun () ->
            request config () >>= function
            | `Ok response ->
              Log.Global.info "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths ~here:[%here]
                (sprintf "Binance %s failed" Operation.endpoint)
                err Error.sexp_of_t] )
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
        ~summary:(sprintf "Binance %s endpoint" Operation.endpoint)
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
            Log.Global.info "Request: %s"
              (Operation.sexp_of_request req |> Sexp.to_string);
            let config = Cfg.or_default config in
            (* CRITICAL: Add intermediate deferred to initialize async scheduler *)
            Deferred.return () >>= fun () ->
            request config req >>= function
            | `Ok response ->
              Log.Global.info "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.t as err ->
              failwiths ~here:[%here]
                (sprintf "Binance %s failed" Operation.endpoint)
                err Error.sexp_of_t] )
end

open Core
open Async

module Error = struct
  type http =
    [ `Bad_request of string
    | `Not_found
    | `Service_unavailable of string
    | `Not_acceptable of string
    | `Unauthorized of string
    | `Too_many_requests of string
    ]
  [@@deriving sexp]

  type json_error =
    { message : string;
      body : string
    }
  [@@deriving sexp]

  type json = [ `Json_parse_error of json_error ] [@@deriving sexp]

  type api_error = { errors : string list } [@@deriving sexp]

  type response = [ `Api_error of api_error ] [@@deriving sexp]

  type post =
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

    type request [@@deriving sexp]

    val request_to_params : request -> (string * string) list

    type response [@@deriving sexp, of_yojson]
  end

  module type S_NO_ARG = sig
    include S with type request = unit
  end
end

module Response = struct
  (** Kraken response format: {"error": [...], "result": {...}} *)
  let parse json result_of_yojson =
    match json with
    | `Assoc fields -> (
      (* Extract error field *)
      let errors =
        List.Assoc.find fields ~equal:String.equal "error"
        |> Option.bind ~f:(function
          | `List lst ->
            Some
              (List.map lst ~f:(function
                | `String s -> s
                | j -> Yojson.Safe.to_string j))
          | _ -> None)
        |> Option.value ~default:[]
      in
      (* Extract result field *)
      let result =
        List.Assoc.find fields ~equal:String.equal "result"
        |> Option.value ~default:`Null
      in
      match errors with
      | [] -> (
        match result_of_yojson result with
        | Result.Ok x -> `Ok x
        | Result.Error e ->
          `Json_parse_error
            Error.{ message = e; body = Yojson.Safe.to_string result } )
      | _ :: _ -> `Api_error Error.{ errors } )
    | _ ->
      `Json_parse_error
        Error.{ message = "Expected object"; body = Yojson.Safe.to_string json }
end

(** Generate nonce from current Unix timestamp in milliseconds *)
let generate_nonce () =
  Int.to_string (Int.of_float (Unix.gettimeofday () *. 1000.0))

(** Build form-encoded POST data *)
let build_post_data params =
  String.concat ~sep:"&"
    (List.map params ~f:(fun (k, v) ->
      sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v)))

module Post (Operation : Operation.S) = struct
  let post (module Cfg : Cfg.S) (request : Operation.request) =
    (* Generate nonce *)
    let nonce = generate_nonce () in

    (* Build POST parameters *)
    let request_params = Operation.request_to_params request in
    let post_params = ("nonce", nonce) :: request_params in
    let post_data = build_post_data post_params in

    (* API path for signing *)
    let api_path = sprintf "/0/private/%s" Operation.endpoint in

    (* Generate signature *)
    let signature =
      Signature.kraken_signature
        ~api_secret_b64:Cfg.api_secret
        ~api_path
        ~nonce
        ~post_data
    in

    (* Build headers *)
    let headers =
      Cohttp.Header.of_list
        [ ("API-Key", Cfg.api_key)
        ; ("API-Sign", signature)
        ; ("Content-Type", "application/x-www-form-urlencoded")
        ]
    in

    (* Build URI *)
    let uri = Uri.make ~scheme:"https" ~host:"api.kraken.com" ~path:api_path () in

    (* Make request *)
    let body = Cohttp_async.Body.of_string post_data in

    Log.Global.debug "Kraken API call: %s" Operation.endpoint;
    Log.Global.debug "POST data: %s" post_data;

    Cohttp_async.Client.post ~headers ~body uri >>= fun (response, body) ->
    match Cohttp.Response.status response with
    | `OK ->
      Cohttp_async.Body.to_string body >>| fun s ->
      Log.Global.debug "Response: %s" s;
      (try
        let json = Yojson.Safe.from_string s in
        Response.parse json Operation.response_of_yojson
      with e ->
        `Json_parse_error
          Error.{ message = Exn.to_string e; body = s })
    | `Not_found ->
      return `Not_found
    | `Not_acceptable ->
      Cohttp_async.Body.to_string body >>| fun b -> `Not_acceptable b
    | `Bad_request ->
      Cohttp_async.Body.to_string body >>| fun b -> `Bad_request b
    | `Service_unavailable ->
      Cohttp_async.Body.to_string body >>| fun b -> `Service_unavailable b
    | `Unauthorized ->
      Cohttp_async.Body.to_string body >>| fun b -> `Unauthorized b
    | `Too_many_requests ->
      Cohttp_async.Body.to_string body >>| fun b -> `Too_many_requests b
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string body >>| fun b ->
      failwiths ~here:[%here]
        (sprintf "Unexpected Kraken API status code (body=%S)" b)
        code Cohttp.Code.sexp_of_status_code
end

module Make (Operation : Operation.S) = struct
  include Post (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name,
      Command.async
        ~summary:(sprintf "Kraken %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param
          and request = anon ("request" %: sexp) in
          fun () ->
            let request = Operation.request_of_sexp request in
            Log.Global.info "Request: %s"
              (Operation.sexp_of_request request |> Sexp.to_string);
            let config = Cfg.or_default config in
            post config request >>= function
            | `Ok response ->
              Log.Global.info "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.post as post_error ->
              failwiths ~here:[%here]
                (sprintf "Kraken %s failed" Operation.endpoint)
                post_error Error.sexp_of_post] )
end

module Make_no_arg (Operation : Operation.S_NO_ARG) = struct
  include Post (Operation)

  let command =
    let open Command.Let_syntax in
    ( Operation.name,
      Command.async
        ~summary:(sprintf "Kraken %s endpoint" Operation.endpoint)
        [%map_open
          let config = Cfg.param in
          fun () ->
            let request = () in
            let config = Cfg.or_default config in
            post config request >>= function
            | `Ok response ->
              Log.Global.info "Response: %s"
                (Sexp.to_string_hum (Operation.sexp_of_response response));
              Log.Global.flushed ()
            | #Error.post as post_error ->
              failwiths ~here:[%here]
                (sprintf "Kraken %s failed" Operation.endpoint)
                post_error Error.sexp_of_post] )
end

(** REST API support for the Kraken exchange. *)
[@@@warning "-67"]

(** Error types for Kraken API calls *)
module Error : sig
  (** HTTP-level errors *)
  type http =
    [ `Bad_request of string
    | `Not_found
    | `Service_unavailable of string
    | `Not_acceptable of string
    | `Unauthorized of string
    | `Too_many_requests of string
    ]
  [@@deriving sexp]

  (** JSON parsing errors *)
  type json_error =
    { message : string;
      body : string
    }
  [@@deriving sexp]

  type json = [ `Json_parse_error of json_error ] [@@deriving sexp]

  (** Kraken API error messages *)
  type api_error = { errors : string list } [@@deriving sexp]

  (** Application-level errors *)
  type response = [ `Api_error of api_error ] [@@deriving sexp]

  (** All possible POST errors *)
  type post =
    [ http
    | json
    | response
    ]
  [@@deriving sexp]
end

(** Operation specification for REST endpoints *)
module Operation : sig
  (** Minimal specification for a REST operation *)
  module type S = sig
    (** Human-readable name *)
    val name : string

    (** API endpoint path (e.g., ["AddOrder"]) *)
    val endpoint : string

    (** Request parameters as (key, value) pairs *)
    type request [@@deriving sexp]

    (** Convert request to form parameters *)
    val request_to_params : request -> (string * string) list

    (** Response type *)
    type response [@@deriving sexp, of_yojson]
  end

  (** Operation with no request parameters *)
  module type S_NO_ARG = sig
    include S with type request = unit
  end
end

(** Response parsing *)
module Response : sig
  (** Parse Kraken response format: {"error": [...], "result": {...}} *)
  val parse :
    Yojson.Safe.t ->
    (Yojson.Safe.t -> ('a, string) result) ->
    [ `Api_error of Error.api_error
    | `Json_parse_error of Error.json_error
    | `Ok of 'a
    ]
end

(** POST request builder *)
module Post : functor (Operation : Operation.S) -> sig
  val post :
    (module Cfg.S) ->
    Operation.request ->
    [ `Ok of Operation.response | Error.post ] Async.Deferred.t
end

(** Make a full REST endpoint with CLI command *)
module Make : functor (Operation : Operation.S) -> sig
  val post :
    (module Cfg.S) ->
    Operation.request ->
    [ `Ok of Operation.response | Error.post ] Async.Deferred.t

  val command : string * Core.Command.t
end

(** Make a no-arg REST endpoint with CLI command *)
module Make_no_arg : functor (Operation : Operation.S_NO_ARG) -> sig
  val post :
    (module Cfg.S) ->
    unit ->
    [ `Ok of Operation.response | Error.post ] Async.Deferred.t

  val command : string * Core.Command.t
end

(** Make a REST endpoint with natural CLI flags instead of sexp *)
module Make_with_params : functor
  (Operation : Operation.S)
  (Params : sig
     val params : Operation.request Core.Command.Param.t
   end) -> sig
  val post :
    (module Cfg.S) ->
    Operation.request ->
    [ `Ok of Operation.response | Error.post ] Async.Deferred.t

  val command : string * Core.Command.t
end

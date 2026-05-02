open Core

type error =
  [ `Http of int * string
  | `Network of string
  | `Json_parse of string
  | `Not_found of string
  | `Rate_limited of string
  | `Symbol_unknown of string
  ]
[@@deriving sexp]

module Price = struct
  type t =
    { symbol : string
    ; price : float
    ; confidence : float option
    ; ts : Time_float_unix.t
    ; venue_metadata : (string * string) list
    }
  [@@deriving sexp, compare, equal]
end

module Symbol_info = struct
  type t =
    { symbol : string
    ; asset_class : string option
    ; quote_currency : string option
    ; description : string option
    }
  [@@deriving sexp]
end

module type S = sig
  val name : string
  val homepage : string
  val us_geo_restricted : bool

  val list_symbols :
    ?query:string ->
    ?limit:int ->
    unit ->
    (Symbol_info.t list, [> error ]) Result.t Async.Deferred.t

  val get_price :
    symbol:string ->
    (Price.t, [> error ]) Result.t Async.Deferred.t

  val get_prices :
    symbols:string list ->
    (Price.t list, [> error ]) Result.t Async.Deferred.t
end

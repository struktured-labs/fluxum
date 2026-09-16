open Core

type error =
  [ `Http of int * string
  | `Network of string
  | `Json_parse of string
  | `Not_found of string
  | `Rate_limited of string
  | `Entity_unknown of string
  ]
[@@deriving sexp]

module Stats = struct
  type t =
    { slug : string
    ; name : string
    ; category : string option
    ; chain : string option
    ; tvl_usd : float option
    ; volume_24h_usd : float option
    ; fees_24h_usd : float option
    ; revenue_24h_usd : float option
    ; ts : Time_float_unix.t
    ; venue_metadata : (string * string) list
    }
  [@@deriving sexp]
end

module type S = sig
  val name : string
  val homepage : string
  val us_geo_restricted : bool

  val list_entities :
    ?category:string ->
    ?limit:int ->
    unit ->
    (Stats.t list, [> error ]) Result.t Async.Deferred.t

  val get_entity :
    slug:string ->
    (Stats.t, [> error ]) Result.t Async.Deferred.t
end

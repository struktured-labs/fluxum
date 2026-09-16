open Core

type error =
  [ `Http of int * string
  | `Network of string
  | `Json_parse of string
  | `Not_found of string
  | `Rate_limited of string
  ]
[@@deriving sexp]

module Outcome = struct
  type t =
    { id : string
    ; label : string
    ; token_id : string option
    }
  [@@deriving sexp, compare, equal]
end

module Event = struct
  type t =
    { id : string
    ; title : string
    ; description : string option
    ; category : string option
    ; outcomes : Outcome.t array
    ; resolution_ts : Time_float_unix.t option
    ; is_resolved : bool
    ; resolved_outcome_id : string option
    ; ts_listed : Time_float_unix.t option
    }
  [@@deriving sexp, compare, equal]
end

module Quote = struct
  type t =
    { outcome_id : string
    ; last_price : float option
    ; bid : float option
    ; ask : float option
    ; bid_size : float option
    ; ask_size : float option
    ; ts : Time_float_unix.t
    }
  [@@deriving sexp, compare, equal]
end

module Trade = struct
  type t =
    { outcome_id : string
    ; price : float
    ; size : float
    ; aggressor : [ `Buy | `Sell ] option
    ; ts : Time_float_unix.t
    ; trade_id : string option
    }
  [@@deriving sexp, compare, equal]
end

module Book_level = struct
  type t =
    { price : float
    ; size : float
    }
  [@@deriving sexp, compare, equal]
end

module Book = struct
  type t =
    { outcome_id : string
    ; bids : Book_level.t array
    ; asks : Book_level.t array
    ; ts : Time_float_unix.t
    }
  [@@deriving sexp, compare, equal]
end

module List_filter = struct
  type t =
    { limit : int option
    ; category : string option
    ; include_resolved : bool
    }
  [@@deriving sexp]

  let default = { limit = None; category = None; include_resolved = false }
end

(** Sibling interfaces for non-event-shaped feeds.
    See {!Spot_feed.S} for per-symbol price feeds (oracles, aggregators)
    and {!Aggregate_feed.S} for protocol/chain-level stats (DefiLlama,
    L2Beat). *)
module Spot_feed = Spot_feed
module Aggregate_feed = Aggregate_feed

module type S = sig
  val name : string
  val homepage : string
  val us_geo_restricted : bool

  val list_events :
    ?filter:List_filter.t ->
    unit ->
    (Event.t list, [> error ]) Result.t Async.Deferred.t

  val get_event :
    event_id:string ->
    (Event.t, [> error ]) Result.t Async.Deferred.t

  val get_quote :
    outcome_id:string ->
    (Quote.t, [> error ]) Result.t Async.Deferred.t

  val get_book :
    outcome_id:string ->
    ?depth:int ->
    unit ->
    (Book.t, [> error ]) Result.t Async.Deferred.t

  val get_recent_trades :
    outcome_id:string ->
    ?limit:int ->
    unit ->
    (Trade.t list, [> error ]) Result.t Async.Deferred.t

  val get_resolved_events :
    ?since:Core.Time_float.Span.t ->
    ?limit:int ->
    ?category:string ->
    unit ->
    (Event.t list, [> error ]) Result.t Async.Deferred.t
end

(** Read-only data-feed interface for venues that expose public market
    data without authenticated trading.

    Distinct from {!Exchange_intf.S}: deliberately omits all trading
    methods (place_order, cancel, balances, etc) at the type level so
    callers cannot accidentally try to trade against a feed-only source.
    The type system rejects misuse rather than relying on runtime errors
    or stub implementations.

    {b Use cases this serves:}
    - Geo-restricted venues whose public data is unrestricted
      (Polymarket trading is CFTC-blocked from US persons; data feed
      is unrestricted)
    - Pure-aggregator services (DefiLlama, L2Beat, CoinGecko)
    - Oracle networks (Pyth, Chainlink)
    - Public analytics feeds (Glassnode, Nansen — read-only by API)
    - Any venue where you want a hard "no trading" guarantee at compile
      time rather than runtime

    {b Domain coverage}: the interface is shaped for prediction-market
    and event-contract data (events with N outcomes, per-outcome
    orderbooks, resolution status). For pure spot/futures price feeds
    where there's no event/outcome structure, a future {!Spot_feed}
    sub-interface may emerge — but the prediction-market shape covers
    Polymarket / Kalshi-feed-mode / Manifold cleanly. *)

(** Errors common to all feed-only adapters. Adapters may extend this
    with venue-specific tags via polymorphic-variant subtyping. *)
type error =
  [ `Http of int * string
      (** HTTP status code + response body excerpt. *)
  | `Network of string
      (** Network-level error (connection refused, DNS, timeout). *)
  | `Json_parse of string
      (** Response body parsed as JSON but didn't match expected schema,
          or wasn't valid JSON at all. *)
  | `Not_found of string
      (** Resource ID does not exist on the venue. *)
  | `Rate_limited of string
      (** Venue rejected the request for rate-limit reasons. Body
          excerpt included for diagnosis. *)
  ]
[@@deriving sexp]

(** A specific outcome of an event. For binary markets this is YES or
    NO; for categorical markets there are N outcomes (race winner,
    weather bin, election candidate). *)
module Outcome : sig
  type t =
    { id : string
        (** Venue-specific stable identifier — use for queries. *)
    ; label : string
        (** Human-readable name ("YES", "Donald Trump", "75-80°F"). *)
    ; token_id : string option
        (** Some venues (Polymarket) issue an on-chain token per outcome
            in addition to the API id. Carry it for callers that need
            the on-chain side. *)
    }
  [@@deriving sexp, compare, equal]
end

(** A market / event with one or more outcomes. *)
module Event : sig
  type t =
    { id : string (** Venue-specific stable identifier. *)
    ; title : string (** Human-readable headline. *)
    ; description : string option
    ; category : string option
        (** Venue's own category label ("Politics", "Sports", "Weather",
            "Crypto", etc). Not normalized across venues. *)
    ; outcomes : Outcome.t array (** Always at least 1; binary = 2. *)
    ; resolution_ts : Time_float_unix.t option
        (** When the event resolves / settles. [None] for events without
            a fixed resolution time. *)
    ; is_resolved : bool
    ; resolved_outcome_id : string option
        (** [Some id] iff [is_resolved] AND a winning outcome exists. *)
    ; ts_listed : Time_float_unix.t option
        (** When the event was first listed for trading. *)
    }
  [@@deriving sexp, compare, equal]
end

(** Top-of-book quote for a single outcome. *)
module Quote : sig
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

(** A public trade event. *)
module Trade : sig
  type t =
    { outcome_id : string
    ; price : float
    ; size : float
    ; aggressor : [ `Buy | `Sell ] option
        (** [None] when the venue doesn't expose taker side. *)
    ; ts : Time_float_unix.t
    ; trade_id : string option
    }
  [@@deriving sexp, compare, equal]
end

(** A single price level on one side of an orderbook. *)
module Book_level : sig
  type t =
    { price : float
    ; size : float
    }
  [@@deriving sexp, compare, equal]
end

(** Snapshot of an orderbook for one outcome. *)
module Book : sig
  type t =
    { outcome_id : string
    ; bids : Book_level.t array (** Sorted descending by price. *)
    ; asks : Book_level.t array (** Sorted ascending by price. *)
    ; ts : Time_float_unix.t
    }
  [@@deriving sexp, compare, equal]
end

(** Filters for {!S.list_events}. *)
module List_filter : sig
  type t =
    { limit : int option
    ; category : string option
    ; include_resolved : bool
        (** When [false] (default), only currently-tradeable events.
            When [true], includes recently-resolved events too. *)
    }
  [@@deriving sexp]

  val default : t
end

(** The interface every read-only data-feed adapter implements. *)
module type S = sig
  (** Human-readable adapter name. *)
  val name : string

  (** Venue homepage URL. *)
  val homepage : string

  (** Whether trading on this venue is geo-restricted from US persons
      (informational, doesn't affect feed access). Distinguishes
      "feed-only by venue policy" from "feed-only by jurisdiction". *)
  val us_geo_restricted : bool

  (** Fetch active events. Default: only currently-tradeable, no limit
      cap (uses venue default). *)
  val list_events :
    ?filter:List_filter.t ->
    unit ->
    (Event.t list, [> error ]) Result.t Async.Deferred.t

  (** Fetch a single event by id. *)
  val get_event :
    event_id:string ->
    (Event.t, [> error ]) Result.t Async.Deferred.t

  (** Top-of-book quote for one outcome. *)
  val get_quote :
    outcome_id:string ->
    (Quote.t, [> error ]) Result.t Async.Deferred.t

  (** Multi-level orderbook for one outcome. [?depth] caps how many
      levels per side; venues may return fewer than requested. *)
  val get_book :
    outcome_id:string ->
    ?depth:int ->
    unit ->
    (Book.t, [> error ]) Result.t Async.Deferred.t

  (** Recent trades for an outcome, newest first. *)
  val get_recent_trades :
    outcome_id:string ->
    ?limit:int ->
    unit ->
    (Trade.t list, [> error ]) Result.t Async.Deferred.t

  (** Resolved events, useful for calibration analysis. *)
  val get_resolved_events :
    ?since:Core.Time_float.Span.t ->
    ?limit:int ->
    ?category:string ->
    unit ->
    (Event.t list, [> error ]) Result.t Async.Deferred.t
end

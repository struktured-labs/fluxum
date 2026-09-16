(** Read-only aggregate-stats feed — protocol/chain/category-level metrics
    rather than per-symbol prices.

    Distinct from {!Spot_feed.S} (per-symbol prices) and
    {!Feed_only_adapter.S} (events with outcomes). Aggregate feeds answer
    "what's the TVL of protocol X" or "what's the daily volume on DEX Y"
    or "what's the active-address count on chain Z." Examples:

    - DefiLlama (TVL, fees, DEX volume)
    - L2Beat (L2 TVL, transaction counts)
    - Token Terminal (protocol revenue, P/E)
    - Glassnode / Nansen-aggregate (chain-level on-chain stats) *)

type error =
  [ `Http of int * string
  | `Network of string
  | `Json_parse of string
  | `Not_found of string
  | `Rate_limited of string
  | `Entity_unknown of string
  ]
[@@deriving sexp]

(** A single aggregate-stats record for one entity (protocol, chain,
    category, etc.). Fields are loosely typed because different feeds
    expose different subsets — callers branch on what's [Some]. *)
module Stats : sig
  type t =
    { slug : string
        (** Venue-specific stable identifier ("uniswap", "ethereum",
            "lending"). *)
    ; name : string
    ; category : string option
        (** Venue's own category taxonomy. *)
    ; chain : string option
        (** Single chain when applicable; [None] for multi-chain or
            chain-level entities. *)
    ; tvl_usd : float option
    ; volume_24h_usd : float option
    ; fees_24h_usd : float option
    ; revenue_24h_usd : float option
    ; ts : Time_float_unix.t
    ; venue_metadata : (string * string) list
        (** Venue-specific extras. *)
    }
  [@@deriving sexp]
end

module type S = sig
  val name : string
  val homepage : string
  val us_geo_restricted : bool

  (** List entities on this venue. Filter by [?category] when supported. *)
  val list_entities :
    ?category:string ->
    ?limit:int ->
    unit ->
    (Stats.t list, [> error ]) Result.t Async.Deferred.t

  (** Fetch one entity by slug. *)
  val get_entity :
    slug:string ->
    (Stats.t, [> error ]) Result.t Async.Deferred.t
end

(** Read-only spot price feed — one price-per-symbol streams.

    Distinct from {!Feed_only_adapter.S} (event-with-outcomes shape).
    Spot feeds answer "what's the canonical price for this symbol right
    now?" without per-event/per-outcome structure. Examples:

    - Pyth Network (oracle, sub-second updates)
    - Chainlink price feeds
    - CoinGecko / CoinMarketCap (aggregator pricing)
    - Exchange-native ticker streams when used as a pure price source

    For exchanges with full trading APIs, prefer {!Exchange_intf.S}.
    For events with outcomes (prediction markets), use
    {!Feed_only_adapter.S}. *)

type error =
  [ `Http of int * string
  | `Network of string
  | `Json_parse of string
  | `Not_found of string
  | `Rate_limited of string
  | `Symbol_unknown of string
      (** Venue doesn't recognize this symbol. *)
  ]
[@@deriving sexp]

(** Latest price snapshot for one symbol. *)
module Price : sig
  type t =
    { symbol : string (** Canonical symbol form, venue-specific. *)
    ; price : float
        (** Normalized price in the symbol's quote currency. *)
    ; confidence : float option
        (** Venue's confidence interval / spread. [None] when not
            exposed by the venue. *)
    ; ts : Time_float_unix.t
        (** Publish time of this update on the venue. *)
    ; venue_metadata : (string * string) list
        (** Venue-specific extra fields the caller may want — slot,
            sequence number, EMA price, etc. Caller treats as opaque. *)
    }
  [@@deriving sexp, compare, equal]
end

(** A symbol's metadata, returned by {!list_symbols}. *)
module Symbol_info : sig
  type t =
    { symbol : string
    ; asset_class : string option
        (** "Crypto", "Equity", "FX", "Commodity", etc. Venue's own
            taxonomy; not normalized across venues. *)
    ; quote_currency : string option
    ; description : string option
    }
  [@@deriving sexp]
end

module type S = sig
  val name : string
  val homepage : string
  val us_geo_restricted : bool

  (** List supported symbols on this venue. *)
  val list_symbols :
    ?query:string ->
    ?limit:int ->
    unit ->
    (Symbol_info.t list, [> error ]) Result.t Async.Deferred.t

  (** Get the latest price for one symbol. *)
  val get_price :
    symbol:string ->
    (Price.t, [> error ]) Result.t Async.Deferred.t

  (** Batched fetch when the venue supports it. Adapters that don't
      support batching loop internally and return the same shape. *)
  val get_prices :
    symbols:string list ->
    (Price.t list, [> error ]) Result.t Async.Deferred.t
end

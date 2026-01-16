(** Order Book Interface - Extracted from Gemini's Production Implementation

    Provides real-time order book management with efficient sorted data structures.
    Supports both single-symbol and multi-symbol order books.

    {b Key Features:}
    - Efficient bid/ask level management (O(log n) updates)
    - Market price calculations with volume
    - TUI rendering with ANSI colors
    - Incremental updates via WebSocket
    - Multi-symbol book aggregation

    {b Design:} Based on Gemini's proven order book implementation.
    Uses Core.Map for O(log n) operations on price levels.

    @see <https://docs.gemini.com/websocket-api/#market-data> for data format
*)

(** {1 Price Levels} *)

module Price_level : sig
  (** Single price level in the order book *)
  type t = {
    price : float;   (** Price at this level *)
    volume : float;  (** Total volume available at this price *)
  }
  [@@deriving sexp, equal, compare, fields]

  val create : price:float -> volume:float -> t
  val empty : t  (** Zero price, zero volume *)
end

(** {1 Book Side} *)

module type BID_ASK = sig
  (** Polymorphic variant for bid/ask *)
  type t = [`Bid | `Ask] [@@deriving sexp, compare, equal]
end

module Bid_ask : BID_ASK

(** {1 Single Symbol Order Book} *)

module type BOOK = sig
  (** Order book for a single trading pair

      Maintains sorted bids (descending) and asks (ascending).
      Provides efficient updates and market price calculations.
  *)

  type t [@@deriving sexp, equal, compare]

  val empty : ?timestamp:Time_float_unix.t -> ?epoch:int -> Types.Symbol.t -> t
  (** Create empty order book for symbol *)

  (** {2 Update Operations} *)

  val set : ?timestamp:Time_float_unix.t -> t -> side:Bid_ask.t -> price:float -> size:float -> t
  (** Set level to exact size (replaces existing) *)

  val update : ?timestamp:Time_float_unix.t -> t -> side:Bid_ask.t -> price:float -> size:float -> t
  (** Update level by delta (adds to existing) *)

  val add : ?timestamp:Time_float_unix.t -> t -> side:Bid_ask.t -> price:float -> size:float -> t
  (** Add size to existing level *)

  val remove : ?timestamp:Time_float_unix.t -> t -> side:Bid_ask.t -> price:float -> size:float -> t
  (** Remove size from level (deletes if zero) *)

  (** {2 Best Price Queries} *)

  val best_bid : t -> Price_level.t
  (** Highest bid (best buy price) *)

  val best_ask : t -> Price_level.t
  (** Lowest ask (best sell price) *)

  val best : side:Bid_ask.t -> t -> Price_level.t
  (** Best price for given side *)

  val best_n_bids : t -> n:int -> unit -> Price_level.t list
  (** Top N bids (highest first) *)

  val best_n_asks : t -> n:int -> unit -> Price_level.t list
  (** Top N asks (lowest first) *)

  (** {2 Market Price Calculations} *)

  val market_price : t -> side:Types.Side.t -> volume:float -> Price_level.t
  (** Calculate effective price for given volume

      Walks the book accumulating volume until target is reached.
      Returns weighted average price and remaining unfilled volume.
  *)

  val bid_market_price : t -> volume:float -> Price_level.t
  (** Market price for selling given volume (walks bids) *)

  val ask_market_price : t -> volume:float -> Price_level.t
  (** Market price for buying given volume (walks asks) *)

  val mid_market_price : t -> volume:float -> Price_level.t
  (** Mid-market price (average of bid and ask market prices) *)

  (** {2 Volume Queries} *)

  val total_volume_at_price_level : t -> side:Bid_ask.t -> price:float -> Price_level.t
  val total_bid_volume_at_price_level : t -> price:float -> Price_level.t
  val total_ask_volume_at_price_level : t -> price:float -> Price_level.t

  (** {2 Notional Conversions} *)

  val quantity_from_notional_bid : t -> notional:float -> float
  (** Calculate base quantity from quote notional (for selling) *)

  val quantity_from_notional_ask : t -> notional:float -> float
  (** Calculate base quantity from quote notional (for buying) *)

  (** {2 Metadata} *)

  val symbol : t -> Types.Symbol.t
  val epoch : t -> int  (** Update sequence number *)
  val update_time : t -> Time_float_unix.t

  (** {2 Display} *)

  val pretty_print :
    ?max_depth:int ->
    ?refresh_ms:float ->
    ?tick_size:float option ->
    t ->
    unit
  (** TUI rendering with ANSI colors

      @param max_depth Number of levels to display (default: 10)
      @param refresh_ms Refresh rate in milliseconds
      @param tick_size Price precision for display
  *)
end

(** {1 Multi-Symbol Order Books} *)

module type BOOKS = sig
  (** Manage order books for multiple symbols

      Aggregates books for different trading pairs.
      Provides symbol-specific update operations.
  *)

  type t [@@deriving sexp, equal, compare]
  type book

  val empty : t
  val symbols : t -> Types.Symbol.t list
  val book : t -> Types.Symbol.t -> book option
  val book_exn : t -> Types.Symbol.t -> book
  val set_book : ?timestamp:Time_float_unix.t -> t -> book -> t

  (** {2 Symbol-Specific Updates} *)

  val add : ?timestamp:Time_float_unix.t -> t -> symbol:Types.Symbol.t -> side:Bid_ask.t -> price:float -> size:float -> t
  val update : ?timestamp:Time_float_unix.t -> t -> symbol:Types.Symbol.t -> side:Bid_ask.t -> price:float -> size:float -> t
  val remove : ?timestamp:Time_float_unix.t -> t -> symbol:Types.Symbol.t -> side:Bid_ask.t -> price:float -> size:float -> t
  val set : ?timestamp:Time_float_unix.t -> t -> symbol:Types.Symbol.t -> side:Bid_ask.t -> price:float -> size:float -> t
end

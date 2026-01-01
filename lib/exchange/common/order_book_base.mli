(** Common order book implementation for all exchanges.

    This module provides a unified order book interface used by all exchange integrations.
    It offers immutable, type-safe order book management with efficient operations and
    built-in analytics.

    {1 Overview}

    The order book is implemented using sorted maps for O(log n) updates and O(1) access
    to best bid/ask. All operations are immutable - updates return new book instances
    while sharing unchanged structure.

    {1 Example}

    {[
      (* Create a book *)
      let book = Book.create ~symbol:"BTC/USD" in

      (* Add price levels *)
      let book = Book.set_many book [
        (`Bid, 50000., 1.5);
        (`Ask, 50001., 1.2);
      ] in

      (* Query market data *)
      let mid = Book.mid_price book in
      let spread = Book.spread book in
      let best_bid = Book.best_bid book in
    ]}
*)

module Price_level : sig
  (** A single price level in the order book.

      Represents a bid or ask at a specific price with a total volume. *)

  type t = {
    price: float;   (** Price of this level *)
    volume: float;  (** Total volume available at this price *)
  }

  (** Create a price level. *)
  val create : price:float -> volume:float -> t

  (** Empty price level (price=0, volume=0). Useful as a default. *)
  val empty : t

  (** S-expression serialization. *)
  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t

  (** Get the price of this level. *)
  val price : t -> float

  (** Get the volume of this level. *)
  val volume : t -> float
end

module Bid_price : sig
  (** Bid price comparator (descending order: highest bid first). *)
  include Core.Comparator.S with type t = float
end

module Ask_price : sig
  (** Ask price comparator (ascending order: lowest ask first). *)
  type t = float
  include Core.Comparator.S with type t := t
end

(** Map of bid prices (sorted descending: best bid first). *)
module Bid_price_map : Core.Map.S with type Key.t = float and type Key.comparator_witness = Bid_price.comparator_witness

(** Map of ask prices (sorted ascending: best ask first). *)
module Ask_price_map : Core.Map.S with type Key.t = float

(** Functor for creating order book implementations.

    Parameterized by symbol type and exchange-specific metadata.

    @param Config Configuration for symbol type and metadata. *)
module Make (Config : sig
  (** Symbol type (e.g., string, variant, custom type). *)
  type symbol

  (** S-expression serialization for symbols. *)
  val sexp_of_symbol : symbol -> Core.Sexp.t
  val symbol_of_sexp : Core.Sexp.t -> symbol

  (** Symbol comparison for Maps. *)
  val compare_symbol : symbol -> symbol -> int

  (** Exchange-specific metadata (e.g., last update ID, timestamp). *)
  type metadata

  (** S-expression serialization for metadata. *)
  val sexp_of_metadata : metadata -> Core.Sexp.t
  val metadata_of_sexp : Core.Sexp.t -> metadata

  (** Default metadata value. *)
  val default_metadata : unit -> metadata
end) : sig

  module Book : sig
    (** Order book for a single symbol.

        Immutable order book with efficient operations:
        - Updates: O(log n)
        - Best bid/ask: O(1)
        - VWAP calculation: O(k) where k is levels consumed

        All update operations return a new book instance. *)

    type t

    (** S-expression serialization. *)
    val sexp_of_t : t -> Core.Sexp.t
    val t_of_sexp : Core.Sexp.t -> t

    (** {1 Construction} *)

    (** Create an empty order book for the given symbol. *)
    val create : symbol:Config.symbol -> t

    (** {1 Metadata} *)

    (** Get the trading symbol. *)
    val symbol : t -> Config.symbol

    (** Get the update counter (increments on each modification). *)
    val epoch : t -> int

    (** Get the last update timestamp (Unix time as float). *)
    val update_time : t -> float

    (** Get exchange-specific metadata. *)
    val metadata : t -> Config.metadata

    (** {1 Updates} *)

    (** Update a single price level.

        @param timestamp Optional timestamp (defaults to current time)
        @param metadata Optional exchange-specific metadata
        @param side Bid or Ask side
        @param price Price level to update
        @param size New size at this price (0 removes the level)
        @return New book with update applied

        {b Example:}
        {[
          (* Add a bid *)
          let book = Book.set book ~side:`Bid ~price:50000. ~size:1.5 in

          (* Remove an ask *)
          let book = Book.set book ~side:`Ask ~price:50001. ~size:0. in
        ]}
    *)
    val set :
      ?timestamp:float ->
      ?metadata:Config.metadata ->
      t ->
      side:[`Bid | `Ask] ->
      price:float ->
      size:float ->
      t

    (** Update multiple price levels in a single operation.

        More efficient than multiple {!set} calls.

        @param timestamp Optional timestamp
        @param metadata Optional exchange-specific metadata
        @param updates List of (side, price, size) tuples
        @return New book with all updates applied

        {b Example:}
        {[
          let book = Book.set_many book [
            (`Bid, 50000., 1.5);
            (`Bid, 49999., 2.0);
            (`Ask, 50001., 1.2);
          ]
        ]}
    *)
    val set_many :
      ?timestamp:float ->
      ?metadata:Config.metadata ->
      t ->
      ([`Bid | `Ask] * float * float) list ->
      t

    (** {1 Market Data} *)

    (** Get the best bid (highest buy price).

        Returns {!Price_level.empty} if no bids exist. *)
    val best_bid : t -> Price_level.t

    (** Get the best ask (lowest sell price).

        Returns {!Price_level.empty} if no asks exist. *)
    val best_ask : t -> Price_level.t

    (** Get the mid price: (best_bid + best_ask) / 2.

        Returns 0.0 if either side is empty. *)
    val mid_price : t -> float

    (** Get the spread: best_ask - best_bid.

        Returns 0.0 if either side is empty. *)
    val spread : t -> float

    (** {1 Level Queries} *)

    (** Get the best N bid levels (highest prices first).

        @param n Number of levels to return
        @return List of up to N levels, sorted descending by price *)
    val best_n_bids : t -> n:int -> unit -> Price_level.t list

    (** Get the best N ask levels (lowest prices first).

        @param n Number of levels to return
        @return List of up to N levels, sorted ascending by price *)
    val best_n_asks : t -> n:int -> unit -> Price_level.t list

    (** Get all bid levels (best first). *)
    val all_bids : t -> Price_level.t list

    (** Get all ask levels (best first). *)
    val all_asks : t -> Price_level.t list

    (** Get all bids as association list: (price, level) pairs. *)
    val bids_alist : t -> (float * Price_level.t) list

    (** Get all asks as association list: (price, level) pairs. *)
    val asks_alist : t -> (float * Price_level.t) list

    (** {1 Analytics} *)

    (** Calculate volume-weighted average price for a buy order.

        Walks the ask side from best to worst, consuming liquidity until
        the requested volume is filled.

        @param volume Amount to buy
        @return Some average_price if sufficient liquidity exists, None otherwise

        {b Example:}
        {[
          match Book.vwap_buy book ~volume:10.0 with
          | Some price -> printf "VWAP to buy 10 BTC: $%.2f\\n" price
          | None -> printf "Insufficient liquidity\\n"
        ]}
    *)
    val vwap_buy : t -> volume:float -> float option

    (** Calculate volume-weighted average price for a sell order.

        Walks the bid side from best to worst, consuming liquidity until
        the requested volume is filled.

        @param volume Amount to sell
        @return Some average_price if sufficient liquidity exists, None otherwise *)
    val vwap_sell : t -> volume:float -> float option

    (** Calculate total volume in the top N levels of a side.

        @param side Bid or Ask
        @param n Number of levels to sum
        @return Total volume in top N levels *)
    val total_volume_n : t -> side:[`Bid | `Ask] -> n:int -> float
  end

  module Books : sig
    (** Collection of order books for multiple symbols.

        Efficiently manages multiple order books using a symbol-keyed map. *)

    type book = Book.t
    type t

    (** S-expression serialization. *)
    val sexp_of_t : t -> Core.Sexp.t
    val t_of_sexp : Core.Sexp.t -> t

    (** Create an empty collection. *)
    val empty : t

    (** Get list of all symbols in the collection. *)
    val symbols : t -> Config.symbol list

    (** Get the order book for a symbol.

        @return Some book if exists, None otherwise *)
    val book : t -> Config.symbol -> book option

    (** Get the order book for a symbol, raising if not found.

        @raise Not_found if symbol doesn't exist *)
    val book_exn : t -> Config.symbol -> book

    (** Add or update a book in the collection.

        If a book for this symbol already exists, it's replaced. *)
    val set_book : t -> book -> t

    (** Remove a book from the collection.

        No-op if symbol doesn't exist. *)
    val remove_book : t -> Config.symbol -> t

    (** Update a book in place using a function.

        @param symbol Symbol to update
        @param f Update function
        @return Updated collection

        {b Example:}
        {[
          let books = Books.update_book books symbol ~f:(fun book ->
            Book.set book ~side:`Bid ~price:50000. ~size:1.5
          )
        ]}
    *)
    val update_book : t -> Config.symbol -> f:(book -> book) -> t

    (** Fold over all books in the collection. *)
    val fold : t -> init:'a -> f:('a -> book -> 'a) -> 'a

    (** Iterate over all books in the collection. *)
    val iter : t -> f:(book -> unit) -> unit

    (** Get the number of books in the collection. *)
    val length : t -> int
  end
end

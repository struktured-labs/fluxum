(** Common order book implementation for all exchanges *)

module Price_level : sig
  type t = {
    price: float;
    volume: float;
  }

  val create : price:float -> volume:float -> t
  val empty : t
  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
  val price : t -> float
  val volume : t -> float
end

module Bid_price : sig
  include Core.Comparator.S with type t = float
end

module Ask_price : sig
  type t = float
  include Core.Comparator.S with type t := t
end

module Bid_price_map : Core.Map.S with type Key.t = float and type Key.comparator_witness = Bid_price.comparator_witness
module Ask_price_map : Core.Map.S with type Key.t = float

module Make (Config : sig
  type symbol
  val sexp_of_symbol : symbol -> Core.Sexp.t
  val symbol_of_sexp : Core.Sexp.t -> symbol
  val compare_symbol : symbol -> symbol -> int

  type metadata
  val sexp_of_metadata : metadata -> Core.Sexp.t
  val metadata_of_sexp : Core.Sexp.t -> metadata
  val default_metadata : unit -> metadata
end) : sig

  module Book : sig
    type t

    val sexp_of_t : t -> Core.Sexp.t
    val t_of_sexp : Core.Sexp.t -> t

    val create : symbol:Config.symbol -> t
    val symbol : t -> Config.symbol
    val epoch : t -> int
    val update_time : t -> float

    val set :
      ?timestamp:float ->
      ?metadata:Config.metadata ->
      t ->
      side:[`Bid | `Ask] ->
      price:float ->
      size:float ->
      t

    val set_many :
      ?timestamp:float ->
      ?metadata:Config.metadata ->
      t ->
      ([`Bid | `Ask] * float * float) list ->
      t

    val best_bid : t -> Price_level.t
    val best_ask : t -> Price_level.t
    val mid_price : t -> float
    val spread : t -> float

    val best_n_bids : t -> n:int -> unit -> Price_level.t list
    val best_n_asks : t -> n:int -> unit -> Price_level.t list
    val all_bids : t -> Price_level.t list
    val all_asks : t -> Price_level.t list

    val vwap_buy : t -> volume:float -> float option
    val vwap_sell : t -> volume:float -> float option

    val total_volume_n : t -> side:[`Bid | `Ask] -> n:int -> float
  end

  module Books : sig
    type book = Book.t
    type t

    val sexp_of_t : t -> Core.Sexp.t
    val t_of_sexp : Core.Sexp.t -> t

    val empty : t
    val symbols : t -> Config.symbol list
    val book : t -> Config.symbol -> book option
    val book_exn : t -> Config.symbol -> book
    val set_book : t -> book -> t
    val remove_book : t -> Config.symbol -> t
    val update_book : t -> Config.symbol -> f:(book -> book) -> t
    val fold : t -> init:'a -> f:('a -> book -> 'a) -> 'a
    val iter : t -> f:(book -> unit) -> unit
    val length : t -> int
  end
end

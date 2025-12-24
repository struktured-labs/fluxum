(** Order Book Interface - Extracted from Gemini's proven implementation *)

open Core

(** Price level with volume *)
module Price_level = struct
  type t = {
    price : float;
    volume : float;
  }
  [@@deriving sexp, equal, compare, fields]

  let create ~price ~volume = { price; volume }
  let empty = create ~price:0. ~volume:0.
end

(** Book side enum *)
module type BID_ASK = sig
  type t = [`Bid | `Ask] [@@deriving sexp, compare, equal]
end

module Bid_ask : BID_ASK = struct
  type t = [`Bid | `Ask] [@@deriving sexp, compare, equal]
end

(** Single symbol order book *)
module type BOOK = sig
  type t [@@deriving sexp, equal, compare]

  (** Create empty book *)
  val empty : ?timestamp:Time_float_unix.t -> ?epoch:int -> Types.Symbol.t -> t

  (** Update operations - set replaces level, update adds/subtracts *)
  val set :
    ?timestamp:Time_float_unix.t ->
    t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val update :
    ?timestamp:Time_float_unix.t ->
    t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val add :
    ?timestamp:Time_float_unix.t ->
    t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val remove :
    ?timestamp:Time_float_unix.t ->
    t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  (** Queries *)
  val best_bid : t -> Price_level.t
  val best_ask : t -> Price_level.t
  val best : side:Bid_ask.t -> t -> Price_level.t
  val best_n_bids : t -> n:int -> unit -> Price_level.t list
  val best_n_asks : t -> n:int -> unit -> Price_level.t list

  (** Market price calculation for given volume *)
  val market_price : t -> side:Types.Side.t -> volume:float -> Price_level.t
  val bid_market_price : t -> volume:float -> Price_level.t
  val ask_market_price : t -> volume:float -> Price_level.t
  val mid_market_price : t -> volume:float -> Price_level.t

  (** Volume at price level *)
  val total_volume_at_price_level :
    t -> side:Bid_ask.t -> price:float -> Price_level.t

  val total_bid_volume_at_price_level : t -> price:float -> Price_level.t
  val total_ask_volume_at_price_level : t -> price:float -> Price_level.t

  (** Notional conversion *)
  val quantity_from_notional_bid : t -> notional:float -> float
  val quantity_from_notional_ask : t -> notional:float -> float

  (** Accessors *)
  val symbol : t -> Types.Symbol.t
  val epoch : t -> int
  val update_time : t -> Time_float_unix.t

  (** TUI rendering with ANSI colors *)
  val pretty_print :
    ?max_depth:int ->
    ?refresh_ms:float ->
    ?tick_size:float option ->
    t ->
    unit

  (** Create live book pipe from exchange market data
      Note: Actual signature will be provided by exchange-specific implementation *)

end

(** Multi-symbol order book manager *)
module type BOOKS = sig
  type t [@@deriving sexp, equal, compare]
  type book

  val empty : t
  val symbols : t -> Types.Symbol.t list
  val book : t -> Types.Symbol.t -> book option
  val book_exn : t -> Types.Symbol.t -> book
  val set_book : ?timestamp:Time_float_unix.t -> t -> book -> t

  (** Update operations on specific symbol *)
  val add :
    ?timestamp:Time_float_unix.t ->
    t ->
    symbol:Types.Symbol.t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val update :
    ?timestamp:Time_float_unix.t ->
    t ->
    symbol:Types.Symbol.t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val remove :
    ?timestamp:Time_float_unix.t ->
    t ->
    symbol:Types.Symbol.t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  val set :
    ?timestamp:Time_float_unix.t ->
    t ->
    symbol:Types.Symbol.t ->
    side:Bid_ask.t ->
    price:float ->
    size:float ->
    t

  (** Multi-symbol pipes
      Note: Actual signature will be provided by exchange-specific implementation *)
end

(** Complete order book interface *)
module type S = sig
  include BOOK

  module Books : BOOKS with type book := t
end

(** Gemini Order Book

    Real-time order book management using the Gemini WebSocket market data feed.
    Supports both standard spot symbols ([Symbol.t]) and arbitrary instrument
    symbols (prediction market contracts like GEMI-BTC100K-YES). *)

open! Common
open V1

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Market_data.Side.Bid_ask

module Book : sig
  type t [@@deriving sexp]

  (** {1 Construction} *)

  val create : symbol:string -> t
  val empty : ?timestamp:float -> ?epoch:int -> Symbol.t -> t

  (** {1 Metadata} *)

  val symbol : t -> string
  val epoch : t -> int
  val update_time : t -> float

  (** {1 Updates} *)

  val set :
    ?timestamp:float ->
    ?metadata:unit ->
    t ->
    side:[`Bid | `Ask] ->
    price:float ->
    size:float ->
    t

  val on_market_data : t -> Market_data.response -> t

  (** {1 Market Data} *)

  val best_bid : t -> Price_level.t
  val best_ask : t -> Price_level.t
  val mid_price : t -> float
  val spread : t -> float

  (** {1 Level Queries} *)

  val find_bid : t -> price:float -> Price_level.t option
  val find_ask : t -> price:float -> Price_level.t option
  val best_n_bids : t -> n:int -> unit -> Price_level.t list
  val best_n_asks : t -> n:int -> unit -> Price_level.t list
  val best_n_bids_map : t -> n:int -> f:(Price_level.t -> 'a) -> 'a list
  val best_n_asks_map : t -> n:int -> f:(Price_level.t -> 'a) -> 'a list
  val all_bids : t -> Price_level.t list
  val all_asks : t -> Price_level.t list

  (** {1 Analytics} *)

  val vwap_buy : t -> volume:float -> float option
  val vwap_sell : t -> volume:float -> float option
  val total_volume_n : t -> side:[`Bid | `Ask] -> n:int -> float

  (** {1 WebSocket Pipes} *)

  (** Stream order book for a standard spot symbol via WebSocket. *)
  val pipe :
    (module Cfg.S) ->
    symbol:Symbol.t ->
    unit ->
    [ `Ok of t | Market_data.Error.t ] Pipe.Reader.t Deferred.t

  val pipe_exn :
    (module Cfg.S) ->
    symbol:Symbol.t ->
    unit ->
    t Pipe.Reader.t Deferred.t

  (** Stream order book for an arbitrary instrument symbol string.
      Used for prediction market contracts like GEMI-BTC100K-YES. *)
  val pipe_for_instrument :
    (module Cfg.S) ->
    instrument_symbol:string ->
    unit ->
    [ `Ok of t | Market_data.Error.t ] Pipe.Reader.t Deferred.t

  val pipe_for_instrument_exn :
    (module Cfg.S) ->
    instrument_symbol:string ->
    unit ->
    t Pipe.Reader.t Deferred.t
end

module Books : sig
  type book = Book.t
  type t [@@deriving sexp]

  val empty : t
  val symbols : t -> string list
  val book : t -> string -> book option
  val book_exn : t -> string -> book
  val set_book : t -> book -> t

  val on_market_data : t -> Symbol.t -> Market_data.response -> t

  val pipe :
    (module Cfg.S) ->
    ?symbols:Symbol.t list ->
    unit ->
    [ `Ok of Book.t | Market_data.Error.t ] Pipe.Reader.t Symbol.Map.t Deferred.t

  val pipe_exn :
    (module Cfg.S) ->
    ?symbols:Symbol.t list ->
    unit ->
    Book.t Pipe.Reader.t Symbol.Map.t Deferred.t
end

val command : string * Command.t

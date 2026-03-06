open Core

(** Common order book implementation for all exchanges

    This module provides shared functionality for price level management,
    bid/ask comparators, and common order book operations. Exchanges can
    use this as a base to avoid duplicating ~800 lines of identical code. *)

(** Price level with price and volume *)
module Price_level = struct
  type t =
    { price: float
    ; volume: float }
  [@@deriving sexp, fields]

  let create ~price ~volume = {price; volume}
  let empty = {price= 0.; volume= 0.}
end

(** Bid price comparator (descending order - highest first) *)
module Bid_price = struct
  include Float

  include Comparator.Make (struct
      type t = float [@@deriving sexp, compare, equal]

      let compare p p' = Float.compare p p' |> Int.neg (* Descending *)
    end)
end

(** Ask price uses standard Float comparator (ascending order - lowest first) *)
module Ask_price = Float

(** Map types for bids and asks *)
module Bid_price_map = Map.Make_using_comparator (Bid_price)

module Ask_price_map = Map.Make (Ask_price)

(** Functor to create exchange-specific order books

    Each exchange provides:
    - Symbol type
    - Optional exchange-specific metadata (sequence numbers, timestamps, etc.) *)
module Make (Config : sig
    type symbol

    val sexp_of_symbol : symbol -> Sexp.t
    val symbol_of_sexp : Sexp.t -> symbol
    val compare_symbol : symbol -> symbol -> int

    (** Exchange-specific metadata in the book *)
    type metadata

    val sexp_of_metadata : metadata -> Sexp.t
    val metadata_of_sexp : Sexp.t -> metadata
    val default_metadata : unit -> metadata
  end) =
struct
  (** Order book for a single symbol *)
  module Book = struct
    type t =
      { symbol: Config.symbol
      ; bids: Price_level.t Bid_price_map.t
      ; asks: Price_level.t Ask_price_map.t
      ; epoch: int
      ; update_time: float (** Unix timestamp as float *)
      ; metadata: Config.metadata (** Exchange-specific data *) }
    [@@deriving sexp]

    let create ~symbol =
      { symbol
      ; bids= Bid_price_map.empty
      ; asks= Ask_price_map.empty
      ; epoch= 0
      ; update_time= 0.
      ; metadata= Config.default_metadata () }

    let symbol t = t.symbol
    let epoch t = t.epoch
    let update_time t = t.update_time
    let metadata t = t.metadata

    (** Apply a single level update to bid and ask maps *)
    let apply_level_to_maps ~bids ~asks ~side ~price ~size =
      match Float.(equal zero size) with
      | true ->
        (match side with
         | `Bid -> (Map.remove bids price, asks)
         | `Ask -> (bids, Map.remove asks price))
      | false ->
        let data = Price_level.create ~price ~volume:size in
        (match side with
         | `Bid -> (Map.set bids ~key:price ~data, asks)
         | `Ask -> (bids, Map.set asks ~key:price ~data))

    (** Set a price level (size=0 removes the level) *)
    let set ?timestamp ?metadata t ~side ~price ~size =
      let update_time =
        match timestamp with
        | Some ts -> ts
        | None -> Core_unix.gettimeofday ()
      in
      let metadata = match metadata with
        | Some m -> m
        | None -> Config.default_metadata ()
      in
      let bids, asks = apply_level_to_maps ~bids:t.bids ~asks:t.asks ~side ~price ~size in
      { t with bids; asks; epoch = t.epoch + 1; update_time; metadata }

    (** Set multiple levels at once.

        More efficient than repeated [set] calls: computes timestamp and metadata
        once, applies all level updates to the maps, then constructs a single
        new record. Avoids N-1 intermediate record copies and N-1 redundant
        [gettimeofday] syscalls. *)
    let set_many ?timestamp ?metadata t levels =
      let update_time =
        match timestamp with
        | Some ts -> ts
        | None -> Core_unix.gettimeofday ()
      in
      let metadata = match metadata with
        | Some m -> m
        | None -> Config.default_metadata ()
      in
      let bids, asks =
        List.fold levels ~init:(t.bids, t.asks) ~f:(fun (bids, asks) (side, price, size) ->
          apply_level_to_maps ~bids ~asks ~side ~price ~size)
      in
      { t with bids; asks; epoch = t.epoch + List.length levels; update_time; metadata }

    (** Get best bid (highest bid price) *)
    let best_bid t =
      Map.min_elt t.bids |> Option.value_map ~default:Price_level.empty ~f:snd

    (** Get best ask (lowest ask price) *)
    let best_ask t =
      Map.min_elt t.asks |> Option.value_map ~default:Price_level.empty ~f:snd

    (** Get mid price *)
    let mid_price t =
      let bid = best_bid t in
      let ask = best_ask t in
        match Float.(bid.price > 0. && ask.price > 0.) with
        | true -> (bid.price +. ask.price) /. 2.
        | false -> 0.

    (** Get spread *)
    let spread t =
      let bid = best_bid t in
      let ask = best_ask t in
        ask.price -. bid.price

    (** Find a bid price level. O(log n) via Map.find. *)
    let find_bid t ~price = Map.find t.bids price

    (** Find an ask price level. O(log n) via Map.find. *)
    let find_ask t ~price = Map.find t.asks price

    (** Get top N bids as Price_level.t list *)
    let best_n_bids t ~n () =
      Map.to_sequence t.bids
      |> Fn.flip Sequence.take n
      |> Sequence.map ~f:snd
      |> Sequence.to_list

    (** Get top N asks as Price_level.t list *)
    let best_n_asks t ~n () =
      Map.to_sequence t.asks
      |> Fn.flip Sequence.take n
      |> Sequence.map ~f:snd
      |> Sequence.to_list

    (** Get top N bids, mapping each level directly to avoid intermediate list.
        Use when the caller needs a different type than Price_level.t. *)
    let best_n_bids_map t ~n ~f =
      Map.to_sequence t.bids
      |> Fn.flip Sequence.take n
      |> Sequence.map ~f:(fun (_key, level) -> f level)
      |> Sequence.to_list

    (** Get top N asks, mapping each level directly to avoid intermediate list. *)
    let best_n_asks_map t ~n ~f =
      Map.to_sequence t.asks
      |> Fn.flip Sequence.take n
      |> Sequence.map ~f:(fun (_key, level) -> f level)
      |> Sequence.to_list

    (** Get all bids *)
    let all_bids t = Map.data t.bids

    (** Get all asks *)
    let all_asks t = Map.data t.asks

    (** Get bids as association list (price, level) *)
    let bids_alist t = Map.to_alist t.bids

    (** Get asks as association list (price, level) *)
    let asks_alist t = Map.to_alist t.asks

    (** Accumulate cost across a sequence of price levels until target volume is met *)
    let vwap_accumulate ~volume seq =
      let rec go remaining acc_cost seq =
        match Sequence.next seq with
        | None ->
          (match Float.(remaining > 0.) with
           | true -> None
           | false -> Some (acc_cost /. volume))
        | Some ((_key, level), rest) ->
          (match Float.(remaining <= level.Price_level.volume) with
           | true ->
             let cost = remaining *. level.price in
             Some ((acc_cost +. cost) /. volume)
           | false ->
             let cost = level.volume *. level.price in
             go (remaining -. level.volume) (acc_cost +. cost) rest)
      in
      go volume 0. seq

    (** Calculate VWAP for buying (consuming asks).
        Operates directly on the map sequence - no intermediate list allocation. *)
    let vwap_buy t ~volume =
      Map.to_sequence t.asks |> vwap_accumulate ~volume

    (** Calculate VWAP for selling (consuming bids).
        Operates directly on the map sequence - no intermediate list allocation. *)
    let vwap_sell t ~volume =
      Map.to_sequence t.bids |> vwap_accumulate ~volume

    (** Get total volume up to N levels deep *)
    let total_volume_n t ~side ~n =
      match side with
      | `Bid ->
        best_n_bids t ~n ()
        |> List.fold ~init:0. ~f:(fun acc level -> acc +. level.Price_level.volume)
      | `Ask ->
        best_n_asks t ~n ()
        |> List.fold ~init:0. ~f:(fun acc level -> acc +. level.volume)
  end

  (** Collection of order books by symbol *)
  module Books = struct
    module Symbol_map = Map.Make (struct
        type t = Config.symbol

        let compare = Config.compare_symbol
        let sexp_of_t = Config.sexp_of_symbol
        let t_of_sexp = Config.symbol_of_sexp
      end)

    type book = Book.t [@@deriving sexp]
    type t = book Symbol_map.t [@@deriving sexp]

    let empty = Symbol_map.empty
    let symbols t = Map.keys t
    let book t symbol = Map.find t symbol
    let book_exn t symbol = Map.find_exn t symbol
    let set_book t (book : book) = Map.set t ~key:(Book.symbol book) ~data:book
    let remove_book t symbol = Map.remove t symbol

    let update_book t symbol ~f:update_fn =
      Map.update t symbol ~f:(fun book_opt ->
        match book_opt with
        | Some book -> update_fn book
        | None ->
          let new_book = Book.create ~symbol in
            update_fn new_book)

    let fold t ~init ~f = Map.fold t ~init ~f:(fun ~key:_ ~data acc -> f acc data)
    let iter t ~f = Map.iteri t ~f:(fun ~key:_ ~data -> f data)
    let length t = Map.length t
  end
end

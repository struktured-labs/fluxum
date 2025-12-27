(** Hyperliquid Order Book - Implements unified Order_book_intf *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Fluxum.Order_book_intf.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Price comparators *)
module Bid_price = struct
  include Float
  include Comparator.Make (struct
    type t = float [@@deriving sexp, compare, equal]
    let compare p p' = Float.compare p p' |> Int.neg  (* Descending *)
  end)
end

module Ask_price = Float

module Bid_price_map = Map.Make_using_comparator (Bid_price)
module Ask_price_map = Map.Make (Ask_price)

(** Single symbol order book *)
module Book = struct
  type t = {
    symbol: Fluxum.Types.Symbol.t;
    bids: Price_level.t Bid_price_map.t;
    asks: Price_level.t Ask_price_map.t;
    last_update_time: int64;
    epoch: int;
    update_time: Time_float_unix.t;
  } [@@deriving fields, sexp]

  let empty ?(timestamp) ?(last_update_time = 0L) symbol =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    { symbol;
      bids = Bid_price_map.empty;
      asks = Ask_price_map.empty;
      last_update_time;
      epoch = 0;
      update_time;
    }

  let set ?timestamp t ~side ~price ~size =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let epoch = t.epoch + 1 in
    match Float.(equal zero size) with
    | true -> (
      match side with
      | `Bid -> { t with bids = Map.remove t.bids price; epoch; update_time }
      | `Ask -> { t with asks = Map.remove t.asks price; epoch; update_time })
    | false -> (
      let data = Price_level.create ~price ~volume:size in
      match side with
      | `Bid -> { t with bids = Map.set t.bids ~key:price ~data; epoch; update_time }
      | `Ask -> { t with asks = Map.set t.asks ~key:price ~data; epoch; update_time })

  let best_bid t =
    Map.min_elt t.bids
    |> Option.value_map ~default:Price_level.empty ~f:snd

  let best_ask t =
    Map.min_elt t.asks
    |> Option.value_map ~default:Price_level.empty ~f:snd

  (** Apply L2 book update from Hyperliquid.
      Hyperliquid l2Book has levels as [[bids], [asks]] where each level is {px, sz, n} *)
  let apply_l2_book t (book : Ws.Message.l2_book) =
    let bids, asks = match book.levels with
      | [bid_levels; ask_levels] -> (bid_levels, ask_levels)
      | _ -> ([], [])  (* Invalid format, return empty *)
    in
    (* Replace bids *)
    let new_bids = List.fold bids ~init:Bid_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.Ws.Message.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    (* Replace asks *)
    let new_asks = List.fold asks ~init:Ask_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.Ws.Message.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    { t with
      bids = new_bids;
      asks = new_asks;
      last_update_time = book.time;
      epoch = t.epoch + 1;
      update_time = Time_float_unix.now ();
    }

  (** Apply REST API l2_book response *)
  let apply_rest_l2_book t (book : Rest.Types.l2_book) =
    let bids, asks = match book.levels with
      | [bid_levels; ask_levels] -> (bid_levels, ask_levels)
      | _ -> ([], [])
    in
    let new_bids = List.fold bids ~init:Bid_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.Rest.Types.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    let new_asks = List.fold asks ~init:Ask_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.Rest.Types.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    { t with
      bids = new_bids;
      asks = new_asks;
      last_update_time = book.time;
      epoch = t.epoch + 1;
      update_time = Time_float_unix.now ();
    }

  (** Create live order book pipe from Hyperliquid WebSocket *)
  let pipe ~symbol ?url () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    let open Deferred.Let_syntax in
    let coin = Fluxum.Types.Symbol.to_string symbol in
    let streams = [Ws.Stream.L2Book { coin; n_sig_figs = None; mantissa = None }] in
    let url = Option.value url ~default:Ws.Endpoint.mainnet in
    let%bind md_result = Market_data.connect ~streams ~url () in
    match md_result with
    | Error _err ->
      let reader, _writer = Pipe.create () in
      Pipe.close_read reader;
      return reader
    | Ok md ->
      let messages = Market_data.messages md in
      let book_reader, book_writer = Pipe.create () in
      let book_ref = ref (empty symbol) in

      don't_wait_for (
        Pipe.iter messages ~f:(fun msg_str ->
          try
            match Ws.parse_message msg_str with
            | Ws.Message.L2Book l2_book when String.equal l2_book.coin coin ->
              book_ref := apply_l2_book !book_ref l2_book;
              Pipe.write book_writer (Ok !book_ref)
            | _ -> Deferred.unit
          with exn ->
            Pipe.write book_writer (Error (Exn.to_string exn))
        )
        >>| fun () -> Pipe.close book_writer
      );

      return book_reader

  let symbol t = t.symbol
  let epoch t = t.epoch
end

(** Multi-symbol order books *)
module Books = struct
  type t = Book.t Fluxum.Types.Symbol.Map.t [@@deriving sexp]
  type book = Book.t

  let empty = Fluxum.Types.Symbol.Map.empty
  let symbols t = Map.keys t
  let book t symbol = Map.find t symbol
  let book_exn t symbol = Map.find_exn t symbol

  let set_book t (book : book) =
    Map.set t ~key:(Book.symbol book) ~data:book
end

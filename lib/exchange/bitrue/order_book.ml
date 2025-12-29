(** Bitrue Order Book - Implements unified Order_book_intf *)

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
    last_update_id: int64;
    epoch: int;
    update_time: Time_float_unix.t;
  } [@@deriving fields, sexp]

  let empty ?(timestamp) ?(last_update_id = 0L) symbol =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    { symbol;
      bids = Bid_price_map.empty;
      asks = Ask_price_map.empty;
      last_update_id;
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

  (** Apply depth update from Bitrue WebSocket *)
  let apply_depth_update t (depth : Ws.Message.depth) =
    (* Replace bids *)
    let new_bids = List.fold depth.tick.buys ~init:Bid_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    (* Replace asks *)
    let new_asks = List.fold depth.tick.asks ~init:Ask_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    { t with
      bids = new_bids;
      asks = new_asks;
      epoch = t.epoch + 1;
      update_time = Time_float_unix.now ();
    }

  (** Apply REST API order book *)
  let apply_rest_depth t (book : Rest.Types.order_book) =
    (* Replace entire book *)
    let bids = List.fold book.bids ~init:Bid_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    let asks = List.fold book.asks ~init:Ask_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    { t with
      bids;
      asks;
      last_update_id = book.lastUpdateId;
      epoch = t.epoch + 1;
      update_time = Time_float_unix.now ();
    }

  (** Create live order book pipe from Bitrue WebSocket (libcurl) *)
  let pipe ~symbol ?url () : (t, string) Result.t Pipe.Reader.t Deferred.Or_error.t =
    let symbol_str = Fluxum.Types.Symbol.to_string symbol in
    let streams = [Ws.Stream.Depth symbol_str] in
    let url = Option.value url ~default:Ws.Endpoint.market in
    let%bind md_result = Market_data_curl.connect ~url ~streams () in
    match md_result with
    | Error err ->
      let reader, _writer = Pipe.create () in
      Pipe.close_read reader;
      return (Error (Error.of_string err))
    | Ok md ->
      let messages = Market_data_curl.messages md in
      let book_reader, book_writer = Pipe.create () in
      let book_ref = ref (empty symbol) in

      don't_wait_for (
        Pipe.iter messages ~f:(fun msg_str ->
          try
            match Ws.parse_message msg_str with
            | Ws.Message.Depth depth ->
              book_ref := apply_depth_update !book_ref depth;
              Pipe.write book_writer (Ok !book_ref)
            | Ws.Message.SubResponse resp ->
              Log.Global.info "Bitrue: Subscription %s"
                (Option.value resp.status ~default:"acknowledged");
              Deferred.unit
            | _ -> Deferred.unit
          with exn ->
            Pipe.write book_writer (Error (Exn.to_string exn))
        )
        >>| fun () -> Pipe.close book_writer
      );

      return (Ok book_reader)

  let symbol t = t.symbol
  let epoch t = t.epoch
  let last_update_id t = t.last_update_id
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

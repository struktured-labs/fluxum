(** Coinbase Order Book - Implements unified Order_book_intf *)

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
    sequence_num: int64;
    epoch: int;
    update_time: Time_float_unix.t;
  } [@@deriving fields, sexp]

  let empty ?(timestamp) ?(sequence_num = 0L) symbol =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    { symbol;
      bids = Bid_price_map.empty;
      asks = Ask_price_map.empty;
      sequence_num;
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

  (** Apply level2 update from Coinbase WebSocket *)
  let apply_level2_update t (msg : Ws.Message.level2) =
    (* Coinbase sends updates with events containing price_level and new_quantity *)
    let t_with_updates = List.fold msg.events ~init:t ~f:(fun acc event ->
      List.fold event.updates ~init:acc ~f:(fun acc2 update ->
        let price = Float.of_string update.price_level in
        let size = Float.of_string update.new_quantity in
        let side = match event.side with
          | "bid" -> `Bid
          | "offer" | "ask" -> `Ask
          | _ -> `Bid  (* Default, shouldn't happen *)
        in
        set acc2 ~side ~price ~size
      )
    ) in
    { t_with_updates with sequence_num = msg.sequence_num }

  (** Apply REST API product book *)
  let apply_product_book t (book : Rest.Types.product_book) =
    (* Replace entire book *)
    let bids = List.fold book.bids ~init:Bid_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.price in
      let volume = Float.of_string level.size in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    let asks = List.fold book.asks ~init:Ask_price_map.empty ~f:(fun acc level ->
      let price = Float.of_string level.price in
      let volume = Float.of_string level.size in
      if Float.(volume > 0.) then
        Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
      else
        acc
    ) in
    { t with
      bids;
      asks;
      epoch = t.epoch + 1;
      update_time = Time_float_unix.now ();
    }

  (** Create live order book pipe from Coinbase WebSocket *)
  let pipe ~symbol ?url () : (t, string) Result.t Pipe.Reader.t Deferred.Or_error.t =
    let product_id = Fluxum.Types.Symbol.to_string symbol in
    let streams = [Ws.Stream.Level2 [product_id]] in
    let url = Option.value url ~default:Ws.Endpoint.advanced_trade in
    let%bind ws_result = Ws.connect ~url ~streams () in
    match ws_result with
    | Error err ->
      eprintf "[COINBASE] WebSocket connection failed: %s\n%!" (Error.to_string_hum err);
      eprintf "[COINBASE] Symbol: %s, URL: %s\n%!" product_id url;
      eprintf "[COINBASE] This is likely due to:\n";
      eprintf "  - Missing or invalid headers (User-Agent, Origin)\n";
      eprintf "  - Authentication required for this endpoint\n";
      eprintf "  - Geographic IP restrictions\n";
      eprintf "  - Network connectivity issues\n%!";
      let reader, _writer = Pipe.create () in
      Pipe.close_read reader;
      return (Ok reader)
    | Ok ws ->
      let messages = Ws.messages ws in
      let book_reader, book_writer = Pipe.create () in
      let book_ref = ref (empty symbol) in

      don't_wait_for (
        Pipe.iter messages ~f:(fun msg_str ->
          try
            match Ws.parse_message msg_str with
            | Ws.Message.Level2 level2 ->
              book_ref := apply_level2_update !book_ref level2;
              Pipe.write book_writer (Ok !book_ref)
            | _ -> Deferred.unit
          with exn ->
            Pipe.write book_writer (Error (Exn.to_string exn))
        )
        >>| fun () -> Pipe.close book_writer
      );

      return (Ok book_reader)

  let symbol t = t.symbol
  let epoch t = t.epoch
  let sequence_num t = t.sequence_num
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

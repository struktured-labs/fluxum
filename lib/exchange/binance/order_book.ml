(** Binance Order Book - Implements unified Order_book_intf *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Fluxum.Order_book_intf.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Price comparators (same as Kraken) *)
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

  let apply_depth_update t (update : Ws.Message.depth_update) =
    (* Apply bid updates *)
    let t_with_bids = List.fold update.bids ~init:t ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let size = Float.of_string qty_str in
      set acc ~side:`Bid ~price ~size
    ) in
    (* Apply ask updates *)
    let t_with_asks = List.fold update.asks ~init:t_with_bids ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let size = Float.of_string qty_str in
      set acc ~side:`Ask ~price ~size
    ) in
    { t_with_asks with last_update_id = update.final_update_id }

  let apply_depth_snapshot t (snapshot : Ws.Message.depth) =
    (* Replace entire book with snapshot *)
    let bids = List.fold snapshot.bids ~init:Bid_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
    ) in
    let asks = List.fold snapshot.asks ~init:Ask_price_map.empty ~f:(fun acc (price_str, qty_str) ->
      let price = Float.of_string price_str in
      let volume = Float.of_string qty_str in
      Map.set acc ~key:price ~data:(Price_level.create ~price ~volume)
    ) in
    { t with
      bids;
      asks;
      last_update_id = snapshot.last_update_id;
      epoch = t.epoch + 1;
    }

  (** Create live order book pipe from Binance WebSocket *)
  let pipe ~symbol ?(depth = 20) () : (t, string) Result.t Pipe.Reader.t Deferred.Or_error.t =
    let streams = [Ws.Stream.Depth { symbol; levels = Some depth }] in
    let%bind ws_result = Ws.connect ~streams () in
    match ws_result with
    | Error err ->
      eprintf "[BINANCE] WebSocket connection failed: %s\n%!" (Error.to_string_hum err);
      let symbol_str = Fluxum.Types.Symbol.to_string symbol in
      eprintf "[BINANCE] Symbol: %s, Depth: %d\n%!" symbol_str depth;
      eprintf "[BINANCE] Stream name: %s@depth%d\n%!" (String.lowercase symbol_str) depth;
      eprintf "[BINANCE] This is likely due to:\n";
      eprintf "  - 451 status (Geographic IP block or legal restrictions)\n";
      eprintf "  - Missing User-Agent header\n";
      eprintf "  - Invalid stream name format\n";
      eprintf "  - Rate limiting or temporary ban\n%!";
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
            | Ws.Message.Depth snapshot ->
              book_ref := apply_depth_snapshot !book_ref snapshot;
              Pipe.write book_writer (Ok !book_ref)
            | Ws.Message.DepthUpdate update when String.equal update.symbol symbol ->
              book_ref := apply_depth_update !book_ref update;
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

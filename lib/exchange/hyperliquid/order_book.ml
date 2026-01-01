(** Hyperliquid Order Book - Uses common Order_book_base *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Hyperliquid-specific metadata *)
type metadata = {
  last_update_time: int64;
} [@@deriving sexp]

let default_metadata () = { last_update_time = 0L }

(** Instantiate order book base with Hyperliquid-specific types *)
module Book_base = Exchange_common.Order_book_base.Make (struct
  type symbol = Fluxum.Types.Symbol.t
  let sexp_of_symbol = Fluxum.Types.Symbol.sexp_of_t
  let symbol_of_sexp = Fluxum.Types.Symbol.t_of_sexp
  let compare_symbol = Fluxum.Types.Symbol.compare

  type nonrec metadata = metadata
  let sexp_of_metadata = sexp_of_metadata
  let metadata_of_sexp = metadata_of_sexp
  let default_metadata = default_metadata
end)

(** Single symbol order book with Hyperliquid-specific extensions *)
module Book = struct
  include Book_base.Book

  let empty ?timestamp:_ ?last_update_time:_ symbol =
    create ~symbol

  (** Apply L2 book update from Hyperliquid.
      Hyperliquid l2Book has levels as [[bids], [asks]] where each level is {px, sz, n} *)
  let apply_l2_book t (book : Ws.Message.l2_book) =
    let new_metadata = { last_update_time = book.time } in
    let bids, asks = match book.levels with
      | [bid_levels; ask_levels] -> (bid_levels, ask_levels)
      | _ -> ([], [])  (* Invalid format, return empty *)
    in
    (* Build level lists *)
    let bid_levels = List.filter_map bids ~f:(fun level ->
      let price = Float.of_string level.Ws.Message.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then Some (`Bid, price, volume) else None
    ) in
    let ask_levels = List.filter_map asks ~f:(fun level ->
      let price = Float.of_string level.Ws.Message.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then Some (`Ask, price, volume) else None
    ) in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    set_many new_book all_levels ~metadata:new_metadata

  (** Apply REST API l2_book response *)
  let apply_rest_l2_book t (book : Rest.Types.l2_book) =
    let new_metadata = { last_update_time = book.time } in
    let bids, asks = match book.levels with
      | [bid_levels; ask_levels] -> (bid_levels, ask_levels)
      | _ -> ([], [])
    in
    let bid_levels = List.filter_map bids ~f:(fun level ->
      let price = Float.of_string level.Rest.Types.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then Some (`Bid, price, volume) else None
    ) in
    let ask_levels = List.filter_map asks ~f:(fun level ->
      let price = Float.of_string level.Rest.Types.px in
      let volume = Float.of_string level.sz in
      if Float.(volume > 0.) then Some (`Ask, price, volume) else None
    ) in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    set_many new_book all_levels ~metadata:new_metadata

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
end

(** Multi-symbol order books *)
module Books = struct
  include Book_base.Books
  type book = Book.t
end

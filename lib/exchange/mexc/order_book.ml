(** MEXC Order Book - Uses common Order_book_base *)

open Core
open Async

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** MEXC-specific metadata *)
type metadata = {
  last_update_id: int64;
} [@@deriving sexp]

let default_metadata () = { last_update_id = 0L }

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

module Book = struct
  include Book_base.Book

  let empty ?timestamp:_ ?epoch:_ symbol =
    create ~symbol

  let apply_depth_snapshot t (depth : V1.Depth.T.response) =
    let open Result.Let_syntax in
    let new_metadata = { last_update_id = depth.lastUpdateId } in
    (* Process bids with safe conversion *)
    let%bind bid_levels =
      List.fold depth.bids ~init:(Ok []) ~f:(fun acc_result (price_str, qty_str) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok ((`Bid, price, size) :: acc))
      |> Result.map ~f:List.rev
    in
    (* Process asks with safe conversion *)
    let%bind ask_levels =
      List.fold depth.asks ~init:(Ok []) ~f:(fun acc_result (price_str, qty_str) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok ((`Ask, price, size) :: acc))
      |> Result.map ~f:List.rev
    in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    Ok (set_many new_book all_levels ~metadata:new_metadata)

  let pretty_print ?(max_depth = 10) ?refresh_ms:_ ?tick_size:_ t =
    ignore (max_depth, t);
    printf "Order book (using base implementation)\n%!"
end

module Books = struct
  include Book_base.Books
  type book = Book.t

  let set_book ?timestamp:_ t (book : book) =
    set_book t book
end

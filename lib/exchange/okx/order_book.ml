(** OKX Order Book - Uses common Order_book_base *)

open Core
open Async

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** OKX-specific metadata *)
type metadata = {
  checksum : int option;  (* OKX provides checksums for validation *)
} [@@deriving sexp]

let default_metadata () = { checksum = None }

module Book_base = Exchange_common.Order_book_base.Make (struct
  type symbol = string
  let sexp_of_symbol = String.sexp_of_t
  let symbol_of_sexp = String.t_of_sexp
  let compare_symbol = String.compare

  type nonrec metadata = metadata
  let sexp_of_metadata = sexp_of_metadata
  let metadata_of_sexp = metadata_of_sexp
  let default_metadata = default_metadata
end)

module Book = struct
  include Book_base.Book

  let empty ?timestamp:_ ?epoch:_ symbol =
    create ~symbol

  (** Apply book snapshot from OKX WebSocket

      OKX book_data has bids/asks as level lists where each level is:
      (price, size, liquidated_orders, num_orders)
  *)
  let apply_book_snapshot t (book : Ws.Message.book_data) : (t, string) Result.t =
    let open Result.Let_syntax in
    let new_metadata = { checksum = book.checksum } in

    (* Process bids with safe conversion *)
    let%bind bid_levels =
      List.fold book.bids ~init:(Ok []) ~f:(fun acc_result (price_str, size_str, _, _) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string size_str in
        Ok ((`Bid, price, size) :: acc))
      |> Result.map ~f:List.rev
    in

    (* Process asks with safe conversion *)
    let%bind ask_levels =
      List.fold book.asks ~init:(Ok []) ~f:(fun acc_result (price_str, size_str, _, _) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string size_str in
        Ok ((`Ask, price, size) :: acc))
      |> Result.map ~f:List.rev
    in

    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    Ok (set_many new_book all_levels ~metadata:new_metadata)

  (** Create live order book pipe from OKX WebSocket *)
  let pipe ~symbol ?(depth = 10) () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    ignore depth;
    let open Deferred.Let_syntax in

    (* Connect to OKX WebSocket with books stream *)
    let stream = { Ws.Stream.channel = Ws.Stream.Books; instId = symbol } in
    let%bind ws_result = Ws.connect ~streams:[stream] () in

    match ws_result with
    | Error err ->
      Log.Global.error "OKX WS connection failed for %s: %s" symbol (Error.to_string_hum err);
      let reader, writer = Pipe.create () in
      don't_wait_for (Pipe.write writer (Error (sprintf "WebSocket connection failed: %s" (Error.to_string_hum err))));
      Pipe.close writer;
      return reader
    | Ok ws ->
      let reader, writer = Pipe.create () in
      let book_ref = ref (empty symbol) in

      (* Process WebSocket messages *)
      don't_wait_for (
        Pipe.iter (Ws.messages ws) ~f:(fun msg_str ->
          let msg = Ws.parse_message msg_str in
          match msg with
          | Ws.Message.Books book_list ->
            (* Process each book update *)
            List.iter book_list ~f:(fun book_data ->
              match apply_book_snapshot !book_ref book_data with
              | Ok updated_book ->
                book_ref := updated_book;
                don't_wait_for (Pipe.write writer (Ok updated_book))
              | Error err ->
                Log.Global.error "OKX book update failed for %s: %s" symbol err;
                don't_wait_for (Pipe.write writer (Error err))
            );
            return ()
          | Ws.Message.Error err ->
            Log.Global.error "OKX WS error for %s: %s" symbol err;
            don't_wait_for (Pipe.write writer (Error err));
            return ()
          | _ ->
            (* Ignore other message types *)
            return ()
        )
        >>= fun () ->
        let%bind () = Ws.close ws in
        Pipe.close writer;
        return ()
      );

      return reader

  let pretty_print ?(max_depth = 10) ?refresh_ms:_ ?tick_size:_ t =
    ignore (max_depth, t);
    printf "OKX Order Book (using base implementation)\n%!"
end

module Books = struct
  include Book_base.Books
  type book = Book.t

  let set_book ?timestamp:_ t (book : book) =
    set_book t book
end

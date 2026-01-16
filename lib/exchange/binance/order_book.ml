(** Binance Order Book - Uses common Order_book_base *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Binance-specific metadata *)
type metadata = {
  last_update_id: int64;
} [@@deriving sexp]

let default_metadata () = { last_update_id = 0L }

(** Instantiate order book base with Binance-specific types *)
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

(** Single symbol order book with Binance-specific extensions *)
module Book = struct
  include Book_base.Book

  (** Create empty book (timestamp and last_update_id set on first update) *)
  let empty ?timestamp:_ ?last_update_id:_ symbol =
    create ~symbol

  (** Get last update ID *)
  let last_update_id t =
    let m : metadata = metadata t in
    m.last_update_id

  (** Apply Binance depth update *)
  let apply_depth_update t (update : Ws.Message.depth_update) : (t, string) Result.t =
    let open Result.Let_syntax in
    let new_metadata = { last_update_id = update.final_update_id } in
    (* Apply bid updates with new metadata *)
    let%bind t_with_bids =
      List.fold update.bids ~init:(Ok t) ~f:(fun acc_result (price_str, qty_str) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok (set acc ~side:`Bid ~price ~size ~metadata:new_metadata))
    in
    (* Apply ask updates with new metadata *)
    List.fold update.asks ~init:(Ok t_with_bids) ~f:(fun acc_result (price_str, qty_str) ->
      let%bind acc = acc_result in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
      let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
      Ok (set acc ~side:`Ask ~price ~size ~metadata:new_metadata))

  (** Apply Binance depth snapshot *)
  let apply_depth_snapshot t (snapshot : Ws.Message.depth) : (t, string) Result.t =
    let open Result.Let_syntax in
    (* Build new book from snapshot using set_many *)
    let%bind bid_levels =
      List.fold snapshot.bids ~init:(Ok []) ~f:(fun acc_result (price_str, qty_str) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok ((`Bid, price, size) :: acc))
      |> Result.map ~f:List.rev
    in
    let%bind ask_levels =
      List.fold snapshot.asks ~init:(Ok []) ~f:(fun acc_result (price_str, qty_str) ->
        let%bind acc = acc_result in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok ((`Ask, price, size) :: acc))
      |> Result.map ~f:List.rev
    in
    let all_levels = bid_levels @ ask_levels in
    let new_metadata = { last_update_id = snapshot.last_update_id } in
    let new_book = create ~symbol:(symbol t) in
    Ok (set_many new_book all_levels ~metadata:new_metadata)

  (** Create live order book pipe from Binance WebSocket *)
  let pipe ~symbol ?(depth = 20) ?(url = Ws.Endpoint.data_stream) () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    let open Deferred.Let_syntax in
    let streams = [Ws.Stream.Depth { symbol; levels = Some depth }] in
    let%bind md_result = Market_data.connect ~streams ~url () in
    match md_result with
    | Error err ->
      eprintf "[BINANCE] WebSocket connection failed: %s\n%!" err;
      let symbol_str = Fluxum.Types.Symbol.to_string symbol in
      eprintf "[BINANCE] Symbol: %s, Depth: %d, URL: %s\n%!" symbol_str depth url;
      eprintf "[BINANCE] Stream name: %s@depth%d\n%!" (String.lowercase symbol_str) depth;
      eprintf "[BINANCE] This is likely due to:\n";
      eprintf "  - Network connectivity issues\n";
      eprintf "  - Invalid stream name format\n";
      eprintf "  - Rate limiting or temporary ban\n%!";
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
            | Ws.Message.Depth snapshot ->
              (match apply_depth_snapshot !book_ref snapshot with
               | Ok new_book ->
                 book_ref := new_book;
                 Pipe.write book_writer (Ok !book_ref)
               | Error msg ->
                 Pipe.write book_writer (Error msg))
            | Ws.Message.DepthUpdate update when String.equal update.symbol symbol ->
              (match apply_depth_update !book_ref update with
               | Ok new_book ->
                 book_ref := new_book;
                 Pipe.write book_writer (Ok !book_ref)
               | Error msg ->
                 Pipe.write book_writer (Error msg))
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

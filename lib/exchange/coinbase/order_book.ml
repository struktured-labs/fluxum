(** Coinbase Order Book - Uses common Order_book_base *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Coinbase-specific metadata *)
type metadata = {
  sequence_num: int64;
} [@@deriving sexp]

let default_metadata () = { sequence_num = 0L }

(** Instantiate order book base with Coinbase-specific types *)
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

(** Single symbol order book with Coinbase-specific extensions *)
module Book = struct
  include Book_base.Book

  (** Create empty book (timestamp and sequence_num set on first update) *)
  let empty ?timestamp:_ ?sequence_num:_ symbol =
    create ~symbol

  (** Get sequence number *)
  let sequence_num t =
    let m : metadata = metadata t in
    m.sequence_num

  (** Apply level2 update from Coinbase WebSocket *)
  let apply_level2_update t (msg : Ws.Message.level2) =
    let new_metadata = { sequence_num = msg.sequence_num } in
    (* Coinbase sends updates with events containing price_level and new_quantity *)
    List.fold msg.events ~init:t ~f:(fun acc event ->
      List.fold event.updates ~init:acc ~f:(fun acc2 update ->
        match (
          let open Result.Let_syntax in
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string update.price_level in
          let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string update.new_quantity in
          Ok (price, size)
        ) with
        | Error err ->
          Log.Global.error "Coinbase WS: Failed to parse level2 update: %s" err;
          acc2
        | Ok (price, size) ->
          let side = match event.side with
            | "bid" -> `Bid
            | "offer" | "ask" -> `Ask
            | _ -> `Bid  (* Default, shouldn't happen *)
          in
          set acc2 ~side ~price ~size ~metadata:new_metadata
      )
    )

  (** Apply REST API product book *)
  let apply_product_book t (book : Rest.Types.product_book) =
    (* Build level list from REST response *)
    let bid_levels = List.filter_map book.bids ~f:(fun level ->
      match (
        let open Result.Let_syntax in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
        Ok (price, size)
      ) with
      | Ok (price, size) when Float.(size > 0.) -> Some (`Bid, price, size)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Coinbase REST: Failed to parse bid level: %s" err;
        None
    ) in
    let ask_levels = List.filter_map book.asks ~f:(fun level ->
      match (
        let open Result.Let_syntax in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
        Ok (price, size)
      ) with
      | Ok (price, size) when Float.(size > 0.) -> Some (`Ask, price, size)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Coinbase REST: Failed to parse ask level: %s" err;
        None
    ) in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    set_many new_book all_levels

  (** Create live order book pipe from Coinbase WebSocket *)
  let pipe ~symbol ?url () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    let open Deferred.Let_syntax in
    let product_id = Fluxum.Types.Symbol.to_string symbol in
    let streams = [Ws.Stream.Level2 [product_id]] in
    let url = Option.value url ~default:Ws.Endpoint.exchange in
    let%bind md_result = Market_data.connect ~streams ~url () in
    match md_result with
    | Error err ->
      eprintf "[COINBASE] WebSocket connection failed: %s\n%!" err;
      eprintf "[COINBASE] Symbol: %s, URL: %s\n%!" product_id url;
      eprintf "[COINBASE] This is likely due to:\n";
      eprintf "  - TLS handshake failure (incompatible TLS configuration)\n";
      eprintf "  - Missing or invalid headers (User-Agent, Origin)\n";
      eprintf "  - Authentication required for this endpoint\n";
      eprintf "  - Network connectivity issues\n%!";
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
            | Ws.Message.Level2 level2 ->
              book_ref := apply_level2_update !book_ref level2;
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

(** Bitrue Order Book - Uses common Order_book_base *)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Bitrue-specific metadata *)
type metadata = {
  last_update_id: int64;
} [@@deriving sexp]

let default_metadata () = { last_update_id = 0L }

(** Instantiate order book base with Bitrue-specific types *)
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

(** Single symbol order book with Bitrue-specific extensions *)
module Book = struct
  include Book_base.Book

  (** Create empty book (timestamp and last_update_id set on first update) *)
  let empty ?timestamp:_ ?last_update_id:_ symbol =
    create ~symbol

  (** Get last update ID *)
  let last_update_id t =
    let m : metadata = metadata t in
    m.last_update_id

  (** Apply depth update from Bitrue WebSocket *)
  let apply_depth_update t (depth : Ws.Message.depth) =
    (* Build level lists from depth message *)
    let bid_levels = List.filter_map depth.tick.buys ~f:(fun (price_str, qty_str) ->
      let open Result.Let_syntax in
      match (
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok (price, volume)
      ) with
      | Ok (price, volume) when Float.(volume > 0.) -> Some (`Bid, price, volume)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Bitrue WS: Failed to parse bid level: %s" err;
        None
    ) in
    let ask_levels = List.filter_map depth.tick.asks ~f:(fun (price_str, qty_str) ->
      let open Result.Let_syntax in
      match (
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok (price, volume)
      ) with
      | Ok (price, volume) when Float.(volume > 0.) -> Some (`Ask, price, volume)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Bitrue WS: Failed to parse ask level: %s" err;
        None
    ) in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    set_many new_book all_levels

  (** Apply REST API order book *)
  let apply_rest_depth t (book : Rest.Types.order_book) =
    let new_metadata = { last_update_id = book.lastUpdateId } in
    (* Build level lists from REST response *)
    let bid_levels = List.filter_map book.bids ~f:(fun (price_str, qty_str) ->
      let open Result.Let_syntax in
      match (
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok (price, volume)
      ) with
      | Ok (price, volume) when Float.(volume > 0.) -> Some (`Bid, price, volume)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Bitrue REST: Failed to parse bid level: %s" err;
        None
    ) in
    let ask_levels = List.filter_map book.asks ~f:(fun (price_str, qty_str) ->
      let open Result.Let_syntax in
      match (
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price_str in
        let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string qty_str in
        Ok (price, volume)
      ) with
      | Ok (price, volume) when Float.(volume > 0.) -> Some (`Ask, price, volume)
      | Ok _ -> None
      | Error err ->
        Log.Global.error "Bitrue REST: Failed to parse ask level: %s" err;
        None
    ) in
    let all_levels = bid_levels @ ask_levels in
    let new_book = create ~symbol:(symbol t) in
    set_many new_book all_levels ~metadata:new_metadata

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
end

(** Multi-symbol order books *)
module Books = struct
  include Book_base.Books
  type book = Book.t
end

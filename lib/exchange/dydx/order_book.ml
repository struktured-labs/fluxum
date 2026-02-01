open Core
open Async

(** Create the order book base using the functor *)
module Config = struct
  type symbol = string
  let sexp_of_symbol s = Sexp.Atom s
  let symbol_of_sexp = function
    | Sexp.Atom s -> s
    | sexp -> raise_s [%message "Invalid symbol sexp" (sexp : Sexp.t)]
  let compare_symbol = String.compare

  type metadata = {
    message_id: int;
    connection_id: string;
  } [@@deriving sexp]

  let default_metadata () = {
    message_id = 0;
    connection_id = "";
  }
end

module Book_base = Exchange_common.Order_book_base.Make(Config)

module Book = struct
  include Book_base.Book

  (** Create order book from WebSocket orderbook message *)
  let update_from_message book (data : Ws.channel_data) (contents : Ws.orderbook_contents) =
    (* Parse bids *)
    let bid_updates = List.filter_map contents.bids ~f:(fun level ->
      match (
        let open Result.Let_syntax in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
        Ok (price, size)
      ) with
      | Ok (price, size) -> Some (`Bid, price, size)
      | Error err ->
        Log.Global.error "dYdX WS: Failed to parse bid level: %s" err;
        None
    ) in

    (* Parse asks *)
    let ask_updates = List.filter_map contents.asks ~f:(fun level ->
      match (
        let open Result.Let_syntax in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
        let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
        Ok (price, size)
      ) with
      | Ok (price, size) -> Some (`Ask, price, size)
      | Error err ->
        Log.Global.error "dYdX WS: Failed to parse ask level: %s" err;
        None
    ) in

    (* Create metadata *)
    let metadata : Config.metadata = {
      message_id = data.message_id;
      connection_id = data.connection_id;
    } in

    (* Apply all updates *)
    set_many book ~metadata (bid_updates @ ask_updates)

  (** Create a pipe that streams order book updates for a market *)
  let pipe (module Cfg : Cfg.S) ~symbol () =
    let%bind conn = Ws.connect ~url:Cfg.ws_url in
    let%bind message_pipe = Ws.create_message_pipe ~conn in

    (* Subscribe to orderbook *)
    let%bind () = Ws.Orderbook.subscribe ~conn ~market:symbol ~batched:true () in

    (* Create initial book *)
    let init_book = create ~symbol in

    (* Fold over messages to build book *)
    let book_pipe = Pipe.folding_map message_pipe ~init:init_book ~f:(fun book json ->
      match Ws.Orderbook.parse_message json with
      | Ok (data, contents) ->
          let updated_book = update_from_message book data contents in
          (updated_book, updated_book)
      | Error (`Json_parse_error msg) ->
          Core.printf "Parse error: %s\n" msg;
          (book, book)
    ) in

    return book_pipe

  (** Like pipe but raises on errors *)
  let pipe_exn cfg ~symbol () =
    pipe cfg ~symbol ()
end

module Books = Book_base.Books

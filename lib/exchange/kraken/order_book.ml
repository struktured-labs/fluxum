(** Kraken Order Book - Uses common Order_book_base *)

open Core
open Async

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Kraken-specific metadata - none beyond base *)
type metadata = unit [@@deriving sexp]

let default_metadata () = ()

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

  (** Update (add or remove) price level - used by tests *)
  let update t ~side ~price ~size =
    (* O(log n) lookup via Map.find instead of O(n) alist search *)
    let current =
      let find = match Poly.(side = `Bid) with true -> find_bid | false -> find_ask in
      match find t ~price with
      | Some level -> Price_level.volume level
      | None -> 0.
    in
    set t ~side ~price ~size:(current +. size)

  (** Remove from price level - used by tests *)
  let remove t ~side ~price ~size =
    update t ~side ~price ~size:(-.size)

  (** Apply WebSocket book update from Kraken *)
  let apply_book_update t (update : Ws.Public.Book_data.update) : t =
    (* Process bid updates *)
    let t_with_bids =
      List.fold update.bids ~init:t ~f:(fun acc level ->
        let open Result.Let_syntax in
        match (
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.Ws.Public.Price_level.price in
          let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string level.Ws.Public.Price_level.volume in
          Ok (price, volume)
        ) with
        | Ok (price, volume) -> set acc ~side:`Bid ~price ~size:volume
        | Error err ->
          Log.Global.error "Kraken WS: Failed to parse bid level: %s" err;
          acc
      )
    in
    (* Process ask updates *)
    let t_with_asks =
      List.fold update.asks ~init:t_with_bids ~f:(fun acc level ->
        let open Result.Let_syntax in
        match (
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.Ws.Public.Price_level.price in
          let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string level.Ws.Public.Price_level.volume in
          Ok (price, volume)
        ) with
        | Ok (price, volume) -> set acc ~side:`Ask ~price ~size:volume
        | Error err ->
          Log.Global.error "Kraken WS: Failed to parse ask level: %s" err;
          acc
      )
    in
    t_with_asks

  (** Create live order book pipe from Kraken WebSocket *)
  let pipe ~symbol ?(depth = 10) () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    let open Deferred.Let_syntax in
    let%bind market_data_result =
      Market_data.connect
        ~subscriptions:[{
          channel = "book";
          pairs = [symbol];
          interval = None;
          depth = Some depth;
        }]
        ()
    in
    match market_data_result with
    | Error _err ->
      let reader, _writer = Pipe.create () in
      Pipe.close_read reader;
      return reader
    | Ok md ->
      let messages = Market_data.messages md in
      let book_reader, book_writer = Pipe.create () in

      (* Start with empty book *)
      let book_ref = ref (empty symbol) in

      (* Process incoming messages *)
      don't_wait_for (
        Pipe.iter messages ~f:(fun msg_str ->
          try
            match Ws.parse_message msg_str with
            | Ok (Ws.Public (Ws.Public.Book book_data)) when String.equal book_data.pair symbol ->
              book_ref := apply_book_update !book_ref book_data.update;
              Pipe.write book_writer (Ok !book_ref)
            | Ok _ ->
              (* Other message types, ignore *)
              Deferred.unit
            | Error err ->
              Pipe.write book_writer (Error (Sexp.to_string (Ws.Error.sexp_of_t err)))
          with
          | exn ->
            Pipe.write book_writer (Error (Exn.to_string exn))
        )
        >>| fun () ->
        Pipe.close book_writer
      );

      return book_reader
end

module Books = struct
  include Book_base.Books
  type book = Book.t
end

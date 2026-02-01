(** Bitstamp Order Book - Uses common Order_book_base with WebSocket diff updates *)

open Core
open Async

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

type metadata = {
  microtimestamp : string;
} [@@deriving sexp]

let default_metadata () = { microtimestamp = "" }

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

  let apply_snapshot book (ob : Types.order_book) =
    let new_metadata = { microtimestamp = ob.microtimestamp } in
    let bid_levels = List.filter_map ob.bids ~f:(fun (price, amount) ->
      match Float.(amount > 0.) with
      | true -> Some (`Bid, price, amount)
      | false -> None
    ) in
    let ask_levels = List.filter_map ob.asks ~f:(fun (price, amount) ->
      match Float.(amount > 0.) with
      | true -> Some (`Ask, price, amount)
      | false -> None
    ) in
    let new_book = create ~symbol:(symbol book) in
    set_many new_book ~metadata:new_metadata (bid_levels @ ask_levels)

  let pipe ~cfg ~pair () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    let channels = [Ws.Channel.Diff_order_book pair] in
    let%bind conn_result = Ws.connect ~cfg ~channels () in
    match conn_result with
    | Error err ->
      let reader, writer = Pipe.create () in
      Pipe.write_without_pushback writer (Error err);
      Pipe.close writer;
      return reader
    | Ok conn ->
      (* Get initial snapshot via REST *)
      let%bind initial_book =
        Rest.order_book ~cfg ~pair () >>| function
        | Ok ob ->
          let book = create ~symbol:pair in
          Ok (apply_snapshot book ob)
        | Error e -> Error (Rest.Error.to_string e)
      in
      let book_reader, book_writer = Pipe.create () in
      let book_ref = ref (match initial_book with
        | Ok book -> book
        | Error _ -> create ~symbol:pair)
      in
      (match initial_book with
       | Ok _ -> Pipe.write_without_pushback book_writer (Ok !book_ref)
       | Error e -> Pipe.write_without_pushback book_writer (Error e));

      don't_wait_for (
        let rec read_loop () =
          Ws.receive conn >>= function
          | None ->
            Pipe.close book_writer;
            return ()
          | Some msg_str ->
            (match Ws.parse_message msg_str with
             | Ws.Message.Order_book ob ->
               book_ref := apply_snapshot !book_ref ob;
               Pipe.write book_writer (Ok !book_ref) >>= read_loop
             | Ws.Message.Data json_str ->
               (* Try parsing as order book data *)
               (try
                  match Yojson.Safe.from_string json_str |> Types.order_book_of_yojson with
                  | Ok ob ->
                    book_ref := apply_snapshot !book_ref ob;
                    Pipe.write book_writer (Ok !book_ref) >>= read_loop
                  | Error _ -> read_loop ()
                with _ -> read_loop ())
             | Ws.Message.Error err ->
               Log.Global.error "Bitstamp WS order book error: %s" err;
               read_loop ()
             | _ -> read_loop ())
        in
        read_loop ()
      );

      return book_reader
end

module Books = struct
  include Book_base.Books
  type book = Book.t
end

(** Jupiter Order Book - Uses common Order_book_base *)

open Core
open Async

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Jupiter-specific metadata - none beyond base *)
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

  (** Update (add or remove) price level *)
  let update t ~side ~price ~size =
    let levels = match Poly.(side = `Bid) with true -> bids_alist t | false -> asks_alist t in
    let current =
      match List.Assoc.find levels ~equal:Float.equal price with
      | Some level -> Price_level.volume level
      | None -> 0.
    in
    set t ~side ~price ~size:(current +. size)

  (** Remove from price level *)
  let remove t ~side ~price ~size =
    update t ~side ~price ~size:(-.size)

  (** Apply book update from Jupiter WebSocket

      Note: This is a placeholder implementation until Jupiter WebSocket client is implemented.
      When the WS client is ready, this should parse Jupiter's specific update format.
  *)
  let apply_book_update t (update : Yojson.Safe.t) : t =
    try
      let open Yojson.Safe.Util in
      (* Jupiter format: {"b": [[price, size], ...], "a": [[price, size], ...]} *)
      (* Use safe extraction - member returns `Null for missing keys *)
      let safe_to_list json = match json with
        | `Null -> []
        | `List l -> l
        | other ->
          Log.Global.debug "Jupiter WS: Expected list for bids/asks, got: %s"
            (Yojson.Safe.to_string other);
          []
      in
      let bids = update |> member "b" |> safe_to_list in
      let asks = update |> member "a" |> safe_to_list in

      (* Process bid updates *)
      let t_with_bids =
        List.fold bids ~init:t ~f:(fun acc level ->
          let open Result.Let_syntax in
          match (
            let level_list = to_list level in
            match level_list with
            | [price_json; size_json] ->
              let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string (to_string price_json) in
              let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string (to_string size_json) in
              Ok (price, volume)
            | _ -> Error "Invalid bid level format"
          ) with
          | Ok (price, volume) -> set acc ~side:`Bid ~price ~size:volume
          | Error err ->
            Log.Global.error "Jupiter WS: Failed to parse bid level: %s" err;
            acc
        )
      in

      (* Process ask updates *)
      let t_with_asks =
        List.fold asks ~init:t_with_bids ~f:(fun acc level ->
          let open Result.Let_syntax in
          match (
            let level_list = to_list level in
            match level_list with
            | [price_json; size_json] ->
              let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string (to_string price_json) in
              let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string (to_string size_json) in
              Ok (price, volume)
            | _ -> Error "Invalid ask level format"
          ) with
          | Ok (price, volume) -> set acc ~side:`Ask ~price ~size:volume
          | Error err ->
            Log.Global.error "Jupiter WS: Failed to parse ask level: %s" err;
            acc
        )
      in
      t_with_asks
    with
    | exn ->
      Log.Global.error "Jupiter WS: Exception in apply_book_update: %s" (Exn.to_string exn);
      t

  (** Create live order book pipe from Jupiter WebSocket

      Note: This is a placeholder implementation that returns an empty pipe.
      When Jupiter WebSocket client is implemented, this should:
      1. Connect to Jupiter WebSocket
      2. Subscribe to orderbook channel for the symbol
      3. Apply updates using apply_book_update
      4. Return a pipe that emits book snapshots
  *)
  let pipe ~symbol ?(depth = 10) () : (t, string) Result.t Pipe.Reader.t Deferred.t =
    ignore depth;
    Log.Global.info "Jupiter order book pipe for %s (placeholder - WebSocket not yet implemented)" symbol;
    let reader, writer = Pipe.create () in

    (* Start with empty book *)
    let init_book = empty symbol in
    don't_wait_for (Pipe.write writer (Ok init_book));

    (* Close immediately for now - real implementation will keep connection open *)
    Pipe.close writer;
    return reader
end

module Books = struct
  include Book_base.Books
  type book = Book.t
end

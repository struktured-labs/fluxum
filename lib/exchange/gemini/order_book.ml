(** Gemini Order Book - Uses common Order_book_base *)

open! Common
open V1

module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Market_data.Side.Bid_ask

(** Gemini metadata - none beyond base epoch and update_time *)
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

  let empty ?timestamp:_ ?epoch:_ symbol_variant =
    (* Convert Symbol.t to string *)
    let symbol = Symbol.to_string symbol_variant in
    create ~symbol

  (** Process Gemini market data events and update order book *)
  let on_market_data t (market_data : Market_data.response) =
    let message = market_data.message in
    let f t (event : Market_data.event) =
      match event with
      | `Auction_open _auction_open -> t
      | `Change { price; side; reason = _; remaining; delta = _ } ->
        (match (
          let open Result.Let_syntax in
          let%bind size = Fluxum.Normalize_common.Float_conv.qty_of_string remaining in
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string price in
          Ok (size, price)
        ) with
        | Ok (size, price) -> set t ~price ~size ~side
        | Error err ->
          Log.Global.error "Gemini order_book: Failed to parse Change event: %s" err;
          t)
      | `Trade _trade -> t
      | `Block_trade _trade -> t
      | `Auction _auction -> t
    in

    match message with
    | `Heartbeat () -> t
    | `Update { event_id = _; events; timestamp = _; timestampms = _ } ->
      Array.fold events ~init:t ~f

  (** Create live order book pipe from Gemini WebSocket *)
  let pipe (module Cfg : Cfg.S) ~(symbol : Symbol.t) () =
    let book = empty symbol in
    Market_data.client (module Cfg) ?query:None ~uri_args:symbol ()
    >>| fun pipe ->
    Pipe.folding_map ~init:book pipe ~f:(fun book response ->
      match response with
      | `Ok response ->
        let book = on_market_data book response in
        (book, `Ok book)
      | #Market_data.Error.t as e -> (book, e))

  (** Create pipe that raises on errors (convenience wrapper) *)
  let pipe_exn (module Cfg : Cfg.S) ~symbol () =
    pipe (module Cfg) ~symbol ()
    >>| Pipe.map ~f:(function
      | `Ok book -> book
      | #Market_data.Error.t as e ->
        failwiths ~here:[%here] "Market data error" e Market_data.Error.sexp_of_t)

  (** Create live order book pipe for an arbitrary instrument symbol string.
      Used for prediction market instruments like GEMI-BTC100K-YES. *)
  let pipe_for_instrument (module Cfg : Cfg.S) ~instrument_symbol () =
    let book = create ~symbol:instrument_symbol in
    Market_data.client_for_string_symbol (module Cfg) ~symbol:instrument_symbol ()
    >>| fun pipe ->
    Pipe.folding_map ~init:book pipe ~f:(fun book response ->
      match response with
      | `Ok response ->
        let book = on_market_data book response in
        (book, `Ok book)
      | #Market_data.Error.t as e -> (book, e))

  (** Create pipe for instrument symbol that raises on errors *)
  let pipe_for_instrument_exn (module Cfg : Cfg.S) ~instrument_symbol () =
    pipe_for_instrument (module Cfg) ~instrument_symbol ()
    >>| Pipe.map ~f:(function
      | `Ok book -> book
      | #Market_data.Error.t as e ->
        failwiths ~here:[%here] "Market data error" e Market_data.Error.sexp_of_t)
end

module Books = struct
  include Book_base.Books
  type book = Book.t

  (** Update books collection with new market data for a symbol *)
  let on_market_data t symbol (market_data : Market_data.response) =
    let symbol_str = Symbol.to_string symbol in
    let current_book =
      match book t symbol_str with
      | None -> Book.empty symbol
      | Some b -> b
    in
    let updated_book = Book.on_market_data current_book market_data in
    set_book t updated_book

  (** Create pipes for multiple symbols *)
  let pipe (module Cfg : Cfg.S) ?(symbols = Symbol.all) () :
      [ `Ok of Book.t | Market_data.Error.t ] Pipe.Reader.t Symbol.Map.t Deferred.t =
    Deferred.List.map ~how:`Parallel symbols ~f:(fun symbol ->
      Deferred.both (return symbol) (Book.pipe (module Cfg) ~symbol ()))
    >>| Symbol.Map.of_alist_exn

  (** Create pipes that raise on errors (convenience wrapper) *)
  let pipe_exn (module Cfg : Cfg.S) ?symbols () :
      Book.t Pipe.Reader.t Symbol.Map.t Deferred.t =
    pipe (module Cfg) ?symbols ()
    >>| Symbol.Map.map ~f:(fun pipe ->
      Pipe.map pipe ~f:(function
        | `Ok x -> x
        | #Market_data.Error.t as e ->
          failwiths "Error updating book" ~here:[%here] e Market_data.Error.sexp_of_t))
end

let command =
  let spec : (_, _) Command.Spec.t =
    let open Command.Spec in
    empty +> Cfg.param
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> flag "--max-depth" (optional_with_default 12 int)
         ~doc:"N maximum number of bid/ask levels to display (default: 12)"
    +> anon (maybe ("uri_args" %: sexp))
  in
  let set_loglevel = function
    | 2 ->
      Log.Global.set_level `Info;
      Logs.set_level @@ Some Logs.Info
    | e when e > 2 ->
      Log.Global.set_level `Debug;
      Logs.set_level @@ Some Logs.Debug
    | _ -> ()
  in
  let run cfg loglevel _max_depth symbol_opt () =
    let cfg = Cfg.or_default cfg in
    let module Cfg = (val cfg : Cfg.S) in
    Option.iter loglevel ~f:set_loglevel;
    let symbol =
      Option.first_some
        (Option.map ~f:Market_data.uri_args_of_sexp symbol_opt)
        Market_data.default_uri_args
      |> Option.value ~default:`Ethusd
    in
    Book.pipe (module Cfg) ~symbol ()
    >>= Pipe.iter ~f:(function
      | `Ok book ->
        (* Simple text output instead of TUI *)
        printf "=== %s Order Book (Epoch: %d) ===\n"
          (Book.symbol book) (Book.epoch book);
        printf "Best bid: %.2f | Best ask: %.2f | Spread: %.2f\n"
          (Price_level.price (Book.best_bid book))
          (Price_level.price (Book.best_ask book))
          (Book.spread book);
        printf "\n";
        Deferred.unit
      | #Market_data.Error.t as e ->
        failwiths ~here:[%here] "Market data error" e Market_data.Error.sexp_of_t)
  in
  ("orderbook",
   Command.async_spec
     ~summary:(sprintf "Gemini %s %s Orderbook Command" Market_data.version "orderbook")
     spec run)

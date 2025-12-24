(** Kraken Order Book - Implements unified Order_book_intf *)

open Core
open Async

(** Re-export types from the unified interface *)
module Price_level = Fluxum.Order_book_intf.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Price module for Float operations *)
module Price = struct
  include Float
end

(** Bid price with descending comparator (highest bid first) *)
module Bid_price = struct
  include Float

  include Comparator.Make (struct
    type t = float [@@deriving sexp, compare, equal]

    let compare p p' = compare p p' |> Int.neg
  end)
end

(** Ask price with ascending comparator (lowest ask first) *)
module Ask_price = struct
  include Price

  let of_price (price : Price.t) : t = price
end

module Bid_price_map = Map.Make_using_comparator (Bid_price)
module Ask_price_map = Map.Make (Ask_price)

(** Single symbol order book *)
module Book = struct
  type t =
    { symbol : Fluxum.Types.Symbol.t;
      bids : Price_level.t Bid_price_map.t;
      asks : Price_level.t Ask_price_map.t;
      epoch : int;
      update_time : Time_float_unix.t
    }
  [@@deriving fields, compare, equal, sexp]

  let empty ?timestamp ?(epoch = 0) symbol =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    { symbol;
      bids = Bid_price_map.empty;
      asks = Ask_price_map.empty;
      epoch;
      update_time
    }

  let set ?timestamp t ~(side : Bid_ask.t) ~price ~size =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let epoch = t.epoch + 1 in
    match Float.(equal zero size) with
    | true -> (
      match side with
      | `Bid -> { t with bids = Map.remove t.bids price; epoch; update_time }
      | `Ask -> { t with asks = Map.remove t.asks price; epoch; update_time } )
    | false -> (
      let data = Price_level.create ~price ~volume:size in
      match side with
      | `Bid ->
        { t with bids = Map.set t.bids ~key:price ~data; epoch; update_time }
      | `Ask ->
        { t with asks = Map.set t.asks ~key:price ~data; epoch; update_time } )

  let update ?timestamp (t : t) ~(side : Bid_ask.t) ~price ~size =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let size_ref = ref 0. in
    let maybe_remove orders =
      match Float.(equal zero !size_ref) with
      | true -> Map.remove orders price
      | false -> orders
    in
    match side with
    | `Bid ->
      { t with
        bids =
          Map.update t.bids price ~f:(function
            | None -> Price_level.create ~price ~volume:size
            | Some { price; volume = size' } ->
              size_ref := size +. size';
              let volume = !size_ref in
              Price_level.create ~price ~volume )
          |> maybe_remove;
        epoch = t.epoch + 1;
        update_time
      }
    | `Ask ->
      { t with
        asks =
          Map.update t.asks price ~f:(function
            | None -> Price_level.create ~price ~volume:size
            | Some { price; volume = size' } ->
              size_ref := size +. size';
              let volume = !size_ref in
              Price_level.create ~price ~volume )
          |> maybe_remove;
        epoch = t.epoch + 1;
        update_time
      }

  let add ?timestamp t ~side ~price ~size =
    update ?timestamp t ~side ~price ~size

  let remove ?timestamp t ~side ~price ~size =
    update ?timestamp t ~side ~price ~size:(Float.neg size)

  (** Get best bid *)
  let best_bid t =
    Map.max_elt t.bids
    |> Option.value_map ~default:Price_level.empty ~f:snd

  (** Get best ask *)
  let best_ask t =
    Map.min_elt t.asks
    |> Option.value_map ~default:Price_level.empty ~f:snd

  (** Get best for a given side *)
  let best ~side t =
    match side with
    | `Bid -> best_bid t
    | `Ask -> best_ask t

  (** Get top N bids *)
  let best_n_bids t ~n () =
    Map.to_alist t.bids
    |> (fun list -> List.take list n)
    |> List.map ~f:snd

  (** Get top N asks *)
  let best_n_asks t ~n () =
    Map.to_alist t.asks
    |> (fun list -> List.take list n)
    |> List.map ~f:snd

  (** Calculate market price for a given volume and side *)
  let market_price t ~side ~volume =
    let levels = match side with
      | Fluxum.Types.Side.Buy -> Map.to_alist t.asks  (* Buying means taking asks *)
      | Fluxum.Types.Side.Sell -> Map.to_alist t.bids  (* Selling means taking bids *)
    in
    let rec accumulate remaining_vol acc_cost levels =
      match levels with
      | [] -> (acc_cost, volume -. remaining_vol)  (* Ran out of liquidity *)
      | (_, level) :: rest ->
        let available = level.Price_level.volume in
        if Float.(remaining_vol <= available) then
          (* This level satisfies the remaining volume *)
          let cost = remaining_vol *. level.Price_level.price in
          (acc_cost +. cost, volume)
        else
          (* Take all of this level and continue *)
          let cost = available *. level.Price_level.price in
          accumulate (remaining_vol -. available) (acc_cost +. cost) rest
    in
    let total_cost, filled_volume = accumulate volume 0. levels in
    let avg_price = if Float.(filled_volume > 0.) then total_cost /. filled_volume else 0. in
    Price_level.create ~price:avg_price ~volume:filled_volume

  let bid_market_price t ~volume =
    market_price t ~side:Fluxum.Types.Side.Sell ~volume

  let ask_market_price t ~volume =
    market_price t ~side:Fluxum.Types.Side.Buy ~volume

  let mid_market_price t ~volume =
    let bid_price = (bid_market_price t ~volume).Price_level.price in
    let ask_price = (ask_market_price t ~volume).Price_level.price in
    let mid = (bid_price +. ask_price) /. 2. in
    Price_level.create ~price:mid ~volume

  (** Get total volume at a price level *)
  let total_volume_at_price_level t ~side ~price =
    match side with
    | `Bid ->
      Map.find t.bids price
      |> Option.value ~default:Price_level.empty
    | `Ask ->
      Map.find t.asks price
      |> Option.value ~default:Price_level.empty

  let total_bid_volume_at_price_level t ~price =
    total_volume_at_price_level t ~side:`Bid ~price

  let total_ask_volume_at_price_level t ~price =
    total_volume_at_price_level t ~side:`Ask ~price

  (** Convert notional (USD) to quantity using bid price *)
  let quantity_from_notional_bid t ~notional =
    let best = best_bid t in
    if Float.(best.price > 0.) then notional /. best.price else 0.

  (** Convert notional (USD) to quantity using ask price *)
  let quantity_from_notional_ask t ~notional =
    let best = best_ask t in
    if Float.(best.price > 0.) then notional /. best.price else 0.

  (** Accessors *)
  let symbol t = t.symbol
  let epoch t = t.epoch
  let update_time t = t.update_time

  (** TUI pretty print (from Gemini's implementation) *)
  let pretty_print ?(max_depth = 10) ?refresh_ms ?tick_size:_ t () =
    (* Use ANSI escape codes for colors *)
    let green = "\027[32m" in
    let red = "\027[31m" in
    let reset = "\027[0m" in
    let clear_screen = "\027[2J\027[H" in

    printf "%s" clear_screen;
    printf "=== %s Order Book (Epoch: %d) ===\n" t.symbol t.epoch;
    printf "Updated: %s\n\n" (Time_float_unix.to_string t.update_time);

    (* Print asks (highest to lowest for display) *)
    let asks = best_n_asks t ~n:max_depth () |> List.rev in
    printf "%sAsks (Sell Orders):%s\n" red reset;
    List.iter asks ~f:(fun level ->
      printf "  %.2f @ %.8f\n" level.Price_level.volume level.Price_level.price
    );

    (* Print spread *)
    let best_bid = best_bid t in
    let best_ask = best_ask t in
    let spread = best_ask.price -. best_bid.price in
    printf "\n--- Spread: %.2f ---\n\n" spread;

    (* Print bids *)
    let bids = best_n_bids t ~n:max_depth () in
    printf "%sBids (Buy Orders):%s\n" green reset;
    List.iter bids ~f:(fun level ->
      printf "  %.2f @ %.8f\n" level.Price_level.volume level.Price_level.price
    );

    printf "\n";
    Out_channel.flush stdout;

    (* Handle refresh if specified *)
    match refresh_ms with
    | None -> ()
    | Some ms ->
      let _ = after (Time_float_unix.Span.of_ms ms) in
      ()
end

(** Multi-symbol order book manager *)
module Books = struct
  type t = Book.t Fluxum.Types.Symbol.Map.t [@@deriving sexp, equal, compare]
  type book = Book.t

  let empty = Fluxum.Types.Symbol.Map.empty

  let symbols t = Map.keys t

  let book t symbol = Map.find t symbol

  let book_exn t symbol =
    Map.find_exn t symbol

  let set_book ?timestamp:_ t (book : book) =
    Map.set t ~key:(Book.symbol book) ~data:book

  let add ?timestamp t ~symbol ~side ~price ~size =
    let book = Map.find t symbol |> Option.value ~default:(Book.empty ?timestamp symbol) in
    let updated_book = Book.add ?timestamp book ~side ~price ~size in
    Map.set t ~key:symbol ~data:updated_book

  let update ?timestamp t ~symbol ~side ~price ~size =
    let book = Map.find t symbol |> Option.value ~default:(Book.empty ?timestamp symbol) in
    let updated_book = Book.update ?timestamp book ~side ~price ~size in
    Map.set t ~key:symbol ~data:updated_book

  let remove ?timestamp t ~symbol ~side ~price ~size =
    let book = Map.find t symbol |> Option.value ~default:(Book.empty ?timestamp symbol) in
    let updated_book = Book.remove ?timestamp book ~side ~price ~size in
    Map.set t ~key:symbol ~data:updated_book

  let set ?timestamp t ~symbol ~side ~price ~size =
    let book = Map.find t symbol |> Option.value ~default:(Book.empty ?timestamp symbol) in
    let updated_book = Book.set ?timestamp book ~side ~price ~size in
    Map.set t ~key:symbol ~data:updated_book
end

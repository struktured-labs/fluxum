(** Consolidated Order Book - Merges order books from multiple exchanges *)

open Core
open Async

module Price_level = Fluxum.Order_book_intf.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Exchange source for a price level *)
type exchange_source =
  | Gemini
  | Kraken
  | Multiple of exchange_source list
  [@@deriving sexp, compare, equal]

(** Price level with exchange attribution *)
module Attributed_level = struct
  type t = {
    price : float;
    volume : float;
    exchange : exchange_source;
  }
  [@@deriving sexp, compare, equal, fields]

  let create ~price ~volume ~exchange = { price; volume; exchange }
  let empty = create ~price:0. ~volume:0. ~exchange:(Multiple [])
end

(** Bid price with descending comparator *)
module Bid_price = struct
  include Float

  include Comparator.Make (struct
    type t = float [@@deriving sexp, compare, equal]
    let compare p p' = Float.compare p p' |> Int.neg
  end)
end

(** Ask price with ascending comparator *)
module Ask_price = struct
  include Float
end

module Bid_price_map = Map.Make_using_comparator (Bid_price)
module Ask_price_map = Map.Make (Ask_price)

(** Consolidated order book for a single symbol *)
module Book = struct
  type t = {
    symbol : string;
    bids : Attributed_level.t Bid_price_map.t;
    asks : Attributed_level.t Ask_price_map.t;
    gemini_book : Gemini.Order_book.Book.t option;
    kraken_book : Kraken.Order_book.Book.t option;
    epoch : int;
    update_time : Time_float_unix.t;
  }
  [@@deriving fields, sexp]

  (** Create empty consolidated book *)
  let empty symbol = {
    symbol;
    bids = Bid_price_map.empty;
    asks = Ask_price_map.empty;
    gemini_book = None;
    kraken_book = None;
    epoch = 0;
    update_time = Time_float_unix.now ();
  }

  (** Merge price levels from multiple exchanges at the same price *)
  let merge_levels (levels : Attributed_level.t list) : Attributed_level.t =
    match levels with
    | [] -> Attributed_level.empty
    | [single] -> single
    | multiple ->
      let total_volume = List.fold multiple ~init:0. ~f:(fun acc level ->
        acc +. level.Attributed_level.volume) in
      let exchanges = List.map multiple ~f:(fun l -> l.Attributed_level.exchange) in
      let price = (List.hd_exn multiple).Attributed_level.price in
      { Attributed_level.price; volume = total_volume; exchange = Multiple exchanges }

  (** Rebuild consolidated book from exchange books *)
  let rebuild t =
    let update_time = Time_float_unix.now () in
    let epoch = t.epoch + 1 in

    (* Collect all bid levels with exchange attribution *)
    let all_bids =
      let gemini_bids = match t.gemini_book with
        | None -> []
        | Some book ->
          Gemini.Order_book.Book.best_n_bids book ~n:100 ()
          |> List.map ~f:(fun level ->
            (level.Gemini.Order_book.Price_level.price,
             { Attributed_level.
               price = level.price;
               volume = level.volume;
               exchange = Gemini
             }))
      in
      let kraken_bids = match t.kraken_book with
        | None -> []
        | Some book ->
          Kraken.Order_book.Book.best_n_bids book ~n:100 ()
          |> List.map ~f:(fun level ->
            (level.Fluxum.Order_book_intf.Price_level.price,
             { Attributed_level.
               price = level.price;
               volume = level.volume;
               exchange = Kraken
             }))
      in
      gemini_bids @ kraken_bids
      |> List.sort ~compare:(fun (p1, _) (p2, _) -> Float.compare p2 p1) (* Descending *)
      |> List.group ~break:(fun (p1, _) (p2, _) -> Float.(p1 <> p2))
      |> List.map ~f:(fun group ->
        let price = fst (List.hd_exn group) in
        let levels = List.map group ~f:snd in
        (price, merge_levels levels))
      |> Bid_price_map.of_alist_exn
    in

    (* Collect all ask levels with exchange attribution *)
    let all_asks =
      let gemini_asks = match t.gemini_book with
        | None -> []
        | Some book ->
          Gemini.Order_book.Book.best_n_asks book ~n:100 ()
          |> List.map ~f:(fun level ->
            (level.Gemini.Order_book.Price_level.price,
             { Attributed_level.
               price = level.price;
               volume = level.volume;
               exchange = Gemini
             }))
      in
      let kraken_asks = match t.kraken_book with
        | None -> []
        | Some book ->
          Kraken.Order_book.Book.best_n_asks book ~n:100 ()
          |> List.map ~f:(fun level ->
            (level.Fluxum.Order_book_intf.Price_level.price,
             { Attributed_level.
               price = level.price;
               volume = level.volume;
               exchange = Kraken
             }))
      in
      gemini_asks @ kraken_asks
      |> List.sort ~compare:(fun (p1, _) (p2, _) -> Float.compare p1 p2) (* Ascending *)
      |> List.group ~break:(fun (p1, _) (p2, _) -> Float.(p1 <> p2))
      |> List.map ~f:(fun group ->
        let price = fst (List.hd_exn group) in
        let levels = List.map group ~f:snd in
        (price, merge_levels levels))
      |> Ask_price_map.of_alist_exn
    in

    { t with bids = all_bids; asks = all_asks; epoch; update_time }

  (** Update Gemini book *)
  let update_gemini t gemini_book =
    let t = { t with gemini_book = Some gemini_book } in
    rebuild t

  (** Update Kraken book *)
  let update_kraken t kraken_book =
    let t = { t with kraken_book = Some kraken_book } in
    rebuild t

  (** Get best bid *)
  let best_bid t =
    Map.min_elt t.bids
    |> Option.value_map ~default:Attributed_level.empty ~f:snd

  (** Get best ask *)
  let best_ask t =
    Map.min_elt t.asks
    |> Option.value_map ~default:Attributed_level.empty ~f:snd

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

  (** Format exchange source for display *)
  let rec exchange_to_string = function
    | Gemini -> "GEM"
    | Kraken -> "KRK"
    | Multiple sources ->
      List.map sources ~f:exchange_to_string
      |> String.concat ~sep:"+"

  (** Pretty print consolidated order book *)
  let pretty_print ?(max_depth = 10) t () =
    let green = "\027[32m" in
    let red = "\027[31m" in
    let blue = "\027[34m" in
    let reset = "\027[0m" in
    let clear_screen = "\027[2J\027[H" in

    printf "%s" clear_screen;
    printf "=== %s Consolidated Order Book (Epoch: %d) ===\n" t.symbol t.epoch;
    printf "Updated: %s\n" (Time_float_unix.to_string t.update_time);
    printf "Sources: ";
    if Option.is_some t.gemini_book then printf "[Gemini] ";
    if Option.is_some t.kraken_book then printf "[Kraken] ";
    printf "\n\n";

    (* Print asks (highest to lowest for display) *)
    let asks = best_n_asks t ~n:max_depth () |> List.rev in
    printf "%sAsks (Sell Orders):%s\n" red reset;
    List.iter asks ~f:(fun level ->
      printf "  %s%-4s%s  %.8f @ %.2f\n"
        blue
        (exchange_to_string level.Attributed_level.exchange)
        reset
        level.volume
        level.price
    );

    (* Print spread *)
    let best_bid = best_bid t in
    let best_ask = best_ask t in
    let spread = best_ask.price -. best_bid.price in
    let spread_pct = if Float.(best_bid.price > 0.) then
      100. *. spread /. best_bid.price
    else 0. in
    printf "\n--- Spread: $%.2f (%.4f%%) ---\n\n" spread spread_pct;

    (* Print bids *)
    let bids = best_n_bids t ~n:max_depth () in
    printf "%sBids (Buy Orders):%s\n" green reset;
    List.iter bids ~f:(fun level ->
      printf "  %s%-4s%s  %.8f @ %.2f\n"
        blue
        (exchange_to_string level.Attributed_level.exchange)
        reset
        level.volume
        level.price
    );

    printf "\n";
    Out_channel.flush stdout
end

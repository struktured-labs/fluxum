(** Consolidated Order Book - Merges order books from multiple exchanges

    Enhanced with:
    - Smart aggregation (best prices across exchanges)
    - Arbitrage opportunity detection
    - Volume-weighted price calculations
    - Depth analysis and liquidity metrics
*)

open Core
open Async

module Price_level = Fluxum.Order_book_intf.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Exchange source for a price level *)
type exchange_source =
  | Gemini
  | Kraken
  | Hyperliquid
  | Bitrue
  | Binance
  | Coinbase
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
    hyperliquid_book : Hyperliquid.Order_book.Book.t option;
    bitrue_book : Bitrue.Order_book.Book.t option;
    binance_book : Binance.Order_book.Book.t option;
    coinbase_book : Coinbase.Order_book.Book.t option;
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
    hyperliquid_book = None;
    bitrue_book = None;
    binance_book = None;
    coinbase_book = None;
    epoch = 0;
    update_time = Time_float_unix.now ();
  }

  (** Merge price levels from multiple exchanges at the same price *)
  let merge_levels (levels : Attributed_level.t list) : Attributed_level.t =
    match levels with
    | [] -> Attributed_level.empty
    | [single] -> single
    | first :: _ as multiple ->
      (* Safe to use first: pattern match ensures non-empty *)
      let total_volume = List.fold multiple ~init:0. ~f:(fun acc level ->
        acc +. level.Attributed_level.volume) in
      let exchanges = List.map multiple ~f:(fun l -> l.Attributed_level.exchange) in
      let price = first.Attributed_level.price in
      { Attributed_level.price; volume = total_volume; exchange = Multiple exchanges }

  (** Generic level extraction from any exchange book

      This eliminates 12 duplicated blocks (6 exchanges × 2 sides).

      @param book_opt Optional exchange book
      @param extract_levels Function to get levels from book
      @param exchange Exchange source identifier
      @return List of (price, attributed_level) tuples
  *)
  let extract_levels_generic
    (type book level)
    ~(book_opt : book option)
    ~(extract_levels : book -> level list)
    ~(get_price : level -> float)
    ~(get_volume : level -> float)
    ~(exchange : exchange_source)
    : (float * Attributed_level.t) list
    =
    match book_opt with
    | None -> []
    | Some book ->
      extract_levels book
      |> List.map ~f:(fun level ->
        let price = get_price level in
        let volume = get_volume level in
        (price, { Attributed_level.price; volume; exchange }))

  (** Rebuild consolidated book from exchange books *)
  let rebuild t =
    let update_time = Time_float_unix.now () in
    let epoch = t.epoch + 1 in

    (* Collect all bid levels with exchange attribution *)
    let all_bids =
      let gemini_bids = extract_levels_generic
        ~book_opt:t.gemini_book
        ~extract_levels:(fun book -> Gemini.Order_book.Book.best_n_bids book ~n:100 ())
        ~get_price:(fun (level : Gemini.Order_book.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Gemini
      in
      let kraken_bids = extract_levels_generic
        ~book_opt:t.kraken_book
        ~extract_levels:(fun book -> Kraken.Order_book.Book.best_n_bids book ~n:100 ())
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Kraken
      in
      let hyperliquid_bids = extract_levels_generic
        ~book_opt:t.hyperliquid_book
        ~extract_levels:(fun book -> Hyperliquid.Order_book.Book.bids_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Hyperliquid
      in
      let bitrue_bids = extract_levels_generic
        ~book_opt:t.bitrue_book
        ~extract_levels:(fun book -> Bitrue.Order_book.Book.bids_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Bitrue
      in
      let binance_bids = extract_levels_generic
        ~book_opt:t.binance_book
        ~extract_levels:(fun book -> Binance.Order_book.Book.bids_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Binance
      in
      let coinbase_bids = extract_levels_generic
        ~book_opt:t.coinbase_book
        ~extract_levels:(fun book -> Coinbase.Order_book.Book.bids_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Coinbase
      in
      gemini_bids @ kraken_bids @ hyperliquid_bids @ bitrue_bids @ binance_bids @ coinbase_bids
      |> List.sort ~compare:(fun (p1, _) (p2, _) -> Float.compare p2 p1) (* Descending *)
      |> List.group ~break:(fun (p1, _) (p2, _) -> Float.(p1 <> p2))
      |> List.map ~f:(fun group ->
        (* Safe: List.group guarantees non-empty groups *)
        let price = fst (List.hd_exn group) in
        let levels = List.map group ~f:snd in
        (price, merge_levels levels))
      |> Bid_price_map.of_alist_exn
    in

    (* Collect all ask levels with exchange attribution *)
    let all_asks =
      let gemini_asks = extract_levels_generic
        ~book_opt:t.gemini_book
        ~extract_levels:(fun book -> Gemini.Order_book.Book.best_n_asks book ~n:100 ())
        ~get_price:(fun (level : Gemini.Order_book.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Gemini
      in
      let kraken_asks = extract_levels_generic
        ~book_opt:t.kraken_book
        ~extract_levels:(fun book -> Kraken.Order_book.Book.best_n_asks book ~n:100 ())
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Kraken
      in
      let hyperliquid_asks = extract_levels_generic
        ~book_opt:t.hyperliquid_book
        ~extract_levels:(fun book -> Hyperliquid.Order_book.Book.asks_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Hyperliquid
      in
      let bitrue_asks = extract_levels_generic
        ~book_opt:t.bitrue_book
        ~extract_levels:(fun book -> Bitrue.Order_book.Book.asks_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Bitrue
      in
      let binance_asks = extract_levels_generic
        ~book_opt:t.binance_book
        ~extract_levels:(fun book -> Binance.Order_book.Book.asks_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Binance
      in
      let coinbase_asks = extract_levels_generic
        ~book_opt:t.coinbase_book
        ~extract_levels:(fun book -> Coinbase.Order_book.Book.asks_alist book |> (fun l -> List.take l 100) |> List.map ~f:snd)
        ~get_price:(fun (level : Exchange_common.Order_book_base.Price_level.t) -> level.price)
        ~get_volume:(fun level -> level.volume)
        ~exchange:Coinbase
      in
      gemini_asks @ kraken_asks @ hyperliquid_asks @ bitrue_asks @ binance_asks @ coinbase_asks
      |> List.sort ~compare:(fun (p1, _) (p2, _) -> Float.compare p1 p2) (* Ascending *)
      |> List.group ~break:(fun (p1, _) (p2, _) -> Float.(p1 <> p2))
      |> List.map ~f:(fun group ->
        (* Safe: List.group guarantees non-empty groups *)
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

  (** Update Hyperliquid book *)
  let update_hyperliquid t hyperliquid_book =
    let t = { t with hyperliquid_book = Some hyperliquid_book } in
    rebuild t

  (** Update Bitrue book *)
  let update_bitrue t bitrue_book =
    let t = { t with bitrue_book = Some bitrue_book } in
    rebuild t

  (** Update Binance book *)
  let update_binance t binance_book =
    let t = { t with binance_book = Some binance_book } in
    rebuild t

  (** Update Coinbase book *)
  let update_coinbase t coinbase_book =
    let t = { t with coinbase_book = Some coinbase_book } in
    rebuild t

  (** Get best bid *)
  let best_bid t =
    Map.min_elt t.bids
    |> Option.value_map ~default:Attributed_level.empty ~f:snd

  (** Get best ask *)
  let best_ask t =
    Map.min_elt t.asks
    |> Option.value_map ~default:Attributed_level.empty ~f:snd

  (** Get spread between best bid and ask *)
  let spread t =
    let best_bid = best_bid t in
    let best_ask = best_ask t in
    match Float.(best_bid.price > 0. && best_ask.price > 0.) with
    | true -> Some (best_ask.price -. best_bid.price)
    | false -> None

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
    | Hyperliquid -> "HYP"
    | Bitrue -> "BTR"
    | Binance -> "BIN"
    | Coinbase -> "CBP"
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
    (match Option.is_some t.gemini_book with true -> printf "[Gemini] " | false -> ());
    (match Option.is_some t.kraken_book with true -> printf "[Kraken] " | false -> ());
    (match Option.is_some t.hyperliquid_book with true -> printf "[Hyperliquid] " | false -> ());
    (match Option.is_some t.bitrue_book with true -> printf "[Bitrue] " | false -> ());
    (match Option.is_some t.binance_book with true -> printf "[Binance] " | false -> ());
    (match Option.is_some t.coinbase_book with true -> printf "[Coinbase] " | false -> ());
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
    let spread_pct =
      match Float.(best_bid.price > 0.) with
      | true -> 100. *. spread /. best_bid.price
      | false -> 0.
    in
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

  (** Calculate volume-weighted average price for bids *)
  let vwap_bid t ~volume =
    let rec accumulate remaining acc_cost levels =
      match levels with
      | [] ->
        (match Float.(remaining > 0.) with
         | true -> None
         | false -> Some (acc_cost /. volume))
      | level :: rest ->
        (match Float.(remaining <= level.Attributed_level.volume) with
         | true ->
           let cost = remaining *. level.price in
           Some ((acc_cost +. cost) /. volume)
         | false ->
           let cost = level.volume *. level.price in
           accumulate (remaining -. level.volume) (acc_cost +. cost) rest)
    in
    let levels = best_n_bids t ~n:100 () in
    accumulate volume 0. levels

  (** Calculate volume-weighted average price for asks *)
  let vwap_ask t ~volume =
    let rec accumulate remaining acc_cost levels =
      match levels with
      | [] ->
        (match Float.(remaining > 0.) with
         | true -> None
         | false -> Some (acc_cost /. volume))
      | level :: rest ->
        (match Float.(remaining <= level.Attributed_level.volume) with
         | true ->
           let cost = remaining *. level.price in
           Some ((acc_cost +. cost) /. volume)
         | false ->
           let cost = level.volume *. level.price in
           accumulate (remaining -. level.volume) (acc_cost +. cost) rest)
    in
    let levels = best_n_asks t ~n:100 () in
    accumulate volume 0. levels

  (** Get total liquidity available within percentage of best price *)
  let liquidity_depth t ~side ~percentage =
    match side with
    | `Bid ->
      let best = best_bid t in
      (match Float.(best.price = 0.) with
       | true -> 0.
       | false ->
         let min_price = best.price *. (1. -. percentage /. 100.) in
         Map.fold t.bids ~init:0. ~f:(fun ~key:price ~data:level acc ->
           match Float.(price >= min_price) with
           | true -> acc +. level.volume
           | false -> acc))
    | `Ask ->
      let best = best_ask t in
      (match Float.(best.price = 0.) with
       | true -> 0.
       | false ->
         let max_price = best.price *. (1. +. percentage /. 100.) in
         Map.fold t.asks ~init:0. ~f:(fun ~key:price ~data:level acc ->
           match Float.(price <= max_price) with
           | true -> acc +. level.volume
           | false -> acc))
end

(** Arbitrage opportunity *)
module Arbitrage = struct
  type opportunity = {
    buy_exchange: exchange_source;
    sell_exchange: exchange_source;
    buy_price: float;
    sell_price: float;
    profit_per_unit: float;
    profit_percentage: float;
    max_volume: float;  (** Maximum profitable volume *)
  } [@@deriving sexp, fields]

  (** Detect arbitrage opportunities across exchanges *)
  let detect_opportunities t =
    let opportunities = ref [] in

    (* Get best ask from each exchange *)
    let exchange_asks = [
      (Gemini, Option.map t.Book.gemini_book ~f:(fun b ->
        let best = Gemini.Order_book.Book.best_ask b in
        (best.price, best.volume)));
      (Kraken, Option.map t.Book.kraken_book ~f:(fun b ->
        let best = Kraken.Order_book.Book.best_ask b in
        (best.price, best.volume)));
      (Hyperliquid, Option.map t.Book.hyperliquid_book ~f:(fun b ->
        let best = Hyperliquid.Order_book.Book.best_ask b in
        (best.price, best.volume)));
      (Bitrue, Option.map t.Book.bitrue_book ~f:(fun b ->
        let best = Bitrue.Order_book.Book.best_ask b in
        (best.price, best.volume)));
      (Binance, Option.map t.Book.binance_book ~f:(fun b ->
        let best = Binance.Order_book.Book.best_ask b in
        (best.price, best.volume)));
      (Coinbase, Option.map t.Book.coinbase_book ~f:(fun b ->
        let best = Coinbase.Order_book.Book.best_ask b in
        (best.price, best.volume)));
    ] |> List.filter_map ~f:(fun (ex, opt) ->
      Option.map opt ~f:(fun (price, vol) -> (ex, price, vol)))
    in

    (* Get best bid from each exchange *)
    let exchange_bids = [
      (Gemini, Option.map t.Book.gemini_book ~f:(fun b ->
        let best = Gemini.Order_book.Book.best_bid b in
        (best.price, best.volume)));
      (Kraken, Option.map t.Book.kraken_book ~f:(fun b ->
        let best = Kraken.Order_book.Book.best_bid b in
        (best.price, best.volume)));
      (Hyperliquid, Option.map t.Book.hyperliquid_book ~f:(fun b ->
        let best = Hyperliquid.Order_book.Book.best_bid b in
        (best.price, best.volume)));
      (Bitrue, Option.map t.Book.bitrue_book ~f:(fun b ->
        let best = Bitrue.Order_book.Book.best_bid b in
        (best.price, best.volume)));
      (Binance, Option.map t.Book.binance_book ~f:(fun b ->
        let best = Binance.Order_book.Book.best_bid b in
        (best.price, best.volume)));
      (Coinbase, Option.map t.Book.coinbase_book ~f:(fun b ->
        let best = Coinbase.Order_book.Book.best_bid b in
        (best.price, best.volume)));
    ] |> List.filter_map ~f:(fun (ex, opt) ->
      Option.map opt ~f:(fun (price, vol) -> (ex, price, vol)))
    in

    (* Find opportunities: buy low (ask) on one exchange, sell high (bid) on another *)
    List.iter exchange_asks ~f:(fun (buy_ex, buy_price, buy_vol) ->
      List.iter exchange_bids ~f:(fun (sell_ex, sell_price, sell_vol) ->
        match not (equal_exchange_source buy_ex sell_ex) && Float.(sell_price > buy_price) with
        | false -> ()
        | true ->
          let profit_per_unit = sell_price -. buy_price in
          let profit_percentage = 100. *. profit_per_unit /. buy_price in
          let max_volume = Float.min buy_vol sell_vol in
          opportunities := {
            buy_exchange = buy_ex;
            sell_exchange = sell_ex;
            buy_price;
            sell_price;
            profit_per_unit;
            profit_percentage;
            max_volume;
          } :: !opportunities
      )
    );

    (* Sort by profit percentage descending *)
    List.sort !opportunities ~compare:(fun a b ->
      Float.compare b.profit_percentage a.profit_percentage)

  (** Print arbitrage opportunities *)
  let print_opportunities opportunities =
    match List.is_empty opportunities with
    | true -> printf "\nNo arbitrage opportunities detected.\n\n"
    | false ->
      printf "\n╔════════════════════════════════════════════════════════╗\n";
      printf "║  Arbitrage Opportunities                               ║\n";
      printf "╠════════════════════════════════════════════════════════╣\n";
      List.iteri opportunities ~f:(fun i opp ->
        match i < 3 with  (* Show top 3 *)
        | false -> ()
        | true ->
          printf "║  #%d: Buy %s @ $%.2f → Sell %s @ $%.2f\n"
            (i + 1)
            (Book.exchange_to_string opp.buy_exchange)
            opp.buy_price
            (Book.exchange_to_string opp.sell_exchange)
            opp.sell_price;
          printf "║      Profit: $%.2f/unit (%.3f%%), Max vol: %.4f       \n"
            opp.profit_per_unit
            opp.profit_percentage
            opp.max_volume
      );
      printf "╚════════════════════════════════════════════════════════╝\n\n"
end

(** Analytics and metrics *)
module Analytics = struct
  type metrics = {
    total_bid_volume: float;
    total_ask_volume: float;
    weighted_mid_price: float;
    bid_depth_1pct: float;  (** Liquidity within 1% of best bid *)
    ask_depth_1pct: float;  (** Liquidity within 1% of best ask *)
    spread_bps: float;      (** Spread in basis points *)
    num_exchanges: int;
    best_bid_exchange: exchange_source;
    best_ask_exchange: exchange_source;
  } [@@deriving sexp, fields]

  let calculate t =
    let bids = Book.best_n_bids t ~n:100 () in
    let asks = Book.best_n_asks t ~n:100 () in
    let best_bid = Book.best_bid t in
    let best_ask = Book.best_ask t in

    let total_bid_volume = List.fold bids ~init:0. ~f:(fun acc l -> acc +. l.Attributed_level.volume) in
    let total_ask_volume = List.fold asks ~init:0. ~f:(fun acc l -> acc +. l.Attributed_level.volume) in

    let weighted_mid_price =
      match Float.(best_bid.price > 0. && best_ask.price > 0.) with
      | true -> (best_bid.price +. best_ask.price) /. 2.
      | false -> 0.
    in

    let spread = best_ask.price -. best_bid.price in
    let spread_bps =
      match Float.(best_bid.price > 0.) with
      | true -> 10000. *. spread /. best_bid.price
      | false -> 0.
    in

    let bid_depth_1pct = Book.liquidity_depth t ~side:`Bid ~percentage:1. in
    let ask_depth_1pct = Book.liquidity_depth t ~side:`Ask ~percentage:1. in

    let num_exchanges =
      (match Option.is_some t.Book.gemini_book with true -> 1 | false -> 0) +
      (match Option.is_some t.kraken_book with true -> 1 | false -> 0) +
      (match Option.is_some t.hyperliquid_book with true -> 1 | false -> 0) +
      (match Option.is_some t.bitrue_book with true -> 1 | false -> 0) +
      (match Option.is_some t.binance_book with true -> 1 | false -> 0) +
      (match Option.is_some t.coinbase_book with true -> 1 | false -> 0)
    in

    {
      total_bid_volume;
      total_ask_volume;
      weighted_mid_price;
      bid_depth_1pct;
      ask_depth_1pct;
      spread_bps;
      num_exchanges;
      best_bid_exchange = best_bid.exchange;
      best_ask_exchange = best_ask.exchange;
    }

  (** Print analytics *)
  let print metrics =
    printf "╔════════════════════════════════════════════════════════╗\n";
    printf "║  Market Analytics                                      ║\n";
    printf "╠════════════════════════════════════════════════════════╣\n";
    printf "║  Active Exchanges: %d                                   ║\n" metrics.num_exchanges;
    printf "║  Weighted Mid Price: $%.2f                             \n" metrics.weighted_mid_price;
    printf "║  Spread: %.2f bps                                      \n" metrics.spread_bps;
    printf "║  Best Bid: %s                                          \n"
      (Book.exchange_to_string metrics.best_bid_exchange);
    printf "║  Best Ask: %s                                          \n"
      (Book.exchange_to_string metrics.best_ask_exchange);
    printf "║  Total Bid Volume: %.4f                               \n" metrics.total_bid_volume;
    printf "║  Total Ask Volume: %.4f                               \n" metrics.total_ask_volume;
    printf "║  Bid Depth (1%%): %.4f                                  \n" metrics.bid_depth_1pct;
    printf "║  Ask Depth (1%%): %.4f                                  \n" metrics.ask_depth_1pct;
    printf "╚════════════════════════════════════════════════════════╝\n"
end

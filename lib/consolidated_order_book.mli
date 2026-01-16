(** Consolidated Multi-Exchange Order Book

    This module merges order books from multiple exchanges into a single
    unified view, enabling cross-exchange liquidity analysis and arbitrage
    opportunity detection.

    {b Key Features:}
    - Smart aggregation: Best prices across all exchanges
    - Exchange attribution: Track which exchange provides each level
    - Price level merging: Combine liquidity at same price
    - Arbitrage detection: Find cross-exchange profit opportunities
    - Depth analysis: Measure liquidity within % of best price
    - VWAP calculations: Volume-weighted average prices
    - TUI rendering: Colored order book display

    {b Architecture:}
    - Individual exchange books stored separately (Gemini, Kraken, etc.)
    - Consolidated bid/ask maps with exchange attribution
    - Automatic rebuild on any exchange update
    - Sorted price levels (bids descending, asks ascending)

    {b Use Cases:}
    - Arbitrage trading (buy low on one exchange, sell high on another)
    - Liquidity aggregation (see total available liquidity)
    - Best execution (route orders to exchange with best price)
    - Market microstructure analysis
    - Multi-exchange market making

    {b Example:}
    {[
      (* Create empty consolidated book *)
      let book = Book.empty "BTCUSD" in

      (* Update with Gemini order book *)
      let%bind gemini_book = Gemini.Order_book.get_snapshot ~symbol:"btcusd" in
      let book = Book.update_gemini book gemini_book in

      (* Update with Kraken order book *)
      let%bind kraken_book = Kraken.Order_book.get_snapshot ~symbol:"XBTUSD" in
      let book = Book.update_kraken book kraken_book in

      (* Get best prices across all exchanges *)
      let best_bid = Book.best_bid book in
      let best_ask = Book.best_ask book in
      printf "Best bid: %.2f (%s)\n"
        best_bid.price
        (Book.exchange_to_string best_bid.exchange);

      (* Detect arbitrage opportunities *)
      let opportunities = Arbitrage.detect_opportunities book in
      Arbitrage.print_opportunities opportunities;

      (* Calculate analytics *)
      let metrics = Analytics.calculate book in
      Analytics.print metrics;

      (* Pretty print consolidated book *)
      Book.pretty_print ~max_depth:10 book ()
    ]}

    @see <lib/order_book_intf.ml> for single-exchange order book interface
*)

(** {1 Exchange Attribution} *)

type exchange_source =
  | Gemini
  | Kraken
  | Hyperliquid
  | Bitrue
  | Binance
  | Coinbase
  | Multiple of exchange_source list
      (** Multiple exchanges at the same price (merged) *)
[@@deriving sexp, compare, equal]
(** Which exchange(s) provide a price level

    Used to track liquidity sources and enable cross-exchange arbitrage.

    {b Multiple variant:} When multiple exchanges have liquidity at the exact
    same price, levels are merged and attributed to Multiple [exchanges].
*)

(** {1 Price Levels with Attribution} *)

module Attributed_level : sig
  (** Price level with exchange source information

      Like Order_book_intf.Price_level but includes exchange attribution
      for cross-exchange analysis.
  *)

  type t = {
    price : float;
    (** Price at this level *)

    volume : float;
    (** Total volume available (sum if Multiple exchanges) *)

    exchange : exchange_source;
    (** Which exchange(s) provide this liquidity *)
  }
  [@@deriving sexp, compare, equal, fields]

  val create : price:float -> volume:float -> exchange:exchange_source -> t
  (** Create attributed level *)

  val empty : t
  (** Empty level: price=0, volume=0, exchange=Multiple [] *)
end

(** {1 Custom Price Comparators} *)

module Bid_price : sig
  (** Bid price with descending comparator

      Bids are sorted high to low (best bid = highest price).
      This module provides a custom comparator for Map operations.
  *)

  include module type of Float

  include Comparator.S with type t := float
  (** Comparator that reverses normal float ordering
      (compare p1 p2 returns negative of normal comparison) *)
end

module Ask_price : sig
  (** Ask price with ascending comparator

      Asks are sorted low to high (best ask = lowest price).
      Uses standard float comparator.
  *)

  include module type of Float
end

module Bid_price_map : module type of Map.Make_using_comparator(Bid_price)
(** Map of bid prices to attributed levels (sorted descending) *)

module Ask_price_map : module type of Map.Make(Ask_price)
(** Map of ask prices to attributed levels (sorted ascending) *)

(** {1 Consolidated Order Book} *)

module Book : sig
  (** Consolidated order book for a single symbol across multiple exchanges

      Maintains both:
      - Original exchange-specific books (gemini_book, kraken_book, etc.)
      - Unified bid/ask maps with exchange attribution

      {b Update Strategy:}
      When any exchange book is updated:
      1. Store new exchange-specific book
      2. Rebuild consolidated bids/asks from all sources
      3. Merge levels at same price across exchanges
      4. Increment epoch counter

      {b Performance:}
      - Rebuild is O(n log n) where n = total levels across all exchanges
      - Typically rebuilds in <1ms for standard depth (100 levels × 6 exchanges)
      - Use epoch counter to detect updates
  *)

  type t = {
    symbol : string;
    (** Trading symbol (exchange-agnostic, e.g., "BTCUSD") *)

    bids : Attributed_level.t Bid_price_map.t;
    (** Consolidated bids (sorted descending by price) *)

    asks : Attributed_level.t Ask_price_map.t;
    (** Consolidated asks (sorted ascending by price) *)

    gemini_book : Gemini.Order_book.Book.t option;
    (** Gemini-specific order book *)

    kraken_book : Kraken.Order_book.Book.t option;
    (** Kraken-specific order book *)

    hyperliquid_book : Hyperliquid.Order_book.Book.t option;
    (** Hyperliquid-specific order book *)

    bitrue_book : Bitrue.Order_book.Book.t option;
    (** Bitrue-specific order book *)

    binance_book : Binance.Order_book.Book.t option;
    (** Binance-specific order book *)

    coinbase_book : Coinbase.Order_book.Book.t option;
    (** Coinbase-specific order book *)

    epoch : int;
    (** Update counter (increments on every rebuild) *)

    update_time : Time_float_unix.t;
    (** Last rebuild timestamp *)
  }
  [@@deriving fields, sexp]

  (** {2 Creation and Updates} *)

  val empty : string -> t
  (** Create empty consolidated book for symbol

      All exchange books are None, epoch=0, maps are empty.
  *)

  val update_gemini : t -> Gemini.Order_book.Book.t -> t
  (** Update Gemini book and rebuild consolidated view *)

  val update_kraken : t -> Kraken.Order_book.Book.t -> t
  (** Update Kraken book and rebuild *)

  val update_hyperliquid : t -> Hyperliquid.Order_book.Book.t -> t
  (** Update Hyperliquid book and rebuild *)

  val update_bitrue : t -> Bitrue.Order_book.Book.t -> t
  (** Update Bitrue book and rebuild *)

  val update_binance : t -> Binance.Order_book.Book.t -> t
  (** Update Binance book and rebuild *)

  val update_coinbase : t -> Coinbase.Order_book.Book.t -> t
  (** Update Coinbase book and rebuild *)

  (** {2 Best Prices} *)

  val best_bid : t -> Attributed_level.t
  (** Get best bid across all exchanges

      Returns highest bid price with exchange attribution.
      Returns empty level if no bids available.

      {b Example:}
      {[
        let best = Book.best_bid book in
        match Float.(best.price > 0.) with
        | true ->
          printf "Best bid: %.2f from %s\n"
            best.price
            (exchange_to_string best.exchange)
        | false ->
          printf "No bids available\n"
      ]}
  *)

  val best_ask : t -> Attributed_level.t
  (** Get best ask across all exchanges

      Returns lowest ask price with exchange attribution.
  *)

  val spread : t -> float option
  (** Get spread between best bid and ask

      @return Some spread if both bid and ask exist
      @return None if either is missing

      {b Example:}
      {[
        match Book.spread book with
        | Some s -> printf "Spread: $%.2f\n" s
        | None -> printf "No spread (missing bid or ask)\n"
      ]}
  *)

  (** {2 Depth Queries} *)

  val best_n_bids : t -> n:int -> unit -> Attributed_level.t list
  (** Get top N bids (sorted descending by price)

      @param n Number of levels to return
      @return List of up to N best bids (may be fewer if book is shallow)

      {b Example:}
      {[
        let top_10_bids = Book.best_n_bids book ~n:10 () in
        List.iter top_10_bids ~f:(fun level ->
          printf "%.2f @ %.8f (%s)\n"
            level.price
            level.volume
            (exchange_to_string level.exchange)
        )
      ]}
  *)

  val best_n_asks : t -> n:int -> unit -> Attributed_level.t list
  (** Get top N asks (sorted ascending by price) *)

  (** {2 Volume-Weighted Average Price} *)

  val vwap_bid : t -> volume:float -> float option
  (** Calculate VWAP for selling given volume

      Walks bid side accumulating volume until target reached.
      Returns weighted average price.

      @param volume Target volume to sell
      @return Some vwap if sufficient liquidity, None otherwise

      {b Use case:} Market impact analysis for large sell orders.

      {b Example:}
      {[
        (* What price would I get selling 10 BTC? *)
        match Book.vwap_bid book ~volume:10.0 with
        | Some vwap ->
          printf "Selling 10 BTC would yield avg price: $%.2f\n" vwap
        | None ->
          printf "Insufficient bid liquidity for 10 BTC\n"
      ]}
  *)

  val vwap_ask : t -> volume:float -> float option
  (** Calculate VWAP for buying given volume

      Walks ask side accumulating volume.

      {b Use case:} Market impact analysis for large buy orders.
  *)

  (** {2 Liquidity Analysis} *)

  val liquidity_depth : t -> side:[`Bid | `Ask] -> percentage:float -> float
  (** Get total liquidity within percentage of best price

      @param side Bid or Ask side
      @param percentage How far from best price (e.g., 1.0 = within 1%)
      @return Total volume available

      {b Example:}
      {[
        (* How much BTC can I sell within 0.5% of best bid? *)
        let depth = Book.liquidity_depth book ~side:`Bid ~percentage:0.5 in
        printf "Liquidity within 0.5%% of best bid: %.4f BTC\n" depth

        (* How much BTC can I buy within 1% of best ask? *)
        let depth = Book.liquidity_depth book ~side:`Ask ~percentage:1.0 in
        printf "Liquidity within 1%% of best ask: %.4f BTC\n" depth
      ]}

      {b Use case:}
      - Assess market depth before large orders
      - Compare liquidity across different markets
      - Risk management (how much can I exit at reasonable price?)
  *)

  (** {2 Display Utilities} *)

  val exchange_to_string : exchange_source -> string
  (** Format exchange source for display

      Returns:
      - "GEM" for Gemini
      - "KRK" for Kraken
      - "HYP" for Hyperliquid
      - "BTR" for Bitrue
      - "BIN" for Binance
      - "CBP" for Coinbase
      - "GEM+KRK" for Multiple [Gemini; Kraken]

      {b Recursive:} Handles nested Multiple variants.
  *)

  val pretty_print : ?max_depth:int -> t -> unit -> unit
  (** Pretty-print consolidated order book with ANSI colors

      Displays:
      - Symbol and epoch
      - Active exchanges
      - Top N asks (red)
      - Spread with percentage
      - Top N bids (green)
      - Exchange attribution for each level (blue)

      @param max_depth Number of levels per side to display (default: 10)

      {b Output Example:}
      {v
        === BTCUSD Consolidated Order Book (Epoch: 42) ===
        Updated: 2026-01-16 15:30:45.123456Z
        Sources: [Gemini] [Kraken] [Hyperliquid]

        Asks (Sell Orders):
          GEM   0.15000000 @ 51250.00
          KRK   0.25000000 @ 51240.00
          GEM+KRK 0.50000000 @ 51230.00
          HYP   1.00000000 @ 51220.00

        --- Spread: $20.00 (0.0390%) ---

        Bids (Buy Orders):
          KRK   0.30000000 @ 51200.00
          GEM   0.20000000 @ 51190.00
          HYP   0.80000000 @ 51180.00
          GEM+KRK 0.60000000 @ 51170.00
      v}

      {b Colors:}
      - Red: Asks
      - Green: Bids
      - Blue: Exchange codes
  *)
end

(** {1 Arbitrage Opportunities} *)

module Arbitrage : sig
  (** Cross-exchange arbitrage detection

      Finds opportunities to profit from price differences between exchanges.

      {b Strategy:} Buy on exchange with lowest ask, sell on exchange with highest bid.

      {b Considerations:}
      - Transfer fees (moving funds between exchanges)
      - Trading fees (maker/taker)
      - Transfer time (opportunity may disappear)
      - Slippage (actual execution may differ from book)
      - Exchange limits (withdrawal minimums/maximums)

      {b Real-world viability:}
      Most detected opportunities are too small after fees.
      Viable arbitrage typically requires:
      - Profit > 0.5% (to cover fees + risk)
      - Large volume (to make absolute profit worthwhile)
      - Fast execution (milliseconds matter)
  *)

  type opportunity = {
    buy_exchange : exchange_source;
    (** Where to buy (lowest ask) *)

    sell_exchange : exchange_source;
    (** Where to sell (highest bid) *)

    buy_price : float;
    (** Best ask on buy exchange *)

    sell_price : float;
    (** Best bid on sell exchange *)

    profit_per_unit : float;
    (** Gross profit per unit: sell_price - buy_price *)

    profit_percentage : float;
    (** Profit as percentage: (profit / buy_price) × 100 *)

    max_volume : float;
    (** Maximum volume for this opportunity (min of bid and ask volumes) *)
  }
  [@@deriving sexp, fields]

  val detect_opportunities : Book.t -> opportunity list
  (** Detect all arbitrage opportunities in consolidated book

      Compares best bid/ask from each exchange pair.
      Returns list sorted by profit_percentage (descending).

      {b Algorithm:}
      1. Get best ask from each exchange (where to buy)
      2. Get best bid from each exchange (where to sell)
      3. For each (buy_exchange, sell_exchange) pair:
         - If sell_price > buy_price: opportunity exists
         - Calculate profit metrics
      4. Sort by profit percentage

      {b Example:}
      {[
        let opportunities = Arbitrage.detect_opportunities book in
        List.iter opportunities ~f:(fun opp ->
          printf "Buy %s @ $%.2f, Sell %s @ $%.2f = %.3f%% profit\n"
            (Book.exchange_to_string opp.buy_exchange)
            opp.buy_price
            (Book.exchange_to_string opp.sell_exchange)
            opp.sell_price
            opp.profit_percentage
        )
      ]}

      @return List of opportunities (may be empty), sorted by profitability
  *)

  val print_opportunities : opportunity list -> unit
  (** Pretty-print arbitrage opportunities

      Displays top 3 opportunities in formatted box with:
      - Buy and sell exchanges
      - Prices on each exchange
      - Profit per unit and percentage
      - Maximum tradeable volume

      {b Output Example:}
      {v
        ╔════════════════════════════════════════════════════════╗
        ║  Arbitrage Opportunities                               ║
        ╠════════════════════════════════════════════════════════╣
        ║  #1: Buy HYP @ $51220.00 → Sell GEM @ $51250.00
        ║      Profit: $30.00/unit (0.059%), Max vol: 0.1500
        ║  #2: Buy KRK @ $51225.00 → Sell GEM @ $51248.00
        ║      Profit: $23.00/unit (0.045%), Max vol: 0.2000
        ║  #3: Buy BIN @ $51230.00 → Sell KRK @ $51245.00
        ║      Profit: $15.00/unit (0.029%), Max vol: 0.5000
        ╚════════════════════════════════════════════════════════╝
      v}

      If no opportunities: prints "No arbitrage opportunities detected."
  *)
end

(** {1 Market Analytics} *)

module Analytics : sig
  (** Market microstructure metrics and liquidity analysis

      Provides comprehensive statistics about the consolidated book:
      - Total liquidity
      - Best price sources
      - Depth at various levels
      - Spread metrics
  *)

  type metrics = {
    total_bid_volume : float;
    (** Sum of all bid volumes across all levels and exchanges *)

    total_ask_volume : float;
    (** Sum of all ask volumes *)

    weighted_mid_price : float;
    (** (best_bid + best_ask) / 2 *)

    bid_depth_1pct : float;
    (** Total volume within 1% of best bid *)

    ask_depth_1pct : float;
    (** Total volume within 1% of best ask *)

    spread_bps : float;
    (** Spread in basis points: (spread / best_bid) × 10000 *)

    num_exchanges : int;
    (** Number of exchanges with active books *)

    best_bid_exchange : exchange_source;
    (** Which exchange has the best bid *)

    best_ask_exchange : exchange_source;
    (** Which exchange has the best ask *)
  }
  [@@deriving sexp, fields]

  val calculate : Book.t -> metrics
  (** Calculate all analytics metrics for consolidated book

      {b Example:}
      {[
        let metrics = Analytics.calculate book in
        printf "Mid price: $%.2f\n" metrics.weighted_mid_price;
        printf "Spread: %.2f bps\n" metrics.spread_bps;
        printf "Total liquidity: %.4f bid, %.4f ask\n"
          metrics.total_bid_volume
          metrics.total_ask_volume;
        printf "Depth within 1%%: %.4f bid, %.4f ask\n"
          metrics.bid_depth_1pct
          metrics.ask_depth_1pct;
        printf "Best prices: %s (bid), %s (ask)\n"
          (Book.exchange_to_string metrics.best_bid_exchange)
          (Book.exchange_to_string metrics.best_ask_exchange);
      ]}

      {b Use cases:}
      - Compare liquidity across different symbols
      - Assess market quality (tight spread + deep book = good)
      - Identify primary liquidity sources
      - Monitor market conditions over time
  *)

  val print : metrics -> unit
  (** Pretty-print analytics metrics

      {b Output Example:}
      {v
        ╔════════════════════════════════════════════════════════╗
        ║  Market Analytics                                      ║
        ╠════════════════════════════════════════════════════════╣
        ║  Active Exchanges: 3                                   ║
        ║  Weighted Mid Price: $51225.00
        ║  Spread: 3.90 bps
        ║  Best Bid: KRK
        ║  Best Ask: GEM
        ║  Total Bid Volume: 12.5000
        ║  Total Ask Volume: 15.2500
        ║  Bid Depth (1%): 8.5000
        ║  Ask Depth (1%): 9.7500
        ╚════════════════════════════════════════════════════════╝
      v}
  *)
end

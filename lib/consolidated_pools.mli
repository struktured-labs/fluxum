(** Consolidated Pools - Aggregate DEX liquidity for best execution

    This module aggregates liquidity pools from multiple DEXes and provides:
    - Best price discovery across all pools for a token pair
    - Ranked quotes for optimal execution
    - Liquidity depth analysis at price impact thresholds
    - Multi-pool split routing for large trades
    - Arbitrage opportunity detection

    {2 Architecture}

    Mirrors the {!Consolidated_order_book} pattern for CEX order books,
    but adapted for AMM pool-based pricing. Pools are indexed by token pair
    and can be queried for best price, quotes, and depth.

    {2 Usage}

    {[
      (* Create consolidated pools and add from multiple DEXes *)
      let t = Consolidated_pools.create () in
      Consolidated_pools.add_pool t ~pool:sushi_pool;
      Consolidated_pools.add_pool t ~pool:uni_pool;

      (* Find best price *)
      let best = Consolidated_pools.best_price t ~token0:"WETH" ~token1:"USDC" in

      (* Get ranked quotes for 10 ETH *)
      let quotes = Consolidated_pools.best_quotes t
        ~amount:10.0 ~token_in:"WETH" ~token_out:"USDC" in

      (* Split large trade across pools *)
      let route = Consolidated_pools.split_route t
        ~amount:1000.0 ~token_in:"WETH" ~token_out:"USDC" ~max_splits:3 in
    ]}

    @see Pool_intf for the unified pool abstraction
    @see Amm_pricing for pricing formulas
*)

(** {1 Attributed Types} *)

(** Pool with venue attribution *)
module Attributed_pool : sig
  type t = {
    venue : string;
    pool : Pool_intf.Pool.t;
  } [@@deriving sexp, compare, equal, fields]

  val create : venue:string -> pool:Pool_intf.Pool.t -> t

  (** Get spot price from this pool *)
  val spot_price : t -> float

  (** Compare by spot price (lower is better for buying token1) *)
  val compare_by_price : t -> t -> int
end

(** Quote with venue attribution *)
module Attributed_quote : sig
  type t = {
    venue : string;
    pool_id : string;
    quote : Pool_intf.Quote.t;
  } [@@deriving sexp, compare, equal, fields]

  val create : venue:string -> pool_id:string -> quote:Pool_intf.Quote.t -> t

  (** Effective price from quote *)
  val effective_price : t -> float

  (** Compare by effective price (higher is better - more output per input) *)
  val compare_by_effective_price : t -> t -> int
end

(** Aggregated liquidity metrics for a token pair *)
module Pair_liquidity : sig
  type t = {
    token0 : string;
    token1 : string;
    total_tvl_usd : float;
    num_pools : int;
    best_spot_price : float;
    best_venue : string;
    weighted_avg_price : float;
  } [@@deriving sexp, compare, equal, fields]
end

(** {1 Consolidated Pools State} *)

(** Consolidated pools aggregator *)
type t

(** Create empty consolidated pools *)
val create : unit -> t

(** {1 Pool Management} *)

(** Add a normalized pool to the consolidated state *)
val add_pool : t -> pool:Pool_intf.Pool.t -> unit

(** Remove a pool by ID *)
val remove_pool : t -> pool_id:string -> unit

(** Get all pools for a token pair *)
val get_pools : t -> token0:string -> token1:string -> Attributed_pool.t list

(** Get pool by ID *)
val get_pool_by_id : t -> pool_id:string -> Attributed_pool.t option

(** {1 Price Discovery} *)

(** Best spot price across all pools for a pair.

    Returns the pool with the lowest spot price (best for buying token1).

    @param token0 Base token symbol
    @param token1 Quote token symbol
    @return Pool with best price, or None if no pools *)
val best_price : t -> token0:string -> token1:string -> Attributed_pool.t option

(** Get ranked quotes from all pools for a pair, sorted by effective price.

    Higher effective price = more output per input = better execution.

    @param amount Amount of input token
    @param token_in Input token symbol
    @param token_out Output token symbol
    @return List of quotes sorted by effective price (best first) *)
val best_quotes : t -> amount:float -> token_in:string -> token_out:string ->
  Attributed_quote.t list

(** Get the single best quote for a trade.

    Convenience wrapper around {!best_quotes} returning first result.

    @param amount Amount of input token
    @param token_in Input token symbol
    @param token_out Output token symbol
    @return Best quote, or None if no pools *)
val best_quote : t -> amount:float -> token_in:string -> token_out:string ->
  Attributed_quote.t option

(** {1 Liquidity Analysis} *)

(** Calculate total depth available before hitting max price impact.

    Sums depth across all pools for the pair.

    @param token0 Base token symbol
    @param token1 Quote token symbol
    @param max_impact_pct Maximum acceptable price impact (e.g., 1.0 for 1%)
    @return Total depth in token0 units *)
val depth_at_impact : t -> token0:string -> token1:string -> max_impact_pct:float -> float

(** Get aggregated liquidity metrics for a pair.

    @param token0 Base token symbol
    @param token1 Quote token symbol
    @return Liquidity metrics, or None if no pools *)
val pair_liquidity : t -> token0:string -> token1:string -> Pair_liquidity.t option

(** {1 Advanced Routing} *)

(** Multi-pool split routing for large trades.

    Splits a large trade across multiple pools to minimize price impact.
    Uses a simple equal-split strategy across top pools by TVL.

    @param amount Total amount to swap
    @param token_in Input token symbol
    @param token_out Output token symbol
    @param max_splits Maximum number of pools to split across
    @return List of (pool_id, amount, quote) tuples *)
val split_route : t -> amount:float -> token_in:string -> token_out:string ->
  max_splits:int -> (string * float * Attributed_quote.t) list

(** Arbitrage detection between pools for the same pair.

    Finds pool pairs where price difference exceeds threshold.

    @param token0 Base token symbol
    @param token1 Quote token symbol
    @param min_profit_bps Minimum spread in basis points
    @return List of (buy_pool, sell_pool, spread_bps) opportunities *)
val find_arbitrage : t -> token0:string -> token1:string -> min_profit_bps:int ->
  (Attributed_pool.t * Attributed_pool.t * float) list

(** {1 Introspection} *)

(** Get all unique token pairs with pools *)
val all_pairs : t -> (string * string) list

(** Get all venues with pools *)
val all_venues : t -> string list

(** Get statistics about the consolidated pools *)
val stats : t -> (string * string) list

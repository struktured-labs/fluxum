(** Unified Pool Interface for DEXes

    All DEX adapters implement this signature for consolidated liquidity access.
    This provides a unified abstraction over different AMM types:

    - {b Constant Product (x*y=k)}: SushiSwap, Thena, TraderJoe Classic
    - {b Concentrated Liquidity}: Uniswap V3, Aerodrome, Orca CLMM
    - {b Liquidity Bins}: TraderJoe LB
    - {b Weighted Pools}: Balancer, Osmosis
    - {b Stable Pools}: Curve StableSwap

    {2 Usage}

    DEX adapters implement {!module-type:S} to provide:
    - Pool normalization to unified {!Pool.t}
    - Spot price calculation
    - Quote generation with price impact

    {2 Example}

    {[
      (* Normalize a SushiSwap pool *)
      let pool = Sushiswap.Pool_adapter.normalize native_pool in

      (* Get quote for 10 ETH -> USDC *)
      let quote = Sushiswap.Pool_adapter.quote native_pool
        ~amount_in:10.0 ~token_in:"WETH" ~token_out:"USDC" in
    ]}

    @see <https://docs.uniswap.org/> for concentrated liquidity math
    @see <https://curve.readthedocs.io/> for StableSwap math
*)

(** {1 Pool Type Classification} *)

(** Pool type determines which pricing formula to use *)
module Pool_type : sig
  type t =
    | Constant_product  (** x*y=k: SushiSwap, Thena, classic AMMs *)
    | Concentrated      (** Uniswap V3, Aerodrome, Orca CLMM *)
    | Liquidity_bin     (** TraderJoe LB with discrete bins *)
    | Weighted          (** Balancer, Osmosis weighted pools *)
    | Stable            (** Curve StableSwap invariant *)
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string : string -> t option
end

(** {1 Token Information} *)

(** Token information within a pool *)
module Token : sig
  type t = {
    address : string;   (** Contract address (0x... for EVM, pubkey for Solana) *)
    symbol : string;    (** Token symbol (e.g., "WETH", "USDC") *)
    decimals : int;     (** Token decimals (18 for most ERC20, 6 for USDC) *)
  } [@@deriving sexp, compare, equal, fields]

  val create : address:string -> symbol:string -> decimals:int -> t
end

(** {1 Normalized Pool} *)

(** Normalized pool representation across all DEXes *)
module Pool : sig
  type t = {
    id : string;                (** Pool address/ID *)
    venue : string;             (** Exchange name (e.g., "sushiswap", "uniswap_v3") *)
    pool_type : Pool_type.t;    (** Type of AMM for pricing math *)
    token0 : Token.t;           (** Base token *)
    token1 : Token.t;           (** Quote token *)
    reserve0 : float;           (** Token0 reserve (normalized to float) *)
    reserve1 : float;           (** Token1 reserve (normalized to float) *)
    tvl_usd : float;            (** Total value locked in USD *)
    fee_bps : int;              (** Fee in basis points (30 = 0.30%) *)
    spot_price : float;         (** Current price: token0 -> token1 *)
    spot_price_inv : float;     (** Inverse price: token1 -> token0 *)
  } [@@deriving sexp, compare, equal, fields]

  val create :
    id:string ->
    venue:string ->
    pool_type:Pool_type.t ->
    token0:Token.t ->
    token1:Token.t ->
    reserve0:float ->
    reserve1:float ->
    tvl_usd:float ->
    fee_bps:int ->
    spot_price:float ->
    spot_price_inv:float ->
    t
end

(** {1 Swap Quote} *)

(** Quote result from pool pricing *)
module Quote : sig
  type t = {
    amount_in : float;          (** Input amount *)
    amount_out : float;         (** Output amount after swap *)
    price_impact_pct : float;   (** Price impact as percentage *)
    effective_price : float;    (** Actual execution price (amount_out/amount_in) *)
    fee_amount : float;         (** Fee paid in input token *)
    route : string list;        (** Route for multi-hop (pool IDs) *)
  } [@@deriving sexp, compare, equal, fields]

  val create :
    amount_in:float ->
    amount_out:float ->
    price_impact_pct:float ->
    effective_price:float ->
    fee_amount:float ->
    route:string list ->
    t

  (** Create quote for single-hop swap *)
  val single :
    amount_in:float ->
    amount_out:float ->
    price_impact_pct:float ->
    effective_price:float ->
    fee_amount:float ->
    pool_id:string ->
    t
end

(** {1 Pool Adapter Signature} *)

(** Module signature that DEX pool adapters must implement *)
module type S = sig
  (** Venue identifier for this pool adapter (e.g., "sushiswap") *)
  val venue : string

  (** Native pool type from the DEX (exchange-specific) *)
  module Native : sig
    type pool
  end

  (** Get spot price for a token pair in the pool.

      @param pool Native pool data
      @param token_in Symbol of input token
      @param token_out Symbol of output token
      @return Spot price (infinitesimal trade), or error *)
  val spot_price : Native.pool -> token_in:string -> token_out:string ->
    (float, string) Result.t

  (** Get quote for swapping a specific amount.

      @param pool Native pool data
      @param amount_in Amount of input token to swap
      @param token_in Symbol of input token
      @param token_out Symbol of output token
      @return Quote with output amount and price impact, or error *)
  val quote : Native.pool -> amount_in:float -> token_in:string -> token_out:string ->
    (Quote.t, string) Result.t

  (** Normalize native pool to unified Pool.t representation.

      @param pool Native pool data
      @return Normalized pool, or error if required fields missing *)
  val normalize : Native.pool -> (Pool.t, string) Result.t

  (** Get all pools for a token pair from the DEX.

      Note: This typically requires network calls to fetch pool data.

      @param token0 Base token symbol
      @param token1 Quote token symbol
      @return List of native pools for the pair *)
  val pools_for_pair : token0:string -> token1:string ->
    (Native.pool list, string) Result.t Async.Deferred.t
end

(** {1 Pricing Math Helpers} *)

(** Helper for constant product AMM math (x*y=k) *)
module Constant_product_math : sig
  (** Spot price = reserve1 / reserve0 *)
  val spot_price : reserve0:float -> reserve1:float -> float

  (** Calculate output amount for constant product swap *)
  val amount_out :
    reserve0:float ->
    reserve1:float ->
    amount_in:float ->
    fee_bps:int ->
    float

  (** Create a full quote for constant product swap *)
  val quote :
    reserve0:float ->
    reserve1:float ->
    amount_in:float ->
    fee_bps:int ->
    pool_id:string ->
    Quote.t

  (** Calculate max trade size before hitting given price impact *)
  val depth_at_impact :
    reserve0:float ->
    reserve1:float ->
    max_impact_pct:float ->
    fee_bps:int ->
    float
end

(** Helper for concentrated liquidity math (Uniswap V3 style) *)
module Concentrated_math : sig
  (** Convert sqrtPriceX96 (Q64.96 format) to human-readable price *)
  val price_from_sqrt_price_x96 :
    sqrt_price_x96:string ->
    decimals0:int ->
    decimals1:int ->
    float

  (** Convert tick to price *)
  val price_from_tick :
    tick:int ->
    decimals0:int ->
    decimals1:int ->
    float
end

(** Helper for stable swap math (Curve style) *)
module Stable_math : sig
  (** Calculate D (invariant) for StableSwap using Newton iteration *)
  val get_d : balances:float list -> amp:int -> float
end

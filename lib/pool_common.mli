(** Shared utilities for DEX pool adapters

    This module provides safe normalization helpers that use Result.t
    for all conversions, eliminating unsafe Float.of_string calls.

    {2 Usage}

    {[
      (* Instead of unsafe conversions: *)
      let reserve0 = Float.of_string pool.reserve0 in  (* CRASHES on bad input *)

      (* Use safe Pool_common helpers: *)
      let open Result.Let_syntax in
      let%bind pool = Pool_common.Constant_product.normalize_pool
        ~venue:"sushiswap"
        ~pool_id:pool.address
        ~token0_addr:pool.token0 ~token0_symbol:pool.token0 ~token0_decimals:18
        ~token1_addr:pool.token1 ~token1_symbol:pool.token1 ~token1_decimals:18
        ~reserve0_str:pool.reserve0
        ~reserve1_str:pool.reserve1
        ~fee_bps:30
        () in
    ]}
*)

(** {1 Safe Integer Conversions} *)

module Int_conv : sig
  (** Convert string to int with Result.t error handling. *)
  val of_string : string -> (int, string) Result.t

  (** Convert string to int with default fallback. *)
  val of_string_with_default : string -> default:int -> int

  (** Parse token decimals from string (defaults to 18). *)
  val decimals_of_string : string -> int

  (** Parse fee in basis points (0-10000). *)
  val fee_bps_of_string : string -> (int, string) Result.t
end

(** {1 Constant Product Pool Helpers} *)

module Constant_product : sig
  (** Create normalized pool from constant product AMM data.

      Uses safe float conversions - returns Error on malformed input.

      @param venue Exchange name (e.g., "sushiswap")
      @param pool_id Pool address/identifier
      @param token0_addr Token0 contract address
      @param token0_symbol Token0 symbol (may be same as address if unknown)
      @param token0_decimals Token0 decimals (default 18)
      @param token1_addr Token1 contract address
      @param token1_symbol Token1 symbol
      @param token1_decimals Token1 decimals (default 18)
      @param reserve0_str Reserve0 as string
      @param reserve1_str Reserve1 as string
      @param fee_bps Fee in basis points (e.g., 30 for 0.3%)
      @param tvl_usd Total value locked in USD (optional, default 0)
  *)
  val normalize_pool :
    venue:string ->
    pool_id:string ->
    token0_addr:string ->
    token0_symbol:string ->
    token0_decimals:int ->
    token1_addr:string ->
    token1_symbol:string ->
    token1_decimals:int ->
    reserve0_str:string ->
    reserve1_str:string ->
    fee_bps:int ->
    ?tvl_usd:float ->
    unit ->
    (Pool_intf.Pool.t, string) Result.t

  (** Get spot price for token pair from reserves. *)
  val spot_price_from_reserves :
    reserve0:float ->
    reserve1:float ->
    token0_id:string ->
    token1_id:string ->
    token_in:string ->
    token_out:string ->
    (float, string) Result.t

  (** Parse reserves from strings and get spot price. *)
  val spot_price :
    reserve0_str:string ->
    reserve1_str:string ->
    token0_id:string ->
    token1_id:string ->
    token_in:string ->
    token_out:string ->
    (float, string) Result.t
end

(** {1 Concentrated Liquidity Pool Helpers} *)

module Concentrated : sig
  (** Q96 constant = 2^96 for sqrtPriceX96 conversion *)
  val q96 : float

  (** Convert sqrtPriceX96 string to price.

      Uses safe float conversion.

      @param sqrt_price_x96 sqrtPriceX96 as string (Q64.96 format)
      @param decimals0 Token0 decimals
      @param decimals1 Token1 decimals
      @return Price (token0 -> token1) or Error
  *)
  val price_from_sqrt_price_x96 :
    sqrt_price_x96:string ->
    decimals0:int ->
    decimals1:int ->
    (float, string) Result.t

  (** Parse liquidity string safely. *)
  val parse_liquidity : string -> (float, string) Result.t
end

(** {1 Stable Pool Helpers} *)

module Stable : sig
  (** Parse virtual price string safely. *)
  val parse_virtual_price : string -> (float, string) Result.t

  (** Parse amplification coefficient. *)
  val parse_amplification : ?amp_str:string -> default:int -> unit -> int
end

(** {1 Weighted Pool Helpers} *)

module Weighted : sig
  (** Parse token weight from string (must be 0.0 to 1.0). *)
  val parse_weight : string -> (float, string) Result.t

  (** Parse balance from string. *)
  val parse_balance : string -> (float, string) Result.t
end

(** {1 Price-Based Pool Helpers (for CEX synthetic pools)} *)

module Price_based : sig
  (** Create pool from price and volume data.

      Used for CEX synthetic pools where we have price rather than reserves.

      @param venue Exchange name
      @param pool_id Pool identifier
      @param base_symbol Base token symbol
      @param quote_symbol Quote token symbol
      @param price_str Current price as string
      @param base_volume_str Base volume as string
      @param quote_volume_str Quote volume as string
      @param fee_bps Fee in basis points
  *)
  val normalize_pool :
    venue:string ->
    pool_id:string ->
    base_symbol:string ->
    quote_symbol:string ->
    price_str:string ->
    base_volume_str:string ->
    quote_volume_str:string ->
    fee_bps:int ->
    (Pool_intf.Pool.t, string) Result.t
end

(** Shared utilities for DEX pool adapters

    This module provides safe normalization helpers that use Result.t
    for all conversions, eliminating unsafe Float.of_string calls.
*)

open Core

module Float_conv = Fluxum.Normalize_common.Float_conv

(** {1 Constant Product Pool Helpers} *)

module Constant_product = struct
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
  let normalize_pool
      ~venue
      ~pool_id
      ~token0_addr
      ~token0_symbol
      ~token0_decimals
      ~token1_addr
      ~token1_symbol
      ~token1_decimals
      ~reserve0_str
      ~reserve1_str
      ~fee_bps
      ?(tvl_usd = 0.0)
      ()
    : (Pool_intf.Pool.t, string) Result.t =
    let open Result.Let_syntax in
    let%bind reserve0 = Float_conv.qty_of_string reserve0_str in
    let%bind reserve1 = Float_conv.qty_of_string reserve1_str in

    let spot_price = match Float.(reserve0 > 0.0) with
      | true -> reserve1 /. reserve0
      | false -> 0.0
    in
    let spot_price_inv = match Float.(reserve1 > 0.0) with
      | true -> reserve0 /. reserve1
      | false -> 0.0
    in

    Ok {
      Pool_intf.Pool.
      id = pool_id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Constant_product;
      token0 = {
        Pool_intf.Token.
        address = token0_addr;
        symbol = token0_symbol;
        decimals = token0_decimals;
      };
      token1 = {
        Pool_intf.Token.
        address = token1_addr;
        symbol = token1_symbol;
        decimals = token1_decimals;
      };
      reserve0;
      reserve1;
      tvl_usd;
      fee_bps;
      spot_price;
      spot_price_inv;
    }

  (** Get spot price for token pair from reserves.

      @param reserve0 Reserve of token0
      @param reserve1 Reserve of token1
      @param token0_id Token0 address or symbol
      @param token1_id Token1 address or symbol
      @param token_in Input token address or symbol
      @param token_out Output token address or symbol
      @return Spot price or Error if tokens don't match
  *)
  let spot_price_from_reserves
      ~reserve0
      ~reserve1
      ~token0_id
      ~token1_id
      ~token_in
      ~token_out
    : (float, string) Result.t =
    match String.equal token_in token0_id with
    | true when String.equal token_out token1_id ->
      Ok (reserve1 /. reserve0)
    | false when String.equal token_in token1_id && String.equal token_out token0_id ->
      Ok (reserve0 /. reserve1)
    | _ -> Error (sprintf "Token pair (%s, %s) does not match pool (%s, %s)"
                    token_in token_out token0_id token1_id)

  (** Parse reserves from strings and get spot price. *)
  let spot_price
      ~reserve0_str
      ~reserve1_str
      ~token0_id
      ~token1_id
      ~token_in
      ~token_out
    : (float, string) Result.t =
    let open Result.Let_syntax in
    let%bind reserve0 = Float_conv.qty_of_string reserve0_str in
    let%bind reserve1 = Float_conv.qty_of_string reserve1_str in
    spot_price_from_reserves ~reserve0 ~reserve1 ~token0_id ~token1_id ~token_in ~token_out
end

(** {1 Concentrated Liquidity Pool Helpers} *)

module Concentrated = struct
  (** Q96 constant = 2^96 for sqrtPriceX96 conversion *)
  let q96 : float = Float.ldexp 1.0 96

  (** Convert sqrtPriceX96 string to price.

      Uses safe float conversion.

      @param sqrt_price_x96 sqrtPriceX96 as string (Q64.96 format)
      @param decimals0 Token0 decimals
      @param decimals1 Token1 decimals
      @return Price (token0 -> token1) or Error
  *)
  let price_from_sqrt_price_x96 ~sqrt_price_x96 ~decimals0 ~decimals1
    : (float, string) Result.t =
    let open Result.Let_syntax in
    let%bind sqrt_price_raw = Float_conv.of_string sqrt_price_x96 in
    let sqrt_price = sqrt_price_raw /. q96 in
    let raw_price = sqrt_price *. sqrt_price in
    let decimal_adjustment = Float.int_pow 10.0 (decimals1 - decimals0) in
    Ok (raw_price *. decimal_adjustment)

  (** Parse liquidity string safely. *)
  let parse_liquidity liquidity_str : (float, string) Result.t =
    Float_conv.qty_of_string liquidity_str
end

(** {1 Stable Pool Helpers} *)

module Stable = struct
  (** Parse virtual price string safely.

      @param virtual_price_str Virtual price as string
      @return Virtual price or Error
  *)
  let parse_virtual_price virtual_price_str : (float, string) Result.t =
    Float_conv.price_of_string virtual_price_str

  (** Parse amplification coefficient.

      @param amp_str Amplification coefficient as string (optional)
      @param default Default value if not provided
      @return Amplification coefficient
  *)
  let parse_amplification ?amp_str ~default () : int =
    match amp_str with
    | Some s -> (try Int.of_string s with _ -> default)
    | None -> default
end

(** {1 Weighted Pool Helpers} *)

module Weighted = struct
  (** Parse token weight from string.

      @param weight_str Weight as string
      @return Weight as float (0.0 to 1.0) or Error
  *)
  let parse_weight weight_str : (float, string) Result.t =
    let open Result.Let_syntax in
    let%bind w = Float_conv.of_string weight_str in
    match Float.(w >= 0.0 && w <= 1.0) with
    | true -> Ok w
    | false -> Error (sprintf "Weight must be between 0 and 1, got: %f" w)

  (** Parse balance from string. *)
  let parse_balance balance_str : (float, string) Result.t =
    Float_conv.qty_of_string balance_str
end

(** {1 Price-Based Pool Helpers (for CEX synthetic pools)} *)

module Price_based = struct
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
  let normalize_pool
      ~venue
      ~pool_id
      ~base_symbol
      ~quote_symbol
      ~price_str
      ~base_volume_str
      ~quote_volume_str
      ~fee_bps
    : (Pool_intf.Pool.t, string) Result.t =
    let open Result.Let_syntax in
    let%bind price = Float_conv.price_of_string price_str in
    let%bind base_volume = Float_conv.qty_of_string base_volume_str in
    let%bind quote_volume = Float_conv.qty_of_string quote_volume_str in

    let spot_price = price in
    let spot_price_inv = match Float.(price > 0.0) with
      | true -> 1.0 /. price
      | false -> 0.0
    in

    Ok {
      Pool_intf.Pool.
      id = pool_id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Constant_product;
      token0 = {
        Pool_intf.Token.
        address = pool_id;
        symbol = base_symbol;
        decimals = 18;
      };
      token1 = {
        Pool_intf.Token.
        address = pool_id;
        symbol = quote_symbol;
        decimals = 18;
      };
      reserve0 = base_volume;
      reserve1 = quote_volume;
      tvl_usd = base_volume +. quote_volume;
      fee_bps;
      spot_price;
      spot_price_inv;
    }
end

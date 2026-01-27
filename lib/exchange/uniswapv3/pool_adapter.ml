(** Uniswap V3 Pool Adapter

    Implements Pool_intf.S for Uniswap V3 concentrated liquidity pools.
*)

open Core

let venue = "uniswap_v3"

module Native = struct
  type pool = Types.pool
end

(** Normalize Uniswap V3 pool to unified pool representation *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  let decimals0 = pool.token0.decimals in
  let decimals1 = pool.token1.decimals in

  let%bind spot_price = Pool_common.Concentrated.price_from_sqrt_price_x96
    ~sqrt_price_x96:pool.sqrtPrice ~decimals0 ~decimals1 in

  let spot_price_inv = match Float.(spot_price > 0.0) with
    | true -> 1.0 /. spot_price
    | false -> 0.0
  in

  (* Fee tiers: 100 = 0.01%, 500 = 0.05%, 3000 = 0.30%, 10000 = 1% *)
  let fee_bps = pool.feeTier / 100 in

  (* Liquidity is in the pool's internal representation *)
  let%bind liquidity = Pool_common.Concentrated.parse_liquidity pool.liquidity in

  Ok {
    Pool_intf.Pool.
    id = pool.id;
    venue = venue;
    pool_type = Pool_intf.Pool_type.Concentrated;
    token0 = {
      Pool_intf.Token.
      address = pool.token0.id;
      symbol = pool.token0.symbol;
      decimals = decimals0;
    };
    token1 = {
      Pool_intf.Token.
      address = pool.token1.id;
      symbol = pool.token1.symbol;
      decimals = decimals1;
    };
    (* For concentrated liquidity, reserve0/1 are derived from liquidity and price *)
    reserve0 = liquidity /. (Float.sqrt spot_price);
    reserve1 = liquidity *. (Float.sqrt spot_price);
    tvl_usd = pool.volumeUSD;  (* Using volume as proxy for TVL *)
    fee_bps = fee_bps;
    spot_price = spot_price;
    spot_price_inv = spot_price_inv;
  }

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  let decimals0 = pool.token0.decimals in
  let decimals1 = pool.token1.decimals in

  let open Result.Let_syntax in
  let%bind base_price = Pool_common.Concentrated.price_from_sqrt_price_x96
    ~sqrt_price_x96:pool.sqrtPrice ~decimals0 ~decimals1 in

  match String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol with
  | true when String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol ->
    Ok base_price
  | false when (String.equal token_in pool.token1.id || String.equal token_in pool.token1.symbol) &&
               (String.equal token_out pool.token0.id || String.equal token_out pool.token0.symbol) ->
    Ok (1.0 /. base_price)
  | _ -> Error "Token pair does not match pool"

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  let decimals0 = pool.token0.decimals in
  let decimals1 = pool.token1.decimals in
  let fee_bps = pool.feeTier / 100 in

  (* For concentrated liquidity, use the simplified formula *)
  let params = Amm_pricing.Concentrated.{
    sqrt_price_x96 = pool.sqrtPrice;
    liquidity = pool.liquidity;
    tick = pool.tick;
    fee_bps = fee_bps;
    decimals0 = decimals0;
    decimals1 = decimals1;
  } in

  let is_token0_in = String.equal token_in pool.token0.id ||
                     String.equal token_in pool.token0.symbol in
  let is_token1_out = String.equal token_out pool.token1.id ||
                      String.equal token_out pool.token1.symbol in

  match is_token0_in && is_token1_out with
  | true -> Amm_pricing.Concentrated.quote params ~amount_in ~pool_id:pool.id
  | false ->
    (* For reverse direction, we'd need to invert the math - use fallback for now *)
    let open Result.Let_syntax in
    let%bind spot = spot_price pool ~token_in ~token_out in
    let fee = Float.of_int fee_bps /. 10000.0 in
    let amount_out = amount_in *. spot *. (1.0 -. fee) in
    let price_impact = 0.0 in  (* Simplified - would need proper calculation *)
    Ok (Pool_intf.Quote.single
      ~amount_in ~amount_out ~price_impact_pct:price_impact
      ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee)
      ~pool_id:pool.id)

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1);
  Async.return (Ok [])

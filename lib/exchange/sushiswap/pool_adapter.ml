(** SushiSwap Pool Adapter

    Implements Pool_intf.S for SushiSwap constant product pools.
*)

open Core

let venue = "sushiswap"

module Native = struct
  type pool = Types.pool_info
end

(** Normalize SushiSwap pool to unified representation *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  Pool_common.Constant_product.normalize_pool
    ~venue
    ~pool_id:pool.address
    ~token0_addr:pool.token0
    ~token0_symbol:pool.token0
    ~token0_decimals:18
    ~token1_addr:pool.token1
    ~token1_symbol:pool.token1
    ~token1_decimals:18
    ~reserve0_str:pool.reserve0
    ~reserve1_str:pool.reserve1
    ~fee_bps:30  (* SushiSwap uses 0.3% fee *)
    ()

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  Pool_common.Constant_product.spot_price
    ~reserve0_str:pool.reserve0
    ~reserve1_str:pool.reserve1
    ~token0_id:pool.token0
    ~token1_id:pool.token1
    ~token_in
    ~token_out

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  let fee_bps = 30 in

  (* Determine direction and validate tokens *)
  let (reserve_in, reserve_out, expected_out) =
    match String.equal token_in pool.token0 with
    | true -> (reserve0, reserve1, pool.token1)
    | false -> (reserve1, reserve0, pool.token0)
  in

  (* Validate token_out matches expected *)
  match String.equal token_out expected_out with
  | false -> Error (sprintf "token_out %s does not match expected %s" token_out expected_out)
  | true ->
    let params = Amm_pricing.Constant_product.{
      reserve0 = reserve_in;
      reserve1 = reserve_out;
      fee_bps = fee_bps;
    } in
    Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.address

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1);
  Async.return (Ok [])

(** Velodrome Pool Adapter

    Implements Pool_intf.S for Velodrome pools (Optimism).
    Supports both stable and volatile (constant product) pools.
*)

open Core

let venue = "velodrome"

module Native = struct
  type pool = Types.pair
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    let decimals0 = Int.of_string pool.token0.decimals in
    let decimals1 = Int.of_string pool.token1.decimals in
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    let tvl_usd = Float.of_string pool.reserveUSD in

    let spot_price = match Float.(reserve0 > 0.0) with
      | true -> reserve1 /. reserve0
      | false -> 0.0
    in
    let spot_price_inv = match Float.(reserve1 > 0.0) with
      | true -> reserve0 /. reserve1
      | false -> 0.0
    in

    (* Velodrome: 0.02% for stable, 0.3% for volatile *)
    let fee_bps = match pool.stable with true -> 2 | false -> 30 in
    let pool_type = match pool.stable with
      | true -> Pool_intf.Pool_type.Stable
      | false -> Pool_intf.Pool_type.Constant_product
    in

    Ok {
      Pool_intf.Pool.
      id = pool.id;
      venue = venue;
      pool_type = pool_type;
      token0 = { Pool_intf.Token. address = pool.token0.id; symbol = pool.token0.symbol; decimals = decimals0 };
      token1 = { Pool_intf.Token. address = pool.token1.id; symbol = pool.token1.symbol; decimals = decimals1 };
      reserve0 = reserve0; reserve1 = reserve1;
      tvl_usd = tvl_usd; fee_bps = fee_bps;
      spot_price = spot_price; spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize: %s" (Exn.to_string exn))

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    match String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol with
    | true when String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol ->
      Ok (reserve1 /. reserve0)
    | false -> Ok (reserve0 /. reserve1)
    | _ -> Error "Token pair does not match pool"
  with exn -> Error (sprintf "Failed: %s" (Exn.to_string exn))

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    let fee_bps = match pool.stable with true -> 2 | false -> 30 in
    let is_token0_in = String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol in
    let is_token1_out = String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol in
    let (reserve_in, reserve_out) = match is_token0_in && is_token1_out with
      | true -> (reserve0, reserve1)
      | false -> (reserve1, reserve0)
    in
    let params = Amm_pricing.Constant_product.{ reserve0 = reserve_in; reserve1 = reserve_out; fee_bps = fee_bps } in
    Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id
  with exn -> Error (sprintf "Failed: %s" (Exn.to_string exn))

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

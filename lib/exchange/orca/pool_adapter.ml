(** Orca Pool Adapter

    Implements Pool_intf.S for Orca whirlpools (Solana concentrated liquidity).
*)

open Core

let venue = "orca"

module Native = struct
  type pool = Types.whirlpool
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind liquidity = Fluxum.Normalize_common.Float_conv.qty_of_string pool.liquidity in
  let spot_price = pool.price in
  let spot_price_inv = match Float.(spot_price > 0.0) with true -> 1.0 /. spot_price | false -> 0.0 in

  (* Fee rate is in decimal form, convert to bps *)
  let fee_bps = Float.to_int (pool.feeRate *. 10000.0) in

  Ok {
    Pool_intf.Pool.
    id = pool.address; venue = venue;
    pool_type = Pool_intf.Pool_type.Concentrated;
    token0 = { Pool_intf.Token. address = pool.tokenA; symbol = pool.tokenA; decimals = 9 };
    token1 = { Pool_intf.Token. address = pool.tokenB; symbol = pool.tokenB; decimals = 9 };
    reserve0 = liquidity /. (Float.sqrt spot_price);
    reserve1 = liquidity *. (Float.sqrt spot_price);
    tvl_usd = pool.volume24h; fee_bps = fee_bps;
    spot_price = spot_price; spot_price_inv = spot_price_inv;
  }

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  match String.equal token_in pool.tokenA with
  | true when String.equal token_out pool.tokenB -> Ok pool.price
  | false when String.equal token_in pool.tokenB && String.equal token_out pool.tokenA ->
    Ok (1.0 /. pool.price)
  | _ -> Error "Token pair does not match pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = Float.to_int (pool.feeRate *. 10000.0) in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct:0.0
    ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee) ~pool_id:pool.address)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

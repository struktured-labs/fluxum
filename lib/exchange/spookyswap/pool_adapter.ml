(** SpookySwap Pool Adapter

    Implements Pool_intf.S for SpookySwap constant product pools (Fantom).
*)

open Core

let venue = "spookyswap"

module Native = struct
  type pool = Types.pair
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  let decimals0 = Pool_common.Int_conv.decimals_of_string pool.token0.decimals in
  let decimals1 = Pool_common.Int_conv.decimals_of_string pool.token1.decimals in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  let%bind tvl_usd = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserveUSD in

  let spot_price = match Float.(reserve0 > 0.0) with true -> reserve1 /. reserve0 | false -> 0.0 in
  let spot_price_inv = match Float.(reserve1 > 0.0) with true -> reserve0 /. reserve1 | false -> 0.0 in
  let fee_bps = 20 in  (* SpookySwap uses 0.2% fee *)

  Ok {
    Pool_intf.Pool.
    id = pool.id; venue = venue;
    pool_type = Pool_intf.Pool_type.Constant_product;
    token0 = { Pool_intf.Token. address = pool.token0.id; symbol = pool.token0.symbol; decimals = decimals0 };
    token1 = { Pool_intf.Token. address = pool.token1.id; symbol = pool.token1.symbol; decimals = decimals1 };
    reserve0 = reserve0; reserve1 = reserve1;
    tvl_usd = tvl_usd; fee_bps = fee_bps;
    spot_price = spot_price; spot_price_inv = spot_price_inv;
  }

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  match String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol with
  | true when String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol ->
    Ok (reserve1 /. reserve0)
  | false -> Ok (reserve0 /. reserve1)
  | _ -> Error "Token pair does not match pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  let is_token0_in = String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol in
  let (reserve_in, reserve_out) = match is_token0_in with true -> (reserve0, reserve1) | false -> (reserve1, reserve0) in
  let _ = token_out in
  let params = Amm_pricing.Constant_product.{ reserve0 = reserve_in; reserve1 = reserve_out; fee_bps = 20 } in
  Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

(** Osmosis Pool Adapter

    Implements Pool_intf.S for Osmosis weighted pools (Cosmos).
*)

open Core

let venue = "osmosis"

module Native = struct
  type pool = Types.pool
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  match pool.pool_assets with
  | asset0 :: asset1 :: _ ->
    let%bind balance0 = Fluxum.Normalize_common.Float_conv.qty_of_string asset0.token.amount in
    let%bind balance1 = Fluxum.Normalize_common.Float_conv.qty_of_string asset1.token.amount in
    let%bind weight0 = Pool_common.Weighted.parse_weight asset0.weight in
    let%bind weight1 = Pool_common.Weighted.parse_weight asset1.weight in

    (* Weighted pool spot price: (B_out / W_out) / (B_in / W_in) *)
    let spot_price = (balance1 /. weight1) /. (balance0 /. weight0) in
    let spot_price_inv = (balance0 /. weight0) /. (balance1 /. weight1) in

    (* Osmosis uses 0.2-0.3% fees typically *)
    let fee_bps = 30 in

    Ok {
      Pool_intf.Pool.
      id = pool.id; venue = venue;
      pool_type = Pool_intf.Pool_type.Weighted;
      token0 = { Pool_intf.Token. address = asset0.token.denom; symbol = asset0.token.denom; decimals = 6 };
      token1 = { Pool_intf.Token. address = asset1.token.denom; symbol = asset1.token.denom; decimals = 6 };
      reserve0 = balance0; reserve1 = balance1;
      tvl_usd = 0.0; fee_bps = fee_bps;
      spot_price = spot_price; spot_price_inv = spot_price_inv;
    }
  | _ -> Error "Osmosis pool needs at least 2 assets"

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let open Result.Let_syntax in
  let find_asset denom = List.find pool.pool_assets ~f:(fun a -> String.equal a.token.denom denom) in
  match (find_asset token_in, find_asset token_out) with
  | (Some ain, Some aout) ->
    let%bind balance_in = Fluxum.Normalize_common.Float_conv.qty_of_string ain.token.amount in
    let%bind balance_out = Fluxum.Normalize_common.Float_conv.qty_of_string aout.token.amount in
    let%bind weight_in = Pool_common.Weighted.parse_weight ain.weight in
    let%bind weight_out = Pool_common.Weighted.parse_weight aout.weight in
    Ok ((balance_out /. weight_out) /. (balance_in /. weight_in))
  | _ -> Error "Token not found in pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let find_asset denom = List.find pool.pool_assets ~f:(fun a -> String.equal a.token.denom denom) in
  match (find_asset token_in, find_asset token_out) with
  | (Some ain, Some aout) ->
    let%bind balance_in_val = Fluxum.Normalize_common.Float_conv.qty_of_string ain.token.amount in
    let%bind weight_in_val = Pool_common.Weighted.parse_weight ain.weight in
    let%bind balance_out_val = Fluxum.Normalize_common.Float_conv.qty_of_string aout.token.amount in
    let%bind weight_out_val = Pool_common.Weighted.parse_weight aout.weight in
    let balance_in = Amm_pricing.Weighted.{
      balance = balance_in_val;
      weight = weight_in_val;
      decimals = 6;
    } in
    let balance_out = Amm_pricing.Weighted.{
      balance = balance_out_val;
      weight = weight_out_val;
      decimals = 6;
    } in
    let params = Amm_pricing.Weighted.{ tokens = [balance_in; balance_out]; swap_fee_pct = 0.3 } in
    Amm_pricing.Weighted.quote params ~balance_in ~balance_out ~amount_in ~pool_id:pool.id
  | _ -> Error "Token not found in pool"

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

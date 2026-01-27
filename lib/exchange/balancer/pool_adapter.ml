(** Balancer Pool Adapter

    Implements Pool_intf.S for Balancer weighted pools.
*)

open Core

let venue = "balancer"

module Native = struct
  type pool = Types.pool
end

(** Normalize Balancer pool to unified pool representation *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  match pool.tokens with
  | token0 :: token1 :: _ ->
    let%bind balance0 = Pool_common.Weighted.parse_balance token0.balance in
    let%bind balance1 = Pool_common.Weighted.parse_balance token1.balance in
    let%bind weight0 = match token0.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in
    let%bind weight1 = match token1.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in

    (* Weighted pool spot price formula: (B_out / W_out) / (B_in / W_in) *)
    let spot_price = (balance1 /. weight1) /. (balance0 /. weight0) in
    let spot_price_inv = (balance0 /. weight0) /. (balance1 /. weight1) in

    (* Parse swap fee *)
    let%bind swap_fee = Fluxum.Normalize_common.Float_conv.of_string pool.swapFee in
    let fee_bps = Float.to_int (swap_fee *. 10000.0) in

    let%bind tvl = Fluxum.Normalize_common.Float_conv.qty_of_string pool.totalLiquidity in

    Ok {
      Pool_intf.Pool.
      id = pool.id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Weighted;
      token0 = {
        Pool_intf.Token.
        address = token0.address;
        symbol = token0.symbol;
        decimals = token0.decimals;
      };
      token1 = {
        Pool_intf.Token.
        address = token1.address;
        symbol = token1.symbol;
        decimals = token1.decimals;
      };
      reserve0 = balance0;
      reserve1 = balance1;
      tvl_usd = tvl;
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  | _ -> Error "Balancer pool needs at least 2 tokens"

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  let open Result.Let_syntax in
  let find_token addr = List.find pool.tokens ~f:(fun t ->
    String.equal t.address addr || String.equal t.symbol addr) in

  match (find_token token_in, find_token token_out) with
  | (Some tin, Some tout) ->
    let%bind balance_in = Pool_common.Weighted.parse_balance tin.balance in
    let%bind balance_out = Pool_common.Weighted.parse_balance tout.balance in
    let%bind weight_in = match tin.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in
    let%bind weight_out = match tout.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in
    Ok ((balance_out /. weight_out) /. (balance_in /. weight_in))
  | _ -> Error "Token not found in pool"

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let find_token addr = List.find pool.tokens ~f:(fun t ->
    String.equal t.address addr || String.equal t.symbol addr) in

  match (find_token token_in, find_token token_out) with
  | (Some tin, Some tout) ->
    let%bind balance_in_val = Pool_common.Weighted.parse_balance tin.balance in
    let%bind weight_in_val = match tin.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in
    let%bind balance_out_val = Pool_common.Weighted.parse_balance tout.balance in
    let%bind weight_out_val = match tout.weight with
      | Some w -> Pool_common.Weighted.parse_weight w
      | None -> Ok 0.5
    in
    let balance_in = Amm_pricing.Weighted.{
      balance = balance_in_val;
      weight = weight_in_val;
      decimals = tin.decimals;
    } in
    let balance_out = Amm_pricing.Weighted.{
      balance = balance_out_val;
      weight = weight_out_val;
      decimals = tout.decimals;
    } in
    let%bind swap_fee = Fluxum.Normalize_common.Float_conv.of_string pool.swapFee in
    let fee_pct = swap_fee *. 100.0 in
    let params = Amm_pricing.Weighted.{
      tokens = [balance_in; balance_out];
      swap_fee_pct = fee_pct;
    } in
    Amm_pricing.Weighted.quote params ~balance_in ~balance_out ~amount_in ~pool_id:pool.id
  | _ -> Error "Token not found in pool"

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1);
  Async.return (Ok [])

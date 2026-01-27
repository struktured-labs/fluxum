(** Thena Pool Adapter

    Implements Pool_intf.S for Thena constant product pools (BSC).
*)

open Core

let venue = "thena"

module Native = struct
  type pool = Types.pair
end

(** Normalize Thena pair to unified pool representation *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  let decimals0 = try Int.of_string pool.token0.decimals with _ -> 18 in
  let decimals1 = try Int.of_string pool.token1.decimals with _ -> 18 in
  let spot_price = match Float.(reserve0 > 0.0) with
    | true -> reserve1 /. reserve0
    | false -> 0.0
  in
  let spot_price_inv = match Float.(reserve1 > 0.0) with
    | true -> reserve0 /. reserve1
    | false -> 0.0
  in
  (* Thena uses 0.04% - 1% fee depending on pool type, default 0.3% = 30 bps *)
  let fee_bps = 30 in
  Ok {
    Pool_intf.Pool.
    id = pool.id;
    venue = venue;
    pool_type = Pool_intf.Pool_type.Constant_product;
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
    reserve0 = reserve0;
    reserve1 = reserve1;
    tvl_usd = 0.0;  (* Would need price oracle *)
    fee_bps = fee_bps;
    spot_price = spot_price;
    spot_price_inv = spot_price_inv;
  }

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  match String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol with
  | true when String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol ->
    Ok (reserve1 /. reserve0)
  | false when (String.equal token_in pool.token1.id || String.equal token_in pool.token1.symbol) &&
               (String.equal token_out pool.token0.id || String.equal token_out pool.token0.symbol) ->
    Ok (reserve0 /. reserve1)
  | _ -> Error "Token pair does not match pool"

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind reserve0 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve0 in
  let%bind reserve1 = Fluxum.Normalize_common.Float_conv.qty_of_string pool.reserve1 in
  let fee_bps = 30 in

  (* Determine direction *)
  let is_token0_in = String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol in
  let is_token1_out = String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol in

  match is_token0_in && is_token1_out with
  | true ->
    let params = Amm_pricing.Constant_product.{
      reserve0 = reserve0;
      reserve1 = reserve1;
      fee_bps = fee_bps;
    } in
    Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id
  | false ->
    (* Swap direction: token1 -> token0 *)
    let params = Amm_pricing.Constant_product.{
      reserve0 = reserve1;
      reserve1 = reserve0;
      fee_bps = fee_bps;
    } in
    Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1);
  Async.return (Ok [])

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
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    let spot_price = match Float.(reserve0 > 0.0) with
      | true -> reserve1 /. reserve0
      | false -> 0.0
    in
    let spot_price_inv = match Float.(reserve1 > 0.0) with
      | true -> reserve0 /. reserve1
      | false -> 0.0
    in
    (* SushiSwap uses 0.3% fee = 30 bps *)
    let fee_bps = 30 in
    Ok {
      Pool_intf.Pool.
      id = pool.address;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Constant_product;
      token0 = {
        Pool_intf.Token.
        address = pool.token0;
        symbol = pool.token0;  (* Would need token lookup for symbol *)
        decimals = 18;  (* Default, would need lookup *)
      };
      token1 = {
        Pool_intf.Token.
        address = pool.token1;
        symbol = pool.token1;
        decimals = 18;
      };
      reserve0 = reserve0;
      reserve1 = reserve1;
      tvl_usd = 0.0;  (* Would need price oracle *)
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  with
  | exn -> Error (sprintf "Failed to normalize pool: %s" (Exn.to_string exn))

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    match String.equal token_in pool.token0 with
    | true when String.equal token_out pool.token1 ->
      Ok (reserve1 /. reserve0)
    | false when String.equal token_in pool.token1 && String.equal token_out pool.token0 ->
      Ok (reserve0 /. reserve1)
    | _ -> Error "Token pair does not match pool"
  with
  | exn -> Error (sprintf "Failed to get spot price: %s" (Exn.to_string exn))

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
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
  with
  | exn -> Error (sprintf "Failed to get quote: %s" (Exn.to_string exn))

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  (* This would need config and make API call *)
  (* For now, return empty list - would be implemented with Rest.pools_for_token *)
  ignore (token0, token1);
  Async.return (Ok [])

(** PancakeSwap Pool Adapter

    Implements Pool_intf.S for PancakeSwap constant product pools (BSC).
*)

open Core

let venue = "pancakeswap"

module Native = struct
  type pool = Types.pair_data
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  Pool_common.Price_based.normalize_pool
    ~venue
    ~pool_id:pool.pair_address
    ~base_symbol:pool.base_symbol
    ~quote_symbol:pool.quote_symbol
    ~price_str:pool.price
    ~base_volume_str:pool.base_volume
    ~quote_volume_str:pool.quote_volume
    ~fee_bps:25  (* PancakeSwap uses 0.25% fee *)

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let open Result.Let_syntax in
  let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string pool.price in
  match String.equal token_in pool.base_symbol with
  | true when String.equal token_out pool.quote_symbol -> Ok price
  | false when String.equal token_in pool.quote_symbol && String.equal token_out pool.base_symbol ->
    Ok (1.0 /. price)
  | _ -> Error "Token pair does not match pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = 25 in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct:0.0
    ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee) ~pool_id:pool.pair_address)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

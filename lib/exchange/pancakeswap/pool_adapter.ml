(** PancakeSwap Pool Adapter

    Implements Pool_intf.S for PancakeSwap constant product pools (BSC).
*)

open Core

let venue = "pancakeswap"

module Native = struct
  type pool = Types.pair_data
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    let price = Float.of_string pool.price in
    let base_volume = Float.of_string pool.base_volume in
    let quote_volume = Float.of_string pool.quote_volume in

    let spot_price = price in
    let spot_price_inv = match Float.(price > 0.0) with true -> 1.0 /. price | false -> 0.0 in

    (* PancakeSwap uses 0.25% fee = 25 bps *)
    let fee_bps = 25 in

    Ok {
      Pool_intf.Pool.
      id = pool.pair_address;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Constant_product;
      token0 = { Pool_intf.Token. address = pool.pair_address; symbol = pool.base_symbol; decimals = 18 };
      token1 = { Pool_intf.Token. address = pool.pair_address; symbol = pool.quote_symbol; decimals = 18 };
      reserve0 = base_volume;
      reserve1 = quote_volume;
      tvl_usd = base_volume +. quote_volume;
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize: %s" (Exn.to_string exn))

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  try
    let price = Float.of_string pool.price in
    match String.equal token_in pool.base_symbol with
    | true when String.equal token_out pool.quote_symbol -> Ok price
    | false when String.equal token_in pool.quote_symbol && String.equal token_out pool.base_symbol ->
      Ok (1.0 /. price)
    | _ -> Error "Token pair does not match pool"
  with exn -> Error (sprintf "Failed: %s" (Exn.to_string exn))

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

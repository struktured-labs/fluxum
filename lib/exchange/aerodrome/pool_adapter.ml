(** Aerodrome Pool Adapter

    Implements Pool_intf.S for Aerodrome concentrated liquidity pools (Base).
*)

open Core

let venue = "aerodrome"

module Native = struct
  type pool = Types.pool
end

let q96 : float = Float.ldexp 1.0 96

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    let decimals0 = Int.of_string pool.token0.decimals in
    let decimals1 = Int.of_string pool.token1.decimals in
    let _tick = Int.of_string pool.tick in
    let liquidity = Float.of_string pool.liquidity in

    let sqrt_price = Float.of_string pool.sqrtPrice /. q96 in
    let spot_price = sqrt_price *. sqrt_price *.
      Float.int_pow 10.0 (decimals1 - decimals0) in
    let spot_price_inv = match Float.(spot_price > 0.0) with
      | true -> 1.0 /. spot_price
      | false -> 0.0
    in

    (* Aerodrome uses 0.3% fee = 30 bps by default *)
    let fee_bps = 30 in

    Ok {
      Pool_intf.Pool.
      id = pool.id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Concentrated;
      token0 = { Pool_intf.Token. address = pool.token0.id; symbol = pool.token0.symbol; decimals = decimals0 };
      token1 = { Pool_intf.Token. address = pool.token1.id; symbol = pool.token1.symbol; decimals = decimals1 };
      reserve0 = liquidity /. (Float.sqrt spot_price);
      reserve1 = liquidity *. (Float.sqrt spot_price);
      tvl_usd = 0.0;
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize: %s" (Exn.to_string exn))

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let open Result.Let_syntax in
  let%bind normalized = normalize pool in
  match String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol with
  | true when String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol ->
    Ok normalized.spot_price
  | false -> Ok normalized.spot_price_inv
  | _ -> Error "Token pair does not match pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = 30 in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct:0.0
    ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee) ~pool_id:pool.id)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

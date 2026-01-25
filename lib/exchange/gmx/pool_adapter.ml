(** GMX Pool Adapter

    Implements Pool_intf.S for GMX GLP pools (Arbitrum/Avalanche).
    GMX uses a multi-asset GLP pool for spot swaps with oracle pricing.
*)

open Core

let venue = "gmx"

module Native = struct
  type pool = Types.glp_pool
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    (* GMX uses oracle prices, spot price is the ratio of USD prices *)
    let spot_price = match Float.(pool.token0_price > 0.0) with
      | true -> pool.token1_price /. pool.token0_price
      | false -> 0.0
    in
    let spot_price_inv = match Float.(pool.token1_price > 0.0) with
      | true -> pool.token0_price /. pool.token1_price
      | false -> 0.0
    in

    (* GMX swap fee is typically 0.2-0.5% depending on pool balance *)
    let fee_bps = pool.swap_fee_bps in

    (* TVL is sum of pool amounts in USD *)
    let tvl_usd = (pool.token0_pool_amount *. pool.token0_price) +.
                  (pool.token1_pool_amount *. pool.token1_price) in

    Ok {
      Pool_intf.Pool.
      id = pool.id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Weighted;  (* GLP is similar to weighted pool *)
      token0 = {
        Pool_intf.Token.
        address = pool.token0.id;
        symbol = pool.token0.symbol;
        decimals = pool.token0.decimals;
      };
      token1 = {
        Pool_intf.Token.
        address = pool.token1.id;
        symbol = pool.token1.symbol;
        decimals = pool.token1.decimals;
      };
      reserve0 = pool.token0_pool_amount;
      reserve1 = pool.token1_pool_amount;
      tvl_usd = tvl_usd;
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize: %s" (Exn.to_string exn))

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let is_token0_in = String.equal token_in pool.token0.id || String.equal token_in pool.token0.symbol in
  let is_token1_out = String.equal token_out pool.token1.id || String.equal token_out pool.token1.symbol in
  match is_token0_in && is_token1_out with
  | true -> Ok (pool.token1_price /. pool.token0_price)
  | false -> Ok (pool.token0_price /. pool.token1_price)

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = pool.swap_fee_bps in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  let fee_amount = amount_in *. fee in

  (* GMX has minimal price impact due to oracle pricing *)
  let price_impact_pct = 0.0 in

  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct
    ~effective_price:(amount_out /. amount_in) ~fee_amount ~pool_id:pool.id)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

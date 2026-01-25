(** TraderJoe Pool Adapter

    Implements Pool_intf.S for TraderJoe pools (Avalanche).
    Supports both classic pairs (constant product) and LB pairs (liquidity bins).
*)

open Core

let venue = "traderjoe"

module Native = struct
  type pool = Types.pair
  type lb_pool = Types.lb_pair
end

(** Normalize classic constant product pair *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in

    let spot_price = match Float.(reserve0 > 0.0) with true -> reserve1 /. reserve0 | false -> 0.0 in
    let spot_price_inv = match Float.(reserve1 > 0.0) with true -> reserve0 /. reserve1 | false -> 0.0 in
    let fee_bps = 30 in

    Ok {
      Pool_intf.Pool.
      id = pool.address; venue = venue;
      pool_type = Pool_intf.Pool_type.Constant_product;
      token0 = { Pool_intf.Token. address = pool.token0.address; symbol = pool.token0.symbol; decimals = pool.token0.decimals };
      token1 = { Pool_intf.Token. address = pool.token1.address; symbol = pool.token1.symbol; decimals = pool.token1.decimals };
      reserve0 = reserve0; reserve1 = reserve1;
      tvl_usd = pool.volumeUSD; fee_bps = fee_bps;
      spot_price = spot_price; spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize: %s" (Exn.to_string exn))

(** Normalize LB (liquidity bin) pair *)
let normalize_lb (pool : Native.lb_pool) : (Pool_intf.Pool.t, string) Result.t =
  try
    (* Get active bin price *)
    let active_bin = List.find pool.bins ~f:(fun b -> b.binId = pool.activeId) in
    let spot_price = match active_bin with
      | Some bin -> bin.price
      | None -> 0.0
    in
    let spot_price_inv = match Float.(spot_price > 0.0) with true -> 1.0 /. spot_price | false -> 0.0 in

    (* Sum reserves across all bins *)
    let (total_x, total_y) = List.fold pool.bins ~init:(0.0, 0.0) ~f:(fun (x, y) bin ->
      (x +. Float.of_string bin.reserveX, y +. Float.of_string bin.reserveY)) in

    (* LB uses dynamic fees based on bin step *)
    let fee_bps = pool.binStep in

    Ok {
      Pool_intf.Pool.
      id = pool.address; venue = venue;
      pool_type = Pool_intf.Pool_type.Liquidity_bin;
      token0 = { Pool_intf.Token. address = pool.token0.address; symbol = pool.token0.symbol; decimals = pool.token0.decimals };
      token1 = { Pool_intf.Token. address = pool.token1.address; symbol = pool.token1.symbol; decimals = pool.token1.decimals };
      reserve0 = total_x; reserve1 = total_y;
      tvl_usd = pool.volumeUSD; fee_bps = fee_bps;
      spot_price = spot_price; spot_price_inv = spot_price_inv;
    }
  with exn -> Error (sprintf "Failed to normalize LB: %s" (Exn.to_string exn))

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    match String.equal token_in pool.token0.address || String.equal token_in pool.token0.symbol with
    | true when String.equal token_out pool.token1.address || String.equal token_out pool.token1.symbol ->
      Ok (reserve1 /. reserve0)
    | false -> Ok (reserve0 /. reserve1)
    | _ -> Error "Token pair does not match pool"
  with exn -> Error (sprintf "Failed: %s" (Exn.to_string exn))

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  try
    let reserve0 = Float.of_string pool.reserve0 in
    let reserve1 = Float.of_string pool.reserve1 in
    let is_token0_in = String.equal token_in pool.token0.address || String.equal token_in pool.token0.symbol in
    let (reserve_in, reserve_out) = match is_token0_in with true -> (reserve0, reserve1) | false -> (reserve1, reserve0) in
    let _ = token_out in
    let params = Amm_pricing.Constant_product.{ reserve0 = reserve_in; reserve1 = reserve_out; fee_bps = 30 } in
    Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.address
  with exn -> Error (sprintf "Failed: %s" (Exn.to_string exn))

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

(** Raydium Pool Adapter

    Implements Pool_intf.S for Raydium pools (Solana).
*)

open Core

let venue = "raydium"

module Native = struct
  type pool = Types.pair_info
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let spot_price = pool.price in
  let spot_price_inv = match Float.(spot_price > 0.0) with true -> 1.0 /. spot_price | false -> 0.0 in

  (* Extract token symbols from name (e.g., "SOL-USDC" -> SOL, USDC) *)
  let tokens = String.split pool.name ~on:'-' in
  let (symbol0, symbol1) = match tokens with
    | [t0; t1] -> (t0, t1)
    | _ -> (pool.name, "USD")
  in

  (* Raydium uses 0.25% fee = 25 bps *)
  let fee_bps = 25 in

  Ok {
    Pool_intf.Pool.
    id = pool.ammId; venue = venue;
    pool_type = Pool_intf.Pool_type.Constant_product;
    token0 = { Pool_intf.Token. address = pool.lpMint; symbol = symbol0; decimals = 9 };
    token1 = { Pool_intf.Token. address = pool.lpMint; symbol = symbol1; decimals = 9 };
    reserve0 = pool.liquidity /. 2.0;
    reserve1 = pool.liquidity /. 2.0 *. spot_price;
    tvl_usd = pool.liquidity; fee_bps = fee_bps;
    spot_price = spot_price; spot_price_inv = spot_price_inv;
  }

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  let tokens = String.split pool.name ~on:'-' in
  match tokens with
  | [t0; t1] when String.equal token_in t0 && String.equal token_out t1 -> Ok pool.price
  | [t0; t1] when String.equal token_in t1 && String.equal token_out t0 -> Ok (1.0 /. pool.price)
  | _ -> Error "Token pair does not match pool"

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = 25 in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct:0.0
    ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee) ~pool_id:pool.ammId)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

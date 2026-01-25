(** Bitfinex Pool Adapter

    Models CEX order book liquidity as a synthetic pool.
*)

open Core

let venue = "bitfinex"

module Native = struct
  (** Synthetic pool from order book depth *)
  type pool = {
    symbol : string;
    bid_price : float;
    bid_size : float;
    ask_price : float;
    ask_size : float;
  }
end

let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let mid_price = (pool.bid_price +. pool.ask_price) /. 2.0 in
  let spot_price = mid_price in
  let spot_price_inv = match Float.(mid_price > 0.0) with true -> 1.0 /. mid_price | false -> 0.0 in
  let fee_bps = 10 in (* Bitfinex maker fee 0.1% *)
  Ok {
    Pool_intf.Pool.
    id = pool.symbol; venue = venue;
    pool_type = Pool_intf.Pool_type.Constant_product;
    token0 = { Pool_intf.Token. address = pool.symbol; symbol = "BASE"; decimals = 8 };
    token1 = { Pool_intf.Token. address = pool.symbol; symbol = "QUOTE"; decimals = 8 };
    reserve0 = pool.bid_size; reserve1 = pool.ask_size *. mid_price;
    tvl_usd = (pool.bid_size +. pool.ask_size) *. mid_price;
    fee_bps = fee_bps; spot_price = spot_price; spot_price_inv = spot_price_inv;
  }

let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string) : (float, string) Result.t =
  ignore (token_in, token_out);
  Ok ((pool.bid_price +. pool.ask_price) /. 2.0)

let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string) : (Pool_intf.Quote.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind spot = spot_price pool ~token_in ~token_out in
  let fee_bps = 10 in
  let fee = Float.of_int fee_bps /. 10000.0 in
  let amount_out = amount_in *. spot *. (1.0 -. fee) in
  Ok (Pool_intf.Quote.single ~amount_in ~amount_out ~price_impact_pct:0.0
    ~effective_price:(amount_out /. amount_in) ~fee_amount:(amount_in *. fee) ~pool_id:pool.symbol)

let pools_for_pair ~(token0 : string) ~(token1 : string) : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1); Async.return (Ok [])

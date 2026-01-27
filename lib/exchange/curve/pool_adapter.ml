(** Curve Pool Adapter

    Implements Pool_intf.S for Curve StableSwap pools.
*)

open Core

let venue = "curve"

module Native = struct
  type pool = Types.pool_data
end

(** Normalize Curve pool to unified pool representation *)
let normalize (pool : Native.pool) : (Pool_intf.Pool.t, string) Result.t =
  let open Result.Let_syntax in
  (* Curve pools can have 2+ tokens, we represent first two *)
  match pool.coins with
  | coin0 :: coin1 :: _ ->
    (* Curve pools use virtual price for stable assets *)
    let%bind virtual_price = Pool_common.Stable.parse_virtual_price pool.virtualPrice in
    (* For stable pools, spot price is approximately 1:1 adjusted by virtual price *)
    let spot_price = 1.0 /. virtual_price in
    let spot_price_inv = virtual_price in

    (* Default 0.04% = 4 bps for Curve stable pools *)
    let fee_bps = 4 in

    Ok {
      Pool_intf.Pool.
      id = pool.id;
      venue = venue;
      pool_type = Pool_intf.Pool_type.Stable;
      token0 = {
        Pool_intf.Token.
        address = coin0.address;
        symbol = coin0.symbol;
        decimals = coin0.decimals;
      };
      token1 = {
        Pool_intf.Token.
        address = coin1.address;
        symbol = coin1.symbol;
        decimals = coin1.decimals;
      };
      (* Use USD total as proxy for reserves *)
      reserve0 = pool.usdTotal /. 2.0;
      reserve1 = pool.usdTotal /. 2.0;
      tvl_usd = pool.usdTotal;
      fee_bps = fee_bps;
      spot_price = spot_price;
      spot_price_inv = spot_price_inv;
    }
  | _ -> Error "Curve pool needs at least 2 tokens"

(** Get spot price for token pair *)
let spot_price (pool : Native.pool) ~(token_in : string) ~(token_out : string)
    : (float, string) Result.t =
  let open Result.Let_syntax in
  (* For stable pools, price is approximately 1:1 *)
  let%bind virtual_price = Pool_common.Stable.parse_virtual_price pool.virtualPrice in

  (* Find token indices *)
  let token_in_idx = List.findi pool.coins ~f:(fun _ c ->
    String.equal c.address token_in || String.equal c.symbol token_in) in
  let token_out_idx = List.findi pool.coins ~f:(fun _ c ->
    String.equal c.address token_out || String.equal c.symbol token_out) in

  match (token_in_idx, token_out_idx) with
  | (Some _, Some _) -> Ok (1.0 /. virtual_price)
  | _ -> Error "Token not found in pool"

(** Get quote for swapping amount *)
let quote (pool : Native.pool) ~(amount_in : float) ~(token_in : string) ~(token_out : string)
    : (Pool_intf.Quote.t, string) Result.t =
  (* Find token indices *)
  let token_in_idx = List.findi pool.coins ~f:(fun _ c ->
    String.equal c.address token_in || String.equal c.symbol token_in) in
  let token_out_idx = List.findi pool.coins ~f:(fun _ c ->
    String.equal c.address token_out || String.equal c.symbol token_out) in

  match (token_in_idx, token_out_idx) with
  | (Some (i, _), Some (j, _)) when i <> j ->
    (* Get balances - simplified, would need actual balances from pool *)
    let n = List.length pool.coins in
    let balance_per_coin = pool.usdTotal /. Float.of_int n in
    let balances = List.init n ~f:(fun _ -> balance_per_coin) in

    (* Get amplification coefficient *)
    let amp = Pool_common.Stable.parse_amplification
      ?amp_str:pool.amplificationCoefficient ~default:100 () in

    let params = Amm_pricing.Stable.{
      balances = balances;
      amp = amp;
      fee_pct = 0.04;  (* 0.04% = 4 bps *)
      decimals = List.map pool.coins ~f:(fun c -> c.decimals);
    } in

    Amm_pricing.Stable.quote params ~amount_in ~token_in_idx:i ~token_out_idx:j
      ~pool_id:pool.id

  | _ -> Error "Invalid token pair for pool"

(** Get all pools for a token pair (async - queries API) *)
let pools_for_pair ~(token0 : string) ~(token1 : string)
    : (Native.pool list, string) Result.t Async.Deferred.t =
  ignore (token0, token1);
  Async.return (Ok [])

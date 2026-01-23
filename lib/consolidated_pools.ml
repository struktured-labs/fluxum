(** Consolidated Pools - Aggregate DEX liquidity for best execution

    This module aggregates liquidity pools from multiple DEXes and provides:
    - Best price discovery across all pools
    - Ranked quotes for execution
    - Liquidity depth analysis
    - Multi-pool routing

    Mirrors the consolidated_order_book.ml pattern for CEX order books,
    but adapted for AMM pool-based pricing.

    @see Pool_intf for the unified pool abstraction
    @see Amm_pricing for pricing formulas
*)

open Core

(** Token pair key for pool lookup *)
module Pair_key = struct
  module T = struct
    type t = string * string [@@deriving sexp, compare, hash]
  end
  include T
  include Comparator.Make(T)
end

(** Pool with venue attribution *)
module Attributed_pool = struct
  type t = {
    venue : string;
    pool : Pool_intf.Pool.t;
  } [@@deriving sexp, compare, equal, fields]

  let create ~venue ~pool = { venue; pool }

  (** Get spot price from this pool *)
  let spot_price t = t.pool.Pool_intf.Pool.spot_price

  (** Compare by spot price (lower is better for buying token1) *)
  let compare_by_price a b =
    Float.compare (spot_price a) (spot_price b)
end

(** Quote with venue attribution *)
module Attributed_quote = struct
  type t = {
    venue : string;
    pool_id : string;
    quote : Pool_intf.Quote.t;
  } [@@deriving sexp, compare, equal, fields]

  let create ~venue ~pool_id ~quote = { venue; pool_id; quote }

  (** Effective price from quote *)
  let effective_price t = t.quote.Pool_intf.Quote.effective_price

  (** Compare by effective price (higher is better - more output per input) *)
  let compare_by_effective_price a b =
    (* Reverse order - higher effective price is better *)
    Float.compare (effective_price b) (effective_price a)
end

(** Aggregated liquidity metrics for a token pair *)
module Pair_liquidity = struct
  type t = {
    token0 : string;
    token1 : string;
    total_tvl_usd : float;
    num_pools : int;
    best_spot_price : float;
    best_venue : string;
    weighted_avg_price : float;
  } [@@deriving sexp, compare, equal, fields]
end

(** Consolidated pools state *)
type t = {
  (* Map from (token0, token1) -> list of pools *)
  pools_by_pair : (Pair_key.t, Attributed_pool.t list) Hashtbl.t;
  (* Map from pool_id -> pool for fast lookup *)
  pools_by_id : (string, Attributed_pool.t) Hashtbl.t;
  (* Last update time *)
  mutable update_time : Time_float_unix.t;
  (* Statistics *)
  mutable total_pools : int;
  mutable total_tvl_usd : float;
}

(** Create empty consolidated pools *)
let create () : t = {
  pools_by_pair = Hashtbl.create (module Pair_key);
  pools_by_id = Hashtbl.create (module String);
  update_time = Time_float_unix.now ();
  total_pools = 0;
  total_tvl_usd = 0.0;
}

(** Add a normalized pool to the consolidated state *)
let add_pool (t : t) ~(pool : Pool_intf.Pool.t) : unit =
  let venue = pool.venue in
  let attributed = Attributed_pool.create ~venue ~pool in

  (* Add to pair index (both directions) *)
  let key0 = (pool.token0.symbol, pool.token1.symbol) in
  let key1 = (pool.token1.symbol, pool.token0.symbol) in

  Hashtbl.update t.pools_by_pair key0 ~f:(function
    | None -> [attributed]
    | Some existing -> attributed :: existing);

  (* For reverse direction, we'd need to flip the pool - skip for now *)
  ignore key1;

  (* Add to ID index *)
  Hashtbl.set t.pools_by_id ~key:pool.id ~data:attributed;

  (* Update statistics *)
  t.total_pools <- t.total_pools + 1;
  t.total_tvl_usd <- t.total_tvl_usd +. pool.tvl_usd;
  t.update_time <- Time_float_unix.now ()

(** Remove a pool by ID *)
let remove_pool (t : t) ~(pool_id : string) : unit =
  match Hashtbl.find t.pools_by_id pool_id with
  | None -> ()
  | Some ap ->
    let pool = ap.pool in
    let key = (pool.token0.symbol, pool.token1.symbol) in

    (* Remove from pair index *)
    Hashtbl.update t.pools_by_pair key ~f:(function
      | None -> []
      | Some pools -> List.filter pools ~f:(fun p -> not (String.equal p.pool.id pool_id)));

    (* Remove from ID index *)
    Hashtbl.remove t.pools_by_id pool_id;

    (* Update statistics *)
    t.total_pools <- t.total_pools - 1;
    t.total_tvl_usd <- t.total_tvl_usd -. pool.tvl_usd;
    t.update_time <- Time_float_unix.now ()

(** Get all pools for a token pair *)
let get_pools (t : t) ~(token0 : string) ~(token1 : string) : Attributed_pool.t list =
  Hashtbl.find t.pools_by_pair (token0, token1)
  |> Option.value ~default:[]

(** Get pool by ID *)
let get_pool_by_id (t : t) ~(pool_id : string) : Attributed_pool.t option =
  Hashtbl.find t.pools_by_id pool_id

(** Best spot price across all pools for a pair *)
let best_price (t : t) ~(token0 : string) ~(token1 : string) : Attributed_pool.t option =
  let pools = get_pools t ~token0 ~token1 in
  match pools with
  | [] -> None
  | pools ->
    (* Sort by spot price and return best *)
    List.min_elt pools ~compare:Attributed_pool.compare_by_price

(** Calculate quote for a specific pool *)
let quote_for_pool (ap : Attributed_pool.t) ~(amount_in : float) : Attributed_quote.t option =
  let pool = ap.pool in
  let pool_type = pool.Pool_intf.Pool.pool_type in

  (* Use appropriate pricing based on pool type *)
  let quote_result = match pool_type with
    | Pool_intf.Pool_type.Constant_product ->
      let params = Amm_pricing.Constant_product.{
        reserve0 = pool.reserve0;
        reserve1 = pool.reserve1;
        fee_bps = pool.fee_bps;
      } in
      Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id

    | Pool_intf.Pool_type.Stable ->
      (* For stable pools, assume 2-token pool with indices 0,1 *)
      let params = Amm_pricing.Stable.{
        balances = [pool.reserve0; pool.reserve1];
        amp = 100;  (* Default amplification *)
        fee_pct = Float.of_int pool.fee_bps /. 100.0;
        decimals = [pool.token0.decimals; pool.token1.decimals];
      } in
      Amm_pricing.Stable.quote params ~amount_in ~token_in_idx:0 ~token_out_idx:1 ~pool_id:pool.id

    | Pool_intf.Pool_type.Weighted ->
      (* For weighted pools, assume equal weights as default *)
      let balance_in = Amm_pricing.Weighted.{
        balance = pool.reserve0;
        weight = 0.5;
        decimals = pool.token0.decimals;
      } in
      let balance_out = Amm_pricing.Weighted.{
        balance = pool.reserve1;
        weight = 0.5;
        decimals = pool.token1.decimals;
      } in
      let params = Amm_pricing.Weighted.{
        tokens = [balance_in; balance_out];
        swap_fee_pct = Float.of_int pool.fee_bps /. 100.0;
      } in
      Amm_pricing.Weighted.quote params ~balance_in ~balance_out ~amount_in ~pool_id:pool.id

    | Pool_intf.Pool_type.Concentrated ->
      (* Simplified constant product fallback for concentrated *)
      let params = Amm_pricing.Constant_product.{
        reserve0 = pool.reserve0;
        reserve1 = pool.reserve1;
        fee_bps = pool.fee_bps;
      } in
      Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id

    | Pool_intf.Pool_type.Liquidity_bin ->
      (* Simplified constant product fallback for LB *)
      let params = Amm_pricing.Constant_product.{
        reserve0 = pool.reserve0;
        reserve1 = pool.reserve1;
        fee_bps = pool.fee_bps;
      } in
      Amm_pricing.Constant_product.quote params ~amount_in ~pool_id:pool.id
  in

  match quote_result with
  | Error _ -> None
  | Ok quote ->
    Some (Attributed_quote.create ~venue:ap.venue ~pool_id:pool.id ~quote)

(** Get ranked quotes from all pools for a pair, sorted by effective price *)
let best_quotes (t : t) ~(amount : float) ~(token_in : string) ~(token_out : string)
    : Attributed_quote.t list =
  let pools = get_pools t ~token0:token_in ~token1:token_out in
  pools
  |> List.filter_map ~f:(fun ap -> quote_for_pool ap ~amount_in:amount)
  |> List.sort ~compare:Attributed_quote.compare_by_effective_price

(** Get the single best quote for a trade *)
let best_quote (t : t) ~(amount : float) ~(token_in : string) ~(token_out : string)
    : Attributed_quote.t option =
  best_quotes t ~amount ~token_in ~token_out
  |> List.hd

(** Calculate total depth available before hitting max price impact *)
let depth_at_impact (t : t) ~(token0 : string) ~(token1 : string) ~(max_impact_pct : float) : float =
  let pools = get_pools t ~token0 ~token1 in
  List.sum (module Float) pools ~f:(fun ap ->
    let pool = ap.pool in
    match pool.pool_type with
    | Pool_intf.Pool_type.Constant_product
    | Pool_intf.Pool_type.Concentrated
    | Pool_intf.Pool_type.Liquidity_bin ->
      let params = Amm_pricing.Constant_product.{
        reserve0 = pool.reserve0;
        reserve1 = pool.reserve1;
        fee_bps = pool.fee_bps;
      } in
      (match Amm_pricing.Constant_product.depth_at_impact params ~max_impact_pct with
       | Ok depth -> depth
       | Error _ -> 0.0)
    | Pool_intf.Pool_type.Stable | Pool_intf.Pool_type.Weighted ->
      (* For stable/weighted, use a simpler heuristic *)
      pool.reserve0 *. (max_impact_pct /. 100.0))

(** Get aggregated liquidity metrics for a pair *)
let pair_liquidity (t : t) ~(token0 : string) ~(token1 : string) : Pair_liquidity.t option =
  let pools = get_pools t ~token0 ~token1 in
  match pools with
  | [] -> None
  | pools ->
    let total_tvl = List.sum (module Float) pools ~f:(fun ap -> ap.pool.tvl_usd) in
    let num_pools = List.length pools in

    (* Find best price *)
    let best = List.min_elt pools ~compare:Attributed_pool.compare_by_price in
    let best_spot_price, best_venue = match best with
      | Some ap -> (ap.pool.spot_price, ap.venue)
      | None -> (0.0, "")
    in

    (* Calculate TVL-weighted average price *)
    let weighted_sum = List.fold pools ~init:0.0 ~f:(fun acc ap ->
      acc +. (ap.pool.spot_price *. ap.pool.tvl_usd)) in
    let weighted_avg = match Float.(total_tvl > 0.0) with
      | true -> weighted_sum /. total_tvl
      | false -> 0.0
    in

    Some {
      Pair_liquidity.
      token0 = token0;
      token1 = token1;
      total_tvl_usd = total_tvl;
      num_pools = num_pools;
      best_spot_price = best_spot_price;
      best_venue = best_venue;
      weighted_avg_price = weighted_avg;
    }

(** Get all unique token pairs with pools *)
let all_pairs (t : t) : (string * string) list =
  Hashtbl.keys t.pools_by_pair

(** Get all venues with pools *)
let all_venues (t : t) : string list =
  Hashtbl.data t.pools_by_id
  |> List.map ~f:(fun ap -> ap.venue)
  |> List.dedup_and_sort ~compare:String.compare

(** Get statistics *)
let stats (t : t) : (string * string) list = [
  ("total_pools", Int.to_string t.total_pools);
  ("total_tvl_usd", sprintf "%.2f" t.total_tvl_usd);
  ("num_pairs", Int.to_string (Hashtbl.length t.pools_by_pair));
  ("num_venues", Int.to_string (List.length (all_venues t)));
  ("last_update", Time_float_unix.to_string t.update_time);
]

(** Multi-pool split routing for large trades

    Splits a large trade across multiple pools to minimize price impact.
    Returns list of (pool_id, amount) pairs.
*)
let split_route (t : t) ~(amount : float) ~(token_in : string) ~(token_out : string)
    ~(max_splits : int) : (string * float * Attributed_quote.t) list =
  let pools = get_pools t ~token0:token_in ~token1:token_out in

  match List.length pools <= 1 || max_splits <= 1 with
  | true ->
    (* Single pool or no splitting allowed *)
    (match best_quote t ~amount ~token_in ~token_out with
     | Some q -> [(q.pool_id, amount, q)]
     | None -> [])
  | false ->
    (* Simple equal-split strategy across top pools by TVL *)
    let sorted_by_tvl = List.sort pools ~compare:(fun a b ->
      Float.compare b.pool.tvl_usd a.pool.tvl_usd) in
    let top_pools = List.take sorted_by_tvl max_splits in
    let num_pools = List.length top_pools in
    let amount_per_pool = amount /. Float.of_int num_pools in

    List.filter_map top_pools ~f:(fun ap ->
      match quote_for_pool ap ~amount_in:amount_per_pool with
      | Some q -> Some (q.pool_id, amount_per_pool, q)
      | None -> None)

(** Arbitrage detection between pools for the same pair *)
let find_arbitrage (t : t) ~(token0 : string) ~(token1 : string) ~(min_profit_bps : int)
    : (Attributed_pool.t * Attributed_pool.t * float) list =
  let pools = get_pools t ~token0 ~token1 in
  let pool_pairs = List.concat_map pools ~f:(fun p1 ->
    List.filter_map pools ~f:(fun p2 ->
      match String.equal p1.pool.id p2.pool.id with
      | true -> None
      | false -> Some (p1, p2))) in

  List.filter_map pool_pairs ~f:(fun (p1, p2) ->
    let price1 = p1.pool.spot_price in
    let price2 = p2.pool.spot_price in
    let spread_bps = Float.abs ((price1 -. price2) /. price1) *. 10000.0 in
    match Float.(spread_bps > of_int min_profit_bps) with
    | true -> Some (p1, p2, spread_bps)
    | false -> None)

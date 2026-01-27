(** Unified Pool Interface for DEXes

    All DEX adapters implement this signature for consolidated liquidity access.
    This provides a unified abstraction over different AMM types:
    - Constant Product (x*y=k): SushiSwap, Thena, TraderJoe Classic
    - Concentrated Liquidity: Uniswap V3, Aerodrome, Orca
    - Liquidity Bins: TraderJoe LB
    - Weighted Pools: Balancer, Osmosis
    - Stable Pools: Curve StableSwap

    @see <https://docs.uniswap.org/> for concentrated liquidity math
    @see <https://curve.readthedocs.io/> for StableSwap math
*)

open Core

(** Pool type classification for AMM math selection *)
module Pool_type = struct
  type t =
    | Constant_product  (** x*y=k: SushiSwap, Thena, classic AMMs *)
    | Concentrated      (** Uniswap V3, Aerodrome, Orca CLMM *)
    | Liquidity_bin     (** TraderJoe LB with discrete bins *)
    | Weighted          (** Balancer, Osmosis weighted pools *)
    | Stable            (** Curve StableSwap invariant *)
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Constant_product -> "constant_product"
    | Concentrated -> "concentrated"
    | Liquidity_bin -> "liquidity_bin"
    | Weighted -> "weighted"
    | Stable -> "stable"

  let of_string = function
    | "constant_product" | "xy=k" -> Some Constant_product
    | "concentrated" | "clmm" | "v3" -> Some Concentrated
    | "liquidity_bin" | "lb" -> Some Liquidity_bin
    | "weighted" -> Some Weighted
    | "stable" | "stableswap" -> Some Stable
    | _ -> None
end

(** Token information in a pool *)
module Token = struct
  type t = {
    address : string;
    symbol : string;
    decimals : int;
  } [@@deriving sexp, compare, equal, fields]

  let create ~address ~symbol ~decimals = { address; symbol; decimals }
end

(** Normalized pool representation *)
module Pool = struct
  type t = {
    id : string;                (** Pool address/ID *)
    venue : string;             (** Exchange name (e.g., "sushiswap", "uniswap_v3") *)
    pool_type : Pool_type.t;
    token0 : Token.t;
    token1 : Token.t;
    reserve0 : float;           (** Token0 reserve (normalized to float) *)
    reserve1 : float;           (** Token1 reserve (normalized to float) *)
    tvl_usd : float;            (** Total value locked in USD *)
    fee_bps : int;              (** Fee in basis points (30 = 0.30%) *)
    spot_price : float;         (** Current price: token0 -> token1 *)
    spot_price_inv : float;     (** Inverse price: token1 -> token0 *)
  } [@@deriving sexp, compare, equal, fields]

  let create ~id ~venue ~pool_type ~token0 ~token1 ~reserve0 ~reserve1
      ~tvl_usd ~fee_bps ~spot_price ~spot_price_inv =
    { id; venue; pool_type; token0; token1; reserve0; reserve1;
      tvl_usd; fee_bps; spot_price; spot_price_inv }
end

(** Quote result from pool pricing *)
module Quote = struct
  type t = {
    amount_in : float;          (** Input amount *)
    amount_out : float;         (** Output amount after swap *)
    price_impact_pct : float;   (** Price impact as percentage *)
    effective_price : float;    (** Actual execution price *)
    fee_amount : float;         (** Fee paid *)
    route : string list;        (** Route for multi-hop (pool IDs) *)
  } [@@deriving sexp, compare, equal, fields]

  let create ~amount_in ~amount_out ~price_impact_pct ~effective_price
      ~fee_amount ~route =
    { amount_in; amount_out; price_impact_pct; effective_price;
      fee_amount; route }

  (** Create quote for single-hop swap *)
  let single ~amount_in ~amount_out ~price_impact_pct ~effective_price
      ~fee_amount ~pool_id =
    create ~amount_in ~amount_out ~price_impact_pct ~effective_price
      ~fee_amount ~route:[pool_id]
end

(** Pool interface signature that DEX adapters must implement *)
module type S = sig
  (** Venue identifier for this pool adapter *)
  val venue : string

  (** Native pool type from the DEX *)
  module Native : sig
    type pool
  end

  (** Get spot price for a token pair in the pool *)
  val spot_price : Native.pool -> token_in:string -> token_out:string ->
    (float, string) Result.t

  (** Get quote for swapping a specific amount *)
  val quote : Native.pool -> amount_in:float -> token_in:string -> token_out:string ->
    (Quote.t, string) Result.t

  (** Normalize native pool to unified Pool.t representation *)
  val normalize : Native.pool -> (Pool.t, string) Result.t

  (** Optional: Get all pools for a token pair *)
  val pools_for_pair : token0:string -> token1:string ->
    (Native.pool list, string) Result.t Async.Deferred.t
end

(** Helper for constant product AMM math *)
module Constant_product_math = struct
  (** Spot price = reserve1 / reserve0 *)
  let spot_price ~reserve0 ~reserve1 : float =
    match Float.(reserve0 > 0.0) with
    | true -> reserve1 /. reserve0
    | false -> 0.0

  (** Calculate output amount for constant product x*y=k *)
  let amount_out ~reserve0 ~reserve1 ~amount_in ~fee_bps : float =
    let fee = Float.of_int fee_bps /. 10000.0 in
    let amount_in_after_fee = amount_in *. (1.0 -. fee) in
    let k = reserve0 *. reserve1 in
    let new_reserve0 = reserve0 +. amount_in_after_fee in
    let new_reserve1 = k /. new_reserve0 in
    reserve1 -. new_reserve1

  (** Create a full quote for constant product swap *)
  let quote ~reserve0 ~reserve1 ~amount_in ~fee_bps ~pool_id : Quote.t =
    let fee = Float.of_int fee_bps /. 10000.0 in
    let fee_amount = amount_in *. fee in
    let amount_out = amount_out ~reserve0 ~reserve1 ~amount_in ~fee_bps in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> 0.0
    in
    let spot = spot_price ~reserve0 ~reserve1 in
    let price_impact_pct = match Float.(spot > 0.0) with
      | true -> ((spot -. effective_price) /. spot) *. 100.0
      | false -> 0.0
    in
    Quote.single ~amount_in ~amount_out ~price_impact_pct ~effective_price
      ~fee_amount ~pool_id

  (** Calculate depth (max trade size) before hitting max price impact *)
  let depth_at_impact ~reserve0 ~reserve1:_ ~max_impact_pct ~fee_bps : float =
    (* For constant product: impact = 2 * amount_in / (reserve0 + amount_in)
       Solving for amount_in: amount_in = impact * reserve0 / (2 - impact) *)
    let impact = max_impact_pct /. 100.0 in
    let fee = Float.of_int fee_bps /. 10000.0 in
    match Float.(impact < 2.0) with
    | true ->
      let raw_amount = impact *. reserve0 /. (2.0 -. impact) in
      raw_amount /. (1.0 -. fee)  (* Gross up for fee *)
    | false -> reserve0  (* Max is all of reserve0 *)
end

(** Helper for concentrated liquidity math (Uniswap V3 style) *)
module Concentrated_math = struct
  (** Q96 constant = 2^96 for sqrtPriceX96 conversion *)
  let q96 : float = Float.ldexp 1.0 96

  (** Convert sqrtPriceX96 to price (safe version returning Result) *)
  let price_from_sqrt_price_x96_result ~sqrt_price_x96 ~decimals0 ~decimals1
      : (float, string) Result.t =
    try
      let sqrt_price = Float.of_string sqrt_price_x96 /. q96 in
      let raw_price = sqrt_price *. sqrt_price in
      let decimal_adjustment = Float.int_pow 10.0 (decimals1 - decimals0) in
      Ok (raw_price *. decimal_adjustment)
    with _ -> Error (sprintf "Invalid sqrtPriceX96: %s" sqrt_price_x96)

  (** Convert sqrtPriceX96 to price (legacy - returns 0.0 on error) *)
  let price_from_sqrt_price_x96 ~sqrt_price_x96 ~decimals0 ~decimals1 : float =
    match price_from_sqrt_price_x96_result ~sqrt_price_x96 ~decimals0 ~decimals1 with
    | Ok price -> price
    | Error _ -> 0.0

  (** Convert tick to price *)
  let price_from_tick ~tick ~decimals0 ~decimals1 : float =
    let base = 1.0001 in
    let raw_price = Float.(base ** of_int tick) in
    let decimal_adjustment = Float.int_pow 10.0 (decimals1 - decimals0) in
    raw_price *. decimal_adjustment
end

(** Helper for stable swap math (Curve style) *)
module Stable_math = struct
  (** Calculate D (invariant) for StableSwap *)
  let get_d ~balances ~amp : float =
    let n = List.length balances in
    let n_f = Float.of_int n in
    let sum = List.fold balances ~init:0.0 ~f:(+.) in
    let ann = Float.of_int amp *. Float.int_pow n_f n in
    (* Newton iteration to find D *)
    let rec newton d prev_d iter =
      match iter > 255 || Float.(abs (d -. prev_d) <= 1.0) with
      | true -> d
      | false ->
        let d_p = List.fold balances ~init:d ~f:(fun acc x ->
          acc *. d /. (x *. n_f)) in
        let new_d = (ann *. sum +. d_p *. n_f) *. d /.
          ((ann -. 1.0) *. d +. (n_f +. 1.0) *. d_p) in
        newton new_d d (iter + 1)
    in
    newton sum sum 0
end

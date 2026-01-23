(** AMM Pricing Formulas

    Implements pricing calculations for each AMM pool type.
    Used by DEX adapters for quote generation and price impact estimation.

    Pool Types:
    - Constant Product (x*y=k): Most common, simple math
    - Concentrated Liquidity: Uniswap V3 style with tick ranges
    - Liquidity Bins: TraderJoe LB with discrete price bins
    - Weighted Pools: Balancer style with token weights
    - Stable Pools: Curve StableSwap for stable assets

    @see <https://uniswap.org/whitepaper.pdf>
    @see <https://uniswap.org/whitepaper-v3.pdf>
    @see <https://curve.fi/files/stableswap-paper.pdf>
    @see <https://balancer.fi/whitepaper.pdf>
*)

open Core

(** Result type for pricing operations *)
type 'a pricing_result = ('a, string) Result.t

(** Constant Product AMM (x * y = k)

    Used by: SushiSwap, Thena, TraderJoe Classic, PancakeSwap, Quickswap
*)
module Constant_product = struct
  type params = {
    reserve0 : float;
    reserve1 : float;
    fee_bps : int;
  }

  (** Spot price = reserve1 / reserve0 *)
  let spot_price (p : params) : float pricing_result =
    match Float.(p.reserve0 > 0.0) with
    | true -> Ok (p.reserve1 /. p.reserve0)
    | false -> Error "reserve0 is zero"

  (** Calculate output amount for a swap *)
  let get_amount_out (p : params) ~(amount_in : float) : float pricing_result =
    match Float.(p.reserve0 > 0.0 && p.reserve1 > 0.0 && amount_in >= 0.0) with
    | false -> Error "invalid reserves or amount"
    | true ->
      let fee = Float.of_int p.fee_bps /. 10000.0 in
      let amount_in_with_fee = amount_in *. (1.0 -. fee) in
      let k = p.reserve0 *. p.reserve1 in
      let new_reserve0 = p.reserve0 +. amount_in_with_fee in
      let new_reserve1 = k /. new_reserve0 in
      let amount_out = p.reserve1 -. new_reserve1 in
      Ok amount_out

  (** Calculate input amount required for desired output *)
  let get_amount_in (p : params) ~(amount_out : float) : float pricing_result =
    match Float.(p.reserve0 > 0.0 && p.reserve1 > amount_out && amount_out >= 0.0) with
    | false -> Error "invalid reserves or output amount"
    | true ->
      let fee = Float.of_int p.fee_bps /. 10000.0 in
      let k = p.reserve0 *. p.reserve1 in
      let new_reserve1 = p.reserve1 -. amount_out in
      let new_reserve0 = k /. new_reserve1 in
      let amount_in_before_fee = new_reserve0 -. p.reserve0 in
      let amount_in = amount_in_before_fee /. (1.0 -. fee) in
      Ok amount_in

  (** Price impact percentage for a given trade *)
  let price_impact (p : params) ~(amount_in : float) : float pricing_result =
    let open Result.Let_syntax in
    let%bind spot = spot_price p in
    let%bind amount_out = get_amount_out p ~amount_in in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> spot
    in
    let impact = match Float.(spot > 0.0) with
      | true -> ((spot -. effective_price) /. spot) *. 100.0
      | false -> 0.0
    in
    Ok (Float.abs impact)

  (** Maximum trade size before hitting max_impact_pct *)
  let depth_at_impact (p : params) ~(max_impact_pct : float) : float pricing_result =
    match Float.(p.reserve0 > 0.0 && max_impact_pct > 0.0 && max_impact_pct < 100.0) with
    | false -> Error "invalid parameters"
    | true ->
      (* For constant product: impact ≈ 2 * amount_in / (reserve0 + amount_in)
         Solving for amount_in: amount_in = impact * reserve0 / (2 - impact) *)
      let impact = max_impact_pct /. 100.0 in
      let fee = Float.of_int p.fee_bps /. 10000.0 in
      let raw_amount = match Float.(impact < 2.0) with
        | true -> impact *. p.reserve0 /. (2.0 -. impact)
        | false -> p.reserve0
      in
      Ok (raw_amount /. (1.0 -. fee))

  (** Full quote with all details *)
  let quote (p : params) ~(amount_in : float) ~(pool_id : string) : Pool_intf.Quote.t pricing_result =
    let open Result.Let_syntax in
    let%bind amount_out = get_amount_out p ~amount_in in
    let%bind impact = price_impact p ~amount_in in
    let fee = Float.of_int p.fee_bps /. 10000.0 in
    let fee_amount = amount_in *. fee in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> 0.0
    in
    Ok (Pool_intf.Quote.single
      ~amount_in ~amount_out ~price_impact_pct:impact
      ~effective_price ~fee_amount ~pool_id)
end

(** Concentrated Liquidity AMM (Uniswap V3 style)

    Used by: Uniswap V3, Aerodrome, Orca CLMM, Camelot V3, Quickswap V3
*)
module Concentrated = struct
  type params = {
    sqrt_price_x96 : string;  (** sqrtPriceX96 from pool *)
    liquidity : string;       (** Active liquidity *)
    tick : int;               (** Current tick *)
    fee_bps : int;
    decimals0 : int;
    decimals1 : int;
  }

  (** Q96 constant = 2^96 *)
  let q96 : float = Float.ldexp 1.0 96

  (** Convert sqrtPriceX96 to actual price *)
  let price_from_sqrt_price_x96 ~(sqrt_price_x96 : string) ~(decimals0 : int) ~(decimals1 : int) : float pricing_result =
    try
      let sqrt_price_raw = Float.of_string sqrt_price_x96 in
      let sqrt_price = sqrt_price_raw /. q96 in
      let raw_price = sqrt_price *. sqrt_price in
      let decimal_adjustment = Float.int_pow 10.0 (decimals1 - decimals0) in
      Ok (raw_price *. decimal_adjustment)
    with
    | _ -> Error "invalid sqrtPriceX96"

  (** Convert tick to price *)
  let price_from_tick ~(tick : int) ~(decimals0 : int) ~(decimals1 : int) : float =
    let base = 1.0001 in
    let raw_price = Float.(base ** of_int tick) in
    let decimal_adjustment = Float.int_pow 10.0 (decimals1 - decimals0) in
    raw_price *. decimal_adjustment

  (** Spot price from pool params *)
  let spot_price (p : params) : float pricing_result =
    price_from_sqrt_price_x96
      ~sqrt_price_x96:p.sqrt_price_x96
      ~decimals0:p.decimals0
      ~decimals1:p.decimals1

  (** Calculate output amount within current tick range
      Note: This is simplified - real implementation needs tick crossing logic *)
  let get_amount_out (p : params) ~(amount_in : float) : float pricing_result =
    try
      let sqrt_price = Float.of_string p.sqrt_price_x96 /. q96 in
      let liq = Float.of_string p.liquidity in
      let fee = Float.of_int p.fee_bps /. 10000.0 in
      let amount_in_after_fee = amount_in *. (1.0 -. fee) in
      (* Simplified single-tick calculation *)
      let delta_sqrt_price = amount_in_after_fee /. liq in
      let new_sqrt_price = sqrt_price +. delta_sqrt_price in
      let amount_out = liq *. (1.0 /. sqrt_price -. 1.0 /. new_sqrt_price) in
      Ok (Float.abs amount_out)
    with
    | _ -> Error "invalid concentrated pool params"

  (** Full quote for concentrated liquidity *)
  let quote (p : params) ~(amount_in : float) ~(pool_id : string) : Pool_intf.Quote.t pricing_result =
    let open Result.Let_syntax in
    let%bind amount_out = get_amount_out p ~amount_in in
    let%bind spot = spot_price p in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> spot
    in
    let price_impact = match Float.(spot > 0.0) with
      | true -> Float.abs ((spot -. effective_price) /. spot) *. 100.0
      | false -> 0.0
    in
    let fee = Float.of_int p.fee_bps /. 10000.0 in
    let fee_amount = amount_in *. fee in
    Ok (Pool_intf.Quote.single
      ~amount_in ~amount_out ~price_impact_pct:price_impact
      ~effective_price ~fee_amount ~pool_id)
end

(** Liquidity Bin AMM (TraderJoe LB style)

    Used by: TraderJoe LB, Maverick
*)
module Liquidity_bin = struct
  type bin = {
    bin_id : int;
    price : float;
    reserve_x : float;
    reserve_y : float;
    liquidity : float;
  }

  type params = {
    bins : bin list;
    active_bin_id : int;
    bin_step : int;  (** Price step between bins in basis points *)
    fee_bps : int;
  }

  (** Get price from bin ID *)
  let price_from_bin_id ~(bin_id : int) ~(bin_step : int) : float =
    let base = 1.0 +. Float.of_int bin_step /. 10000.0 in
    Float.(base ** of_int bin_id)

  (** Spot price from active bin *)
  let spot_price (p : params) : float pricing_result =
    match List.find p.bins ~f:(fun b -> b.bin_id = p.active_bin_id) with
    | Some bin -> Ok bin.price
    | None -> Ok (price_from_bin_id ~bin_id:p.active_bin_id ~bin_step:p.bin_step)

  (** Calculate output by walking through bins *)
  let get_amount_out (p : params) ~(amount_in : float) : float pricing_result =
    let fee = Float.of_int p.fee_bps /. 10000.0 in
    let amount_in_after_fee = amount_in *. (1.0 -. fee) in
    (* Sort bins by ID for traversal *)
    let sorted_bins = List.sort p.bins ~compare:(fun a b -> Int.compare a.bin_id b.bin_id) in
    (* Walk through bins consuming liquidity *)
    let rec walk remaining_in accumulated_out = function
      | [] -> Ok accumulated_out
      | bin :: rest ->
        match Float.(remaining_in <= 0.0) with
        | true -> Ok accumulated_out
        | false ->
          let available = bin.reserve_y in
          let can_get = Float.min available (remaining_in *. bin.price) in
          let used = can_get /. bin.price in
          walk (remaining_in -. used) (accumulated_out +. can_get) rest
    in
    walk amount_in_after_fee 0.0 sorted_bins
end

(** Weighted Pool AMM (Balancer style)

    Used by: Balancer, Osmosis, Beethoven X
*)
module Weighted = struct
  type token_balance = {
    balance : float;
    weight : float;  (** Weight as decimal, e.g., 0.8 for 80% *)
    decimals : int;
  }

  type params = {
    tokens : token_balance list;
    swap_fee_pct : float;  (** Fee as percentage, e.g., 0.3 for 0.3% *)
  }

  (** Spot price between two tokens in a weighted pool *)
  let spot_price ~(balance_in : token_balance) ~(balance_out : token_balance) : float pricing_result =
    match Float.(balance_in.balance > 0.0 && balance_in.weight > 0.0 && balance_out.weight > 0.0) with
    | false -> Error "invalid balance or weight"
    | true ->
      (* Weighted pool spot price formula:
         SP = (B_out / W_out) / (B_in / W_in) *)
      let spot = (balance_out.balance /. balance_out.weight) /.
                 (balance_in.balance /. balance_in.weight) in
      Ok spot

  (** Calculate output amount for weighted pool swap *)
  let get_amount_out (p : params) ~(balance_in : token_balance) ~(balance_out : token_balance)
      ~(amount_in : float) : float pricing_result =
    match Float.(balance_in.balance > 0.0 && balance_out.balance > 0.0) with
    | false -> Error "invalid balances"
    | true ->
      (* Weighted pool swap formula:
         A_out = B_out * (1 - (B_in / (B_in + A_in * (1-fee)))^(W_in/W_out)) *)
      let fee = p.swap_fee_pct /. 100.0 in
      let amount_in_after_fee = amount_in *. (1.0 -. fee) in
      let weight_ratio = balance_in.weight /. balance_out.weight in
      let balance_ratio = balance_in.balance /.
                          (balance_in.balance +. amount_in_after_fee) in
      let power_term = Float.(balance_ratio ** weight_ratio) in
      let amount_out = balance_out.balance *. (1.0 -. power_term) in
      Ok amount_out

  (** Full quote for weighted pool *)
  let quote (p : params) ~(balance_in : token_balance) ~(balance_out : token_balance)
      ~(amount_in : float) ~(pool_id : string) : Pool_intf.Quote.t pricing_result =
    let open Result.Let_syntax in
    let%bind amount_out = get_amount_out p ~balance_in ~balance_out ~amount_in in
    let%bind spot = spot_price ~balance_in ~balance_out in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> spot
    in
    let price_impact = match Float.(spot > 0.0) with
      | true -> Float.abs ((spot -. effective_price) /. spot) *. 100.0
      | false -> 0.0
    in
    let fee_amount = amount_in *. (p.swap_fee_pct /. 100.0) in
    Ok (Pool_intf.Quote.single
      ~amount_in ~amount_out ~price_impact_pct:price_impact
      ~effective_price ~fee_amount ~pool_id)
end

(** StableSwap AMM (Curve style)

    Used by: Curve, Saddle, Synapse, Solidly stable pools
*)
module Stable = struct
  type params = {
    balances : float list;      (** Normalized balances for each token *)
    amp : int;                   (** Amplification coefficient *)
    fee_pct : float;             (** Fee as percentage *)
    decimals : int list;
  }

  (** Calculate D (invariant) using Newton iteration *)
  let get_d ~(balances : float list) ~(amp : int) : float =
    let n = List.length balances in
    let n_f = Float.of_int n in
    let sum = List.fold balances ~init:0.0 ~f:(+.) in
    let ann = Float.of_int amp *. Float.int_pow n_f n in

    let rec newton d iterations =
      match iterations > 255 with
      | true -> d
      | false ->
        let d_p = List.fold balances ~init:d ~f:(fun acc x ->
          match Float.(x > 0.0) with
          | true -> acc *. d /. (x *. n_f)
          | false -> acc) in
        let numerator = (ann *. sum +. d_p *. n_f) *. d in
        let denominator = (ann -. 1.0) *. d +. (n_f +. 1.0) *. d_p in
        let new_d = match Float.(denominator > 0.0) with
          | true -> numerator /. denominator
          | false -> d
        in
        match Float.(abs (new_d -. d) <= 1.0) with
        | true -> new_d
        | false -> newton new_d (iterations + 1)
    in
    newton sum 0

  (** Get y (output balance) given x (input balance) and D *)
  let get_y ~(balances : float list) ~(amp : int) ~(d : float)
      ~(token_in_idx : int) ~(new_balance_in : float) : float pricing_result =
    let n = List.length balances in
    let n_f = Float.of_int n in
    let ann = Float.of_int amp *. Float.int_pow n_f n in

    (* Sum of all balances except token_out *)
    let s = List.foldi balances ~init:0.0 ~f:(fun i acc x ->
      match i = token_in_idx with
      | true -> acc +. new_balance_in
      | false -> acc +. x) in

    (* Product of all balances except token_out *)
    let c = List.foldi balances ~init:d ~f:(fun i acc x ->
      match i = token_in_idx with
      | true -> acc *. d /. (new_balance_in *. n_f)
      | false -> acc *. d /. (x *. n_f)) in

    let b = s +. d /. ann in

    (* Newton iteration to find y *)
    let rec newton y iterations =
      match iterations > 255 with
      | true -> Ok y
      | false ->
        let y_new = (y *. y +. c) /. (2.0 *. y +. b -. d) in
        match Float.(abs (y_new -. y) <= 1.0) with
        | true -> Ok y_new
        | false -> newton y_new (iterations + 1)
    in
    newton d 0

  (** Calculate output amount for stable swap *)
  let get_amount_out (p : params) ~(amount_in : float) ~(token_in_idx : int)
      ~(token_out_idx : int) : float pricing_result =
    match token_in_idx >= 0 && token_out_idx >= 0 &&
          token_in_idx < List.length p.balances &&
          token_out_idx < List.length p.balances &&
          token_in_idx <> token_out_idx with
    | false -> Error "invalid token indices"
    | true ->
      let open Result.Let_syntax in
      let fee = p.fee_pct /. 100.0 in
      let amount_in_after_fee = amount_in *. (1.0 -. fee) in
      let balance_in = List.nth_exn p.balances token_in_idx in
      let balance_out = List.nth_exn p.balances token_out_idx in
      let new_balance_in = balance_in +. amount_in_after_fee in
      let d = get_d ~balances:p.balances ~amp:p.amp in
      let%bind new_balance_out = get_y ~balances:p.balances ~amp:p.amp ~d
        ~token_in_idx ~new_balance_in in
      let amount_out = balance_out -. new_balance_out in
      Ok (Float.max 0.0 amount_out)

  (** Spot price between two tokens (derivative at current point) *)
  let spot_price (p : params) ~(token_in_idx : int) ~(token_out_idx : int) : float pricing_result =
    (* Use small amount to approximate derivative *)
    let epsilon = 0.0001 in
    let open Result.Let_syntax in
    let%bind amount_out = get_amount_out p ~amount_in:epsilon ~token_in_idx ~token_out_idx in
    Ok (amount_out /. epsilon)

  (** Full quote for stable swap *)
  let quote (p : params) ~(amount_in : float) ~(token_in_idx : int)
      ~(token_out_idx : int) ~(pool_id : string) : Pool_intf.Quote.t pricing_result =
    let open Result.Let_syntax in
    let%bind amount_out = get_amount_out p ~amount_in ~token_in_idx ~token_out_idx in
    let%bind spot = spot_price p ~token_in_idx ~token_out_idx in
    let effective_price = match Float.(amount_in > 0.0) with
      | true -> amount_out /. amount_in
      | false -> spot
    in
    let price_impact = match Float.(spot > 0.0) with
      | true -> Float.abs ((spot -. effective_price) /. spot) *. 100.0
      | false -> 0.0
    in
    let fee_amount = amount_in *. (p.fee_pct /. 100.0) in
    Ok (Pool_intf.Quote.single
      ~amount_in ~amount_out ~price_impact_pct:price_impact
      ~effective_price ~fee_amount ~pool_id)
end

(** Utility functions for working with decimal-adjusted values *)
module Decimals = struct
  (** Convert raw amount to float with decimal adjustment *)
  let to_float ~(raw : string) ~(decimals : int) : float pricing_result =
    try
      let divisor = Float.int_pow 10.0 decimals in
      Ok (Float.of_string raw /. divisor)
    with
    | _ -> Error (sprintf "invalid amount string: %s" raw)

  (** Convert float to raw amount with decimal adjustment *)
  let from_float ~(value : float) ~(decimals : int) : string =
    let multiplier = Float.int_pow 10.0 decimals in
    let raw = value *. multiplier in
    sprintf "%.0f" raw
end

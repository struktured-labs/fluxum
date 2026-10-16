open Liquidity_rewards

let checks = ref 0
let close ?(eps = 1e-9) name got want =
  incr checks;
  if Float.( > ) (Float.abs (got -. want)) eps
  then failwithf "%s: got %.10f want %.10f" name got want ()

let () =
  let open Gemini in
  let c = default_config in
  close "w at mid" (spread_weight c ~distance_cents:0.) 1.;
  close "w at 5c" (spread_weight c ~distance_cents:5.) 0.25;
  close "w at max" (spread_weight c ~distance_cents:10.) 0.;
  close "w beyond max" (spread_weight c ~distance_cents:10.5) 0.;
  (* below min size scores zero; size capped at 250 *)
  close "min size" (side_score c ~reference:0.5 [ { price = 0.49; size = 9.99 } ]) 0.;
  close "cap" (side_score c ~reference:0.5 [ { price = 0.50; size = 1000. } ]) 250.;
  (* two-sided multiplier: bid 1c and ask 1c from mid, 10 each: (0.81*10)*2*1.5 *)
  let bids = [ { price = 0.49; size = 10. } ] and asks = [ { price = 0.51; size = 10. } ] in
  close "two-sided" (snapshot_score c ~reference:0.5 ~bids ~asks) (0.81 *. 10. *. 2. *. 1.5) ~eps:1e-9;
  close "one-sided" (snapshot_score c ~reference:0.5 ~bids ~asks:[]) (0.81 *. 10.);
  (* Gemini's docs example ordering: A (tight, two-sided) > C (tight one-sided) > B (wide, large) *)
  let mid = 0.505 in
  let a = snapshot_score c ~reference:mid ~bids:[ { price = 0.48; size = 50. } ] ~asks:[ { price = 0.52; size = 50. } ] in
  let b = snapshot_score c ~reference:mid ~bids:[ { price = 0.40; size = 200. } ] ~asks:[ { price = 0.60; size = 200. } ] in
  let cc = snapshot_score c ~reference:mid ~bids:[ { price = 0.49; size = 30. } ] ~asks:[] in
  incr checks; assert (Float.( > ) a cc && Float.( > ) cc b);
  (* payouts *)
  close "uptime gate" (daily_payout c ~pool_usd:500. ~own_score:1. ~total_score:1. ~uptime:0.35) 0.;
  close "min payout" (daily_payout c ~pool_usd:100. ~own_score:0.009 ~total_score:1. ~uptime:1.) 0.;
  close "floor cents" (daily_payout c ~pool_usd:500. ~own_score:0.773 ~total_score:1. ~uptime:0.96) 386.5;
  close "floor cents 2" (daily_payout c ~pool_usd:10. ~own_score:1. ~total_score:3. ~uptime:1.) 3.33

let () =
  let open Polymarket in
  close "pm order" (order_score ~max_spread:3. ~spread:1. ~multiplier:1.) (4. /. 9.);
  close "pm beyond" (order_score ~max_spread:3. ~spread:3.5 ~multiplier:1.) 0.;
  close "pm side" (side_score ~max_spread:3. ~min_size:20. ~multiplier:1. [ 1., 100.; 0., 10. ]) (400. /. 9.);
  close "pm qmin mid-range one-sided" (q_min ~midpoint:0.5 ~c:3. ~q_one:90. ~q_two:0.) 30.;
  close "pm qmin mid-range two-sided" (q_min ~midpoint:0.5 ~c:3. ~q_one:90. ~q_two:60.) 60.;
  close "pm qmin extreme" (q_min ~midpoint:0.05 ~c:3. ~q_one:90. ~q_two:0.) 0.;
  close "pm epoch" (epoch_score ~sample_q_mins:[ 10.; 20. ] ~uptime:0.5) 15.;
  (* Polymarket blog worked example: 44 / (44 + 32) * 75 *)
  close "pm payout" (payout ~reward:75. ~own_epoch:44. ~total_epoch:76.) (75. *. 44. /. 76.)

let () =
  let open Kalshi in
  let o ?(mine = false) p s = { price_ticks = p; size = s; mine } in
  (* target 100: 10 @50 (cum 10 < 20), 15 @49 (cum 25 >= 20 -> ref 49), 80 @45 (cum 105 >= 100 stop) *)
  let book = [ o 50 10.; o 49 15.; o ~mine:true 45 80.; o 40 500. ] in
  (match qualify_side ~target_size:100. ~max_price_ticks:100 book with
   | None -> failwith "should qualify"
   | Some r ->
     incr checks;
     assert (r.reference_ticks = 49);
     assert (List.length r.qualifying = 3);
     (* scores: 10*1 + 15*1 + 80*0.5^4 = 30 ; mine = 5 *)
     close "k share" (side_share ~discount_factor:0.5 r) (5. /. 30.));
  incr checks;
  assert (Option.is_none (qualify_side ~target_size:1000. ~max_price_ticks:100 book));
  assert (Option.is_none (qualify_side ~target_size:10. ~max_price_ticks:50 book));
  close "k at/above ref full credit" (order_score ~discount_factor:0.5 ~reference_ticks:49 (o 50 10.)) 10.;
  let yes = [ o ~mine:true 50 100. ] and no = [ o 48 50.; o ~mine:true 48 50. ] in
  close "k snapshot" (snapshot_score ~target_size:100. ~max_price_ticks:100 ~discount_factor:0.9 ~yes_bids:yes ~no_bids:no) 1.5;
  close "k one side missing" (snapshot_score ~target_size:100. ~max_price_ticks:100 ~discount_factor:0.9 ~yes_bids:yes ~no_bids:[]) 0.;
  close "k payout floor" (payout ~reward:10. ~own:1. ~total:3. ~min_payout:1.) 3.33;
  close "k payout min" (payout ~reward:10. ~own:0.05 ~total:1. ~min_payout:1.) 0.;
  printf "liquidity_rewards: %d checks passed\n" !checks

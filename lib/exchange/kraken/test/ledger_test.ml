(** Unit tests for Kraken Ledger module *)

open Core
open Kraken

(** Test helper to compare floats with tolerance *)
let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  if Float.abs (expected -. actual) > tolerance then
    failwith (Printf.sprintf "%s: expected %.8f, got %.8f" msg expected actual)

(** Test 1: Create empty entry with defaults *)
let%test_unit "create_empty_entry" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  assert_float_equal 0.0 entry.pnl "pnl should be 0";
  assert_float_equal 0.0 entry.position "position should be 0";
  assert_float_equal 0.0 entry.spot "spot should be 0";
  assert_float_equal 0.0 entry.notional "notional should be 0";
  assert_float_equal 0.0 entry.cost_basis "cost_basis should be 0";
  [%test_result: string] ~expect:"BTC/USD" entry.symbol

(** Test 2: Simple buy trade - long position *)
let%test_unit "simple_buy_trade" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in

  assert_float_equal 1.0 entry.position "position should be 1.0 BTC";
  assert_float_equal 50000.0 entry.spot "spot should be $50,000";
  assert_float_equal 50000.0 entry.pnl_spot "pnl_spot should be $50,000";
  assert_float_equal (-50000.0) entry.notional "notional should be -$50,000 (cash out)";
  assert_float_equal 0.0 entry.pnl "pnl should be 0 (breakeven at entry)";
  assert_float_equal 50000.0 entry.cost_basis "cost_basis should be $50,000";
  assert_float_equal 1.0 entry.total_buy_qty "total_buy_qty should be 1.0";
  assert_float_equal 0.0 entry.total_sell_qty "total_sell_qty should be 0";
  assert_float_equal 50000.0 entry.avg_buy_price "avg_buy_price should be $50,000"

(** Test 3: Buy with fee *)
let%test_unit "buy_trade_with_fee" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 with $25 fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 ~fee_usd:25.0 in

  assert_float_equal 1.0 entry.position "position should be 1.0 BTC";
  assert_float_equal (-50025.0) entry.notional "notional should be -$50,025 (price + fee)";
  assert_float_equal 50025.0 entry.cost_basis "cost_basis should include fee";
  assert_float_equal (-25.0) entry.pnl "pnl should be -$25 (fee cost)"

(** Test 4: Buy then sell - profit scenario *)
let%test_unit "buy_then_sell_profit" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Sell 1 BTC at $55,000 - $5,000 profit *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in

  assert_float_equal 0.0 entry.position "position should be flat";
  assert_float_equal 5000.0 entry.notional "notional should be $5,000 profit";
  assert_float_equal 0.0 entry.pnl_spot "pnl_spot should be 0 (no position)";
  assert_float_equal 5000.0 entry.pnl "pnl should be $5,000";
  assert_float_equal 1.0 entry.total_buy_qty "total_buy_qty should be 1.0";
  assert_float_equal 1.0 entry.total_sell_qty "total_sell_qty should be 1.0";
  assert_float_equal 0.0 entry.cost_basis "cost_basis should be 0 (position closed)"

(** Test 5: Buy then sell - loss scenario *)
let%test_unit "buy_then_sell_loss" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Sell 1 BTC at $45,000 - $5,000 loss *)
  let entry = Ledger.Entry.on_trade entry ~price:45000.0 ~side:Sell ~qty:1.0 in

  assert_float_equal 0.0 entry.position "position should be flat";
  assert_float_equal (-5000.0) entry.notional "notional should be -$5,000 loss";
  assert_float_equal (-5000.0) entry.pnl "pnl should be -$5,000"

(** Test 6: Partial position close *)
let%test_unit "partial_position_close" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 2 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:2.0 in
  assert_float_equal 2.0 entry.position "position should be 2.0 BTC";
  assert_float_equal (-100000.0) entry.notional "notional should be -$100,000";

  (* Sell 1 BTC at $55,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  assert_float_equal 1.0 entry.position "position should be 1.0 BTC";
  assert_float_equal (-45000.0) entry.notional "notional should be -$45,000";
  assert_float_equal 55000.0 entry.pnl_spot "pnl_spot should be $55,000 (1 BTC at $55k)";
  assert_float_equal 10000.0 entry.pnl "pnl should be $10,000";
  assert_float_equal 50000.0 entry.cost_basis "cost_basis should be $50,000 (half remaining)"

(** Test 7: Average price calculation *)
let%test_unit "average_price_calculation" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Buy 1 BTC at $52,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Buy ~qty:1.0 in

  assert_float_equal 2.0 entry.position "position should be 2.0 BTC";
  assert_float_equal 2.0 entry.total_buy_qty "total_buy_qty should be 2.0";
  assert_float_equal 51000.0 entry.avg_buy_price "avg_buy_price should be $51,000";
  assert_float_equal 51000.0 entry.avg_price "avg_price should be $51,000";

  (* Sell 1 BTC at $54,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:54000.0 ~side:Sell ~qty:1.0 in
  assert_float_equal 1.0 entry.total_sell_qty "total_sell_qty should be 1.0";
  assert_float_equal 54000.0 entry.avg_sell_price "avg_sell_price should be $54,000";
  (* Overall avg = (51000*2 + 54000*1) / 3 = 52000 *)
  assert_float_equal 52000.0 entry.avg_price "avg_price should be $52,000"

(** Test 8: Multiple buys at different prices *)
let%test_unit "multiple_buys_different_prices" =
  let entry = Ledger.Entry.create ~symbol:"ETH/USD" () in
  (* Buy 10 ETH at $3,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:3000.0 ~side:Buy ~qty:10.0 in
  (* Buy 5 ETH at $3,200 *)
  let entry = Ledger.Entry.on_trade entry ~price:3200.0 ~side:Buy ~qty:5.0 in
  (* Buy 5 ETH at $2,800 *)
  let entry = Ledger.Entry.on_trade entry ~price:2800.0 ~side:Buy ~qty:5.0 in

  assert_float_equal 20.0 entry.position "position should be 20 ETH";
  (* Avg = (10*3000 + 5*3200 + 5*2800) / 20 = 3000 *)
  assert_float_equal 3000.0 entry.avg_buy_price "avg_buy_price should be $3,000";
  assert_float_equal 60000.0 entry.cost_basis "cost_basis should be $60,000"

(** Test 9: Update spot price - unrealized P&L *)
let%test_unit "update_spot_unrealized_pnl" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  assert_float_equal 0.0 entry.pnl "pnl should be 0 at entry";

  (* Spot price moves to $55,000 *)
  let entry = Ledger.Entry.update_spot entry 55000.0 in
  assert_float_equal 55000.0 entry.spot "spot should be $55,000";
  assert_float_equal 55000.0 entry.pnl_spot "pnl_spot should be $55,000";
  assert_float_equal 5000.0 entry.pnl "pnl should be $5,000 unrealized";

  (* Spot price moves to $45,000 *)
  let entry = Ledger.Entry.update_spot entry 45000.0 in
  assert_float_equal 45000.0 entry.spot "spot should be $45,000";
  assert_float_equal (-5000.0) entry.pnl "pnl should be -$5,000 unrealized"

(** Test 10: Cost basis on partial sells *)
let%test_unit "cost_basis_partial_sells" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 4 BTC at $50,000 each = $200,000 cost basis *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:4.0 in
  assert_float_equal 200000.0 entry.cost_basis "initial cost_basis should be $200,000";
  assert_float_equal 4.0 entry.running_qty "running_qty should be 4.0";

  (* Sell 1 BTC - should reduce cost basis by 1/4 = $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  assert_float_equal 150000.0 entry.cost_basis "cost_basis should be $150,000";
  assert_float_equal 3.0 entry.running_qty "running_qty should be 3.0";
  assert_float_equal 50000.0 entry.running_price "running_price should be $50,000";

  (* Sell another 1 BTC - reduce by 1/3 of remaining *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Sell ~qty:1.0 in
  assert_float_equal 100000.0 entry.cost_basis "cost_basis should be $100,000";
  assert_float_equal 2.0 entry.running_qty "running_qty should be 2.0"

(** Test 11: Short position unwinding *)
let%test_unit "short_position_unwinding" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  assert_float_equal 1.0 entry.position "position should be 1.0";

  (* Sell 2 BTC at $55,000 - this would create a -1 BTC short position *)
  (* The ledger should unwind this by: *)
  (* 1. Selling 1 BTC to close long position *)
  (* 2. Then applying the trade that caused the short (selling 1 BTC) *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:2.0 in

  (* After unwinding, position should be 0 (we don't allow shorts) *)
  assert_float_equal 0.0 entry.position "position should be flat after unwinding";
  assert_float_equal 5000.0 entry.notional "should have captured the profit from closing long"

(** Test 12: Running price calculation *)
let%test_unit "running_price_calculation" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 2 BTC at $50,000 each *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:2.0 in
  assert_float_equal 100000.0 entry.cost_basis "cost_basis should be $100,000";
  assert_float_equal 2.0 entry.running_qty "running_qty should be 2.0";
  assert_float_equal 50000.0 entry.running_price "running_price should be $50,000";

  (* Buy 2 more BTC at $60,000 each *)
  let entry = Ledger.Entry.on_trade entry ~price:60000.0 ~side:Buy ~qty:2.0 in
  assert_float_equal 220000.0 entry.cost_basis "cost_basis should be $220,000";
  assert_float_equal 4.0 entry.running_qty "running_qty should be 4.0";
  assert_float_equal 55000.0 entry.running_price "running_price should be $55,000 avg"

(** Test 13: Multiple symbol ledger *)
let%test_unit "multi_symbol_ledger" =
  let ledger = Fluxum.Types.Symbol.Map.empty in

  (* Trade BTC *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC/USD" ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Trade ETH *)
  let ledger = Ledger.on_trade ledger ~symbol:"ETH/USD" ~price:3000.0 ~side:Buy ~qty:10.0 in

  (* Check BTC entry *)
  let btc_entry = Map.find_exn ledger "BTC/USD" in
  assert_float_equal 1.0 btc_entry.position "BTC position should be 1.0";
  assert_float_equal 50000.0 btc_entry.cost_basis "BTC cost_basis should be $50,000";

  (* Check ETH entry *)
  let eth_entry = Map.find_exn ledger "ETH/USD" in
  assert_float_equal 10.0 eth_entry.position "ETH position should be 10.0";
  assert_float_equal 30000.0 eth_entry.cost_basis "ETH cost_basis should be $30,000"

(** Test 14: Update spots for multiple symbols *)
let%test_unit "update_spots_multi_symbol" =
  let ledger = Fluxum.Types.Symbol.Map.empty in

  (* Create positions *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC/USD" ~price:50000.0 ~side:Buy ~qty:1.0 in
  let ledger = Ledger.on_trade ledger ~symbol:"ETH/USD" ~price:3000.0 ~side:Buy ~qty:10.0 in

  (* Update spot prices *)
  let spots = Fluxum.Types.Symbol.Map.empty
    |> Map.set ~key:"BTC/USD" ~data:55000.0
    |> Map.set ~key:"ETH/USD" ~data:3500.0
  in
  let ledger = Ledger.update_spots ledger spots in

  (* Check updated P&L *)
  let btc_entry = Map.find_exn ledger "BTC/USD" in
  assert_float_equal 5000.0 btc_entry.pnl "BTC pnl should be $5,000";

  let eth_entry = Map.find_exn ledger "ETH/USD" in
  assert_float_equal 5000.0 eth_entry.pnl "ETH pnl should be $5,000 (10 * $500)"

(** Test 15: Zero position after full sell *)
let%test_unit "zero_position_after_full_sell" =
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Sell ~qty:1.0 in

  assert_float_equal 0.0 entry.position "position should be 0";
  assert_float_equal 0.0 entry.running_qty "running_qty should be 0";
  assert_float_equal 0.0 entry.cost_basis "cost_basis should be 0";
  assert_float_equal 0.0 entry.pnl "pnl should be 0 (breakeven)"

let () = print_endline "All Ledger tests defined"

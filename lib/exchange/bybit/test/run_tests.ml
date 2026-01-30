(** Comprehensive test suite for Bybit unified adapter *)

open Core

module Ledger = Bybit.Ledger
module Order_book = Bybit.Order_book
module Fluxum_adapter = Bybit.Fluxum_adapter
module Types = Fluxum.Types

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  PASS: %s\n" msg;
    true

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match equal expected actual with
  | false ->
    incr tests_failed;
    printf "  FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | true ->
    incr tests_passed;
    printf "  PASS: %s\n" msg;
    true

let assert_true condition msg =
  incr tests_run;
  match condition with
  | true ->
    incr tests_passed;
    printf "  PASS: %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  FAIL: %s\n" msg;
    false

let assert_false condition msg =
  assert_true (not condition) msg

let order_status_equal a b = Types.Order_status.compare a b = 0

(** ========== LEDGER TESTS ========== *)

let test_ledger_entry_creation () =
  printf "\n=== Ledger: Entry Creation ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let _ = assert_float_equal 0.0 entry.pnl "Empty entry has 0 pnl" in
  let _ = assert_float_equal 0.0 entry.position "Empty entry has 0 position" in
  let _ = assert_float_equal 0.0 entry.spot "Empty entry has 0 spot" in
  let _ = assert_float_equal 0.0 entry.notional "Empty entry has 0 notional" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Empty entry has 0 cost_basis" in
  let _ = assert_float_equal 0.0 entry.avg_buy_price "Empty entry has 0 avg_buy_price" in
  let _ = assert_float_equal 0.0 entry.avg_sell_price "Empty entry has 0 avg_sell_price" in
  let _ = assert_float_equal 0.0 entry.avg_price "Empty entry has 0 avg_price" in
  let _ = assert_float_equal 0.0 entry.total_buy_qty "Empty entry has 0 total_buy_qty" in
  let _ = assert_float_equal 0.0 entry.total_sell_qty "Empty entry has 0 total_sell_qty" in
  let _ = assert_float_equal 0.0 entry.buy_notional "Empty entry has 0 buy_notional" in
  let _ = assert_float_equal 0.0 entry.sell_notional "Empty entry has 0 sell_notional" in
  let _ = assert_float_equal 0.0 entry.total_original "Empty entry has 0 total_original" in
  let _ = assert_float_equal 0.0 entry.total_executed "Empty entry has 0 total_executed" in
  let _ = assert_float_equal 0.0 entry.total_remaining "Empty entry has 0 total_remaining" in
  let _ = assert_float_equal 0.0 entry.running_price "Empty entry has 0 running_price" in
  let _ = assert_float_equal 0.0 entry.running_qty "Empty entry has 0 running_qty" in
  let _ = assert_float_equal 0.0 entry.pnl_spot "Empty entry has 0 pnl_spot" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC/USD" entry.symbol "Symbol is set correctly" in
  let _ = assert_equal ~equal:[%equal: Types.Side.t option] ~sexp_of_t:[%sexp_of: Types.Side.t option]
    None entry.side "Empty entry has no side" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.qty "Empty entry has no qty" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.price "Empty entry has no price" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.package_price "Empty entry has no package_price" in
  ()

let test_ledger_simple_trades () =
  printf "\n=== Ledger: Simple Trades ===\n";

  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  let _ = assert_float_equal 1.0 entry.position "Buy 1 BTC: position = 1.0" in
  let _ = assert_float_equal (-50000.0) entry.notional "Buy 1 BTC: notional = -$50k" in
  let _ = assert_float_equal 50000.0 entry.cost_basis "Buy 1 BTC: cost_basis = $50k" in
  let _ = assert_float_equal 0.0 entry.pnl "Buy at entry: pnl = 0" in
  let _ = assert_float_equal 50000.0 entry.spot "Buy sets spot = $50k" in
  let _ = assert_float_equal 50000.0 entry.pnl_spot "Buy: pnl_spot = position * price = $50k" in
  let _ = assert_float_equal 1.0 entry.total_buy_qty "Total buy qty = 1.0" in
  let _ = assert_float_equal 50000.0 entry.avg_buy_price "Avg buy price = $50k" in

  (* Sell 1 BTC at $55,000 - $5k profit *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 0.0 entry.position "After sell: position = 0" in
  let _ = assert_float_equal 5000.0 entry.notional "After sell: notional = $5k profit" in
  let _ = assert_float_equal 5000.0 entry.pnl "Profit: pnl = $5k" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Flat position: cost_basis = 0" in
  let _ = assert_float_equal 55000.0 entry.avg_sell_price "Avg sell price = $55k" in
  let _ = assert_float_equal 1.0 entry.total_sell_qty "Total sell qty = 1.0" in
  ()

let test_ledger_fees () =
  printf "\n=== Ledger: Fee Handling ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50k with $25 fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 ~fee_usd:25.0 in
  let _ = assert_float_equal (-50025.0) entry.notional "Fee reduces cash: notional = -$50,025" in
  let _ = assert_float_equal 50025.0 entry.cost_basis "Fee in cost basis: $50,025" in
  let _ = assert_float_equal 1.0 entry.position "Position = 1.0 (unaffected by fee)" in

  (* Sell at breakeven price should show loss due to fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal (-25.0) entry.pnl "Fee creates loss: pnl = -$25" in
  let _ = assert_float_equal 0.0 entry.position "Flat after sell" in

  (* Buy with higher fee *)
  let entry2 = Ledger.Entry.create ~symbol:"ETH/USD" () in
  let entry2 = Ledger.Entry.on_trade entry2 ~price:3000.0 ~side:Buy ~qty:10.0 ~fee_usd:100.0 in
  let _ = assert_float_equal (-30100.0) entry2.notional "Large fee: notional = -$30,100" in
  let _ = assert_float_equal 30100.0 entry2.cost_basis "Large fee: cost_basis = $30,100" in

  (* Sell at same price *)
  let entry2 = Ledger.Entry.on_trade entry2 ~price:3000.0 ~side:Sell ~qty:10.0 in
  let _ = assert_float_equal (-100.0) entry2.pnl "Fee-only loss: pnl = -$100" in
  ()

let test_ledger_partial_fills () =
  printf "\n=== Ledger: Partial Position Close ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 4 BTC at $50k each *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:4.0 in
  let _ = assert_float_equal 4.0 entry.position "Buy 4 BTC: position = 4.0" in
  let _ = assert_float_equal 200000.0 entry.cost_basis "Buy 4 BTC: cost_basis = $200k" in

  (* Sell 1 BTC - reduces cost basis proportionally *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 3.0 entry.position "Sell 1/4: position = 3.0" in
  let _ = assert_float_equal 150000.0 entry.cost_basis "Sell 1/4: cost_basis = $150k" in
  let _ = assert_float_equal 50000.0 entry.running_price "Running price = $50k" in

  (* Sell another 1 BTC *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 2.0 entry.position "Sell another: position = 2.0" in
  let _ = assert_float_equal 100000.0 entry.cost_basis "Sell 1/3 remaining: cost_basis = $100k" in

  (* Sell 1 more BTC *)
  let entry = Ledger.Entry.on_trade entry ~price:48000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 1.0 entry.position "Sell another: position = 1.0" in
  let _ = assert_float_equal 50000.0 entry.cost_basis "Sell 1/2 remaining: cost_basis = $50k" in

  (* Sell last BTC *)
  let entry = Ledger.Entry.on_trade entry ~price:51000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 0.0 entry.position "All sold: position = 0" in
  let _ = assert_float_equal 0.0 entry.cost_basis "All sold: cost_basis = 0" in
  ()

let test_ledger_average_prices () =
  printf "\n=== Ledger: Average Price Calculations ===\n";

  let entry = Ledger.Entry.create ~symbol:"ETH/USD" () in
  (* Buy at different prices *)
  let entry = Ledger.Entry.on_trade entry ~price:3000.0 ~side:Buy ~qty:10.0 in
  let entry = Ledger.Entry.on_trade entry ~price:3200.0 ~side:Buy ~qty:5.0 in
  let entry = Ledger.Entry.on_trade entry ~price:2800.0 ~side:Buy ~qty:5.0 in

  (* Average = (10*3000 + 5*3200 + 5*2800) / 20 = 3000 *)
  let _ = assert_float_equal 3000.0 entry.avg_buy_price "Weighted avg buy = $3,000" in
  let _ = assert_float_equal 20.0 entry.total_buy_qty "Total buy qty = 20" in
  let _ = assert_float_equal 20.0 entry.position "Position = 20" in

  (* Sell some *)
  let entry = Ledger.Entry.on_trade entry ~price:3500.0 ~side:Sell ~qty:5.0 in
  let _ = assert_float_equal 3500.0 entry.avg_sell_price "Avg sell = $3,500" in
  let _ = assert_float_equal 5.0 entry.total_sell_qty "Total sell qty = 5" in

  (* Overall avg = (3000*20 + 3500*5) / 25 = 3100 *)
  let _ = assert_float_equal 3100.0 entry.avg_price "Overall avg = $3,100" in

  (* Sell more at different price *)
  let entry = Ledger.Entry.on_trade entry ~price:3300.0 ~side:Sell ~qty:5.0 in
  (* New avg sell = (3500*5 + 3300*5) / 10 = 3400 *)
  let _ = assert_float_equal 3400.0 entry.avg_sell_price "Updated avg sell = $3,400" in
  let _ = assert_float_equal 10.0 entry.total_sell_qty "Total sell qty = 10" in
  (* Overall avg = (3000*20 + 3400*10) / 30 = (60000+34000)/30 = 94000/30 *)
  let expected_avg = (3000.0 *. 20.0 +. 3400.0 *. 10.0) /. 30.0 in
  let _ = assert_float_equal expected_avg entry.avg_price "Overall avg updated" in
  ()

let test_ledger_spot_updates () =
  printf "\n=== Ledger: Spot Price Updates (Unrealized P&L) ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in

  (* Spot moves to $55k - unrealized profit *)
  let entry = Ledger.Entry.update_spot entry 55000.0 in
  let _ = assert_float_equal 55000.0 entry.spot "Spot updated to $55k" in
  let _ = assert_float_equal 55000.0 entry.pnl_spot "pnl_spot = $55k" in
  let _ = assert_float_equal 5000.0 entry.pnl "Unrealized profit = $5k" in

  (* Spot moves to $45k - unrealized loss *)
  let entry = Ledger.Entry.update_spot entry 45000.0 in
  let _ = assert_float_equal 45000.0 entry.spot "Spot updated to $45k" in
  let _ = assert_float_equal (-5000.0) entry.pnl "Unrealized loss = -$5k" in
  let _ = assert_float_equal 45000.0 entry.pnl_spot "pnl_spot = $45k" in

  (* Spot back to $50k - breakeven *)
  let entry = Ledger.Entry.update_spot entry 50000.0 in
  let _ = assert_float_equal 0.0 entry.pnl "Breakeven: pnl = 0" in

  (* Multi-position: buy 3 BTC, spot goes up $1k *)
  let entry2 = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let entry2 = Ledger.Entry.on_trade entry2 ~price:50000.0 ~side:Buy ~qty:3.0 in
  let entry2 = Ledger.Entry.update_spot entry2 51000.0 in
  let _ = assert_float_equal 3000.0 entry2.pnl "3 BTC * $1k up = $3k profit" in
  let _ = assert_float_equal 153000.0 entry2.pnl_spot "pnl_spot = 3 * 51000" in

  (* update_spot clears trade-specific fields *)
  let _ = assert_equal ~equal:[%equal: Types.Side.t option] ~sexp_of_t:[%sexp_of: Types.Side.t option]
    None entry.side "update_spot clears side" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.price "update_spot clears price" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.qty "update_spot clears qty" in
  ()

let test_ledger_cost_basis_accounting () =
  printf "\n=== Ledger: Cost Basis Accounting ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 3 BTC at $50k = $150k cost basis *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:3.0 in
  let _ = assert_float_equal 150000.0 entry.cost_basis "Initial cost_basis = $150k" in
  let _ = assert_float_equal 3.0 entry.running_qty "Running qty = 3.0" in
  let _ = assert_float_equal 50000.0 entry.running_price "Running price = $50k" in

  (* Sell 1 BTC - reduces cost basis by 1/3 *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 100000.0 entry.cost_basis "After sell 1/3: cost_basis = $100k" in
  let _ = assert_float_equal 2.0 entry.running_qty "Running qty = 2.0" in

  (* Sell 1 more - reduces by 1/2 of remaining *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 50000.0 entry.cost_basis "After sell 1/2: cost_basis = $50k" in
  let _ = assert_float_equal 1.0 entry.running_qty "Running qty = 1.0" in
  let _ = assert_float_equal 50000.0 entry.running_price "Running price = $50k" in

  (* Buy more at different price - adds to cost basis *)
  let entry = Ledger.Entry.on_trade entry ~price:60000.0 ~side:Buy ~qty:2.0 in
  let _ = assert_float_equal 170000.0 entry.cost_basis "After buy 2 more: cost_basis = $170k" in
  let _ = assert_float_equal 3.0 entry.running_qty "Running qty = 3.0" in
  (* running_price = 170000 / 3 *)
  let expected_running = 170000.0 /. 3.0 in
  let _ = assert_float_equal expected_running entry.running_price "Running price = cost_basis / qty" in

  (* Close entire position *)
  let entry = Ledger.Entry.on_trade entry ~price:58000.0 ~side:Sell ~qty:3.0 in
  let _ = assert_float_equal 0.0 entry.cost_basis "Fully closed: cost_basis = 0" in
  let _ = assert_float_equal 0.0 entry.running_qty "Fully closed: running_qty = 0" in
  let _ = assert_float_equal 0.0 entry.running_price "Fully closed: running_price = 0" in
  ()

let test_ledger_multi_symbol () =
  printf "\n=== Ledger: Multi-Symbol Tracking ===\n";

  let ledger = Fluxum.Types.Symbol.Map.empty in

  (* Trade BTC *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC/USD" ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Trade ETH *)
  let ledger = Ledger.on_trade ledger ~symbol:"ETH/USD" ~price:3000.0 ~side:Buy ~qty:10.0 in

  (* Check BTC *)
  let btc = Map.find_exn ledger "BTC/USD" in
  let _ = assert_float_equal 1.0 btc.position "BTC position = 1.0" in
  let _ = assert_float_equal 50000.0 btc.cost_basis "BTC cost_basis = $50k" in

  (* Check ETH *)
  let eth = Map.find_exn ledger "ETH/USD" in
  let _ = assert_float_equal 10.0 eth.position "ETH position = 10.0" in
  let _ = assert_float_equal 30000.0 eth.cost_basis "ETH cost_basis = $30k" in

  (* Update spots *)
  let spots = Fluxum.Types.Symbol.Map.empty
    |> Map.set ~key:"BTC/USD" ~data:55000.0
    |> Map.set ~key:"ETH/USD" ~data:3500.0
  in
  let ledger = Ledger.update_spots ledger spots in

  let btc = Map.find_exn ledger "BTC/USD" in
  let _ = assert_float_equal 5000.0 btc.pnl "BTC unrealized profit = $5k" in

  let eth = Map.find_exn ledger "ETH/USD" in
  let _ = assert_float_equal 5000.0 eth.pnl "ETH unrealized profit = $5k" in

  (* Sell BTC at a profit *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC/USD" ~price:55000.0 ~side:Sell ~qty:1.0 in
  let btc = Map.find_exn ledger "BTC/USD" in
  let _ = assert_float_equal 0.0 btc.position "BTC fully closed" in
  let _ = assert_float_equal 5000.0 btc.pnl "BTC realized profit = $5k" in

  (* ETH should be unaffected *)
  let eth_after = Map.find_exn ledger "ETH/USD" in
  let _ = assert_float_equal 10.0 eth_after.position "ETH position unchanged" in
  ()

(** ========== ORDER BOOK TESTS ========== *)

let test_order_book_creation () =
  printf "\n=== Order Book: Creation ===\n";

  let book = Order_book.Book.create ~symbol:"BTC/USD" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC/USD" (Order_book.Book.symbol book) "Symbol is set" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 best_bid.price "Empty book: best_bid price = 0" in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.volume best_bid) "Empty book: best_bid volume = 0" in

  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.0 best_ask.price "Empty book: best_ask price = 0" in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.volume best_ask) "Empty book: best_ask volume = 0" in

  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (List.length bids) "Empty book: no bids" in

  let asks = Order_book.Book.asks_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (List.length asks) "Empty book: no asks" in
  ()

let test_order_book_bid_sorting () =
  printf "\n=== Order Book: Bid Sorting (Descending) ===\n";

  let book = Order_book.Book.create ~symbol:"BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in

  let best = Order_book.Book.best_bid book in
  let _ = assert_float_equal 51000.0 best.price "Best bid = highest ($51k)" in

  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length bids) "Got 3 bids" in

  (* Bids should be descending: 51000, 50000, 49000 *)
  let bid_prices = List.map bids ~f:(fun (price, _level) -> price) in
  let _ = assert_float_equal 51000.0 (List.nth_exn bid_prices 0) "1st bid = $51k" in
  let _ = assert_float_equal 50000.0 (List.nth_exn bid_prices 1) "2nd bid = $50k" in
  let _ = assert_float_equal 49000.0 (List.nth_exn bid_prices 2) "3rd bid = $49k" in

  (* Verify volumes *)
  let bid_levels = List.map bids ~f:snd in
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume (List.nth_exn bid_levels 0)) "1st bid volume = 1.5" in
  let _ = assert_float_equal 1.0 (Order_book.Price_level.volume (List.nth_exn bid_levels 1)) "2nd bid volume = 1.0" in
  let _ = assert_float_equal 2.0 (Order_book.Price_level.volume (List.nth_exn bid_levels 2)) "3rd bid volume = 2.0" in

  (* best_n_bids should return same ordering *)
  let top_3 = Order_book.Book.best_n_bids book ~n:3 () in
  let _ = assert_float_equal 51000.0 (List.nth_exn top_3 0).price "best_n_bids 1st = $51k" in
  let _ = assert_float_equal 50000.0 (List.nth_exn top_3 1).price "best_n_bids 2nd = $50k" in
  let _ = assert_float_equal 49000.0 (List.nth_exn top_3 2).price "best_n_bids 3rd = $49k" in
  ()

let test_order_book_ask_sorting () =
  printf "\n=== Order Book: Ask Sorting (Ascending) ===\n";

  let book = Order_book.Book.create ~symbol:"BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:2.0 in

  let best = Order_book.Book.best_ask book in
  let _ = assert_float_equal 51000.0 best.price "Best ask = lowest ($51k)" in

  let asks = Order_book.Book.asks_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length asks) "Got 3 asks" in

  (* Asks should be ascending: 51000, 52000, 53000 *)
  let ask_prices = List.map asks ~f:(fun (price, _level) -> price) in
  let _ = assert_float_equal 51000.0 (List.nth_exn ask_prices 0) "1st ask = $51k" in
  let _ = assert_float_equal 52000.0 (List.nth_exn ask_prices 1) "2nd ask = $52k" in
  let _ = assert_float_equal 53000.0 (List.nth_exn ask_prices 2) "3rd ask = $53k" in

  (* Verify volumes *)
  let ask_levels = List.map asks ~f:snd in
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume (List.nth_exn ask_levels 0)) "1st ask volume = 1.5" in
  let _ = assert_float_equal 1.0 (Order_book.Price_level.volume (List.nth_exn ask_levels 1)) "2nd ask volume = 1.0" in
  let _ = assert_float_equal 2.0 (Order_book.Price_level.volume (List.nth_exn ask_levels 2)) "3rd ask volume = 2.0" in

  (* best_n_asks should return same ordering *)
  let top_3 = Order_book.Book.best_n_asks book ~n:3 () in
  let _ = assert_float_equal 51000.0 (List.nth_exn top_3 0).price "best_n_asks 1st = $51k" in
  let _ = assert_float_equal 52000.0 (List.nth_exn top_3 1).price "best_n_asks 2nd = $52k" in
  let _ = assert_float_equal 53000.0 (List.nth_exn top_3 2).price "best_n_asks 3rd = $53k" in
  ()

let test_order_book_operations () =
  printf "\n=== Order Book: Set/Update/Remove Operations ===\n";

  let book = Order_book.Book.create ~symbol:"BTC/USD" in

  (* Set creates new level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume best_bid) "Set creates level with size 1.5" in
  let _ = assert_float_equal 50000.0 (Order_book.Price_level.price best_bid) "Set creates level at $50k" in

  (* Set with size=0 removes level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let best = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.price best) "Size=0 removes level (price=0)" in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.volume best) "Size=0 removes level (volume=0)" in

  (* Set replaces existing level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:2.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 2.5 (Order_book.Price_level.volume best_bid) "Set replaces: 2.5" in

  (* Same for asks *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:3.0 in
  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 3.0 (Order_book.Price_level.volume best_ask) "Ask set: size 3.0" in
  let _ = assert_float_equal 51000.0 (Order_book.Price_level.price best_ask) "Ask set: price $51k" in

  (* Remove ask *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:0.0 in
  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.0 best_ask.price "Ask removed (price=0)" in

  (* Set multiple levels, remove one *)
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:48000.0 ~size:2.0 in
  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length bids) "3 bid levels" in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:0.0 in
  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (List.length bids) "Remove middle: 2 bid levels" in
  ()

let test_order_book_epoch () =
  printf "\n=== Order Book: Epoch Tracking ===\n";

  let book = Order_book.Book.create ~symbol:"BTC/USD" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (Order_book.Book.epoch book) "After 1st set: epoch = 1" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (Order_book.Book.epoch book) "After 2nd set: epoch = 2" in

  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (Order_book.Book.epoch book) "After ask set: epoch = 3" in

  (* Removing a level also increments epoch *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    4 (Order_book.Book.epoch book) "After remove: epoch = 4" in

  (* Epoch increments monotonically on rapid updates *)
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:54000.0 ~size:3.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    7 (Order_book.Book.epoch book) "After 3 more sets: epoch = 7" in
  ()

(** ========== FLUXUM ADAPTER TESTS ========== *)

let test_fluxum_adapter_venue () =
  printf "\n=== Fluxum Adapter: Venue ===\n";

  let venue = Fluxum_adapter.Adapter.Venue.t in
  let _ = assert_equal ~equal:Types.Venue.equal ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Bybit venue "Adapter venue = Bybit" in

  (* Verify it is NOT another venue *)
  let _ = assert_false (Types.Venue.equal Types.Venue.Kraken venue) "Venue is not Kraken" in
  let _ = assert_false (Types.Venue.equal Types.Venue.Gemini venue) "Venue is not Gemini" in
  let _ = assert_false (Types.Venue.equal Types.Venue.Binance venue) "Venue is not Binance" in
  ()

(** ========== NORMALIZE TESTS ========== *)

let test_normalize_float_conversion_errors () =
  printf "\n=== Normalize: Float Conversion Errors ===\n";

  (* Test valid float conversions *)
  (match Fluxum.Normalize_common.Float_conv.of_string "123.456" with
   | Ok f ->
     let _ = assert_float_equal 123.456 f "Valid float string parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse valid float: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test integer string *)
  (match Fluxum.Normalize_common.Float_conv.of_string "42" with
   | Ok f ->
     let _ = assert_float_equal 42.0 f "Integer string parsed as float" in ()
   | Error msg ->
     printf "  FAIL: Should parse integer string: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test zero *)
  (match Fluxum.Normalize_common.Float_conv.of_string "0" with
   | Ok f ->
     let _ = assert_float_equal 0.0 f "Zero string parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse zero: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test malformed float *)
  (match Fluxum.Normalize_common.Float_conv.of_string "not_a_float" with
   | Error _ ->
     printf "  PASS: Rejected malformed float string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject malformed float\n";
     incr tests_run; incr tests_failed);

  (* Test empty string *)
  (match Fluxum.Normalize_common.Float_conv.of_string "" with
   | Error _ ->
     printf "  PASS: Rejected empty string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject empty string\n";
     incr tests_run; incr tests_failed);

  (* Test infinity *)
  (match Fluxum.Normalize_common.Float_conv.of_string "inf" with
   | Error _ ->
     printf "  PASS: Rejected non-finite float (infinity)\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject non-finite float\n";
     incr tests_run; incr tests_failed);

  (* Test negative infinity *)
  (match Fluxum.Normalize_common.Float_conv.of_string "-inf" with
   | Error _ ->
     printf "  PASS: Rejected negative infinity\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject negative infinity\n";
     incr tests_run; incr tests_failed);

  (* Test NaN *)
  (match Fluxum.Normalize_common.Float_conv.of_string "nan" with
   | Error _ ->
     printf "  PASS: Rejected NaN\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject NaN\n";
     incr tests_run; incr tests_failed);

  (* Test price validation (must be positive) *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "-100.0" with
   | Error _ ->
     printf "  PASS: Rejected negative price\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Price should be positive\n";
     incr tests_run; incr tests_failed);

  (* Test zero price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "0.0" with
   | Error _ ->
     printf "  PASS: Rejected zero price\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Price must be strictly positive\n";
     incr tests_run; incr tests_failed);

  (* Test valid price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "50000.50" with
   | Ok p ->
     let _ = assert_float_equal 50000.50 p "Valid price parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse valid price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test quantity validation (non-negative) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "-1.5" with
   | Error _ ->
     printf "  PASS: Rejected negative quantity\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Quantity cannot be negative\n";
     incr tests_run; incr tests_failed);

  (* Test zero quantity (should be allowed) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "0.0" with
   | Ok q ->
     let _ = assert_float_equal 0.0 q "Zero quantity allowed" in ()
   | Error msg ->
     printf "  FAIL: Should allow zero quantity: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test valid quantity *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "5.25" with
   | Ok q ->
     let _ = assert_float_equal 5.25 q "Valid quantity parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse valid quantity: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test malformed price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "abc" with
   | Error _ ->
     printf "  PASS: Rejected malformed price string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject malformed price\n";
     incr tests_run; incr tests_failed);

  (* Test malformed quantity *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "xyz" with
   | Error _ ->
     printf "  PASS: Rejected malformed quantity string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject malformed quantity\n";
     incr tests_run; incr tests_failed);

  ()

let test_normalize_edge_cases () =
  printf "\n=== Normalize: Edge Cases ===\n";

  (* Test very large numbers *)
  (match Fluxum.Normalize_common.Float_conv.of_string "999999999999.99" with
   | Ok f ->
     let _ = assert_float_equal 999999999999.99 f "Very large number parsed" in ()
   | Error msg ->
     printf "  FAIL: Should handle large numbers: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test very small numbers *)
  (match Fluxum.Normalize_common.Float_conv.of_string "0.000000001" with
   | Ok f ->
     let _ = assert_float_equal 0.000000001 f ~tolerance:0.0000000001 "Very small number parsed" in ()
   | Error msg ->
     printf "  FAIL: Should handle small numbers: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test scientific notation *)
  (match Fluxum.Normalize_common.Float_conv.of_string "1.23e10" with
   | Ok f ->
     let _ = assert_float_equal 12300000000.0 f "Scientific notation parsed" in ()
   | Error msg ->
     printf "  FAIL: Should handle scientific notation: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test negative scientific notation *)
  (match Fluxum.Normalize_common.Float_conv.of_string "1.5e-8" with
   | Ok f ->
     let _ = assert_float_equal 0.000000015 f ~tolerance:0.0000000001 "Negative exponent parsed" in ()
   | Error msg ->
     printf "  FAIL: Should handle negative exponents: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test leading zeros *)
  (match Fluxum.Normalize_common.Float_conv.of_string "00123.456" with
   | Ok f ->
     let _ = assert_float_equal 123.456 f "Leading zeros handled" in ()
   | Error msg ->
     printf "  FAIL: Should handle leading zeros: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test negative number with of_string *)
  (match Fluxum.Normalize_common.Float_conv.of_string "-42.5" with
   | Ok f ->
     let _ = assert_float_equal (-42.5) f "Negative number parsed with of_string" in ()
   | Error msg ->
     printf "  FAIL: Should handle negative with of_string: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test amount_of_string allows negative *)
  (match Fluxum.Normalize_common.Float_conv.amount_of_string "-100.5" with
   | Ok f ->
     let _ = assert_float_equal (-100.5) f "Negative amount allowed" in ()
   | Error msg ->
     printf "  FAIL: Should allow negative amount: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test large price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "100000.0" with
   | Ok f ->
     let _ = assert_float_equal 100000.0 f "Large price accepted" in ()
   | Error msg ->
     printf "  FAIL: Should accept large price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test small price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "0.00001" with
   | Ok f ->
     let _ = assert_float_equal 0.00001 f ~tolerance:0.000001 "Small price accepted" in ()
   | Error msg ->
     printf "  FAIL: Should accept small price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test large quantity *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "1000000.0" with
   | Ok f ->
     let _ = assert_float_equal 1000000.0 f "Large quantity accepted" in ()
   | Error msg ->
     printf "  FAIL: Should accept large quantity: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test price scientific notation *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "1.5e4" with
   | Ok f ->
     let _ = assert_float_equal 15000.0 f "Price in scientific notation" in ()
   | Error msg ->
     printf "  FAIL: Should parse scientific price: %s\n" msg;
     incr tests_run; incr tests_failed);

  ()

let test_normalize_side_conversion () =
  printf "\n=== Normalize: Side Conversion ===\n";

  (* Valid sides via Normalize_common *)
  (match Fluxum.Normalize_common.Side.of_string "buy" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Buy side "Side 'buy' = Buy" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'buy': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Side.of_string "sell" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Sell side "Side 'sell' = Sell" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'sell': %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Shorthand *)
  (match Fluxum.Normalize_common.Side.of_string "b" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Buy side "Side 'b' = Buy" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'b': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Side.of_string "s" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Sell side "Side 's' = Sell" in ()
   | Error msg ->
     printf "  FAIL: Should parse 's': %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Case-insensitive via lowercase in implementation *)
  (match Fluxum.Normalize_common.Side.of_string "BUY" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Buy side "Side 'BUY' = Buy (case insensitive)" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'BUY': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Side.of_string "SELL" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Sell side "Side 'SELL' = Sell (case insensitive)" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'SELL': %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Bid/Ask aliases *)
  (match Fluxum.Normalize_common.Side.of_string "bid" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Buy side "Side 'bid' = Buy" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'bid': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Side.of_string "ask" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Sell side "Side 'ask' = Sell" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'ask': %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Invalid side *)
  (match Fluxum.Normalize_common.Side.of_string "invalid_side" with
   | Error _ ->
     printf "  PASS: Rejected unrecognized side\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject unrecognized side\n";
     incr tests_run; incr tests_failed);

  (* Empty string side *)
  (match Fluxum.Normalize_common.Side.of_string "" with
   | Error _ ->
     printf "  PASS: Rejected empty side string\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject empty side\n";
     incr tests_run; incr tests_failed);

  ()

let test_normalize_order_status () =
  printf "\n=== Normalize: Order Status Conversion ===\n";

  (* Valid statuses *)
  (match Fluxum.Normalize_common.Order_status.of_string "new" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.New s "Status 'new' = New" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'new': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "open" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.New s "Status 'open' = New" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'open': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "pending" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.New s "Status 'pending' = New" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'pending': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "filled" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Filled s "Status 'filled' = Filled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'filled': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "closed" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Filled s "Status 'closed' = Filled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'closed': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "canceled" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Canceled s "Status 'canceled' = Canceled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'canceled': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "cancelled" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Canceled s "Status 'cancelled' = Canceled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'cancelled': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "expired" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Canceled s "Status 'expired' = Canceled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'expired': %s\n" msg;
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Order_status.of_string "partially_filled" with
   | Ok s ->
     let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
       Types.Order_status.Partially_filled s "Status 'partially_filled' = Partially_filled" in ()
   | Error msg ->
     printf "  FAIL: Should parse 'partially_filled': %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Invalid status *)
  (match Fluxum.Normalize_common.Order_status.of_string "unknown_status" with
   | Error _ ->
     printf "  PASS: Rejected unrecognized status\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  FAIL: Should reject unrecognized status\n";
     incr tests_run; incr tests_failed);

  ()

(** ========== TEST RUNNER ========== *)

let () =
  printf "\n";
  printf "+========================================================+\n";
  printf "|  Bybit Unified Adapter - Comprehensive Test Suite      |\n";
  printf "+========================================================+\n";

  (* Ledger tests *)
  test_ledger_entry_creation ();
  test_ledger_simple_trades ();
  test_ledger_fees ();
  test_ledger_partial_fills ();
  test_ledger_average_prices ();
  test_ledger_spot_updates ();
  test_ledger_cost_basis_accounting ();
  test_ledger_multi_symbol ();

  (* Order book tests *)
  test_order_book_creation ();
  test_order_book_bid_sorting ();
  test_order_book_ask_sorting ();
  test_order_book_operations ();
  test_order_book_epoch ();

  (* Fluxum adapter tests *)
  test_fluxum_adapter_venue ();

  (* Normalize tests *)
  test_normalize_float_conversion_errors ();
  test_normalize_edge_cases ();
  test_normalize_side_conversion ();
  test_normalize_order_status ();

  (* Print summary *)
  printf "\n";
  printf "+========================================================+\n";
  printf "|  Test Results Summary                                  |\n";
  printf "+--------------------------------------------------------+\n";
  printf "|  Total:  %3d tests                                     |\n" !tests_run;
  printf "|  Passed: %3d tests                                     |\n" !tests_passed;
  printf "|  Failed: %3d tests                                     |\n" !tests_failed;
  printf "+========================================================+\n";
  printf "\n";

  (match !tests_failed > 0 with
   | true ->
     printf "TEST SUITE FAILED\n\n";
     exit 1
   | false ->
     printf "ALL TESTS PASSED\n\n";
     exit 0)

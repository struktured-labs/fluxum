(** Comprehensive test suite for OKX unified adapter *)

open Core

module Ledger = Okx.Ledger
module Order_book = Okx.Order_book
module Fluxum_adapter = Okx.Fluxum_adapter
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
    printf "  OK: %s\n" msg;
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
    printf "  OK: %s\n" msg;
    true

let assert_bool expected actual msg =
  incr tests_run;
  match Bool.equal expected actual with
  | true ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  FAIL: %s (expected %b, got %b)\n" msg expected actual;
    false

let assert_error result msg =
  incr tests_run;
  match result with
  | Error _ ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true
  | Ok _ ->
    incr tests_failed;
    printf "  FAIL: %s (expected Error, got Ok)\n" msg;
    false

let assert_ok result msg =
  incr tests_run;
  match result with
  | Ok _ ->
    incr tests_passed;
    printf "  OK: %s\n" msg;
    true
  | Error e ->
    incr tests_failed;
    printf "  FAIL: %s (expected Ok, got Error: %s)\n" msg e;
    false

(** ========== LEDGER TESTS ========== *)

let test_ledger_entry_creation () =
  printf "\n=== Ledger: Entry Creation ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  let _ = assert_float_equal 0.0 entry.pnl "Empty entry has 0 pnl" in
  let _ = assert_float_equal 0.0 entry.position "Empty entry has 0 position" in
  let _ = assert_float_equal 0.0 entry.spot "Empty entry has 0 spot" in
  let _ = assert_float_equal 0.0 entry.pnl_spot "Empty entry has 0 pnl_spot" in
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
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC-USDT" entry.symbol "Symbol is set correctly" in
  let _ = assert_equal ~equal:[%equal: Types.Price.Option.t] ~sexp_of_t:[%sexp_of: Types.Price.Option.t]
    None entry.price "Empty entry has None price" in
  let _ = assert_equal ~equal:[%equal: Types.Side.Option.t] ~sexp_of_t:[%sexp_of: Types.Side.Option.t]
    None entry.side "Empty entry has None side" in
  let _ = assert_equal ~equal:[%equal: float option] ~sexp_of_t:[%sexp_of: float option]
    None entry.qty "Empty entry has None qty" in
  let _ = assert_equal ~equal:[%equal: Types.Price.Option.t] ~sexp_of_t:[%sexp_of: Types.Price.Option.t]
    None entry.package_price "Empty entry has None package_price" in
  let _ = assert_bool true (Float.equal entry.position 0.0) "Position is exactly zero" in
  let _ = assert_bool true (Float.equal entry.pnl 0.0) "PnL is exactly zero" in
  ()

let test_ledger_simple_trades () =
  printf "\n=== Ledger: Simple Trades ===\n";

  (* Buy 1 BTC at $50,000 *)
  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in
  let _ = assert_float_equal 1.0 entry.position "Buy 1 BTC: position = 1.0" in
  let _ = assert_float_equal 50000.0 entry.cost_basis "Buy 1 BTC: cost_basis = $50k" in
  let _ = assert_float_equal 0.0 entry.pnl "Buy at entry: pnl = 0" in
  let _ = assert_float_equal 50000.0 entry.pnl_spot "Buy 1 BTC: pnl_spot = price * position" in
  let _ = assert_float_equal 1.0 entry.total_buy_qty "Buy: total_buy_qty = 1.0" in
  let _ = assert_float_equal 50000.0 entry.buy_notional "Buy: buy_notional = $50k" in
  let _ = assert_float_equal 50000.0 entry.avg_buy_price "Buy: avg_buy_price = $50k" in

  (* Sell 1 BTC at $55,000 - $5k profit *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 0.0 entry.position "After sell: position = 0" in
  let _ = assert_float_equal 5000.0 entry.pnl "Profit: pnl = $5k" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Flat position: cost_basis = 0" in
  let _ = assert_float_equal 1.0 entry.total_sell_qty "Sell: total_sell_qty = 1.0" in
  let _ = assert_float_equal 55000.0 entry.sell_notional "Sell: sell_notional = $55k" in
  let _ = assert_float_equal 55000.0 entry.avg_sell_price "Sell: avg_sell_price = $55k" in
  ()

let test_ledger_fees () =
  printf "\n=== Ledger: Fee Handling ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  (* Buy 1 BTC at $50k with $25 fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 ~fee_usd:25.0 in
  let _ = assert_float_equal 50025.0 entry.cost_basis "Fee in cost basis: $50,025" in
  let _ = assert_float_equal (-25.0) entry.pnl "Fee creates loss on buy: pnl = -$25" in

  (* Sell at breakeven price should show loss due to fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal (-50.0) entry.pnl "Fee creates total loss: pnl = -$50 (fee on buy + cost basis drag)" in

  (* Buy with fee, sell at higher price to cover fee *)
  let entry2 = Ledger.Entry.create ~symbol:"ETH-USDT" () in
  let entry2 = Ledger.Entry.on_trade entry2 ~price:3000.0 ~side:Buy ~qty:1.0 ~fee_usd:10.0 in
  let _ = assert_float_equal 3010.0 entry2.cost_basis "ETH fee in cost basis: $3,010" in

  (* Sell with fee *)
  let entry2 = Ledger.Entry.on_trade entry2 ~price:3100.0 ~side:Sell ~qty:1.0 ~fee_usd:10.0 in
  let _ = assert_float_equal 0.0 entry2.position "After sell: flat" in
  (* pnl = -10 (buy fee) + (3100*1 - 3010) - 10 (sell fee) = -10 + 90 - 10 = 70 *)
  let _ = assert_float_equal 70.0 entry2.pnl "Sell at $3100 with fees: pnl = $70" in
  ()

let test_ledger_partial_fills () =
  printf "\n=== Ledger: Partial Position Close ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  (* Buy 4 BTC at $50k each *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:4.0 in
  let _ = assert_float_equal 4.0 entry.position "Buy 4 BTC: position = 4.0" in
  let _ = assert_float_equal 200000.0 entry.cost_basis "Buy 4 BTC: cost_basis = $200k" in
  let _ = assert_float_equal 4.0 entry.total_buy_qty "total_buy_qty = 4.0" in

  (* Sell 1 BTC - reduces cost basis proportionally (1/4) *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 3.0 entry.position "Sell 1/4: position = 3.0" in
  let _ = assert_float_equal 150000.0 entry.cost_basis "Sell 1/4: cost_basis = $150k" in
  (* pnl = (55000*1 - 200000/4*1) = 55000 - 50000 = 5000 *)
  let _ = assert_float_equal 5000.0 entry.pnl "Profit from 1/4 sell: pnl = $5k" in

  (* Sell another 1 BTC at $52k *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 2.0 entry.position "Sell another: position = 2.0" in
  (* cost_basis = 150000 * (3-1)/3 = 100000 *)
  let _ = assert_float_equal 100000.0 entry.cost_basis "Sell 1/3 remaining: cost_basis = $100k" in
  (* pnl = 5000 + (52000 - 150000/3) = 5000 + (52000 - 50000) = 7000 *)
  let _ = assert_float_equal 7000.0 entry.pnl "After second sell: pnl = $7k" in

  (* Sell remaining 2 BTC at $48k (loss on these) *)
  let entry = Ledger.Entry.on_trade entry ~price:48000.0 ~side:Sell ~qty:2.0 in
  let _ = assert_float_equal 0.0 entry.position "Fully closed: position = 0" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Fully closed: cost_basis = 0" in
  (* pnl = 7000 + (48000*2 - 100000/2*2) = 7000 + (96000 - 100000) = 7000 - 4000 = 3000 *)
  let _ = assert_float_equal 3000.0 entry.pnl "Final pnl after all sells = $3k" in
  ()

let test_ledger_average_prices () =
  printf "\n=== Ledger: Average Price Calculations ===\n";

  let entry = Ledger.Entry.create ~symbol:"ETH-USDT" () in
  (* Buy at different prices *)
  let entry = Ledger.Entry.on_trade entry ~price:3000.0 ~side:Buy ~qty:10.0 in
  let entry = Ledger.Entry.on_trade entry ~price:3200.0 ~side:Buy ~qty:5.0 in
  let entry = Ledger.Entry.on_trade entry ~price:2800.0 ~side:Buy ~qty:5.0 in

  (* Average = (10*3000 + 5*3200 + 5*2800) / 20 = (30000 + 16000 + 14000) / 20 = 3000 *)
  let _ = assert_float_equal 3000.0 entry.avg_buy_price "Weighted avg buy = $3,000" in
  let _ = assert_float_equal 20.0 entry.total_buy_qty "Total buy qty = 20" in
  let _ = assert_float_equal 20.0 entry.position "Position = 20" in
  (* buy_notional = 10*3000 + 5*3200 + 5*2800 = 60000 *)
  let _ = assert_float_equal 60000.0 entry.buy_notional "Buy notional = $60k" in

  (* avg_price when only buys = buy_notional / total_buy_qty *)
  let _ = assert_float_equal 3000.0 entry.avg_price "Overall avg (buys only) = $3,000" in

  (* Sell some *)
  let entry = Ledger.Entry.on_trade entry ~price:3500.0 ~side:Sell ~qty:5.0 in
  let _ = assert_float_equal 3500.0 entry.avg_sell_price "Avg sell = $3,500" in
  let _ = assert_float_equal 5.0 entry.total_sell_qty "Total sell qty = 5.0" in

  (* Overall avg = (60000 + 17500) / (20 + 5) = 77500 / 25 = 3100 *)
  let _ = assert_float_equal 3100.0 entry.avg_price "Overall avg = $3,100" in
  let _ = assert_float_equal 15.0 entry.position "Position after sell = 15" in
  ()

let test_ledger_spot_updates () =
  printf "\n=== Ledger: Spot Price Updates (Unrealized P&L) ===\n";

  (* OKX ledger uses pnl_spot = price * position after trades *)
  (* To test spot-like updates, we use on_market_data with a book *)
  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 in

  (* After a buy, pnl_spot = price * position = 50000 * 1 = 50000 *)
  let _ = assert_float_equal 50000.0 entry.pnl_spot "After buy: pnl_spot = $50k" in
  let _ = assert_float_equal 0.0 entry.pnl "After buy: pnl = 0" in

  (* Sell at higher price - pnl_spot should reflect new spot *)
  let entry_sold = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:0.5 in
  (* position = 0.5, pnl_spot = 55000 * 0.5 = 27500 *)
  let _ = assert_float_equal 0.5 entry_sold.position "After partial sell: position = 0.5" in
  let _ = assert_float_equal 27500.0 entry_sold.pnl_spot "pnl_spot = $27,500" in

  (* Test on_market_data for spot updates *)
  let book = Order_book.Book.create ~symbol:"BTC-USDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:55000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:55100.0 ~size:1.0 in
  let entry_updated = Ledger.Entry.on_market_data entry ~book in
  (* mid_price = (55000 + 55100) / 2 = 55050 *)
  (* pnl_spot = 1.0 * 55050 = 55050 *)
  let _ = assert_float_equal 55050.0 entry_updated.pnl_spot "Market data: pnl_spot = $55,050" in

  (* Lower spot *)
  let book2 = Order_book.Book.create ~symbol:"BTC-USDT" in
  let book2 = Order_book.Book.set book2 ~side:`Bid ~price:45000.0 ~size:1.0 in
  let book2 = Order_book.Book.set book2 ~side:`Ask ~price:45100.0 ~size:1.0 in
  let entry_lower = Ledger.Entry.on_market_data entry ~book:book2 in
  (* mid_price = (45000 + 45100) / 2 = 45050 *)
  (* pnl_spot = 1.0 * 45050 = 45050 *)
  let _ = assert_float_equal 45050.0 entry_lower.pnl_spot "Lower spot: pnl_spot = $45,050" in
  ()

let test_ledger_cost_basis_accounting () =
  printf "\n=== Ledger: Cost Basis Accounting ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC-USDT" () in
  (* Buy 3 BTC at $50k = $150k cost basis *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:3.0 in
  let _ = assert_float_equal 150000.0 entry.cost_basis "Initial cost_basis = $150k" in
  let _ = assert_float_equal 3.0 entry.position "Position = 3.0" in

  (* Sell 1 BTC - reduces cost basis by 1/3 *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 100000.0 entry.cost_basis "After sell 1/3: cost_basis = $100k" in
  let _ = assert_float_equal 2.0 entry.position "Position = 2.0" in

  (* Sell 1 more - reduces by 1/2 of remaining *)
  let entry = Ledger.Entry.on_trade entry ~price:52000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 50000.0 entry.cost_basis "After sell 1/2: cost_basis = $50k" in
  let _ = assert_float_equal 1.0 entry.position "Position = 1.0" in

  (* Buy 1 more BTC at $48k - adds to cost basis *)
  let entry = Ledger.Entry.on_trade entry ~price:48000.0 ~side:Buy ~qty:1.0 in
  let _ = assert_float_equal 98000.0 entry.cost_basis "After buy 1 more: cost_basis = $98k" in
  let _ = assert_float_equal 2.0 entry.position "Position = 2.0" in

  (* Sell all at $51k *)
  let entry = Ledger.Entry.on_trade entry ~price:51000.0 ~side:Sell ~qty:2.0 in
  let _ = assert_float_equal 0.0 entry.position "Flat" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Flat: cost_basis = 0" in

  (* Total realized: from sells *)
  (* Sell 1 @ 55000: pnl += 55000 - 50000 = 5000 *)
  (* Sell 1 @ 52000: pnl += 52000 - 50000 = 2000 *)
  (* Sell 2 @ 51000: pnl += 51000*2 - 98000 = 102000 - 98000 = 4000 *)
  (* Total = 5000 + 2000 + 4000 = 11000 *)
  let _ = assert_float_equal 11000.0 entry.pnl "Total realized pnl = $11k" in
  ()

let test_ledger_multi_symbol () =
  printf "\n=== Ledger: Multi-Symbol Tracking ===\n";

  let ledger = Fluxum.Types.Symbol.Map.empty in

  (* Trade BTC *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC-USDT" ~price:50000.0 ~side:Buy ~qty:1.0 in
  (* Trade ETH *)
  let ledger = Ledger.on_trade ledger ~symbol:"ETH-USDT" ~price:3000.0 ~side:Buy ~qty:10.0 in

  (* Check BTC *)
  let btc = Map.find_exn ledger "BTC-USDT" in
  let _ = assert_float_equal 1.0 btc.position "BTC position = 1.0" in
  let _ = assert_float_equal 50000.0 btc.cost_basis "BTC cost_basis = $50k" in

  (* Check ETH *)
  let eth = Map.find_exn ledger "ETH-USDT" in
  let _ = assert_float_equal 10.0 eth.position "ETH position = 10.0" in
  let _ = assert_float_equal 30000.0 eth.cost_basis "ETH cost_basis = $30k" in

  (* Trade more on each symbol *)
  let ledger = Ledger.on_trade ledger ~symbol:"BTC-USDT" ~price:52000.0 ~side:Buy ~qty:0.5 in
  let ledger = Ledger.on_trade ledger ~symbol:"ETH-USDT" ~price:3200.0 ~side:Sell ~qty:5.0 in

  let btc2 = Map.find_exn ledger "BTC-USDT" in
  let _ = assert_float_equal 1.5 btc2.position "BTC position after 2nd buy = 1.5" in
  let _ = assert_float_equal 76000.0 btc2.cost_basis "BTC cost_basis = $76k (50k + 26k)" in

  let eth2 = Map.find_exn ledger "ETH-USDT" in
  let _ = assert_float_equal 5.0 eth2.position "ETH position after partial sell = 5.0" in
  (* cost_basis = 30000 * (10 - 5) / 10 = 15000 *)
  let _ = assert_float_equal 15000.0 eth2.cost_basis "ETH cost_basis after sell = $15k" in

  (* Verify symbols are independent *)
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (Map.length ledger) "Ledger has 2 symbols" in
  ()

(** ========== ORDER BOOK TESTS ========== *)

let test_order_book_creation () =
  printf "\n=== Order Book: Creation ===\n";

  let book = Order_book.Book.create ~symbol:"BTC-USDT" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC-USDT" (Order_book.Book.symbol book) "Symbol is set" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 best_bid.price "Empty book: best_bid = 0" in

  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.0 best_ask.price "Empty book: best_ask = 0" in

  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (List.length bids) "Empty book: 0 bids" in

  let asks = Order_book.Book.asks_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (List.length asks) "Empty book: 0 asks" in

  (* Mid price and spread of empty book *)
  let _ = assert_float_equal 0.0 (Order_book.Book.mid_price book) "Empty book: mid_price = 0" in
  let _ = assert_float_equal 0.0 (Order_book.Book.spread book) "Empty book: spread = 0" in
  ()

let test_order_book_bid_sorting () =
  printf "\n=== Order Book: Bid Sorting (Descending) ===\n";

  let book = Order_book.Book.create ~symbol:"BTC-USDT" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in

  let best = Order_book.Book.best_bid book in
  let _ = assert_float_equal 51000.0 best.price "Best bid = highest ($51k)" in

  let top_3 = Order_book.Book.best_n_bids book ~n:3 () in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length top_3) "Got 3 bids" in
  let _ = assert_float_equal 51000.0 (List.nth_exn top_3 0).price "1st bid = $51k" in
  let _ = assert_float_equal 50000.0 (List.nth_exn top_3 1).price "2nd bid = $50k" in
  let _ = assert_float_equal 49000.0 (List.nth_exn top_3 2).price "3rd bid = $49k" in

  (* Verify bids_alist is also descending *)
  let bids = Order_book.Book.bids_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length bids) "bids_alist has 3 levels" in
  let _ = assert_float_equal 51000.0 (fst (List.nth_exn bids 0)) "bids_alist[0] price = $51k" in
  let _ = assert_float_equal 50000.0 (fst (List.nth_exn bids 1)) "bids_alist[1] price = $50k" in
  let _ = assert_float_equal 49000.0 (fst (List.nth_exn bids 2)) "bids_alist[2] price = $49k" in

  (* Verify volumes via Price_level *)
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume (snd (List.nth_exn bids 0))) "Best bid volume = 1.5" in
  let _ = assert_float_equal 1.0 (Order_book.Price_level.volume (snd (List.nth_exn bids 1))) "2nd bid volume = 1.0" in
  let _ = assert_float_equal 2.0 (Order_book.Price_level.volume (snd (List.nth_exn bids 2))) "3rd bid volume = 2.0" in
  ()

let test_order_book_ask_sorting () =
  printf "\n=== Order Book: Ask Sorting (Ascending) ===\n";

  let book = Order_book.Book.create ~symbol:"BTC-USDT" in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.5 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:2.0 in

  let best = Order_book.Book.best_ask book in
  let _ = assert_float_equal 51000.0 best.price "Best ask = lowest ($51k)" in

  let top_3 = Order_book.Book.best_n_asks book ~n:3 () in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length top_3) "Got 3 asks" in
  let _ = assert_float_equal 51000.0 (List.nth_exn top_3 0).price "1st ask = $51k" in
  let _ = assert_float_equal 52000.0 (List.nth_exn top_3 1).price "2nd ask = $52k" in
  let _ = assert_float_equal 53000.0 (List.nth_exn top_3 2).price "3rd ask = $53k" in

  (* Verify asks_alist is also ascending *)
  let asks = Order_book.Book.asks_alist book in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (List.length asks) "asks_alist has 3 levels" in
  let _ = assert_float_equal 51000.0 (fst (List.nth_exn asks 0)) "asks_alist[0] price = $51k" in
  let _ = assert_float_equal 52000.0 (fst (List.nth_exn asks 1)) "asks_alist[1] price = $52k" in
  let _ = assert_float_equal 53000.0 (fst (List.nth_exn asks 2)) "asks_alist[2] price = $53k" in

  (* Verify volumes via Price_level *)
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume (snd (List.nth_exn asks 0))) "Best ask volume = 1.5" in
  let _ = assert_float_equal 1.0 (Order_book.Price_level.volume (snd (List.nth_exn asks 1))) "2nd ask volume = 1.0" in
  let _ = assert_float_equal 2.0 (Order_book.Price_level.volume (snd (List.nth_exn asks 2))) "3rd ask volume = 2.0" in
  ()

let test_order_book_operations () =
  printf "\n=== Order Book: Set/Update/Remove Operations ===\n";

  let book = Order_book.Book.create ~symbol:"BTC-USDT" in

  (* Set creates new level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 1.5 (Order_book.Price_level.volume best_bid) "Set creates level with size 1.5" in
  let _ = assert_float_equal 50000.0 (Order_book.Price_level.price best_bid) "Set creates level at $50k" in

  (* Set with size=0 removes level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let best = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.price best) "Size=0 removes level" in

  (* Set replaces existing level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:2.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 2.5 (Order_book.Price_level.volume best_bid) "Set replaces: 2.5" in

  (* Set on ask side *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:3.0 in
  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 3.0 (Order_book.Price_level.volume best_ask) "Ask set: size 3.0" in
  let _ = assert_float_equal 51000.0 (Order_book.Price_level.price best_ask) "Ask set at $51k" in

  (* Replace ask *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:0.5 in
  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.5 (Order_book.Price_level.volume best_ask) "Ask replaced: 0.5" in

  (* Remove ask with 0 *)
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:0.0 in
  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.0 (Order_book.Price_level.price best_ask) "Ask removed: price = 0" in
  ()

let test_order_book_epoch () =
  printf "\n=== Order Book: Epoch Tracking ===\n";

  let book = Order_book.Book.create ~symbol:"BTC-USDT" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (Order_book.Book.epoch book) "After set: epoch = 1" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (Order_book.Book.epoch book) "After update: epoch = 2" in

  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 (Order_book.Book.epoch book) "After ask set: epoch = 3" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    4 (Order_book.Book.epoch book) "After remove: epoch = 4" in

  (* set_many increments for each update *)
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    5 (Order_book.Book.epoch book) "After 5th operation: epoch = 5" in
  ()

(** ========== FLUXUM ADAPTER TESTS ========== *)

let test_fluxum_adapter_venue () =
  printf "\n=== Fluxum Adapter: Venue ===\n";

  let venue = Fluxum_adapter.Adapter.Venue.t in
  let _ = assert_equal ~equal:Types.Venue.equal ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Okx venue "Adapter venue = Okx" in

  (* Verify it has correct string representation *)
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "OKX" (Types.Venue.to_string venue) "Venue to_string = OKX" in
  ()

(** ========== NORMALIZE COMMON TESTS ========== *)

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

  (* Test malformed float *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.of_string "not_a_float")
    "Rejected malformed float string" in

  (* Test empty string *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.of_string "")
    "Rejected empty string" in

  (* Test infinity *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.of_string "inf")
    "Rejected non-finite float (infinity)" in

  (* Test negative infinity *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.of_string "-inf")
    "Rejected negative infinity" in

  (* Test NaN *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.of_string "nan")
    "Rejected NaN" in

  (* Test price validation: must be positive *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.price_of_string "-100.0")
    "Rejected negative price" in

  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.price_of_string "0.0")
    "Rejected zero price" in

  (* Test valid price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "50000.50" with
   | Ok p ->
     let _ = assert_float_equal 50000.50 p "Valid price parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse valid price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test quantity validation: non-negative *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.qty_of_string "-1.5")
    "Rejected negative quantity" in

  (* Test zero quantity (should be allowed) *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "0.0" with
   | Ok q ->
     let _ = assert_float_equal 0.0 q "Zero quantity allowed" in ()
   | Error msg ->
     printf "  FAIL: Should allow zero quantity: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test valid quantity *)
  (match Fluxum.Normalize_common.Float_conv.qty_of_string "10.5" with
   | Ok q ->
     let _ = assert_float_equal 10.5 q "Valid quantity parsed" in ()
   | Error msg ->
     printf "  FAIL: Should parse valid quantity: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test amount_of_string allows negatives *)
  (match Fluxum.Normalize_common.Float_conv.amount_of_string "-500.0" with
   | Ok a ->
     let _ = assert_float_equal (-500.0) a "Negative amount allowed" in ()
   | Error msg ->
     printf "  FAIL: Should allow negative amount: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test price_of_string with malformed input *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.price_of_string "abc")
    "Rejected malformed price string" in

  (* Test qty_of_string with malformed input *)
  let _ = assert_error
    (Fluxum.Normalize_common.Float_conv.qty_of_string "xyz")
    "Rejected malformed qty string" in
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

  (* Test negative value *)
  (match Fluxum.Normalize_common.Float_conv.of_string "-42.5" with
   | Ok f ->
     let _ = assert_float_equal (-42.5) f "Negative value parsed" in ()
   | Error msg ->
     printf "  FAIL: Should handle negative values: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test leading/trailing zeros *)
  (match Fluxum.Normalize_common.Float_conv.of_string "0050.500" with
   | Ok f ->
     let _ = assert_float_equal 50.5 f "Leading zeros handled" in ()
   | Error msg ->
     printf "  FAIL: Should handle leading zeros: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test very small price as valid *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "0.00000001" with
   | Ok p ->
     let _ = assert_float_equal 0.00000001 p ~tolerance:0.000000001 "Very small positive price" in ()
   | Error msg ->
     printf "  FAIL: Should accept very small positive price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test scientific notation as price *)
  (match Fluxum.Normalize_common.Float_conv.price_of_string "1e5" with
   | Ok p ->
     let _ = assert_float_equal 100000.0 p "Scientific notation price" in ()
   | Error msg ->
     printf "  FAIL: Should accept scientific notation price: %s\n" msg;
     incr tests_run; incr tests_failed);

  (* Test Side.of_string valid cases *)
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "buy")
    "Side 'buy' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "sell")
    "Side 'sell' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "BUY")
    "Side 'BUY' is valid (case insensitive)" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "SELL")
    "Side 'SELL' is valid (case insensitive)" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "b")
    "Side 'b' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "s")
    "Side 's' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "bid")
    "Side 'bid' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Side.of_string "ask")
    "Side 'ask' is valid" in

  (* Test Side.of_string invalid *)
  let _ = assert_error
    (Fluxum.Normalize_common.Side.of_string "invalid_side")
    "Side 'invalid_side' rejected" in
  let _ = assert_error
    (Fluxum.Normalize_common.Side.of_string "")
    "Side '' rejected" in

  (* Test Side.of_string actual values *)
  (match Fluxum.Normalize_common.Side.of_string "buy" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Buy side "Side 'buy' = Buy" in ()
   | Error _ -> ());
  (match Fluxum.Normalize_common.Side.of_string "sell" with
   | Ok side ->
     let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
       Types.Side.Sell side "Side 'sell' = Sell" in ()
   | Error _ -> ());

  (* Test Order_status.of_string *)
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "new")
    "Status 'new' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "filled")
    "Status 'filled' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "canceled")
    "Status 'canceled' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "cancelled")
    "Status 'cancelled' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "expired")
    "Status 'expired' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "partially_filled")
    "Status 'partially_filled' is valid" in
  let _ = assert_ok
    (Fluxum.Normalize_common.Order_status.of_string "rejected")
    "Status 'rejected' is valid" in
  let _ = assert_error
    (Fluxum.Normalize_common.Order_status.of_string "unknown_status")
    "Status 'unknown_status' rejected" in

  (* Test Result_util.transpose *)
  (match Fluxum.Normalize_common.Result_util.transpose [Ok 1; Ok 2; Ok 3] with
   | Ok l ->
     let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
       3 (List.length l) "Transpose all Ok: length 3" in ()
   | Error _ ->
     printf "  FAIL: Transpose all Ok should succeed\n";
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Result_util.transpose [Ok 1; Error "bad"; Ok 3] with
   | Error _ ->
     incr tests_run; incr tests_passed;
     printf "  OK: Transpose with Error returns Error\n"
   | Ok _ ->
     printf "  FAIL: Transpose with Error should fail\n";
     incr tests_run; incr tests_failed);

  (match Fluxum.Normalize_common.Result_util.transpose [] with
   | Ok l ->
     let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
       0 (List.length l) "Transpose empty list: Ok []" in ()
   | Error _ ->
     printf "  FAIL: Transpose empty list should be Ok []\n";
     incr tests_run; incr tests_failed);
  ()

(** ========== TEST RUNNER ========== *)

let () =
  printf "\n";
  printf "========================================================\n";
  printf "  OKX Unified Adapter - Comprehensive Test Suite\n";
  printf "========================================================\n";

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

  (* Normalize common tests *)
  test_normalize_float_conversion_errors ();
  test_normalize_edge_cases ();

  (* Print summary *)
  printf "\n";
  printf "========================================================\n";
  printf "  Test Results Summary\n";
  printf "--------------------------------------------------------\n";
  printf "  Total:  %3d tests\n" !tests_run;
  printf "  Passed: %3d tests\n" !tests_passed;
  printf "  Failed: %3d tests\n" !tests_failed;
  printf "========================================================\n";
  printf "\n";

  (match !tests_failed > 0 with
   | true ->
     printf "TEST SUITE FAILED\n\n";
     exit 1
   | false ->
     printf "ALL TESTS PASSED\n\n";
     exit 0)

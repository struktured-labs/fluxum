(** Comprehensive test suite for Kraken unified adapter *)

open Core

module Ledger = Kraken.Ledger
module Order_book = Kraken.Order_book
module Fluxum_adapter = Kraken.Fluxum_adapter
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
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match equal expected actual with
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true

(** ========== LEDGER TESTS ========== *)

let test_ledger_entry_creation () =
  printf "\n=== Ledger: Entry Creation ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  let _ = assert_float_equal 0.0 entry.pnl "Empty entry has 0 pnl" in
  let _ = assert_float_equal 0.0 entry.position "Empty entry has 0 position" in
  let _ = assert_float_equal 0.0 entry.spot "Empty entry has 0 spot" in
  let _ = assert_float_equal 0.0 entry.notional "Empty entry has 0 notional" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Empty entry has 0 cost_basis" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC/USD" entry.symbol "Symbol is set correctly" in
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

  (* Sell 1 BTC at $55,000 - $5k profit *)
  let entry = Ledger.Entry.on_trade entry ~price:55000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal 0.0 entry.position "After sell: position = 0" in
  let _ = assert_float_equal 5000.0 entry.notional "After sell: notional = $5k profit" in
  let _ = assert_float_equal 5000.0 entry.pnl "Profit: pnl = $5k" in
  let _ = assert_float_equal 0.0 entry.cost_basis "Flat position: cost_basis = 0" in
  ()

let test_ledger_fees () =
  printf "\n=== Ledger: Fee Handling ===\n";

  let entry = Ledger.Entry.create ~symbol:"BTC/USD" () in
  (* Buy 1 BTC at $50k with $25 fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Buy ~qty:1.0 ~fee_usd:25.0 in
  let _ = assert_float_equal (-50025.0) entry.notional "Fee reduces cash: notional = -$50,025" in
  let _ = assert_float_equal 50025.0 entry.cost_basis "Fee in cost basis: $50,025" in

  (* Sell at breakeven price should show loss due to fee *)
  let entry = Ledger.Entry.on_trade entry ~price:50000.0 ~side:Sell ~qty:1.0 in
  let _ = assert_float_equal (-25.0) entry.pnl "Fee creates loss: pnl = -$25" in
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

  (* Sell some *)
  let entry = Ledger.Entry.on_trade entry ~price:3500.0 ~side:Sell ~qty:5.0 in
  let _ = assert_float_equal 3500.0 entry.avg_sell_price "Avg sell = $3,500" in

  (* Overall avg = (3000*20 + 3500*5) / 25 = 3100 *)
  let _ = assert_float_equal 3100.0 entry.avg_price "Overall avg = $3,100" in
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
  ()

(** ========== ORDER BOOK TESTS ========== *)

let test_order_book_creation () =
  printf "\n=== Order Book: Creation ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTC/USD" (Order_book.Book.symbol book) "Symbol is set" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 best_bid.price "Empty book: best_bid = 0" in

  let best_ask = Order_book.Book.best_ask book in
  let _ = assert_float_equal 0.0 best_ask.price "Empty book: best_ask = 0" in
  ()

let test_order_book_bid_sorting () =
  printf "\n=== Order Book: Bid Sorting (Descending) ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
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
  ()

let test_order_book_ask_sorting () =
  printf "\n=== Order Book: Ask Sorting (Ascending) ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
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
  ()

let test_order_book_operations () =
  printf "\n=== Order Book: Set/Update/Remove Operations ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in

  (* Set creates new level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Set creates level with size 1.5" in

  (* Set with size=0 removes level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.0 in
  let best = Order_book.Book.best_bid book in
  let _ = assert_float_equal 0.0 (Exchange_common.Order_book_base.Price_level.price best) "Size=0 removes level" in

  (* Set replaces existing level *)
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.5 in
  let best_bid = Order_book.Book.best_bid book in
  let _ = assert_float_equal 1.5 (Exchange_common.Order_book_base.Price_level.volume best_bid) "Set replaces: 1.5" in

  ()

(* DISABLED: Uses old API (ask_market_price)
let test_order_book_market_price () =
  printf "\n=== Order Book: Market Price Calculations ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:52000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:53000.0 ~size:3.0 in

  (* Buy 0.5 BTC - takes from $51k level *)
  let result = Order_book.Book.ask_market_price book ~volume:0.5 in
  let _ = assert_float_equal 51000.0 result.price "Buy 0.5 BTC @ $51k" in
  let _ = assert_float_equal 0.5 result.volume "Filled 0.5 BTC" in

  (* Buy 2.5 BTC - takes 1.0@$51k + 1.5@$52k *)
  (* Cost = 1.0*51000 + 1.5*52000 = 51000 + 78000 = 129000 *)
  (* Avg = 129000/2.5 = 51600 *)
  let result = Order_book.Book.ask_market_price book ~volume:2.5 in
  let _ = assert_float_equal 51600.0 result.price "Buy 2.5 BTC avg = $51,600" in
  let _ = assert_float_equal 2.5 result.volume "Filled 2.5 BTC" in

  (* Buy more than available - partial fill *)
  let result = Order_book.Book.ask_market_price book ~volume:10.0 in
  let _ = assert_float_equal 6.0 result.volume "Only 6.0 BTC available" in
  ()
*)

(* DISABLED: Uses old API (bid_market_price)
let test_order_book_bid_market_price () =
  printf "\n=== Order Book: Bid Market Price (Selling) ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:49000.0 ~size:2.0 in
  let book = Order_book.Book.set book ~side:`Bid ~price:48000.0 ~size:3.0 in

  (* Sell 0.5 BTC - takes best bid $50k *)
  let result = Order_book.Book.bid_market_price book ~volume:0.5 in
  let _ = assert_float_equal 50000.0 result.price "Sell 0.5 BTC @ $50k" in

  (* Sell 2.5 BTC - takes 1.0@$50k + 1.5@$49k *)
  (* Cost = 1.0*50000 + 1.5*49000 = 50000 + 73500 = 123500 *)
  (* Avg = 123500/2.5 = 49400 *)
  let result = Order_book.Book.bid_market_price book ~volume:2.5 in
  let _ = assert_float_equal 49400.0 result.price "Sell 2.5 BTC avg = $49,400" in
  ()
*)

(* DISABLED: Uses old API (mid_market_price)
let test_order_book_mid_price () =
  printf "\n=== Order Book: Mid-Market Price ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:1.0 in

  let result = Order_book.Book.mid_market_price book ~volume:0.5 in
  (* Mid = (50000 + 51000) / 2 = 50500 *)
  let _ = assert_float_equal 50500.0 result.price "Mid price = $50,500" in
  ()
*)

(* DISABLED: Uses old API (quantity_from_notional)
let test_order_book_quantity_conversions () =
  printf "\n=== Order Book: Notional ↔ Quantity Conversions ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:5.0 in
  let book = Order_book.Book.set book ~side:`Ask ~price:51000.0 ~size:5.0 in

  (* $100k / $50k = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_bid book ~notional:100000.0 in
  let _ = assert_float_equal 2.0 qty "Bid: $100k = 2.0 BTC @ $50k" in

  (* $102k / $51k = 2.0 BTC *)
  let qty = Order_book.Book.quantity_from_notional_ask book ~notional:102000.0 in
  let _ = assert_float_equal 2.0 qty "Ask: $102k = 2.0 BTC @ $51k" in
  ()
*)

let test_order_book_epoch () =
  printf "\n=== Order Book: Epoch Tracking ===\n";

  let book = Order_book.Book.create ~symbol: "BTC/USD" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 (Order_book.Book.epoch book) "Initial epoch = 0" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:1.0 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    1 (Order_book.Book.epoch book) "After set: epoch = 1" in

  let book = Order_book.Book.set book ~side:`Bid ~price:50000.0 ~size:0.5 in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (Order_book.Book.epoch book) "After set again: epoch = 2" in
  ()

(* DISABLED: Uses old API (Books.set/update)
let test_order_book_multi_symbol () =
  printf "\n=== Order Book: Multi-Symbol Books ===\n";

  let books = Order_book.Books.empty in

  (* Add BTC and ETH books *)
  let books = Order_book.Books.set books ~symbol:"BTC/USD" ~side:`Bid ~price:50000.0 ~size:1.0 in
  let books = Order_book.Books.set books ~symbol:"ETH/USD" ~side:`Bid ~price:3000.0 ~size:10.0 in

  let symbols = Order_book.Books.symbols books in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 (List.length symbols) "Books has 2 symbols" in

  (* Check BTC book *)
  let btc_book = Order_book.Books.book_exn books "BTC/USD" in
  let best = Order_book.Book.best_bid btc_book in
  let _ = assert_float_equal 50000.0 best.price "BTC best bid = $50k" in

  (* Check ETH book *)
  let eth_book = Order_book.Books.book_exn books "ETH/USD" in
  let best = Order_book.Book.best_bid eth_book in
  let _ = assert_float_equal 3000.0 best.price "ETH best bid = $3k" in

  (* Update BTC book *)
  let books = Order_book.Books.update books ~symbol:"BTC/USD" ~side:`Bid ~price:50000.0 ~size:0.5 in
  let btc_book = Order_book.Books.book_exn books "BTC/USD" in
  let best = Order_book.Book.best_bid btc_book in
  let _ = assert_float_equal 1.5 best.volume "BTC bid volume = 1.5 after update" in
  ()
*)

(** ========== FLUXUM ADAPTER TESTS ========== *)

let test_fluxum_adapter_side_conversion () =
  printf "\n=== Fluxum Adapter: Side Conversion ===\n";

  (* Test side conversion from string *)
  let buy = Fluxum_adapter.Adapter.Normalize.side_of_string "buy" in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Buy buy "side_of_string 'buy' = Buy" in

  let sell = Fluxum_adapter.Adapter.Normalize.side_of_string "sell" in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Sell sell "side_of_string 'sell' = Sell" in

  let buy_b = Fluxum_adapter.Adapter.Normalize.side_of_string "b" in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Buy buy_b "side_of_string 'b' = Buy" in

  let sell_s = Fluxum_adapter.Adapter.Normalize.side_of_string "s" in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Sell sell_s "side_of_string 's' = Sell" in

  (* Test side conversion from Common.Side.t *)
  let buy_common = Fluxum_adapter.Adapter.Normalize.side_of_common `Buy in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Buy buy_common "side_of_common Buy = Buy" in

  let sell_common = Fluxum_adapter.Adapter.Normalize.side_of_common `Sell in
  let _ = assert_equal ~equal:Types.Side.equal ~sexp_of_t:Types.Side.sexp_of_t
    Types.Side.Sell sell_common "side_of_common Sell = Sell" in
  ()

let order_status_equal a b = Types.Order_status.compare a b = 0

let test_fluxum_adapter_status_conversion () =
  printf "\n=== Fluxum Adapter: Status Conversion ===\n";

  let new_status = Fluxum_adapter.Adapter.Normalize.status_of_string "open" in
  let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
    Types.Order_status.New new_status "status_of_string 'open' = New" in

  let pending = Fluxum_adapter.Adapter.Normalize.status_of_string "pending" in
  let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
    Types.Order_status.New pending "status_of_string 'pending' = New" in

  let closed = Fluxum_adapter.Adapter.Normalize.status_of_string "closed" in
  let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
    Types.Order_status.Filled closed "status_of_string 'closed' = Filled" in

  let canceled = Fluxum_adapter.Adapter.Normalize.status_of_string "canceled" in
  let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
    Types.Order_status.Canceled canceled "status_of_string 'canceled' = Canceled" in

  let expired = Fluxum_adapter.Adapter.Normalize.status_of_string "expired" in
  let _ = assert_equal ~equal:order_status_equal ~sexp_of_t:Types.Order_status.sexp_of_t
    Types.Order_status.Canceled expired "status_of_string 'expired' = Canceled" in
  ()

let test_fluxum_adapter_venue () =
  printf "\n=== Fluxum Adapter: Venue ===\n";

  let venue = Fluxum_adapter.Adapter.Venue.t in
  let _ = assert_equal ~equal:Types.Venue.equal ~sexp_of_t:Types.Venue.sexp_of_t
    Types.Venue.Kraken venue "Adapter venue = Kraken" in
  ()

(** ========== NORMALIZE ERROR PATH TESTS (Phase 1) ========== *)

let test_normalize_balance_error_paths () =
  printf "\n=== Normalize: Balance Error Paths ===\n";

  (* Test malformed balance - invalid float in amount *)
  let bad_balance = ("BTC", "not_a_number") in
  (match Fluxum_adapter.Adapter.Normalize.balance bad_balance with
   | Error _ ->
     printf "  ✓ Rejected invalid balance amount\n";
     incr tests_run; incr tests_passed
   | Ok _ ->
     printf "  ✗ FAIL: Should reject non-numeric balance\n";
     incr tests_run; incr tests_failed);

  (* Test valid balance - ensure it still works *)
  let good_balance = ("ETH", "10.5") in
  (match Fluxum_adapter.Adapter.Normalize.balance good_balance with
   | Ok bal ->
     let _ = assert_float_equal 10.5 bal.total "Valid balance parsed correctly" in
     let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
       "ETH" bal.currency "Currency preserved" in ()
   | Error msg ->
     printf "  ✗ FAIL: Valid balance should succeed: %s\n" msg;
     incr tests_run; incr tests_failed);
  ()

(** ========== TEST RUNNER ========== *)

let () =
  printf "\n";
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  Kraken Unified Adapter - Comprehensive Test Suite    ║\n";
  printf "╚════════════════════════════════════════════════════════╝\n";

  (* Run all tests *)
  test_ledger_entry_creation ();
  test_ledger_simple_trades ();
  test_ledger_fees ();
  test_ledger_partial_fills ();
  test_ledger_average_prices ();
  test_ledger_spot_updates ();
  test_ledger_cost_basis_accounting ();
  test_ledger_multi_symbol ();

  test_order_book_creation ();
  test_order_book_bid_sorting ();
  test_order_book_ask_sorting ();
  test_order_book_operations ();
  (* test_order_book_market_price (); *)
  (* test_order_book_bid_market_price (); *)
  (* test_order_book_mid_price (); *)
  (* test_order_book_quantity_conversions (); *)
  test_order_book_epoch ();
  (* test_order_book_multi_symbol (); *)

  test_fluxum_adapter_side_conversion ();
  test_fluxum_adapter_status_conversion ();
  test_fluxum_adapter_venue ();

  (* Phase 1 normalize error path tests *)
  test_normalize_balance_error_paths ();

  (* Print summary *)
  printf "\n";
  printf "╔════════════════════════════════════════════════════════╗\n";
  printf "║  Test Results Summary                                  ║\n";
  printf "╠════════════════════════════════════════════════════════╣\n";
  printf "║  Total:  %3d tests                                     ║\n" !tests_run;
  printf "║  Passed: %3d tests  ✓                                  ║\n" !tests_passed;
  printf "║  Failed: %3d tests  ✗                                  ║\n" !tests_failed;
  printf "╚════════════════════════════════════════════════════════╝\n";
  printf "\n";

  (match !tests_failed > 0 with
   | true ->
     printf "❌ TEST SUITE FAILED\n\n";
     exit 1
   | false ->
     printf "✅ ALL TESTS PASSED!\n\n";
     exit 0)

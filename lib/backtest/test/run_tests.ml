(** Comprehensive test suite for the Backtest framework *)

open Core

(** Test result tracking *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) > tolerance) with
  | true ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false
  | false ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

let assert_equal ~equal ~sexp_of_t expected actual msg =
  incr tests_run;
  match not (equal expected actual) with
  | true ->
    incr tests_failed;
    printf "  X FAIL: %s\n     Expected: %s\n     Got: %s\n"
      msg
      (Sexp.to_string (sexp_of_t expected))
      (Sexp.to_string (sexp_of_t actual));
    false
  | false ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true

let assert_true condition msg =
  incr tests_run;
  match condition with
  | true ->
    incr tests_passed;
    printf "  * %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  X FAIL: %s\n" msg;
    false

let assert_false condition msg =
  assert_true (not condition) msg

(** ========== CANDLE TESTS ========== *)

let test_candle_creation () =
  printf "\n=== Candle: Creation ===\n";

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:50000.0
      ~high:51000.0
      ~low:49000.0
      ~close:50500.0
      ~volume:100.0
  in
  let _ = assert_equal ~equal:String.equal ~sexp_of_t:String.sexp_of_t
    "BTCUSD" candle.symbol "Symbol is set correctly" in
  let _ = assert_float_equal 50000.0 candle.open_ "Open price correct" in
  let _ = assert_float_equal 51000.0 candle.high "High price correct" in
  let _ = assert_float_equal 49000.0 candle.low "Low price correct" in
  let _ = assert_float_equal 50500.0 candle.close "Close price correct" in
  let _ = assert_float_equal 100.0 candle.volume "Volume correct" in
  ()

let test_candle_calculations () =
  printf "\n=== Candle: Calculations ===\n";

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:50000.0
      ~high:52000.0
      ~low:48000.0
      ~close:51000.0
      ~volume:100.0
  in

  (* Mid = (high + low) / 2 = (52000 + 48000) / 2 = 50000 *)
  let _ = assert_float_equal 50000.0 (Backtest.Candle.mid candle) "Mid price calculation" in

  (* Typical = (high + low + close) / 3 = (52000 + 48000 + 51000) / 3 = 50333.33 *)
  let _ = assert_float_equal 50333.333333 (Backtest.Candle.typical candle) ~tolerance:0.01 "Typical price calculation" in

  (* Body = |close - open| = |51000 - 50000| = 1000 *)
  let _ = assert_float_equal 1000.0 (Backtest.Candle.body candle) "Body size calculation" in

  (* Range = high - low = 52000 - 48000 = 4000 *)
  let _ = assert_float_equal 4000.0 (Backtest.Candle.range candle) "Range calculation" in

  (* Bullish: close > open *)
  let _ = assert_true (Backtest.Candle.is_bullish candle) "Is bullish (close > open)" in
  let _ = assert_false (Backtest.Candle.is_bearish candle) "Not bearish" in
  ()

let test_candle_bearish () =
  printf "\n=== Candle: Bearish Detection ===\n";

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:51000.0
      ~high:52000.0
      ~low:49000.0
      ~close:50000.0
      ~volume:100.0
  in

  let _ = assert_true (Backtest.Candle.is_bearish candle) "Is bearish (close < open)" in
  let _ = assert_false (Backtest.Candle.is_bullish candle) "Not bullish" in
  ()

let test_candle_csv_parsing () =
  printf "\n=== Candle: CSV Parsing ===\n";

  let row = ["2024-01-01T00:00:00Z"; "50000.0"; "51000.0"; "49000.0"; "50500.0"; "100.0"] in
  let candle = Backtest.Candle.of_csv_row ~symbol:"BTCUSD" row in

  let _ = assert_float_equal 50000.0 candle.open_ "CSV parsed open" in
  let _ = assert_float_equal 51000.0 candle.high "CSV parsed high" in
  let _ = assert_float_equal 49000.0 candle.low "CSV parsed low" in
  let _ = assert_float_equal 50500.0 candle.close "CSV parsed close" in
  let _ = assert_float_equal 100.0 candle.volume "CSV parsed volume" in
  ()

let test_candle_sorting () =
  printf "\n=== Candle: Sorting by Time ===\n";

  let ts1 = Time_ns_unix.of_string "2024-01-03 00:00:00Z" in
  let ts2 = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let ts3 = Time_ns_unix.of_string "2024-01-02 00:00:00Z" in

  let c1 = Backtest.Candle.create ~symbol:"BTCUSD" ~timestamp:ts1
      ~open_:50000.0 ~high:51000.0 ~low:49000.0 ~close:50500.0 ~volume:100.0 in
  let c2 = Backtest.Candle.create ~symbol:"BTCUSD" ~timestamp:ts2
      ~open_:49000.0 ~high:50000.0 ~low:48000.0 ~close:49500.0 ~volume:200.0 in
  let c3 = Backtest.Candle.create ~symbol:"BTCUSD" ~timestamp:ts3
      ~open_:49500.0 ~high:50500.0 ~low:49000.0 ~close:50000.0 ~volume:150.0 in

  let sorted = Backtest.Candle.sort_by_time [c1; c2; c3] in
  let _ = assert_equal ~equal:Time_ns.equal ~sexp_of_t:Time_ns_unix.sexp_of_t
    ts2 (List.nth_exn sorted 0).timestamp "First candle is earliest" in
  let _ = assert_equal ~equal:Time_ns.equal ~sexp_of_t:Time_ns_unix.sexp_of_t
    ts3 (List.nth_exn sorted 1).timestamp "Second candle is middle" in
  let _ = assert_equal ~equal:Time_ns.equal ~sexp_of_t:Time_ns_unix.sexp_of_t
    ts1 (List.nth_exn sorted 2).timestamp "Third candle is latest" in
  ()

(** ========== METRICS TESTS ========== *)

let test_metrics_std_dev () =
  printf "\n=== Metrics: Standard Deviation ===\n";

  (* Empty list *)
  let _ = assert_float_equal 0.0 (Backtest.Metrics.std_dev []) "Empty list std_dev = 0" in

  (* Single value *)
  let _ = assert_float_equal 0.0 (Backtest.Metrics.std_dev [5.0]) "Single value std_dev = 0" in

  (* Known values: [2, 4, 4, 4, 5, 5, 7, 9] has mean=5, variance=4, std=2 *)
  let values = [2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0] in
  let _ = assert_float_equal 2.0 (Backtest.Metrics.std_dev values) ~tolerance:0.01 "Known std_dev = 2" in
  ()

let test_metrics_calculate_basic () =
  printf "\n=== Metrics: Basic Calculation ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in

  (* Simple equity curve: 10000 -> 11000 (10% return) *)
  let equity_curve = [
    (start, 10000.0);
    (Time_ns.add start (Time_ns.Span.of_day 30.0), 10500.0);
    (Time_ns.add start (Time_ns.Span.of_day 60.0), 11000.0);
  ] in

  let trade_pnls = [500.0; 500.0] in  (* Two winning trades *)

  let metrics = Backtest.Metrics.calculate ~equity_curve ~trade_pnls () in

  let _ = assert_float_equal 0.10 metrics.total_return ~tolerance:0.001 "Total return = 10%" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 metrics.total_trades "Total trades = 2" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 metrics.winning_trades "Winning trades = 2" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 metrics.losing_trades "Losing trades = 0" in
  let _ = assert_float_equal 1.0 metrics.win_rate "Win rate = 100%" in
  let _ = assert_float_equal 500.0 metrics.avg_winner "Avg winner = 500" in
  ()

let test_metrics_win_loss () =
  printf "\n=== Metrics: Win/Loss Calculations ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in

  let equity_curve = [
    (start, 10000.0);
    (Time_ns.add start (Time_ns.Span.of_day 30.0), 10000.0);  (* Flat *)
  ] in

  (* 3 winners, 2 losers *)
  let trade_pnls = [100.0; -50.0; 200.0; -100.0; 150.0] in

  let metrics = Backtest.Metrics.calculate ~equity_curve ~trade_pnls () in

  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    5 metrics.total_trades "Total trades = 5" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    3 metrics.winning_trades "Winning trades = 3" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    2 metrics.losing_trades "Losing trades = 2" in
  let _ = assert_float_equal 0.6 metrics.win_rate "Win rate = 60%" in

  (* Avg winner = (100 + 200 + 150) / 3 = 150 *)
  let _ = assert_float_equal 150.0 metrics.avg_winner "Avg winner = 150" in

  (* Avg loser = (-50 + -100) / 2 = -75 *)
  let _ = assert_float_equal (-75.0) metrics.avg_loser "Avg loser = -75" in

  (* Profit factor = 450 / 150 = 3.0 *)
  let _ = assert_float_equal 3.0 metrics.profit_factor "Profit factor = 3.0" in

  (* Avg trade P&L = (100 - 50 + 200 - 100 + 150) / 5 = 60 *)
  let _ = assert_float_equal 60.0 metrics.avg_trade_pnl "Avg trade P&L = 60" in

  (* Expectancy = 0.6 * 150 + 0.4 * (-75) = 90 - 30 = 60 *)
  let _ = assert_float_equal 60.0 metrics.expectancy "Expectancy = 60" in
  ()

let test_metrics_max_drawdown () =
  printf "\n=== Metrics: Max Drawdown ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in

  (* Equity: 10000 -> 12000 -> 9000 -> 11000 *)
  (* Peak at 12000, trough at 9000, drawdown = (12000-9000)/12000 = 25% *)
  let equity_curve = [
    (start, 10000.0);
    (Time_ns.add start (Time_ns.Span.of_day 10.0), 12000.0);  (* Peak *)
    (Time_ns.add start (Time_ns.Span.of_day 20.0), 9000.0);   (* Trough *)
    (Time_ns.add start (Time_ns.Span.of_day 30.0), 11000.0);
  ] in

  let metrics = Backtest.Metrics.calculate ~equity_curve ~trade_pnls:[] () in

  let _ = assert_float_equal 0.25 metrics.max_drawdown ~tolerance:0.001 "Max drawdown = 25%" in
  ()

let test_metrics_empty () =
  printf "\n=== Metrics: Empty Data ===\n";

  let metrics = Backtest.Metrics.empty in

  let _ = assert_float_equal 0.0 metrics.total_return "Empty total_return = 0" in
  let _ = assert_float_equal 0.0 metrics.sharpe_ratio "Empty sharpe_ratio = 0" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    0 metrics.total_trades "Empty total_trades = 0" in
  ()

(** ========== FILL MODEL TESTS ========== *)

let test_fill_model_config () =
  printf "\n=== Fill Model: Configuration ===\n";

  let default = Backtest.Fill_model.Config.default in
  let _ = assert_float_equal 0.001 default.slippage_pct "Default slippage = 0.1%" in
  let _ = assert_float_equal 0.001 default.commission_pct "Default commission = 0.1%" in

  let no_costs = Backtest.Fill_model.Config.no_costs in
  let _ = assert_float_equal 0.0 no_costs.slippage_pct "No costs slippage = 0" in
  let _ = assert_float_equal 0.0 no_costs.commission_pct "No costs commission = 0" in

  let custom = Backtest.Fill_model.Config.create
      ~slippage_pct:0.002
      ~commission_pct:0.0005
      ~fill_on:`Open
      ()
  in
  let _ = assert_float_equal 0.002 custom.slippage_pct "Custom slippage = 0.2%" in
  let _ = assert_float_equal 0.0005 custom.commission_pct "Custom commission = 0.05%" in
  ()

let test_fill_model_base_price () =
  printf "\n=== Fill Model: Base Price Selection ===\n";

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:50000.0
      ~high:52000.0
      ~low:48000.0
      ~close:51000.0
      ~volume:100.0
  in

  let config_open = Backtest.Fill_model.Config.create ~fill_on:`Open () in
  let config_close = Backtest.Fill_model.Config.create ~fill_on:`Close () in
  let config_mid = Backtest.Fill_model.Config.create ~fill_on:`Midpoint () in

  let _ = assert_float_equal 50000.0 (Backtest.Fill_model.base_price ~config:config_open candle)
    "Base price on Open = 50000" in
  let _ = assert_float_equal 51000.0 (Backtest.Fill_model.base_price ~config:config_close candle)
    "Base price on Close = 51000" in
  let _ = assert_float_equal 50000.0 (Backtest.Fill_model.base_price ~config:config_mid candle)
    "Base price on Midpoint = 50000" in
  ()

let test_fill_model_slippage () =
  printf "\n=== Fill Model: Slippage Application ===\n";

  let config = Backtest.Fill_model.Config.create ~slippage_pct:0.01 () in  (* 1% slippage *)

  (* Buy slippage increases price *)
  let buy_price = Backtest.Fill_model.apply_slippage ~config ~side:`Buy 50000.0 in
  let _ = assert_float_equal 50500.0 buy_price "Buy slippage: 50000 + 1% = 50500" in

  (* Sell slippage decreases price *)
  let sell_price = Backtest.Fill_model.apply_slippage ~config ~side:`Sell 50000.0 in
  let _ = assert_float_equal 49500.0 sell_price "Sell slippage: 50000 - 1% = 49500" in
  ()

let test_fill_model_commission () =
  printf "\n=== Fill Model: Commission Calculation ===\n";

  let config = Backtest.Fill_model.Config.create ~commission_pct:0.001 () in  (* 0.1% commission *)

  (* Commission on $50k trade of 1.0 BTC = $50 *)
  let commission = Backtest.Fill_model.calculate_commission ~config ~price:50000.0 ~qty:1.0 in
  let _ = assert_float_equal 50.0 commission "Commission on $50k = $50" in

  (* Commission on $50k trade of 2.0 BTC = $100 *)
  let commission = Backtest.Fill_model.calculate_commission ~config ~price:50000.0 ~qty:2.0 in
  let _ = assert_float_equal 100.0 commission "Commission on $100k = $100" in
  ()

let test_fill_model_market_fill () =
  printf "\n=== Fill Model: Market Order Fill ===\n";

  let config = Backtest.Fill_model.Config.create
      ~slippage_pct:0.001  (* 0.1% *)
      ~commission_pct:0.001  (* 0.1% *)
      ~fill_on:`Close
      ()
  in

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:50000.0
      ~high:52000.0
      ~low:48000.0
      ~close:51000.0
      ~volume:100.0
  in

  match Backtest.Fill_model.fill_market ~config ~side:`Buy ~qty:1.0 candle with
  | `Filled fill ->
    (* Price = 51000 + 0.1% = 51051 *)
    let _ = assert_float_equal 51051.0 fill.price ~tolerance:0.01 "Fill price with slippage" in
    let _ = assert_float_equal 1.0 fill.qty "Fill qty = 1.0" in
    (* Commission = 51051 * 1.0 * 0.001 = 51.051 *)
    let _ = assert_float_equal 51.051 fill.commission ~tolerance:0.01 "Fill commission" in
    ()
  | _ ->
    incr tests_run;
    incr tests_failed;
    printf "  X FAIL: Market order should fill\n"

let test_fill_model_limit_fill () =
  printf "\n=== Fill Model: Limit Order Fill ===\n";

  let config = Backtest.Fill_model.Config.create
      ~slippage_pct:0.0
      ~commission_pct:0.001
      ~fill_on:`Close
      ()
  in

  let ts = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let candle = Backtest.Candle.create
      ~symbol:"BTCUSD"
      ~timestamp:ts
      ~open_:50000.0
      ~high:52000.0
      ~low:48000.0
      ~close:51000.0
      ~volume:100.0
  in

  (* Buy limit at 49000 should fill (low is 48000) *)
  (match Backtest.Fill_model.fill_limit ~config ~side:`Buy ~qty:1.0 ~limit:49000.0 candle with
   | `Filled _ ->
     incr tests_run; incr tests_passed;
     printf "  * Buy limit at 49000 fills (low = 48000)\n"
   | `Pending ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Buy limit at 49000 should fill\n"
   | `Rejected _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Buy limit at 49000 should fill, not reject\n");

  (* Buy limit at 47000 should NOT fill (low is 48000) *)
  (match Backtest.Fill_model.fill_limit ~config ~side:`Buy ~qty:1.0 ~limit:47000.0 candle with
   | `Pending ->
     incr tests_run; incr tests_passed;
     printf "  * Buy limit at 47000 pending (low = 48000)\n"
   | `Filled _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Buy limit at 47000 should not fill\n"
   | `Rejected _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Buy limit at 47000 should pend, not reject\n");

  (* Sell limit at 51500 should fill (high is 52000) *)
  (match Backtest.Fill_model.fill_limit ~config ~side:`Sell ~qty:1.0 ~limit:51500.0 candle with
   | `Filled _ ->
     incr tests_run; incr tests_passed;
     printf "  * Sell limit at 51500 fills (high = 52000)\n"
   | `Pending ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Sell limit at 51500 should fill\n"
   | `Rejected _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Sell limit at 51500 should fill, not reject\n");

  (* Sell limit at 53000 should NOT fill (high is 52000) *)
  (match Backtest.Fill_model.fill_limit ~config ~side:`Sell ~qty:1.0 ~limit:53000.0 candle with
   | `Pending ->
     incr tests_run; incr tests_passed;
     printf "  * Sell limit at 53000 pending (high = 52000)\n"
   | `Filled _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Sell limit at 53000 should not fill\n"
   | `Rejected _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Sell limit at 53000 should pend, not reject\n")

let test_fill_model_max_buy_qty () =
  printf "\n=== Fill Model: Max Buy Quantity ===\n";

  let config = Backtest.Fill_model.Config.create
      ~slippage_pct:0.0
      ~commission_pct:0.001  (* 0.1% *)
      ()
  in

  (* With $10000 and price $50000, max qty with 0.1% commission *)
  (* qty <= 10000 / (50000 * 1.001) = 10000 / 50050 = 0.1998 *)
  let max_qty = Backtest.Fill_model.max_buy_qty ~balance:10000.0 ~price:50000.0 ~config in
  let _ = assert_float_equal 0.1998 max_qty ~tolerance:0.001 "Max buy qty with commission" in

  let config_no_comm = Backtest.Fill_model.Config.create ~commission_pct:0.0 () in
  let max_qty = Backtest.Fill_model.max_buy_qty ~balance:10000.0 ~price:50000.0 ~config:config_no_comm in
  let _ = assert_float_equal 0.2 max_qty "Max buy qty no commission" in
  ()

(** ========== STRATEGY INTERFACE TESTS ========== *)

let test_signal_creation () =
  printf "\n=== Strategy Interface: Signal Creation ===\n";

  let buy = Backtest.Signal.buy 1.0 in
  (match buy with
   | Backtest.Strategy_intf.Signal.Buy { qty; limit } ->
     let _ = assert_float_equal 1.0 qty "Buy qty = 1.0" in
     let _ = assert_true (Option.is_none limit) "Buy has no limit" in
     ()
   | _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Expected Buy signal\n");

  let sell_limit = Backtest.Signal.sell ~limit:50000.0 2.0 in
  (match sell_limit with
   | Backtest.Strategy_intf.Signal.Sell { qty; limit } ->
     let _ = assert_float_equal 2.0 qty "Sell qty = 2.0" in
     let _ = assert_true (Option.is_some limit) "Sell has limit" in
     let _ = assert_float_equal 50000.0 (Option.value_exn limit) "Sell limit = 50000" in
     ()
   | _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Expected Sell signal\n");

  let hold = Backtest.Signal.hold in
  (match hold with
   | Backtest.Strategy_intf.Signal.Hold ->
     incr tests_run; incr tests_passed;
     printf "  * Hold signal created\n"
   | _ ->
     incr tests_run; incr tests_failed;
     printf "  X FAIL: Expected Hold signal\n")

let test_position () =
  printf "\n=== Strategy Interface: Position ===\n";

  let empty = Backtest.Position.empty in
  let _ = assert_float_equal 0.0 empty.qty "Empty position qty = 0" in
  let _ = assert_true (Backtest.Position.is_flat empty) "Empty position is flat" in

  let long = { Backtest.Strategy_intf.Position.qty = 1.0; avg_price = 50000.0; unrealized_pnl = 0.0 } in
  let _ = assert_true (Backtest.Position.is_long long) "Positive qty is long" in
  let _ = assert_false (Backtest.Position.is_short long) "Positive qty is not short" in
  let _ = assert_false (Backtest.Position.is_flat long) "Positive qty is not flat" in

  let short = { Backtest.Strategy_intf.Position.qty = -1.0; avg_price = 50000.0; unrealized_pnl = 0.0 } in
  let _ = assert_true (Backtest.Position.is_short short) "Negative qty is short" in
  let _ = assert_false (Backtest.Position.is_long short) "Negative qty is not long" in
  let _ = assert_false (Backtest.Position.is_flat short) "Negative qty is not flat" in
  ()

(** ========== DATA SOURCE TESTS ========== *)

let test_interval_parsing () =
  printf "\n=== Data Source: Interval Parsing ===\n";

  let _ = assert_float_equal 1.0 (Time_ns.Span.to_min (Backtest.Interval.of_string "1m"))
    "1m interval = 1 minute" in
  let _ = assert_float_equal 5.0 (Time_ns.Span.to_min (Backtest.Interval.of_string "5m"))
    "5m interval = 5 minutes" in
  let _ = assert_float_equal 60.0 (Time_ns.Span.to_min (Backtest.Interval.of_string "1h"))
    "1h interval = 60 minutes" in
  let _ = assert_float_equal 1440.0 (Time_ns.Span.to_min (Backtest.Interval.of_string "1d"))
    "1d interval = 1440 minutes" in
  ()

let test_synthetic_data_generation () =
  printf "\n=== Data Source: Synthetic Data Generation ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let end_ = Time_ns_unix.of_string "2024-01-02 00:00:00Z" in
  let interval = Backtest.Interval.hour_1 in

  let candles = Backtest.Data_source.Synthetic.random_walk
      ~symbol:"BTCUSD"
      ~start
      ~end_
      ~interval
      ~initial_price:50000.0
      ~volatility:0.01
      ()
  in

  (* 24 hours inclusive = 25 candles (start to end inclusive) *)
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    25 (List.length candles) "24 hour candles generated (inclusive)" in

  (* First candle should be around initial price *)
  let first = match List.hd candles with
    | Some c -> c
    | None -> failwith "Expected non-empty candles list"
  in
  let _ = assert_true (Float.(first.open_ > 45000.0 && first.open_ < 55000.0))
    "First candle open within range" in

  (* All candles should have positive volume *)
  let _ = assert_true (List.for_all candles ~f:(fun c -> Float.(c.volume > 0.0)))
    "All candles have positive volume" in

  (* Candles should be in chronological order *)
  let sorted = Backtest.Candle.sort_by_time candles in
  let _ = assert_true (List.equal Backtest.Candle.equal candles sorted)
    "Candles are in chronological order" in
  ()

let test_trending_data_generation () =
  printf "\n=== Data Source: Trending Data Generation ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let end_ = Time_ns_unix.of_string "2024-01-08 00:00:00Z" in  (* 1 week *)
  let interval = Backtest.Interval.day_1 in

  let candles = Backtest.Data_source.Synthetic.trending
      ~symbol:"BTCUSD"
      ~start
      ~end_
      ~interval
      ~initial_price:50000.0
      ~trend_pct:0.01  (* 1% daily trend *)
      ~volatility:0.005
      ()
  in

  (* 7 days inclusive = 8 candles (start to end inclusive) *)
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    8 (List.length candles) "7 daily candles generated (inclusive)" in
  ()

(** ========== ENGINE INTEGRATION TESTS ========== *)

let test_engine_config () =
  printf "\n=== Engine: Configuration ===\n";

  let default = Backtest.Engine.Config.default in
  let _ = assert_float_equal 10000.0 default.initial_balance "Default balance = 10000" in
  let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
    100 default.history_size "Default history size = 100" in

  let custom = Backtest.Engine.Config.create
      ~initial_balance:50000.0
      ~slippage_pct:0.002
      ~commission_pct:0.0005
      ~history_size:50
      ()
  in
  let _ = assert_float_equal 50000.0 custom.initial_balance "Custom balance = 50000" in
  let _ = assert_float_equal 0.002 custom.fill_model.slippage_pct "Custom slippage" in
  let _ = assert_float_equal 0.0005 custom.fill_model.commission_pct "Custom commission" in
  ()

let test_engine_buy_and_hold () =
  printf "\n=== Engine: Buy and Hold Strategy ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in

  (* Generate upward trending candles *)
  let candles = Backtest.Data_source.Synthetic.trending
      ~symbol:"BTCUSD"
      ~start
      ~end_:(Time_ns.add start (Time_ns.Span.of_day 30.0))
      ~interval:Backtest.Interval.day_1
      ~initial_price:50000.0
      ~trend_pct:0.01  (* 1% daily up *)
      ~volatility:0.001
      ()
  in

  let config = Backtest.Engine.Config.create
      ~initial_balance:10000.0
      ~slippage_pct:0.0
      ~commission_pct:0.0
      ()
  in

  don't_wait_for begin
    match%map Backtest.Engine.run
        (module Backtest_strategies.Buy_and_hold.Default)
        ~config
        ~candles
        ()
    with
    | Ok result ->
      (* Buy and hold in uptrend should be profitable *)
      let _ = assert_true Float.(result.final_balance > result.initial_balance)
        "Buy and hold in uptrend is profitable" in
      let _ = assert_equal ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
        1 (List.length result.trades) "Buy and hold has 1 trade (exit)" in
      ()
    | Error e ->
      incr tests_run; incr tests_failed;
      printf "  X FAIL: Engine error: %s\n" e
  end

let test_result_calculations () =
  printf "\n=== Result: Calculations ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let end_ = Time_ns_unix.of_string "2024-02-01 00:00:00Z" in

  let trade = Backtest.Result.Trade_record.create
      ~entry_time:start
      ~exit_time:end_
      ~symbol:"BTCUSD"
      ~side:`Long
      ~entry_price:50000.0
      ~exit_price:55000.0
      ~qty:1.0
      ~commission:100.0
  in

  (* P&L = (55000 - 50000) * 1.0 - 100 = 4900 *)
  let _ = assert_float_equal 4900.0 trade.pnl "Trade P&L = 4900" in

  (* P&L % = 4900 / (50000 * 1.0) = 9.8% *)
  let _ = assert_float_equal 0.098 trade.pnl_pct "Trade P&L % = 9.8%" in
  ()

let test_result_short_trade () =
  printf "\n=== Result: Short Trade P&L ===\n";

  let start = Time_ns_unix.of_string "2024-01-01 00:00:00Z" in
  let end_ = Time_ns_unix.of_string "2024-02-01 00:00:00Z" in

  let trade = Backtest.Result.Trade_record.create
      ~entry_time:start
      ~exit_time:end_
      ~symbol:"BTCUSD"
      ~side:`Short
      ~entry_price:50000.0
      ~exit_price:45000.0
      ~qty:1.0
      ~commission:100.0
  in

  (* Short P&L = (50000 - 45000) * 1.0 - 100 = 4900 *)
  let _ = assert_float_equal 4900.0 trade.pnl "Short trade P&L = 4900" in
  ()

(** ========== STRATEGY TESTS ========== *)

let test_strategy_registry () =
  printf "\n=== Strategy: Registry ===\n";

  let strategies = Backtest_strategies.list () in
  let _ = assert_true (List.length strategies >= 3) "At least 3 strategies registered" in

  let names = List.map strategies ~f:fst in
  let _ = assert_true (List.mem names "sma-crossover" ~equal:String.equal)
    "sma-crossover registered" in
  let _ = assert_true (List.mem names "momentum" ~equal:String.equal)
    "momentum registered" in
  let _ = assert_true (List.mem names "buy-and-hold" ~equal:String.equal)
    "buy-and-hold registered" in
  ()

let test_strategy_lookup () =
  printf "\n=== Strategy: Lookup ===\n";

  let sma = Backtest_strategies.get "sma-crossover" in
  let _ = assert_true (Option.is_some sma) "sma-crossover found" in

  let momentum = Backtest_strategies.get "momentum" in
  let _ = assert_true (Option.is_some momentum) "momentum found" in

  let unknown = Backtest_strategies.get "unknown-strategy" in
  let _ = assert_true (Option.is_none unknown) "unknown-strategy not found" in
  ()

(** ========== TEST RUNNER ========== *)

let () =
  printf "\n";
  printf "====================================================\n";
  printf "  Backtest Framework - Comprehensive Test Suite\n";
  printf "====================================================\n";

  (* Run all tests *)
  test_candle_creation ();
  test_candle_calculations ();
  test_candle_bearish ();
  test_candle_csv_parsing ();
  test_candle_sorting ();

  test_metrics_std_dev ();
  test_metrics_calculate_basic ();
  test_metrics_win_loss ();
  test_metrics_max_drawdown ();
  test_metrics_empty ();

  test_fill_model_config ();
  test_fill_model_base_price ();
  test_fill_model_slippage ();
  test_fill_model_commission ();
  test_fill_model_market_fill ();
  test_fill_model_limit_fill ();
  test_fill_model_max_buy_qty ();

  test_signal_creation ();
  test_position ();

  test_interval_parsing ();
  test_synthetic_data_generation ();
  test_trending_data_generation ();

  test_engine_config ();
  test_engine_buy_and_hold ();

  test_result_calculations ();
  test_result_short_trade ();

  test_strategy_registry ();
  test_strategy_lookup ();

  (* Print summary *)
  printf "\n";
  printf "====================================================\n";
  printf "  Test Results Summary\n";
  printf "====================================================\n";
  printf "  Total:  %3d tests\n" !tests_run;
  printf "  Passed: %3d tests  *\n" !tests_passed;
  printf "  Failed: %3d tests  X\n" !tests_failed;
  printf "====================================================\n";
  printf "\n";

  match !tests_failed > 0 with
  | true ->
    printf "X TEST SUITE FAILED\n\n";
    exit 1
  | false ->
    printf "* ALL TESTS PASSED!\n\n";
    exit 0

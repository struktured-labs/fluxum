open Core

(* Test infrastructure *)
let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  ✓ %s\n" msg

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  ✗ FAIL: %s\n" msg

let assert_error result msg =
  incr tests_run;
  match result with
  | Error err ->
    incr tests_passed;
    printf "  ✓ %s: %s\n" msg err;
    true
  | Ok _ ->
    incr tests_failed;
    printf "  ✗ FAIL: %s (expected error, got Ok)\n" msg;
    false

let assert_string_equal expected actual msg =
  incr tests_run;
  match String.equal expected actual with
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %s, Got: %s\n" msg expected actual;
    false

let assert_float_equal ?(tolerance = 0.0001) expected actual msg =
  incr tests_run;
  match Float.(abs (expected - actual) <= tolerance) with
  | true ->
    incr tests_passed;
    printf "  ✓ %s\n" msg;
    true
  | false ->
    incr tests_failed;
    printf "  ✗ FAIL: %s\n     Expected: %.8f, Got: %.8f\n" msg expected actual;
    false

(* ============================================================ *)
(* Normalize Function Tests *)
(* ============================================================ *)

let test_normalize_trade_valid () =
  printf "\n[Normalize] Trade with valid data\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "BUY";
    size = "1.5";
    price = "50000.50";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.trade trade with
  | Ok t ->
    ignore (assert_string_equal "trade123" (Option.value_exn t.trade_id) "Trade ID preserved");
    ignore (assert_float_equal 1.5 t.qty "Quantity parsed correctly");
    ignore (assert_float_equal 50000.50 t.price "Price parsed correctly")
  | Error msg ->
    fail (sprintf "Trade normalization failed: %s" msg)

let test_normalize_trade_invalid_price () =
  printf "\n[Normalize] Trade with invalid price\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "BUY";
    size = "1.5";
    price = "not_a_number";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.trade trade)
    "Rejected invalid price")

let test_normalize_trade_invalid_size () =
  printf "\n[Normalize] Trade with invalid size\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "BUY";
    size = "NaN";
    price = "50000.0";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.trade trade)
    "Rejected NaN size")

let test_normalize_trade_negative_price () =
  printf "\n[Normalize] Trade with negative price\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "SELL";
    size = "1.0";
    price = "-50000.0";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.trade trade)
    "Rejected negative price")

let test_normalize_trade_zero_size () =
  printf "\n[Normalize] Trade with zero size\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "BUY";
    size = "0.0";
    price = "50000.0";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.trade trade with
  | Ok t ->
    ignore (assert_float_equal 0.0 t.qty "Zero quantity accepted");
    pass "Accepted zero-size trade"
  | Error msg ->
    fail (sprintf "Should accept zero-size trade: %s" msg)

let test_normalize_trade_invalid_side () =
  printf "\n[Normalize] Trade with invalid side\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "trade123";
    side = "INVALID_SIDE";
    size = "1.0";
    price = "50000.0";
    createdAt = "2024-01-01T00:00:00.000Z";
    createdAtHeight = None;
  } in
  (* Side conversion uses of_string_exn which defaults to Buy on error *)
  match Dydx.Fluxum_adapter.Adapter.Normalize.trade trade with
  | Ok t ->
    (match t.side with
     | Fluxum.Types.Side.Buy ->
       pass "Invalid side defaulted to Buy"
     | _ -> fail "Invalid side should default to Buy")
  | Error msg ->
    fail (sprintf "Should accept with default side: %s" msg)

let test_normalize_order_book_valid () =
  printf "\n[Normalize] Order book with valid data\n";
  let book : Dydx.Rest.Types.orderbook = {
    bids = [
      { price = "50000.0"; size = "1.5" };
      { price = "49999.0"; size = "2.0" };
    ];
    asks = [
      { price = "50001.0"; size = "1.0" };
      { price = "50002.0"; size = "3.0" };
    ];
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.order_book book with
  | Ok b ->
    ignore (assert_string_equal "2 bids" (sprintf "%d bids" (List.length b.bids)) "2 bids");
    ignore (assert_string_equal "2 asks" (sprintf "%d asks" (List.length b.asks)) "2 asks");
    pass "Valid order book normalized"
  | Error msg ->
    fail (sprintf "Order book normalization failed: %s" msg)

let test_normalize_order_book_invalid_bid_price () =
  printf "\n[Normalize] Order book with invalid bid price\n";
  let book : Dydx.Rest.Types.orderbook = {
    bids = [{ price = "invalid_price"; size = "1.0" }];
    asks = [{ price = "50001.0"; size = "1.0" }];
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.order_book book)
    "Rejected invalid bid price")

let test_normalize_order_book_invalid_ask_size () =
  printf "\n[Normalize] Order book with invalid ask size\n";
  let book : Dydx.Rest.Types.orderbook = {
    bids = [{ price = "50000.0"; size = "1.0" }];
    asks = [{ price = "50001.0"; size = "not_a_number" }];
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.order_book book)
    "Rejected invalid ask size")

let test_normalize_order_book_empty () =
  printf "\n[Normalize] Order book with empty lists\n";
  let book : Dydx.Rest.Types.orderbook = {
    bids = [];
    asks = [];
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.order_book book with
  | Ok b ->
    ignore (assert_string_equal "0 bids" (sprintf "%d bids" (List.length b.bids)) "Empty bids");
    ignore (assert_string_equal "0 asks" (sprintf "%d asks" (List.length b.asks)) "Empty asks");
    pass "Empty order book accepted"
  | Error msg ->
    fail (sprintf "Empty order book should be valid: %s" msg)

let test_normalize_symbol_info_valid () =
  printf "\n[Normalize] Symbol info with valid data\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = Some "1";
    ticker = "BTC-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "1000000";
    atomicResolution = -10;
    quantumConversionExponent = -9;
    tickSize = "1";
    stepSize = "0.0001";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = Some "50000.0";
    priceChange24H = Some "500.0";
    volume24H = Some "1000000";
    trades24H = Some 10000;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.symbol_info ("BTC-USD", market) with
  | Ok s ->
    ignore (assert_string_equal "BTC-USD" s.symbol "Symbol preserved");
    ignore (assert_string_equal "ACTIVE" s.status "Status preserved");
    ignore (assert_float_equal 0.0001 s.min_order_size "Min order size parsed");
    pass "Valid symbol info normalized"
  | Error msg ->
    fail (sprintf "Symbol info normalization failed: %s" msg)

let test_normalize_symbol_info_invalid_step_size () =
  printf "\n[Normalize] Symbol info with invalid step size\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = None;
    ticker = "BTC-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "1000000";
    atomicResolution = -10;
    quantumConversionExponent = -9;
    tickSize = "1";
    stepSize = "invalid";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = None;
    priceChange24H = None;
    volume24H = None;
    trades24H = None;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.symbol_info ("BTC-USD", market))
    "Rejected invalid step size")

let test_normalize_symbol_info_invalid_tick_size () =
  printf "\n[Normalize] Symbol info with invalid tick size\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = None;
    ticker = "ETH-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "500000";
    atomicResolution = -9;
    quantumConversionExponent = -9;
    tickSize = "NaN";
    stepSize = "0.001";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = None;
    priceChange24H = None;
    volume24H = None;
    trades24H = None;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.symbol_info ("ETH-USD", market))
    "Rejected NaN tick size")

let test_normalize_ticker_valid () =
  printf "\n[Normalize] Ticker with valid data\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = Some "1";
    ticker = "BTC-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "1000000";
    atomicResolution = -10;
    quantumConversionExponent = -9;
    tickSize = "1";
    stepSize = "0.0001";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = Some "50000.0";
    priceChange24H = Some "500.0";
    volume24H = Some "1000000";
    trades24H = Some 10000;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.ticker ("BTC-USD", market) with
  | Ok t ->
    ignore (assert_string_equal "BTC-USD" t.symbol "Symbol preserved");
    ignore (assert_float_equal 50000.0 t.last_price "Oracle price as last price");
    ignore (assert_float_equal 1000000.0 t.volume_24h "24h volume parsed");
    pass "Valid ticker normalized"
  | Error msg ->
    fail (sprintf "Ticker normalization failed: %s" msg)

let test_normalize_ticker_invalid_oracle_price () =
  printf "\n[Normalize] Ticker with invalid oracle price\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = None;
    ticker = "BTC-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "1000000";
    atomicResolution = -10;
    quantumConversionExponent = -9;
    tickSize = "1";
    stepSize = "0.0001";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = Some "invalid_price";
    priceChange24H = None;
    volume24H = None;
    trades24H = None;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.ticker ("BTC-USD", market))
    "Rejected invalid oracle price")

let test_normalize_ticker_invalid_volume () =
  printf "\n[Normalize] Ticker with invalid volume\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = None;
    ticker = "ETH-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.05";
    maintenanceMarginFraction = "0.03";
    openInterest = "500000";
    atomicResolution = -9;
    quantumConversionExponent = -9;
    tickSize = "0.1";
    stepSize = "0.001";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100000;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = Some "3000.0";
    priceChange24H = None;
    volume24H = Some "not_a_number";
    trades24H = None;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.ticker ("ETH-USD", market))
    "Rejected invalid volume")

let test_normalize_ticker_missing_optional_fields () =
  printf "\n[Normalize] Ticker with missing optional fields\n";
  let market : Dydx.Rest.Types.perpetual_market = {
    clobPairId = None;
    ticker = "SOL-USD";
    status = "ACTIVE";
    initialMarginFraction = "0.10";
    maintenanceMarginFraction = "0.05";
    openInterest = "100000";
    atomicResolution = -7;
    quantumConversionExponent = -9;
    tickSize = "0.01";
    stepSize = "0.1";
    stepBaseQuantums = 1000000;
    subticksPerTick = 100;
    marketType = "PERPETUAL";
    openInterestLowerCap = None;
    openInterestUpperCap = None;
    baseOpenInterest = None;
    oraclePrice = None;  (* Missing oracle price *)
    priceChange24H = None;  (* Missing price change *)
    volume24H = None;  (* Missing volume *)
    trades24H = None;
    nextFundingRate = None;
    defaultFundingRate1H = None;
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.ticker ("SOL-USD", market) with
  | Ok t ->
    ignore (assert_float_equal 0.0 t.last_price "Missing oracle defaults to 0");
    ignore (assert_float_equal 0.0 t.volume_24h "Missing volume defaults to 0");
    pass "Missing optional fields handled with defaults"
  | Error msg ->
    fail (sprintf "Should handle missing optional fields: %s" msg)

let test_normalize_public_trade_valid () =
  printf "\n[Normalize] Public trade with valid data\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "pub_trade_456";
    side = "SELL";
    size = "0.5";
    price = "49500.0";
    createdAt = "2024-01-02T12:00:00.000Z";
    createdAtHeight = Some "1000000";
  } in
  match Dydx.Fluxum_adapter.Adapter.Normalize.public_trade trade with
  | Ok pt ->
    ignore (assert_string_equal "pub_trade_456" (Option.value_exn pt.trade_id) "Trade ID preserved");
    ignore (assert_float_equal 0.5 pt.qty "Quantity parsed");
    ignore (assert_float_equal 49500.0 pt.price "Price parsed");
    pass "Public trade normalized"
  | Error msg ->
    fail (sprintf "Public trade normalization failed: %s" msg)

let test_normalize_public_trade_malformed () =
  printf "\n[Normalize] Public trade with malformed data\n";
  let trade : Dydx.Rest.Types.trade = {
    id = "bad_trade";
    side = "BUY";
    size = "infinity";
    price = "50000.0";
    createdAt = "2024-01-02T12:00:00.000Z";
    createdAtHeight = None;
  } in
  ignore (assert_error
    (Dydx.Fluxum_adapter.Adapter.Normalize.public_trade trade)
    "Rejected malformed public trade")

(* ============================================================ *)
(* Main Test Runner *)
(* ============================================================ *)

let () =
  printf "\n";
  printf "===========================================\n";
  printf "dYdX Fluxum Adapter Unit Tests\n";
  printf "===========================================\n";

  (* Trade tests *)
  test_normalize_trade_valid ();
  test_normalize_trade_invalid_price ();
  test_normalize_trade_invalid_size ();
  test_normalize_trade_negative_price ();
  test_normalize_trade_zero_size ();
  test_normalize_trade_invalid_side ();

  (* Order book tests *)
  test_normalize_order_book_valid ();
  test_normalize_order_book_invalid_bid_price ();
  test_normalize_order_book_invalid_ask_size ();
  test_normalize_order_book_empty ();

  (* Symbol info tests *)
  test_normalize_symbol_info_valid ();
  test_normalize_symbol_info_invalid_step_size ();
  test_normalize_symbol_info_invalid_tick_size ();

  (* Ticker tests *)
  test_normalize_ticker_valid ();
  test_normalize_ticker_invalid_oracle_price ();
  test_normalize_ticker_invalid_volume ();
  test_normalize_ticker_missing_optional_fields ();

  (* Public trade tests *)
  test_normalize_public_trade_valid ();
  test_normalize_public_trade_malformed ();

  (* Summary *)
  printf "\n";
  printf "===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d ✓\n" !tests_passed;
  printf "Failed:       %d ✗\n" !tests_failed;
  printf "Success rate: %.1f%%\n" (Float.of_int !tests_passed /. Float.of_int !tests_run *. 100.0);
  printf "===========================================\n";
  printf "\n";

  match !tests_failed with
  | 0 -> exit 0
  | _ -> exit 1

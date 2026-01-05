(** Bot Framework Comprehensive Tests *)

open Core

(* ============================================================================
   EVENT MODULE TESTS
   ============================================================================ *)

let%expect_test "event serialization - system bot_started" =
  let event = Bot.Event.System (Bot.Event.System_event.Bot_started
    { bot_id = "test-bot"
    ; config_json = "{\"key\": \"value\"}"
    ; version = "1.0.0"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let serialized = Bot.Event.serialize envelope in
  let deserialized = Bot.Event.deserialize serialized in
  (match deserialized with
  | Ok env ->
    printf "Round-trip: OK\n";
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error e ->
    printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {|
    Round-trip: OK
    Category: system
    Description: START test-bot
  |}]

let%expect_test "event serialization - system bot_stopped" =
  let event = Bot.Event.System (Bot.Event.System_event.Bot_stopped
    { reason = "user request"
    ; exit_code = Some 0
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let serialized = Bot.Event.serialize envelope in
  (match Bot.Event.deserialize serialized with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: system
    Description: STOP: user request
  |}]

let%expect_test "event serialization - system connection_state_changed" =
  let event = Bot.Event.System (Bot.Event.System_event.Connection_state_changed
    { venue = Bot.Event.Venue.Kraken
    ; old_state = Bot.Event.Connection_state.Disconnected
    ; new_state = Bot.Event.Connection_state.Connected
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let serialized = Bot.Event.serialize envelope in
  (match Bot.Event.deserialize serialized with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description contains Kraken: %b\n" (String.is_substring (Bot.Event.describe env.event) ~substring:"Kraken")
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: system
    Description contains Kraken: true
  |}]

let%expect_test "event serialization - system error" =
  let event = Bot.Event.System (Bot.Event.System_event.Error
    { venue = Some Bot.Event.Venue.Gemini
    ; message = "Connection timeout"
    ; is_fatal = false
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let serialized = Bot.Event.serialize envelope in
  (match Bot.Event.deserialize serialized with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: system
    Description: error: Connection timeout
  |}]

let%expect_test "event serialization - system fatal error" =
  let event = Bot.Event.System (Bot.Event.System_event.Error
    { venue = None
    ; message = "Out of memory"
    ; is_fatal = true
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Description: FATAL: Out of memory
  |}]

let%expect_test "event serialization - system heartbeat" =
  let event = Bot.Event.System (Bot.Event.System_event.Heartbeat { sequence = 42L }) in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: system
    Description: heartbeat #42
  |}]

let%expect_test "event serialization - order submitted" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-123"
    ; symbol = "ETHUSD"
    ; venue = Bot.Event.Venue.Coinbase
    ; side = Bot.Event.Side.Sell
    ; qty = 1.5
    ; price = Some 3000.0
    ; time_in_force = Bot.Event.Time_in_force.IOC
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: submit sell ETHUSD 1.50000000@3000.00 [ord-123]
  |}]

let%expect_test "event serialization - order accepted" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_accepted
    { order_id = "ord-123"
    ; exchange_id = "exch-456"
    ; venue = Bot.Event.Venue.Binance
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: accept ord-123 -> exch-456
  |}]

let%expect_test "event serialization - order filled" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "ord-123"
    ; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.5
    ; fill_price = 50000.0
    ; fee = 2.5
    ; is_maker = Some true
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: fill ord-123 0.50000000@50000.00
  |}]

let%expect_test "event serialization - order partially filled" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_partially_filled
    { order_id = "ord-123"
    ; venue = Bot.Event.Venue.Kraken
    ; fill_qty = 0.3
    ; fill_price = 49000.0
    ; remaining_qty = 0.7
    ; fee = 1.5
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: partial ord-123 0.30000000 (rem: 0.70000000)
  |}]

let%expect_test "event serialization - order cancelled" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_cancelled
    { order_id = "ord-123"
    ; venue = Bot.Event.Venue.Mexc
    ; reason = "user requested"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: cancel ord-123: user requested
  |}]

let%expect_test "event serialization - order rejected" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_rejected
    { order_id = "ord-123"
    ; venue = Bot.Event.Venue.Hyperliquid
    ; reason = "insufficient funds"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: order
    Description: reject ord-123: insufficient funds
  |}]

let%expect_test "event serialization - market book_update snapshot" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Book_update
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = [{ price = 50000.0; qty = 1.0 }]
    ; asks = [{ price = 50001.0; qty = 1.0 }]
    ; is_snapshot = true
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: market
    Description: book_snap BTCUSD@Gemini
  |}]

let%expect_test "event serialization - market book_update incremental" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Book_update
    { symbol = "ETHUSD"
    ; venue = Bot.Event.Venue.Kraken
    ; bids = [{ price = 3000.0; qty = 2.0 }]
    ; asks = [{ price = 3001.0; qty = 2.0 }]
    ; is_snapshot = false
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Description: book_upd ETHUSD@Kraken
  |}]

let%expect_test "event serialization - market trade with side" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Trade
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Binance
    ; price = 49999.0
    ; qty = 0.1
    ; side = Some Bot.Event.Side.Buy
    ; trade_id = Some "trade-789"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: market
    Description: trade BTCUSD@Binance 0.10000000@49999.00 buy
  |}]

let%expect_test "event serialization - market trade without side" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Trade
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; price = 50000.0
    ; qty = 0.5
    ; side = None
    ; trade_id = None
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Description: trade BTCUSD@Gemini 0.50000000@50000.00 ?
  |}]

let%expect_test "event serialization - market ticker" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Ticker
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Coinbase
    ; bid = 49999.0
    ; ask = 50001.0
    ; last = 50000.0
    ; volume_24h = Some 1000.0
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: market
    Description: tick BTCUSD@Coinbase last=50000.00
  |}]

let%expect_test "event serialization - balance update" =
  let event = Bot.Event.Balance (Bot.Event.Balance_event.Balance_update
    { venue = Bot.Event.Venue.Gemini
    ; currency = "USD"
    ; available = 10000.0
    ; locked = 500.0
    ; total = 10500.0
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: balance
    Description: bal USD@Gemini 10500.00000000
  |}]

let%expect_test "event serialization - balance snapshot" =
  let event = Bot.Event.Balance (Bot.Event.Balance_event.Balance_snapshot
    { venue = Bot.Event.Venue.Kraken
    ; balances = [("USD", 5000.0, 0.0, 5000.0); ("BTC", 0.5, 0.1, 0.6)]
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: balance
    Description: bal_snap Kraken (2 assets)
  |}]

let%expect_test "event serialization - strategy signal_generated" =
  let event = Bot.Event.Strategy (Bot.Event.Strategy_event.Signal_generated
    { signal_type = Bot.Event.Strategy_event.Place_order
    ; symbol = Some "BTCUSD"
    ; reason = "Momentum signal triggered"
    ; details = Some "RSI > 70"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: strategy
  |}]

let%expect_test "event serialization - strategy position_changed" =
  let event = Bot.Event.Strategy (Bot.Event.Strategy_event.Position_changed
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; old_qty = 0.0
    ; new_qty = 0.5
    ; avg_cost = 50000.0
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: strategy
    Description: pos BTCUSD -> 0.50000000
  |}]

let%expect_test "event serialization - strategy pnl_update" =
  let event = Bot.Event.Strategy (Bot.Event.Strategy_event.Pnl_update
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Kraken
    ; realized = 100.0
    ; unrealized = 50.0
    ; total = 150.0
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  (match Bot.Event.deserialize (Bot.Event.serialize envelope) with
  | Ok env ->
    printf "Category: %s\n" (Bot.Event.category env.event);
    printf "Description: %s\n" (Bot.Event.describe env.event)
  | Error _ -> printf "Error\n");
  [%expect {|
    Category: strategy
    Description: pnl BTCUSD 150.00
  |}]

let%expect_test "event venue conversions" =
  let venues = [
    Bot.Event.Venue.Gemini;
    Bot.Event.Venue.Kraken;
    Bot.Event.Venue.Mexc;
    Bot.Event.Venue.Coinbase;
    Bot.Event.Venue.Binance;
    Bot.Event.Venue.Hyperliquid;
    Bot.Event.Venue.Bitrue;
    Bot.Event.Venue.Dydx;
    Bot.Event.Venue.Jupiter;
    Bot.Event.Venue.OneInch;
    Bot.Event.Venue.Other "TestExchange";
  ] in
  List.iter venues ~f:(fun v ->
    printf "%s\n" (Bot.Event.Venue.to_string v));
  [%expect {|
    Gemini
    Kraken
    MEXC
    Coinbase
    Binance
    Hyperliquid
    Bitrue
    dYdX
    Jupiter
    1inch
    TestExchange
  |}]

let%expect_test "event side conversions" =
  printf "Buy: %s\n" (Bot.Event.Side.to_string Bot.Event.Side.Buy);
  printf "Sell: %s\n" (Bot.Event.Side.to_string Bot.Event.Side.Sell);
  [%expect {|
    Buy: buy
    Sell: sell
  |}]

let%expect_test "time in force to_string" =
  printf "GTC: %s\n" (Bot.Event.Time_in_force.to_string Bot.Event.Time_in_force.GTC);
  printf "IOC: %s\n" (Bot.Event.Time_in_force.to_string Bot.Event.Time_in_force.IOC);
  printf "FOK: %s\n" (Bot.Event.Time_in_force.to_string Bot.Event.Time_in_force.FOK);
  [%expect {|
    GTC: GTC
    IOC: IOC
    FOK: FOK
  |}]

(* ============================================================================
   STATE MODULE TESTS
   ============================================================================ *)

let%expect_test "state empty initialization" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  printf "Bot ID: %s\n" state.bot_id;
  printf "Is Running: %b\n" state.is_running;
  printf "Event Count: %d\n" state.event_count;
  printf "Error Count: %d\n" state.error_count;
  printf "Active Orders: %d\n" (Map.length state.active_orders);
  [%expect {|
    Bot ID: test-bot
    Is Running: false
    Event Count: 0
    Error Count: 0
    Active Orders: 0
  |}]

let%expect_test "state apply_event - bot started" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event = Bot.Event.System (Bot.Event.System_event.Bot_started
    { bot_id = "test-bot"
    ; config_json = "{}"
    ; version = "1.0.0"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let state = Bot.State.apply_event state envelope in
  printf "Is Running: %b\n" state.is_running;
  printf "Version: %s\n" state.version;
  printf "Event Count: %d\n" state.event_count;
  printf "Last Sequence: %Ld\n" state.last_sequence;
  [%expect {|
    Is Running: true
    Version: 1.0.0
    Event Count: 1
    Last Sequence: 1
  |}]

let%expect_test "state apply_event - bot stopped" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let start_event = Bot.Event.System (Bot.Event.System_event.Bot_started
    { bot_id = "test-bot"; config_json = "{}"; version = "1.0.0" })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L start_event) in

  let stop_event = Bot.Event.System (Bot.Event.System_event.Bot_stopped
    { reason = "shutdown"; exit_code = Some 0 })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L stop_event) in
  printf "Is Running: %b\n" state.is_running;
  printf "Event Count: %d\n" state.event_count;
  [%expect {|
    Is Running: false
    Event Count: 2
  |}]

let%expect_test "state apply_event - connection state changed" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event = Bot.Event.System (Bot.Event.System_event.Connection_state_changed
    { venue = Bot.Event.Venue.Gemini
    ; old_state = Bot.Event.Connection_state.Disconnected
    ; new_state = Bot.Event.Connection_state.Ready
    })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L event) in
  printf "Connected Venues: %d\n" (List.length (Bot.State.connected_venues state));
  printf "Ready Venues: %d\n" (List.length (Bot.State.ready_venues state));
  [%expect {|
    Connected Venues: 1
    Ready Venues: 1
  |}]

let%expect_test "state apply_event - error tracking" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event1 = Bot.Event.System (Bot.Event.System_event.Error
    { venue = Some Bot.Event.Venue.Gemini
    ; message = "Connection timeout"
    ; is_fatal = false
    })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L event1) in
  printf "Error Count: %d\n" state.error_count;
  printf "Last Error: %s\n" (Option.value state.last_error ~default:"none");
  printf "Is Running: %b\n" state.is_running;

  (* Fatal error *)
  let event2 = Bot.Event.System (Bot.Event.System_event.Error
    { venue = None; message = "Fatal error"; is_fatal = true })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L event2) in
  printf "Error Count after fatal: %d\n" state.error_count;
  printf "Is Running after fatal: %b\n" state.is_running;
  [%expect {|
    Error Count: 1
    Last Error: Connection timeout
    Is Running: false
    Error Count after fatal: 2
    Is Running after fatal: false
  |}]

let%expect_test "state apply_event - order lifecycle" =
  let state = Bot.State.empty ~bot_id:"test-bot" in

  (* Submit *)
  let submit = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L submit) in
  printf "After submit - Active: %d\n" (Map.length state.active_orders);

  (* Accept *)
  let accept = Bot.Event.Order (Bot.Event.Order_event.Order_accepted
    { order_id = "ord-1"; exchange_id = "exch-1"; venue = Bot.Event.Venue.Gemini })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L accept) in
  let order = Map.find_exn state.active_orders "ord-1" in
  printf "After accept - Exchange ID: %s\n" (Option.value order.exchange_id ~default:"none");

  (* Partial fill *)
  let partial = Bot.Event.Order (Bot.Event.Order_event.Order_partially_filled
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.3; fill_price = 50000.0; remaining_qty = 0.7; fee = 1.5 })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:3L partial) in
  let order = Map.find_exn state.active_orders "ord-1" in
  printf "After partial - Filled: %.1f, Remaining: %.1f\n" order.filled_qty order.remaining_qty;

  (* Full fill *)
  let fill = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.7; fill_price = 50000.0; fee = 3.5; is_maker = Some true })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:4L fill) in
  printf "After full fill - Active: %d\n" (Map.length state.active_orders);
  [%expect {|
    After submit - Active: 1
    After accept - Exchange ID: exch-1
    After partial - Filled: 0.3, Remaining: 0.7
    After full fill - Active: 0
  |}]

let%expect_test "state apply_event - order cancelled" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let submit = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L submit) in

  let cancel = Bot.Event.Order (Bot.Event.Order_event.Order_cancelled
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini; reason = "user request" })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L cancel) in
  printf "After cancel - Active: %d\n" (Map.length state.active_orders);
  [%expect {|
    After cancel - Active: 0
  |}]

let%expect_test "state apply_event - order rejected" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let submit = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L submit) in

  let reject = Bot.Event.Order (Bot.Event.Order_event.Order_rejected
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini; reason = "insufficient funds" })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L reject) in
  printf "After reject - Active: %d\n" (Map.length state.active_orders);
  [%expect {|
    After reject - Active: 0
  |}]

let%expect_test "state apply_event - balance update" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event = Bot.Event.Balance (Bot.Event.Balance_event.Balance_update
    { venue = Bot.Event.Venue.Gemini
    ; currency = "USD"
    ; available = 10000.0
    ; locked = 500.0
    ; total = 10500.0
    })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L event) in
  let balance = Bot.State.balance state ~venue:Bot.Event.Venue.Gemini ~currency:"USD" in
  (match balance with
  | Some b ->
    printf "Currency: %s\n" b.currency;
    printf "Available: %.2f\n" b.available;
    printf "Locked: %.2f\n" b.locked;
    printf "Total: %.2f\n" b.total
  | None -> printf "No balance found\n");
  [%expect {|
    Currency: USD
    Available: 10000.00
    Locked: 500.00
    Total: 10500.00
  |}]

let%expect_test "state apply_event - balance snapshot" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event = Bot.Event.Balance (Bot.Event.Balance_event.Balance_snapshot
    { venue = Bot.Event.Venue.Kraken
    ; balances = [("USD", 5000.0, 0.0, 5000.0); ("BTC", 0.5, 0.1, 0.6)]
    })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L event) in
  let usd_bal = Bot.State.balance state ~venue:Bot.Event.Venue.Kraken ~currency:"USD" in
  let btc_bal = Bot.State.balance state ~venue:Bot.Event.Venue.Kraken ~currency:"BTC" in
  printf "USD: %s\n" (Option.value_map usd_bal ~default:"none" ~f:(fun b -> sprintf "%.2f" b.total));
  printf "BTC: %s\n" (Option.value_map btc_bal ~default:"none" ~f:(fun b -> sprintf "%.4f" b.total));
  [%expect {|
    USD: 5000.00
    BTC: 0.6000
  |}]

let%expect_test "state apply_event - market book update updates ledger" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let event = Bot.Event.Market (Bot.Event.Market_event.Book_update
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = [{ price = 50000.0; qty = 1.0 }]
    ; asks = [{ price = 50002.0; qty = 1.0 }]
    ; is_snapshot = true
    })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L event) in
  printf "Event Count: %d\n" state.event_count;
  [%expect {|
    Event Count: 1
  |}]

let%expect_test "state helper functions" =
  let state = Bot.State.empty ~bot_id:"test-bot" in

  (* Add some orders *)
  let submit1 = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let submit2 = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-2"; symbol = "ETHUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Sell; qty = 2.0; price = Some 3000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let submit3 = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-3"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Kraken
    ; side = Bot.Event.Side.Buy; qty = 0.5; price = Some 49999.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L submit1) in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:2L submit2) in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:3L submit3) in

  printf "Total Active: %d\n" (Map.length state.active_orders);
  printf "BTCUSD orders: %d\n" (List.length (Bot.State.orders_for_symbol state ~symbol:"BTCUSD"));
  printf "Gemini orders: %d\n" (List.length (Bot.State.orders_for_venue state ~venue:Bot.Event.Venue.Gemini));
  printf "Kraken orders: %d\n" (List.length (Bot.State.orders_for_venue state ~venue:Bot.Event.Venue.Kraken));
  [%expect {|
    Total Active: 3
    BTCUSD orders: 2
    Gemini orders: 2
    Kraken orders: 1
  |}]

let%expect_test "state summary" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let summary = Bot.State.summary state in
  printf "Summary contains bot_id: %b\n" (String.is_substring summary ~substring:"test-bot");
  printf "Summary contains running: %b\n" (String.is_substring summary ~substring:"running");
  [%expect {|
    Summary contains bot_id: true
    Summary contains running: true
  |}]

let%expect_test "state validate" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  (match Bot.State.validate state with
  | Ok () -> printf "Valid\n"
  | Error _ -> printf "Invalid\n");
  [%expect {|
    Valid
  |}]

(* ============================================================================
   STRATEGY INTERFACE TESTS
   ============================================================================ *)

let%expect_test "strategy signal - buy_market" =
  let signal = Bot.Strategy_intf.Signal.buy_market
    ~symbol:"BTCUSD"
    ~venue:Bot.Event.Venue.Gemini
    ~qty:0.1
  in
  (match signal with
  | Bot.Strategy_intf.Signal.Place_order req ->
    printf "Side: %s\n" (Bot.Event.Side.to_string req.side);
    printf "Price: %s\n" (Option.value_map req.price ~default:"MKT" ~f:(sprintf "%.2f"));
    printf "TIF: %s\n" (Bot.Strategy_intf.Time_in_force.to_string req.time_in_force)
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Side: buy
    Price: MKT
    TIF: IOC
  |}]

let%expect_test "strategy signal - sell_market" =
  let signal = Bot.Strategy_intf.Signal.sell_market
    ~symbol:"BTCUSD"
    ~venue:Bot.Event.Venue.Gemini
    ~qty:0.1
  in
  (match signal with
  | Bot.Strategy_intf.Signal.Place_order req ->
    printf "Side: %s\n" (Bot.Event.Side.to_string req.side);
    printf "Price: %s\n" (Option.value_map req.price ~default:"MKT" ~f:(sprintf "%.2f"))
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Side: sell
    Price: MKT
  |}]

let%expect_test "strategy signal - buy_limit" =
  let signal = Bot.Strategy_intf.Signal.buy_limit
    ~symbol:"BTCUSD"
    ~venue:Bot.Event.Venue.Gemini
    ~qty:0.1
    ~price:50000.0
  in
  (match signal with
  | Bot.Strategy_intf.Signal.Place_order req ->
    printf "Side: %s\n" (Bot.Event.Side.to_string req.side);
    printf "Price: %.2f\n" (Option.value_exn req.price);
    printf "TIF: %s\n" (Bot.Strategy_intf.Time_in_force.to_string req.time_in_force)
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Side: buy
    Price: 50000.00
    TIF: GTC
  |}]

let%expect_test "strategy signal - sell_limit" =
  let signal = Bot.Strategy_intf.Signal.sell_limit
    ~symbol:"BTCUSD"
    ~venue:Bot.Event.Venue.Gemini
    ~qty:0.1
    ~price:51000.0
  in
  (match signal with
  | Bot.Strategy_intf.Signal.Place_order req ->
    printf "Side: %s\n" (Bot.Event.Side.to_string req.side);
    printf "Price: %.2f\n" (Option.value_exn req.price)
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Side: sell
    Price: 51000.00
  |}]

let%expect_test "strategy signal - cancel" =
  let signal = Bot.Strategy_intf.Signal.cancel ~order_id:"ord-123" in
  (match signal with
  | Bot.Strategy_intf.Signal.Cancel (Bot.Strategy_intf.Cancel_request.Cancel_order { order_id }) ->
    printf "Cancel order: %s\n" order_id
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Cancel order: ord-123
  |}]

let%expect_test "strategy signal - cancel_all" =
  let signal = Bot.Strategy_intf.Signal.cancel_all ~symbol:"BTCUSD" () in
  (match signal with
  | Bot.Strategy_intf.Signal.Cancel (Bot.Strategy_intf.Cancel_request.Cancel_all { symbol; venue }) ->
    printf "Cancel all - Symbol: %s, Venue: %s\n"
      (Option.value symbol ~default:"all")
      (Option.value_map venue ~default:"all" ~f:Bot.Event.Venue.to_string)
  | _ -> printf "Wrong signal type\n");
  [%expect {|
    Cancel all - Symbol: BTCUSD, Venue: all
  |}]

let%expect_test "book snapshot - empty book" =
  let book : Bot.Strategy_intf.Book_snapshot.t =
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = []
    ; asks = []
    ; timestamp = Bot.Event.Time.now ()
    }
  in
  printf "Mid price: %s\n" (Option.value_map (Bot.Strategy_intf.Book_snapshot.mid_price book) ~default:"none" ~f:(sprintf "%.2f"));
  printf "Spread: %s\n" (Option.value_map (Bot.Strategy_intf.Book_snapshot.spread book) ~default:"none" ~f:(sprintf "%.2f"));
  printf "Best bid: %s\n" (Option.value_map (Bot.Strategy_intf.Book_snapshot.best_bid book) ~default:"none" ~f:(fun l -> sprintf "%.2f" l.price));
  printf "Best ask: %s\n" (Option.value_map (Bot.Strategy_intf.Book_snapshot.best_ask book) ~default:"none" ~f:(fun l -> sprintf "%.2f" l.price));
  [%expect {|
    Mid price: none
    Spread: none
    Best bid: none
    Best ask: none
  |}]

let%expect_test "book snapshot - normal book" =
  let book : Bot.Strategy_intf.Book_snapshot.t =
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = [{ price = 50000.0; qty = 1.0 }; { price = 49999.0; qty = 2.0 }]
    ; asks = [{ price = 50001.0; qty = 1.0 }; { price = 50002.0; qty = 2.0 }]
    ; timestamp = Bot.Event.Time.now ()
    }
  in
  printf "Mid price: %.2f\n" (Option.value_exn (Bot.Strategy_intf.Book_snapshot.mid_price book));
  printf "Spread: %.2f\n" (Option.value_exn (Bot.Strategy_intf.Book_snapshot.spread book));
  printf "Spread bps: %.4f\n" (Option.value_exn (Bot.Strategy_intf.Book_snapshot.spread_bps book));
  printf "Best bid: %.2f\n" (Option.value_exn (Bot.Strategy_intf.Book_snapshot.best_bid book)).price;
  printf "Best ask: %.2f\n" (Option.value_exn (Bot.Strategy_intf.Book_snapshot.best_ask book)).price;
  [%expect {|
    Mid price: 50000.50
    Spread: 1.00
    Spread bps: 0.2000
    Best bid: 50000.00
    Best ask: 50001.00
  |}]

let%expect_test "context helper functions" =
  let ctx : Bot.Strategy_intf.Context.t =
    { timestamp = Bot.Event.Time.now ()
    ; positions = [("BTCUSD", 0.5); ("ETHUSD", -2.0)]
    ; balances = [("USD", 10000.0); ("BTC", 0.5)]
    ; active_orders = []
    ; total_pnl = 100.0
    ; realized_pnl = 50.0
    ; unrealized_pnl = 50.0
    }
  in
  printf "BTC position: %.4f\n" (Bot.Strategy_intf.Context.position ctx ~symbol:"BTCUSD");
  printf "ETH position: %.4f\n" (Bot.Strategy_intf.Context.position ctx ~symbol:"ETHUSD");
  printf "SOL position: %.4f\n" (Bot.Strategy_intf.Context.position ctx ~symbol:"SOLUSD");
  printf "USD balance: %.2f\n" (Bot.Strategy_intf.Context.balance ctx ~currency:"USD");
  printf "Has BTC position: %b\n" (Bot.Strategy_intf.Context.has_position ctx ~symbol:"BTCUSD");
  printf "Has SOL position: %b\n" (Bot.Strategy_intf.Context.has_position ctx ~symbol:"SOLUSD");
  [%expect {|
    BTC position: 0.5000
    ETH position: -2.0000
    SOL position: 0.0000
    USD balance: 10000.00
    Has BTC position: true
    Has SOL position: false
  |}]

let%expect_test "noop strategy" =
  let state = Bot.Strategy_intf.Noop.init () in
  let book : Bot.Strategy_intf.Book_snapshot.t =
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = [{ price = 50000.0; qty = 1.0 }]
    ; asks = [{ price = 50001.0; qty = 1.0 }]
    ; timestamp = Bot.Event.Time.now ()
    }
  in
  let ctx : Bot.Strategy_intf.Context.t =
    { timestamp = Bot.Event.Time.now ()
    ; positions = []; balances = []; active_orders = []
    ; total_pnl = 0.0; realized_pnl = 0.0; unrealized_pnl = 0.0
    }
  in
  let signals, _new_state = Bot.Strategy_intf.Noop.on_book_update state ~book ~context:ctx in
  printf "Strategy name: %s\n" Bot.Strategy_intf.Noop.name;
  printf "Strategy version: %s\n" Bot.Strategy_intf.Noop.version;
  printf "Signals count: %d\n" (List.length signals);
  [%expect {|
    Strategy name: noop
    Strategy version: 1.0.0
    Signals count: 0
  |}]

(* ============================================================================
   UNIFIED LEDGER TESTS
   ============================================================================ *)

let%expect_test "ledger entry empty" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  printf "Symbol: %s\n" entry.symbol;
  printf "Position: %.4f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Trade Count: %d\n" entry.trade_count;
  [%expect {|
    Symbol: BTCUSD
    Position: 0.0000
    Avg Cost: 0.00
    Trade Count: 0
  |}]

let%expect_test "ledger entry apply_fill - single buy" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:0.1 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  printf "Position: %.4f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Buy Volume: %.4f\n" entry.buy_volume;
  printf "Fees: %.2f\n" entry.total_fees;
  printf "Trade Count: %d\n" entry.trade_count;
  [%expect {|
    Position: 0.1000
    Avg Cost: 50000.00
    Buy Volume: 0.1000
    Fees: 5.00
    Trade Count: 1
  |}]

let%expect_test "ledger entry apply_fill - multiple buys average cost" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  let entry = Bot.Entry.apply_fill entry ~price:52000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  printf "Position: %.4f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Trade Count: %d\n" entry.trade_count;
  [%expect {|
    Position: 2.0000
    Avg Cost: 51000.00
    Trade Count: 2
  |}]

let%expect_test "ledger entry apply_fill - buy then sell (realize profit)" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  let entry = Bot.Entry.apply_fill entry ~price:51000.0 ~qty:0.5 ~side:Bot.Entry.Side.Sell ~fee:2.5 in
  printf "Position: %.4f\n" entry.position;
  printf "Realized P&L: %.2f\n" entry.realized_pnl;
  printf "Winning Trades: %d\n" entry.winning_trades;
  printf "Losing Trades: %d\n" entry.losing_trades;
  [%expect {|
    Position: 0.5000
    Realized P&L: 500.00
    Winning Trades: 1
    Losing Trades: 0
  |}]

let%expect_test "ledger entry apply_fill - buy then sell (realize loss)" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  let entry = Bot.Entry.apply_fill entry ~price:49000.0 ~qty:0.5 ~side:Bot.Entry.Side.Sell ~fee:2.5 in
  printf "Position: %.4f\n" entry.position;
  printf "Realized P&L: %.2f\n" entry.realized_pnl;
  printf "Winning Trades: %d\n" entry.winning_trades;
  printf "Losing Trades: %d\n" entry.losing_trades;
  [%expect {|
    Position: 0.5000
    Realized P&L: -500.00
    Winning Trades: 0
    Losing Trades: 1
  |}]

let%expect_test "ledger entry apply_fill - short position" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Sell ~fee:5.0 in
  printf "Position: %.4f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Sell Volume: %.4f\n" entry.sell_volume;
  [%expect {|
    Position: -1.0000
    Avg Cost: 50000.00
    Sell Volume: 1.0000
  |}]

let%expect_test "ledger entry mark_to_market" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  let entry = Bot.Entry.mark_to_market entry ~price:51000.0 in
  printf "Unrealized P&L: %.2f\n" entry.unrealized_pnl;
  printf "Total P&L: %.2f\n" entry.total_pnl;
  printf "Mark Price: %.2f\n" entry.mark_price;
  [%expect {|
    Unrealized P&L: 1000.00
    Total P&L: 1000.00
    Mark Price: 51000.00
  |}]

let%expect_test "ledger entry mark_to_market short" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Sell ~fee:5.0 in
  let entry = Bot.Entry.mark_to_market entry ~price:49000.0 in
  printf "Unrealized P&L: %.2f\n" entry.unrealized_pnl;
  [%expect {|
    Unrealized P&L: 1000.00
  |}]

let%expect_test "ledger entry summary" =
  let entry = Bot.Entry.empty ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini in
  let entry = Bot.Entry.apply_fill entry ~price:50000.0 ~qty:1.0 ~side:Bot.Entry.Side.Buy ~fee:5.0 in
  let summary = Bot.Entry.summary entry in
  printf "Summary contains BTCUSD: %b\n" (String.is_substring summary ~substring:"BTCUSD");
  printf "Summary contains Gemini: %b\n" (String.is_substring summary ~substring:"Gemini");
  [%expect {|
    Summary contains BTCUSD: true
    Summary contains Gemini: true
  |}]

let%expect_test "ledger create and operations" =
  let ledger = Bot.Ledger.create () in
  printf "Initial count: %d\n" (Bot.Ledger.count ledger);
  printf "Initial total P&L: %.2f\n" (Bot.Ledger.total_pnl ledger);
  [%expect {|
    Initial count: 0
    Initial total P&L: 0.00
  |}]

let%expect_test "ledger apply_fill creates entries" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"ETHUSD" ~venue:Bot.Entry.Venue.Kraken
    ~price:3000.0 ~qty:2.0 ~side:Fluxum.Types.Side.Buy ~fee:3.0
  in
  printf "Entry count: %d\n" (Bot.Ledger.count ledger);
  printf "Total fees: %.2f\n" (Bot.Ledger.total_fees ledger);
  [%expect {|
    Entry count: 2
    Total fees: 8.00
  |}]

let%expect_test "ledger mark_to_market" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger = Bot.Ledger.mark_to_market ledger ~prices:[("BTCUSD", 51000.0)] in
  printf "Total unrealized: %.2f\n" (Bot.Ledger.total_unrealized ledger);
  printf "Total P&L: %.2f\n" (Bot.Ledger.total_pnl ledger);
  [%expect {|
    Total unrealized: 1000.00
    Total P&L: 1000.00
  |}]

let%expect_test "ledger pnl_by_venue" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"ETHUSD" ~venue:Bot.Entry.Venue.Kraken
    ~price:3000.0 ~qty:2.0 ~side:Fluxum.Types.Side.Buy ~fee:3.0
  in
  let by_venue = Bot.Ledger.pnl_by_venue ledger in
  printf "Venues with P&L: %d\n" (List.length by_venue);
  [%expect {|
    Venues with P&L: 2
  |}]

let%expect_test "ledger pnl_by_symbol" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Kraken
    ~price:50100.0 ~qty:0.5 ~side:Fluxum.Types.Side.Buy ~fee:2.5
  in
  let by_symbol = Bot.Ledger.pnl_by_symbol ledger in
  printf "Symbols with P&L: %d\n" (List.length by_symbol);
  [%expect {|
    Symbols with P&L: 1
  |}]

let%expect_test "ledger entries_for_venue" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"ETHUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:3000.0 ~qty:2.0 ~side:Fluxum.Types.Side.Buy ~fee:3.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Kraken
    ~price:50000.0 ~qty:0.5 ~side:Fluxum.Types.Side.Buy ~fee:2.5
  in
  let gemini_entries = Bot.Ledger.entries_for_venue ledger ~venue:Bot.Entry.Venue.Gemini in
  let kraken_entries = Bot.Ledger.entries_for_venue ledger ~venue:Bot.Entry.Venue.Kraken in
  printf "Gemini entries: %d\n" (List.length gemini_entries);
  printf "Kraken entries: %d\n" (List.length kraken_entries);
  [%expect {|
    Gemini entries: 2
    Kraken entries: 1
  |}]

let%expect_test "ledger entries_for_symbol" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Kraken
    ~price:50100.0 ~qty:0.5 ~side:Fluxum.Types.Side.Buy ~fee:2.5
  in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"ETHUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:3000.0 ~qty:2.0 ~side:Fluxum.Types.Side.Buy ~fee:3.0
  in
  let btc_entries = Bot.Ledger.entries_for_symbol ledger ~symbol:"BTCUSD" in
  let eth_entries = Bot.Ledger.entries_for_symbol ledger ~symbol:"ETHUSD" in
  printf "BTCUSD entries: %d\n" (List.length btc_entries);
  printf "ETHUSD entries: %d\n" (List.length eth_entries);
  [%expect {|
    BTCUSD entries: 2
    ETHUSD entries: 1
  |}]

let%expect_test "ledger summary" =
  let ledger = Bot.Ledger.create () in
  let ledger, _ = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD" ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0
  in
  let summary = Bot.Ledger.summary ledger in
  printf "Summary contains Ledger: %b\n" (String.is_substring summary ~substring:"Ledger");
  printf "Summary contains entries: %b\n" (String.is_substring summary ~substring:"entries");
  [%expect {|
    Summary contains Ledger: true
    Summary contains entries: true
  |}]

(* ============================================================================
   PARQUET EXPORT TESTS
   ============================================================================ *)

let%expect_test "parquet export row - order_submitted" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  printf "Category: %s\n" row.category;
  printf "Event type: %s\n" row.event_type;
  printf "Symbol: %s\n" (Option.value row.symbol ~default:"none");
  printf "Venue: %s\n" (Option.value row.venue ~default:"none");
  printf "Side: %s\n" (Option.value row.side ~default:"none");
  [%expect {|
    Category: order
    Event type: order_submitted
    Symbol: BTCUSD
    Venue: Gemini
    Side: buy
  |}]

let%expect_test "parquet export row - trade" =
  let event = Bot.Event.Market (Bot.Event.Market_event.Trade
    { symbol = "ETHUSD"; venue = Bot.Event.Venue.Kraken
    ; price = 3000.0; qty = 2.0; side = Some Bot.Event.Side.Sell
    ; trade_id = Some "trade-123" })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  printf "Event type: %s\n" row.event_type;
  printf "Symbol: %s\n" (Option.value row.symbol ~default:"none");
  printf "Price: %.2f\n" (Option.value_exn row.price);
  printf "Qty: %.4f\n" (Option.value_exn row.qty);
  [%expect {|
    Event type: trade
    Symbol: ETHUSD
    Price: 3000.00
    Qty: 2.0000
  |}]

let%expect_test "parquet export row - balance_update" =
  let event = Bot.Event.Balance (Bot.Event.Balance_event.Balance_update
    { venue = Bot.Event.Venue.Gemini
    ; currency = "USD"
    ; available = 10000.0
    ; locked = 500.0
    ; total = 10500.0 })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  printf "Category: %s\n" row.category;
  printf "Event type: %s\n" row.event_type;
  printf "Symbol (currency): %s\n" (Option.value row.symbol ~default:"none");
  [%expect {|
    Category: balance
    Event type: balance_update
    Symbol (currency): USD
  |}]

let%expect_test "parquet export row - system error" =
  let event = Bot.Event.System (Bot.Event.System_event.Error
    { venue = Some Bot.Event.Venue.Binance
    ; message = "Connection failed"
    ; is_fatal = false })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  printf "Event type: %s\n" row.event_type;
  printf "Message: %s\n" (Option.value row.message ~default:"none");
  [%expect {|
    Event type: error
    Message: Connection failed
  |}]

let%expect_test "parquet export row to_csv" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.5; fill_price = 50000.0; fee = 2.5; is_maker = Some true })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  let csv = Bot.Parquet_export.Row.to_csv row in
  printf "CSV contains order: %b\n" (String.is_substring csv ~substring:"order");
  printf "CSV contains order_filled: %b\n" (String.is_substring csv ~substring:"order_filled");
  printf "CSV contains Gemini: %b\n" (String.is_substring csv ~substring:"Gemini");
  [%expect {|
    CSV contains order: true
    CSV contains order_filled: true
    CSV contains Gemini: true
  |}]

let%expect_test "parquet export row to_json" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.5; fill_price = 50000.0; fee = 2.5; is_maker = Some true })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  let json = Bot.Parquet_export.Row.to_json row in
  printf "JSON starts with {: %b\n" (String.is_prefix json ~prefix:"{");
  printf "JSON ends with }: %b\n" (String.is_suffix json ~suffix:"}");
  printf "JSON contains category: %b\n" (String.is_substring json ~substring:"\"category\"");
  [%expect {|
    JSON starts with {: true
    JSON ends with }: true
    JSON contains category: true
  |}]

let%expect_test "parquet export csv_header" =
  let header = Bot.Parquet_export.Row.csv_header in
  printf "Header contains event_id: %b\n" (String.is_substring header ~substring:"event_id");
  printf "Header contains timestamp: %b\n" (String.is_substring header ~substring:"timestamp");
  printf "Header contains category: %b\n" (String.is_substring header ~substring:"category");
  printf "Header contains symbol: %b\n" (String.is_substring header ~substring:"symbol");
  [%expect {|
    Header contains event_id: true
    Header contains timestamp: true
    Header contains category: true
    Header contains symbol: true
  |}]

let%expect_test "parquet export stats" =
  let events = [
    Bot.Event.create_envelope ~sequence:1L
      (Bot.Event.Order (Bot.Event.Order_event.Order_submitted
        { order_id = "ord-1"; symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
        ; side = Bot.Event.Side.Buy; qty = 1.0; price = Some 50000.0
        ; time_in_force = Bot.Event.Time_in_force.GTC }));
    Bot.Event.create_envelope ~sequence:2L
      (Bot.Event.Order (Bot.Event.Order_event.Order_filled
        { order_id = "ord-1"; venue = Bot.Event.Venue.Gemini
        ; fill_qty = 1.0; fill_price = 50000.0; fee = 5.0; is_maker = Some true }));
    Bot.Event.create_envelope ~sequence:3L
      (Bot.Event.Market (Bot.Event.Market_event.Trade
        { symbol = "BTCUSD"; venue = Bot.Event.Venue.Gemini
        ; price = 50100.0; qty = 0.5; side = Some Bot.Event.Side.Buy
        ; trade_id = None }));
  ] in
  let stats = Bot.Parquet_export.Stats.of_events events in
  printf "Total events: %d\n" stats.total_events;
  printf "Total volume: %.4f\n" stats.total_volume;
  printf "Total fees: %.2f\n" stats.total_fees;
  printf "Venues: %d\n" (List.length stats.venues);
  printf "Symbols: %d\n" (List.length stats.symbols);
  [%expect {|
    Total events: 3
    Total volume: 2.5000
    Total fees: 5.00
    Venues: 1
    Symbols: 1
  |}]

(* ============================================================================
   DASHBOARD TESTS
   ============================================================================ *)

let%expect_test "dashboard format_uptime" =
  (* Test time formatting logic *)
  let format_uptime_test span =
    let secs = Time_ns.Span.to_sec span |> Int.of_float in
    let hours = secs / 3600 in
    let mins = (secs mod 3600) / 60 in
    let secs = secs mod 60 in
    match hours > 0 with
    | true -> sprintf "%dh %02dm %02ds" hours mins secs
    | false ->
      match mins > 0 with
      | true -> sprintf "%dm %02ds" mins secs
      | false -> sprintf "%ds" secs
  in
  printf "1 second: %s\n" (format_uptime_test (Time_ns.Span.of_sec 1.0));
  printf "65 seconds: %s\n" (format_uptime_test (Time_ns.Span.of_sec 65.0));
  printf "3661 seconds: %s\n" (format_uptime_test (Time_ns.Span.of_sec 3661.0));
  [%expect {|
    1 second: 1s
    65 seconds: 1m 05s
    3661 seconds: 1h 01m 01s
  |}]

let%expect_test "dashboard text_summary" =
  let state = Bot.State.empty ~bot_id:"test-bot" in
  let start_event = Bot.Event.System (Bot.Event.System_event.Bot_started
    { bot_id = "test-bot"; config_json = "{}"; version = "1.0.0" })
  in
  let state = Bot.State.apply_event state (Bot.Event.create_envelope ~sequence:1L start_event) in
  let summary = Bot.Dashboard.text_summary state in
  printf "Summary contains test-bot: %b\n" (String.is_substring summary ~substring:"test-bot");
  printf "Summary contains RUNNING: %b\n" (String.is_substring summary ~substring:"RUNNING");
  printf "Summary contains P&L: %b\n" (String.is_substring summary ~substring:"P&L");
  [%expect {|
    Summary contains test-bot: true
    Summary contains RUNNING: true
    Summary contains P&L: true
  |}]

let%expect_test "dashboard create and config" =
  let config = Bot.Dashboard.Config.default in
  let _dashboard = Bot.Dashboard.create config in
  printf "Max events: %d\n" config.max_events;
  printf "Show positions: %b\n" config.show_positions;
  printf "Show orders: %b\n" config.show_orders;
  printf "Show events: %b\n" config.show_events;
  [%expect {|
    Max events: 10
    Show positions: true
    Show orders: true
    Show events: true
  |}]

let%expect_test "dashboard render_once basic" =
  let state = Bot.State.empty ~bot_id:"render-test" in
  let dashboard = Bot.Dashboard.create Bot.Dashboard.Config.default in
  let output = Bot.Dashboard.render_once dashboard state in
  printf "Output contains render-test: %b\n" (String.is_substring output ~substring:"render-test");
  printf "Output contains escape codes: %b\n" (String.is_substring output ~substring:"\027[");
  [%expect {|
    Output contains render-test: true
    Output contains escape codes: true
  |}]

(* ============================================================================
   ENGINE TESTS
   ============================================================================ *)

let%expect_test "engine config default" =
  let config = Bot.Engine.Config.default in
  printf "Bot ID: %s\n" config.bot_id;
  printf "Symbols: %s\n" (String.concat ~sep:"," config.symbols);
  printf "Max reconnect: %d\n" config.max_reconnect_attempts;
  [%expect {|
    Bot ID: fluxum-bot
    Symbols: BTCUSD
    Max reconnect: 10
  |}]

let%expect_test "engine config to_json" =
  let config = Bot.Engine.Config.default in
  let json = Bot.Engine.Config.to_json config in
  printf "JSON is non-empty: %b\n" (String.length json > 0);
  printf "JSON contains bot_id: %b\n" (String.is_substring json ~substring:"bot_id");
  [%expect {|
    JSON is non-empty: true
    JSON contains bot_id: true
  |}]

let%expect_test "engine strategy wrapper with noop" =
  let strategy = Bot.Engine.Strategy_wrapper.wrap (module Bot.Strategy_intf.Noop) () in
  printf "Strategy name: %s\n" strategy.name;
  printf "Strategy version: %s\n" strategy.version;
  [%expect {|
    Strategy name: noop
    Strategy version: 1.0.0
  |}]

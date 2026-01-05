(** Bot Framework Tests *)

open Core

let%expect_test "event serialization round-trip" =
  let event = Bot.Event.System (Bot.Event.System_event.Bot_started
    { bot_id = "test-bot"
    ; config_json = "{}"
    ; version = "1.0.0"
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let serialized = Bot.Event.serialize envelope in
  let deserialized = Bot.Event.deserialize serialized in
  (match deserialized with
  | Ok env ->
    printf "Round-trip successful\n";
    printf "Sequence: %Ld\n" env.Bot.Event.sequence;
    printf "Category: %s\n" (Bot.Event.category env.event)
  | Error e ->
    printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {|
    Round-trip successful
    Sequence: 1
    Category: system
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
  printf "Bot ID: %s\n" state.bot_id;
  printf "Is Running: %b\n" state.is_running;
  printf "Version: %s\n" state.version;
  printf "Event Count: %d\n" state.event_count;
  [%expect {|
    Bot ID: test-bot
    Is Running: true
    Version: 1.0.0
    Event Count: 1
  |}]

let%expect_test "state apply_event - order submitted and filled" =
  let state = Bot.State.empty ~bot_id:"test-bot" in

  (* Submit order *)
  let submit_event = Bot.Event.Order (Bot.Event.Order_event.Order_submitted
    { order_id = "order-1"
    ; symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; side = Bot.Event.Side.Buy
    ; qty = 0.1
    ; price = Some 50000.0
    ; time_in_force = Bot.Event.Time_in_force.GTC
    })
  in
  let envelope1 = Bot.Event.create_envelope ~sequence:1L submit_event in
  let state = Bot.State.apply_event state envelope1 in

  printf "Active orders after submit: %d\n" (Map.length state.active_orders);

  (* Fill order *)
  let fill_event = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "order-1"
    ; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.1
    ; fill_price = 50000.0
    ; fee = 5.0
    ; is_maker = Some true
    })
  in
  let envelope2 = Bot.Event.create_envelope ~sequence:2L fill_event in
  let state = Bot.State.apply_event state envelope2 in

  printf "Active orders after fill: %d\n" (Map.length state.active_orders);
  printf "Event Count: %d\n" state.event_count;
  [%expect {|
    Active orders after submit: 1
    Active orders after fill: 0
    Event Count: 2
  |}]

let%expect_test "strategy signal generation" =
  let signal = Bot.Strategy_intf.Signal.buy_limit
    ~symbol:"BTCUSD"
    ~venue:Bot.Event.Venue.Gemini
    ~qty:0.1
    ~price:50000.0
  in
  (match signal with
  | Bot.Strategy_intf.Signal.Place_order req ->
    printf "Signal: Place Order\n";
    printf "Symbol: %s\n" req.symbol;
    printf "Side: %s\n" (Bot.Event.Side.to_string req.side);
    printf "Qty: %.4f\n" req.qty;
    printf "Price: %s\n" (Option.value_map req.price ~default:"MKT" ~f:(sprintf "%.2f"))
  | _ ->
    printf "Unexpected signal type\n");
  [%expect {|
    Signal: Place Order
    Symbol: BTCUSD
    Side: buy
    Qty: 0.1000
    Price: 50000.00
  |}]

let%expect_test "book snapshot mid price" =
  let book : Bot.Strategy_intf.Book_snapshot.t =
    { symbol = "BTCUSD"
    ; venue = Bot.Event.Venue.Gemini
    ; bids = [{ price = 49999.0; qty = 1.0 }; { price = 49998.0; qty = 2.0 }]
    ; asks = [{ price = 50001.0; qty = 1.0 }; { price = 50002.0; qty = 2.0 }]
    ; timestamp = Bot.Event.Time.now ()
    }
  in
  (match Bot.Strategy_intf.Book_snapshot.mid_price book with
  | Some mid -> printf "Mid price: %.2f\n" mid
  | None -> printf "No mid price\n");
  (match Bot.Strategy_intf.Book_snapshot.spread book with
  | Some spread -> printf "Spread: %.2f\n" spread
  | None -> printf "No spread\n");
  (match Bot.Strategy_intf.Book_snapshot.spread_bps book with
  | Some bps -> printf "Spread (bps): %.2f\n" bps
  | None -> printf "No spread bps\n");
  [%expect {|
    Mid price: 50000.00
    Spread: 2.00
    Spread (bps): 0.40
  |}]

let%expect_test "parquet export row conversion" =
  let event = Bot.Event.Order (Bot.Event.Order_event.Order_filled
    { order_id = "order-1"
    ; venue = Bot.Event.Venue.Gemini
    ; fill_qty = 0.1
    ; fill_price = 50000.0
    ; fee = 5.0
    ; is_maker = Some true
    })
  in
  let envelope = Bot.Event.create_envelope ~sequence:1L event in
  let row = Bot.Parquet_export.Row.of_envelope envelope in
  printf "Category: %s\n" row.category;
  printf "Event type: %s\n" row.event_type;
  printf "Order ID: %s\n" (Option.value row.order_id ~default:"none");
  printf "Qty: %s\n" (Option.value_map row.qty ~default:"none" ~f:(sprintf "%.4f"));
  printf "Price: %s\n" (Option.value_map row.price ~default:"none" ~f:(sprintf "%.2f"));
  printf "Fee: %s\n" (Option.value_map row.fee ~default:"none" ~f:(sprintf "%.2f"));
  [%expect {|
    Category: order
    Event type: order_filled
    Order ID: order-1
    Qty: 0.1000
    Price: 50000.00
    Fee: 5.00
  |}]

let%expect_test "unified ledger apply fill" =
  let ledger = Bot.Ledger.create () in
  let ledger, entry = Bot.Ledger.apply_fill ledger
    ~symbol:"BTCUSD"
    ~venue:Bot.Entry.Venue.Gemini
    ~price:50000.0
    ~qty:0.1
    ~side:Fluxum.Types.Side.Buy
    ~fee:5.0
  in
  printf "Position: %.4f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Fees: %.2f\n" entry.total_fees;
  printf "Trade Count: %d\n" entry.trade_count;
  printf "Total P&L: %.2f\n" (Bot.Ledger.total_pnl ledger);
  [%expect {|
    Position: 0.1000
    Avg Cost: 50000.00
    Fees: 5.00
    Trade Count: 1
    Total P&L: 0.00
    |}]

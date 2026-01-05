# Bot Framework Guide

Comprehensive guide to building trading bots with Fluxum's bot framework.

## Table of Contents

1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [Event System](#event-system)
4. [State Management](#state-management)
5. [Strategy Interface](#strategy-interface)
6. [Unified Ledger](#unified-ledger)
7. [Dashboard](#dashboard)
8. [Python Export](#python-export)
9. [CLI Commands](#cli-commands)
10. [Complete Examples](#complete-examples)

## Overview

The bot framework provides infrastructure for building trading bots with:

- **Event Sourcing**: All state changes are captured as events, enabling deterministic replay
- **Persistent Storage**: Binary event logs with bin_prot serialization
- **Python Interoperability**: Export to CSV/JSON for analysis with pandas
- **Real-time Dashboard**: TUI for monitoring bot status
- **Strategy Interface**: Clean separation between strategy logic and infrastructure

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Bot Engine                           │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────────────┐│
│  │ Events  │→ │  State  │→ │ Ledger  │→ │    Dashboard    ││
│  │ Store   │  │ Machine │  │  (P&L)  │  │      (TUI)      ││
│  └─────────┘  └─────────┘  └─────────┘  └─────────────────┘│
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────────┐  ┌────────────────────────────────┐│
│  │      Strategy       │→ │         Signal Executor        ││
│  │  (Your Logic Here)  │  │    (Place/Cancel Orders)       ││
│  └─────────────────────┘  └────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                   Exchange Connections                       │
│   Gemini │ Kraken │ Binance │ Coinbase │ Hyperliquid │ ...  │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### Basic Bot Setup

```ocaml
open Core
open Async
open Bot

let run () =
  (* 1. Create engine configuration *)
  let config = Engine.Config.{
    default with
    bot_id = "my-trading-bot";
    symbols = ["BTCUSD"; "ETHUSD"];
    venues = [Event.Venue.Gemini; Event.Venue.Kraken];
    event_store_path = "./bot_events";
    tick_interval = Time_float.Span.of_sec 1.0;
    enable_dashboard = true;
  } in

  (* 2. Create the engine *)
  let%bind engine = Engine.create config in

  (* 3. Optional: Set a strategy *)
  let strategy = Engine.Strategy_wrapper.wrap
    (module Strategy_intf.Noop) ()
  in
  Engine.set_strategy engine strategy;

  (* 4. Start the bot *)
  let%bind result = Engine.start engine in
  (match result with
  | Ok () -> printf "Bot started successfully\n"
  | Error e -> printf "Failed to start: %s\n" (Error.to_string_hum e));

  (* 5. Feed market data (typically from WebSocket) *)
  let%bind () = Engine.on_book_update engine
    ~symbol:"BTCUSD"
    ~venue:Event.Venue.Gemini
    ~bids:[{ Event.Price_level.price = 67000.; qty = 1.0 }]
    ~asks:[{ Event.Price_level.price = 67001.; qty = 1.0 }]
    ~is_snapshot:true
  in

  (* 6. Run until stopped *)
  let%bind () = Clock.after (Time_float.Span.of_hr 1.0) in
  Engine.stop engine ~reason:"Manual stop"
```

### Minimal Bot with Dashboard

```ocaml
open Core
open Async
open Bot

let run_with_dashboard () =
  let config = Engine.Config.{
    default with
    bot_id = "dashboard-demo";
    enable_dashboard = true;
  } in

  let%bind engine = Engine.create config in
  let%bind _ = Engine.start engine in

  (* Create and run dashboard *)
  let dashboard = Dashboard.create Dashboard.Config.default in
  Dashboard.run_with_engine dashboard engine
```

## Event System

### Event Types

The bot framework captures 5 categories of events:

```ocaml
type t =
  | Market of Market_event.t     (* Book updates, trades, tickers *)
  | Order of Order_event.t       (* Order lifecycle events *)
  | Balance of Balance_event.t   (* Balance changes *)
  | System of System_event.t     (* Bot lifecycle, connections *)
  | Strategy of Strategy_event.t (* Signals, position changes *)
```

### Market Events

```ocaml
(* Book update event *)
let book_update = Event.Market (Event.Market_event.Book_update {
  symbol = "BTCUSD";
  venue = Event.Venue.Gemini;
  bids = [
    { Event.Price_level.price = 67000.0; qty = 1.5 };
    { price = 66999.0; qty = 2.0 };
    { price = 66998.0; qty = 0.5 };
  ];
  asks = [
    { price = 67001.0; qty = 1.0 };
    { price = 67002.0; qty = 3.0 };
  ];
  is_snapshot = true;
})

(* Trade event *)
let trade = Event.Market (Event.Market_event.Trade {
  symbol = "BTCUSD";
  venue = Event.Venue.Gemini;
  price = 67000.50;
  qty = 0.1;
  side = Some Event.Side.Buy;
  trade_id = Some "trade-123456";
})

(* Ticker event *)
let ticker = Event.Market (Event.Market_event.Ticker {
  symbol = "BTCUSD";
  venue = Event.Venue.Gemini;
  bid = 67000.0;
  ask = 67001.0;
  last = 67000.50;
  volume_24h = Some 1234.5;
})
```

### Order Events

```ocaml
(* Submit order *)
let order_submitted = Event.Order (Event.Order_event.Order_submitted {
  order_id = "my-order-001";
  symbol = "BTCUSD";
  venue = Event.Venue.Gemini;
  side = Event.Side.Buy;
  qty = 0.1;
  price = Some 66500.0;  (* None for market orders *)
  time_in_force = Event.Time_in_force.GTC;
})

(* Order accepted by exchange *)
let order_accepted = Event.Order (Event.Order_event.Order_accepted {
  order_id = "my-order-001";
  exchange_id = "exchange-abc123";
  venue = Event.Venue.Gemini;
})

(* Order filled *)
let order_filled = Event.Order (Event.Order_event.Order_filled {
  order_id = "my-order-001";
  venue = Event.Venue.Gemini;
  fill_qty = 0.1;
  fill_price = 66498.50;
  fee = 0.05;
  is_maker = Some true;
})

(* Partial fill *)
let partial_fill = Event.Order (Event.Order_event.Order_partially_filled {
  order_id = "my-order-002";
  venue = Event.Venue.Kraken;
  fill_qty = 0.05;
  fill_price = 66500.0;
  remaining_qty = 0.05;
  fee = 0.025;
})

(* Order cancelled *)
let order_cancelled = Event.Order (Event.Order_event.Order_cancelled {
  order_id = "my-order-003";
  venue = Event.Venue.Gemini;
  reason = "User requested";
})

(* Order rejected *)
let order_rejected = Event.Order (Event.Order_event.Order_rejected {
  order_id = "my-order-004";
  venue = Event.Venue.Gemini;
  reason = "Insufficient funds";
})
```

### System Events

```ocaml
(* Bot started *)
let bot_started = Event.System (Event.System_event.Bot_started {
  bot_id = "my-bot";
  config_json = "{\"symbols\": [\"BTCUSD\"]}";
  version = "1.0.0";
})

(* Connection state change *)
let connection_change = Event.System (Event.System_event.Connection_state_changed {
  venue = Event.Venue.Gemini;
  old_state = Event.Connection_state.Connecting;
  new_state = Event.Connection_state.Ready;
})

(* Error event *)
let error_event = Event.System (Event.System_event.Error {
  venue = Some Event.Venue.Gemini;
  message = "WebSocket disconnected unexpectedly";
  is_fatal = false;
})

(* Heartbeat *)
let heartbeat = Event.System (Event.System_event.Heartbeat {
  sequence = 12345L;
})
```

### Event Serialization

Events use bin_prot for efficient binary serialization:

```ocaml
(* Serialize event to binary *)
let serialize_example () =
  let event = Event.Market (Event.Market_event.Trade {
    symbol = "BTCUSD";
    venue = Event.Venue.Gemini;
    price = 67000.0;
    qty = 0.1;
    side = Some Event.Side.Buy;
    trade_id = None;
  }) in
  let envelope = Event.create_envelope event in
  let binary = Event.serialize envelope in
  printf "Serialized to %d bytes\n" (String.length binary);

  (* Deserialize back *)
  match Event.deserialize binary with
  | Ok restored -> printf "Restored: %s\n" (Event.describe restored.event)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e)
```

## State Management

### Event Sourcing Pattern

All state is reconstructible from the event log:

```ocaml
(* Create empty state *)
let state = State.empty ~bot_id:"my-bot"

(* Apply events to rebuild state *)
let apply_events () =
  let events = [
    Event.create_envelope (Event.System (Event.System_event.Bot_started {
      bot_id = "my-bot"; config_json = "{}"; version = "1.0.0"
    }));
    Event.create_envelope (Event.Order (Event.Order_event.Order_submitted {
      order_id = "order-1"; symbol = "BTCUSD"; venue = Event.Venue.Gemini;
      side = Event.Side.Buy; qty = 0.1; price = Some 66000.0;
      time_in_force = Event.Time_in_force.GTC
    }));
    Event.create_envelope (Event.Order (Event.Order_event.Order_filled {
      order_id = "order-1"; venue = Event.Venue.Gemini;
      fill_qty = 0.1; fill_price = 66000.0; fee = 0.05; is_maker = None
    }));
  ] in

  (* Reconstruct state from events *)
  let final_state = State.reconstruct ~events ~bot_id:"my-bot" in

  printf "Bot running: %b\n" final_state.is_running;
  printf "Event count: %d\n" final_state.event_count;
  printf "Active orders: %d\n" (Map.length final_state.active_orders);
  printf "Total P&L: %.2f\n" (Unified_ledger.Ledger.total_pnl final_state.ledger)
```

### State Queries

```ocaml
let query_state state =
  (* Check if running *)
  printf "Running: %b\n" state.State.is_running;

  (* Get uptime *)
  let uptime = State.uptime state in
  printf "Uptime: %.0f seconds\n" (Time_ns.Span.to_sec uptime);

  (* Get connected venues *)
  let connected = State.connected_venues state in
  List.iter connected ~f:(fun venue ->
    printf "Connected to: %s\n" (Event.Venue.to_string venue)
  );

  (* Get orders for a symbol *)
  let btc_orders = State.orders_for_symbol state ~symbol:"BTCUSD" in
  printf "BTC orders: %d\n" (List.length btc_orders);

  (* Get balance *)
  match State.balance state ~venue:Event.Venue.Gemini ~currency:"USD" with
  | Some bal -> printf "USD balance: %.2f\n" bal.State.Balance.available
  | None -> printf "No USD balance found\n";

  (* Summary *)
  printf "\n%s\n" (State.summary state)
```

### Replaying from Event Store

```ocaml
let replay_bot () =
  (* Replay all events from storage to rebuild state *)
  let%bind state = Engine.replay
    ~base_path:"./bot_events"
    ~bot_id:"my-bot"
  in
  printf "Replayed state:\n%s\n" (State.summary state);
  return state
```

## Strategy Interface

### Strategy Module Signature

```ocaml
module type S = sig
  type config     (* Strategy configuration *)
  type state      (* Strategy internal state *)

  val name : string
  val version : string
  val default_config : config
  val init : config -> state

  (* Event handlers - return signals and new state *)
  val on_book_update : state -> book:Book_snapshot.t -> context:Context.t
                       -> Signal.t list * state
  val on_trade : state -> symbol:string -> venue:Event.Venue.t
                 -> price:float -> qty:float -> side:Event.Side.t option
                 -> context:Context.t -> Signal.t list * state
  val on_fill : state -> order_id:string -> symbol:string -> venue:Event.Venue.t
                -> side:Event.Side.t -> fill_qty:float -> fill_price:float
                -> context:Context.t -> Signal.t list * state
  val on_tick : state -> time:Event.Time.t -> context:Context.t
                -> Signal.t list * state
  val on_start : state -> context:Context.t -> Signal.t list * state
  val on_stop : state -> context:Context.t -> Signal.t list * state
end
```

### Signal Types

```ocaml
(* Market order *)
let buy_market_signal =
  Strategy_intf.Signal.buy_market
    ~symbol:"BTCUSD"
    ~venue:Event.Venue.Gemini
    ~qty:0.1

let sell_market_signal =
  Strategy_intf.Signal.sell_market
    ~symbol:"BTCUSD"
    ~venue:Event.Venue.Gemini
    ~qty:0.1

(* Limit order *)
let buy_limit_signal =
  Strategy_intf.Signal.buy_limit
    ~symbol:"BTCUSD"
    ~venue:Event.Venue.Gemini
    ~qty:0.1
    ~price:66000.0

let sell_limit_signal =
  Strategy_intf.Signal.sell_limit
    ~symbol:"BTCUSD"
    ~venue:Event.Venue.Gemini
    ~qty:0.1
    ~price:68000.0

(* Cancel orders *)
let cancel_signal =
  Strategy_intf.Signal.cancel ~order_id:"order-123"

let cancel_all_signal =
  Strategy_intf.Signal.cancel_all ~symbol:"BTCUSD" ()
```

### Book Snapshot Helpers

```ocaml
let analyze_book (book : Strategy_intf.Book_snapshot.t) =
  (* Get best prices *)
  let best_bid = Strategy_intf.Book_snapshot.best_bid book in
  let best_ask = Strategy_intf.Book_snapshot.best_ask book in

  (* Calculate mid price *)
  match Strategy_intf.Book_snapshot.mid_price book with
  | Some mid ->
    printf "Mid price: %.2f\n" mid;

    (* Calculate spread *)
    (match Strategy_intf.Book_snapshot.spread book with
    | Some spread -> printf "Spread: %.2f\n" spread
    | None -> ());

    (* Spread in basis points *)
    (match Strategy_intf.Book_snapshot.spread_bps book with
    | Some bps -> printf "Spread: %.1f bps\n" bps
    | None -> ())

  | None ->
    printf "No mid price available (empty book)\n"
```

### Context Helpers

```ocaml
let use_context (ctx : Strategy_intf.Context.t) =
  (* Check position *)
  let btc_position = Strategy_intf.Context.position ctx ~symbol:"BTCUSD" in
  printf "BTC position: %.8f\n" btc_position;

  (* Check if we have a position *)
  if Strategy_intf.Context.has_position ctx ~symbol:"BTCUSD" then
    printf "We have a BTC position\n";

  (* Check balance *)
  let usd_balance = Strategy_intf.Context.balance ctx ~currency:"USD" in
  printf "USD available: %.2f\n" usd_balance;

  (* Get orders for symbol *)
  let btc_orders = Strategy_intf.Context.orders_for_symbol ctx ~symbol:"BTCUSD" in
  printf "Active BTC orders: %d\n" (List.length btc_orders);

  (* P&L info *)
  printf "Total P&L: %.2f\n" ctx.total_pnl;
  printf "Realized: %.2f\n" ctx.realized_pnl;
  printf "Unrealized: %.2f\n" ctx.unrealized_pnl
```

### Example: Momentum Strategy

```ocaml
module Momentum_strategy : Strategy_intf.S = struct
  type config = {
    symbol : string;
    venue : Event.Venue.t;
    lookback : int;
    threshold_bps : float;
    order_size : float;
  }

  type state = {
    prices : float list;
    last_signal : [`Long | `Short | `None];
  }

  let name = "momentum"
  let version = "1.0.0"

  let default_config = {
    symbol = "BTCUSD";
    venue = Event.Venue.Gemini;
    lookback = 10;
    threshold_bps = 5.0;
    order_size = 0.01;
  }

  let init _ = { prices = []; last_signal = `None }

  let on_book_update state ~book ~context =
    let cfg = default_config in

    (* Only process our symbol *)
    match String.equal book.Strategy_intf.Book_snapshot.symbol cfg.symbol with
    | false -> ([], state)
    | true ->
      match Strategy_intf.Book_snapshot.mid_price book with
      | None -> ([], state)
      | Some price ->
        (* Update price history *)
        let prices = price :: List.take state.prices (cfg.lookback - 1) in
        let state = { state with prices } in

        (* Need enough history *)
        match List.length prices >= cfg.lookback with
        | false -> ([], state)
        | true ->
          let oldest = List.last_exn prices in
          let momentum_bps = (price -. oldest) /. oldest *. 10000. in

          (* Generate signals based on momentum *)
          let signals, new_signal =
            match Float.(momentum_bps > cfg.threshold_bps), state.last_signal with
            | true, `Long -> ([], `Long)  (* Already long *)
            | true, _ ->
              (* Go long *)
              let position = Strategy_intf.Context.position context ~symbol:cfg.symbol in
              match Float.(position < 0.) with
              | true ->
                (* Close short first *)
                ([Strategy_intf.Signal.buy_market
                    ~symbol:cfg.symbol ~venue:cfg.venue
                    ~qty:(Float.abs position +. cfg.order_size)], `Long)
              | false ->
                ([Strategy_intf.Signal.buy_market
                    ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size], `Long)
            | false, _ when Float.(momentum_bps < Float.neg cfg.threshold_bps) ->
              (match state.last_signal with
              | `Short -> ([], `Short)  (* Already short *)
              | _ ->
                let position = Strategy_intf.Context.position context ~symbol:cfg.symbol in
                match Float.(position > 0.) with
                | true ->
                  ([Strategy_intf.Signal.sell_market
                      ~symbol:cfg.symbol ~venue:cfg.venue
                      ~qty:(position +. cfg.order_size)], `Short)
                | false ->
                  ([Strategy_intf.Signal.sell_market
                      ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size], `Short))
            | _ -> ([], state.last_signal)
          in
          (signals, { state with last_signal = new_signal })

  let on_trade state ~symbol:_ ~venue:_ ~price:_ ~qty:_ ~side:_ ~context:_ =
    ([], state)

  let on_fill state ~order_id:_ ~symbol:_ ~venue:_ ~side:_ ~fill_qty:_ ~fill_price:_ ~context:_ =
    ([], state)

  let on_tick state ~time:_ ~context:_ =
    ([], state)

  let on_start state ~context:_ =
    ([], state)

  let on_stop state ~context =
    (* Close any position on stop *)
    let cfg = default_config in
    let position = Strategy_intf.Context.position context ~symbol:cfg.symbol in
    match Float.(abs position > 0.00001) with
    | false -> ([], state)
    | true ->
      match Float.(position > 0.) with
      | true ->
        ([Strategy_intf.Signal.sell_market
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:position], state)
      | false ->
        ([Strategy_intf.Signal.buy_market
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:(Float.abs position)], state)
end
```

### Example: Simple Market Maker

```ocaml
module Simple_mm : Strategy_intf.S = struct
  type config = {
    symbol : string;
    venue : Event.Venue.t;
    spread_bps : float;
    order_size : float;
    max_position : float;
  }

  type state = {
    last_mid : float option;
    bid_order : string option;
    ask_order : string option;
  }

  let name = "simple-mm"
  let version = "1.0.0"

  let default_config = {
    symbol = "BTCUSD";
    venue = Event.Venue.Gemini;
    spread_bps = 10.0;
    order_size = 0.001;
    max_position = 0.01;
  }

  let init _ = { last_mid = None; bid_order = None; ask_order = None }

  let on_book_update state ~book ~context =
    let cfg = default_config in
    match String.equal book.Strategy_intf.Book_snapshot.symbol cfg.symbol with
    | false -> ([], state)
    | true ->
      match Strategy_intf.Book_snapshot.mid_price book with
      | None -> ([], state)
      | Some mid ->
        let position = Strategy_intf.Context.position context ~symbol:cfg.symbol in
        let half_spread = mid *. cfg.spread_bps /. 10000. /. 2. in
        let bid_price = mid -. half_spread in
        let ask_price = mid +. half_spread in

        let signals = ref [] in

        (* Cancel if price moved significantly *)
        (match state.last_mid with
        | Some last when Float.(abs (mid -. last) > half_spread /. 2.) ->
          Option.iter state.bid_order ~f:(fun id ->
            signals := Strategy_intf.Signal.cancel ~order_id:id :: !signals);
          Option.iter state.ask_order ~f:(fun id ->
            signals := Strategy_intf.Signal.cancel ~order_id:id :: !signals)
        | _ -> ());

        (* Place bid if not too long *)
        (match Float.(position < cfg.max_position) with
        | true ->
          signals := Strategy_intf.Signal.buy_limit
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size ~price:bid_price
            :: !signals
        | false -> ());

        (* Place ask if not too short *)
        (match Float.(position > Float.neg cfg.max_position) with
        | true ->
          signals := Strategy_intf.Signal.sell_limit
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size ~price:ask_price
            :: !signals
        | false -> ());

        (!signals, { state with last_mid = Some mid })

  let on_trade state ~symbol:_ ~venue:_ ~price:_ ~qty:_ ~side:_ ~context:_ = ([], state)

  let on_fill state ~order_id ~symbol:_ ~venue:_ ~side:_ ~fill_qty:_ ~fill_price:_ ~context:_ =
    let bid_order = match state.bid_order with
      | Some id when String.equal id order_id -> None
      | x -> x
    in
    let ask_order = match state.ask_order with
      | Some id when String.equal id order_id -> None
      | x -> x
    in
    ([], { state with bid_order; ask_order })

  let on_tick state ~time:_ ~context:_ = ([], state)
  let on_start state ~context:_ = ([], state)

  let on_stop state ~context:_ =
    (* Cancel all orders on stop *)
    ([Strategy_intf.Signal.cancel_all ~symbol:default_config.symbol ()], state)
end
```

### Using Stateless Strategy Helper

```ocaml
(* For simple strategies without internal state *)
module Echo_spread = Strategy_intf.Make_stateless(struct
  let name = "echo-spread"
  let version = "1.0.0"

  let on_book book ~context:_ =
    match Strategy_intf.Book_snapshot.spread_bps book with
    | Some bps ->
      printf "Spread on %s: %.1f bps\n" book.symbol bps;
      []  (* No signals *)
    | None -> []
end)
```

## Unified Ledger

### Entry Operations

```ocaml
(* Create empty entry *)
let entry = Unified_ledger.Entry.empty
  ~symbol:"BTCUSD"
  ~venue:Unified_ledger.Entry.Venue.Gemini

(* Apply a buy fill *)
let entry = Unified_ledger.Entry.apply_fill entry
  ~price:50000.0
  ~qty:1.0
  ~side:Unified_ledger.Entry.Side.Buy
  ~fee:5.0

(* Check position and P&L *)
let () =
  printf "Position: %.8f\n" entry.position;
  printf "Avg Cost: %.2f\n" entry.avg_cost;
  printf "Realized P&L: %.2f\n" entry.realized_pnl;
  printf "Total Fees: %.2f\n" entry.total_fees

(* Mark to market *)
let entry = Unified_ledger.Entry.mark_to_market entry ~price:51000.0

let () =
  printf "Mark Price: %.2f\n" entry.mark_price;
  printf "Unrealized P&L: %.2f\n" entry.unrealized_pnl;
  printf "Total P&L: %.2f\n" entry.total_pnl

(* Close position with sell *)
let entry = Unified_ledger.Entry.apply_fill entry
  ~price:51000.0
  ~qty:1.0
  ~side:Unified_ledger.Entry.Side.Sell
  ~fee:5.0

(* P&L after closing *)
let () =
  printf "Final position: %.8f\n" entry.position;
  printf "Realized P&L: %.2f\n" entry.realized_pnl;
  printf "Total fees: %.2f\n" entry.total_fees;
  printf "\nDetailed report:\n%s" (Unified_ledger.Entry.report entry)
```

### Ledger Operations

```ocaml
(* Create ledger *)
let ledger = Unified_ledger.Ledger.create ()

(* Apply fills across multiple symbols/venues *)
let ledger, entry1 = Unified_ledger.Ledger.apply_fill ledger
  ~symbol:"BTCUSD" ~venue:Unified_ledger.Entry.Venue.Gemini
  ~price:50000.0 ~qty:1.0 ~side:Fluxum.Types.Side.Buy ~fee:5.0

let ledger, entry2 = Unified_ledger.Ledger.apply_fill ledger
  ~symbol:"ETHUSD" ~venue:Unified_ledger.Entry.Venue.Kraken
  ~price:3000.0 ~qty:10.0 ~side:Fluxum.Types.Side.Buy ~fee:3.0

(* Mark all to market *)
let ledger = Unified_ledger.Ledger.mark_to_market ledger
  ~prices:[("BTCUSD", 52000.0); ("ETHUSD", 3200.0)]

(* Get aggregate P&L *)
let () =
  printf "Total P&L: %.2f\n" (Unified_ledger.Ledger.total_pnl ledger);
  printf "Total Realized: %.2f\n" (Unified_ledger.Ledger.total_realized ledger);
  printf "Total Unrealized: %.2f\n" (Unified_ledger.Ledger.total_unrealized ledger);
  printf "Total Fees: %.2f\n" (Unified_ledger.Ledger.total_fees ledger);
  printf "Total Volume: %.2f\n" (Unified_ledger.Ledger.total_volume ledger)

(* Get all entries *)
let entries = Unified_ledger.Ledger.all_entries ledger
let () = List.iter entries ~f:(fun entry ->
  printf "%s\n" (Unified_ledger.Entry.summary entry)
)

(* Get specific entry *)
match Unified_ledger.Ledger.get_entry ledger ~symbol:"BTCUSD" with
| Some entry -> printf "BTC: %s\n" (Unified_ledger.Entry.summary entry)
| None -> printf "No BTC entry\n"
```

## Dashboard

### Dashboard Configuration

```ocaml
let dashboard_config = Dashboard.Config.{
  refresh_rate = Time_float.Span.of_ms 250.;
  max_events = 10;
  show_positions = true;
  show_orders = true;
  show_events = true;
  compact_mode = false;
}
```

### Running the Dashboard

```ocaml
(* With engine *)
let run_dashboard_with_engine () =
  let%bind engine = Engine.create Engine.Config.default in
  let%bind _ = Engine.start engine in

  let dashboard = Dashboard.create Dashboard.Config.default in
  Dashboard.run_with_engine dashboard engine

(* With state pipe *)
let run_dashboard_with_pipe () =
  let reader, writer = Pipe.create () in
  let dashboard = Dashboard.create Dashboard.Config.default in

  (* Feed state updates *)
  don't_wait_for (
    let rec loop () =
      let state = (* get state somehow *) State.empty ~bot_id:"test" in
      let%bind () = Pipe.write writer state in
      let%bind () = Clock.after (Time_float.Span.of_ms 250.) in
      loop ()
    in
    loop ()
  );

  Dashboard.run dashboard ~state_reader:reader

(* Get text summary (no ANSI codes) *)
let print_summary state =
  print_string (Dashboard.text_summary state)

(* Render once without loop *)
let render_snapshot () =
  let state = State.empty ~bot_id:"snapshot-demo" in
  let dashboard = Dashboard.create Dashboard.Config.default in
  let output = Dashboard.render_once dashboard state in
  print_string output
```

### Dashboard Output Example

```
┌────────────────────────────────────────────────────────────────────────────────┐
│ Bot: my-bot  │  Status: ● RUNNING  │  Uptime: 1h 23m 45s                       │
├────────────────────────────────────────────────────────────────────────────────┤
│ Connections: Gemini: ● Ready  │  Kraken: ● Ready                               │
├────────────────────────────────────────────────────────────────────────────────┤
│ Positions                                                                       │
│   BTCUSD: +0.50000000 @ 67234.50  P&L: +$1,234.56                               │
│   ETHUSD: -2.00000000 @ 3456.78  P&L: -$123.45                                  │
│ P&L: Total: +$1,111.11  │  Realized: +$500.00  │  Unrealized: +$611.11  │ Fees: 12.50 │
├────────────────────────────────────────────────────────────────────────────────┤
│ Active Orders (2)                                                               │
│   #abc12345 BUY  0.10000000 BTCUSD @ 67000.00 [GTC]                             │
│   #def67890 SELL 1.00000000 ETHUSD @ 3500.00 [IOC]                              │
├────────────────────────────────────────────────────────────────────────────────┤
│ Recent Events                                                                   │
│   09:15:23.456 fill abc12345 0.05000000@67001.00                                │
│   09:15:20.123 book_upd BTCUSD@Gemini                                           │
│   09:15:18.789 submit buy BTCUSD 0.10000000@67000.00 [abc12345]                 │
└────────────────────────────────────────────────────────────────────────────────┘
Last update: 2025-01-05 09:15:23  │  Events: 1234  │  Press Ctrl+C to stop
```

## Python Export

### Export to CSV

```ocaml
let export_to_csv () =
  (* Read events from store *)
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  (* Export to CSV *)
  let%bind () = Parquet_export.export_csv
    ~events
    ~output_path:"./exports/events.csv"
    ~include_header:true
  in

  printf "Exported %d events to CSV\n" (List.length events);
  return ()
```

### Export to JSON Lines

```ocaml
let export_to_jsonl () =
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  let%bind () = Parquet_export.export_jsonl
    ~events
    ~output_path:"./exports/events.jsonl"
  in
  return ()
```

### Export Orders Only

```ocaml
let export_orders_only () =
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  let%bind () = Parquet_export.export_orders
    ~events
    ~output_path:"./exports/orders.csv"
  in
  return ()
```

### Export Trades Only

```ocaml
let export_trades_only () =
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  let%bind () = Parquet_export.export_trades
    ~events
    ~output_path:"./exports/trades.csv"
  in
  return ()
```

### Streaming Export

```ocaml
let streaming_export () =
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind event_pipe = Event_store.Reader.stream reader in

  let%bind () = Parquet_export.streaming_export_csv
    ~events:event_pipe
    ~output_path:"./exports/stream.csv"
    ~include_header:true
  in
  Event_store.Reader.close reader
```

### Export Statistics

```ocaml
let print_stats () =
  let%bind reader = Event_store.Reader.open_ ~path:"./bot_events/my-bot" in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  let stats = Parquet_export.Stats.of_events events in
  printf "%s\n" (Parquet_export.Stats.to_string stats);
  return ()
```

### Generate Python Helper

```ocaml
let generate_helper () =
  Parquet_export.write_python_helper ~output_dir:"./exports"
```

### Using Exported Data in Python

```python
# load_events.py - Generated by Fluxum
import pandas as pd

def load_events(path: str) -> pd.DataFrame:
    """Load events from CSV export."""
    return pd.read_csv(path, parse_dates=['timestamp'])

def load_events_jsonl(path: str) -> pd.DataFrame:
    """Load events from JSON Lines export."""
    return pd.read_json(path, lines=True)

# Example usage
df = load_events('./exports/events.csv')

# Filter to trades
trades = df[df['event_type'].isin(['trade', 'order_filled'])]

# Calculate P&L
buys = trades[trades['side'] == 'buy']
sells = trades[trades['side'] == 'sell']
pnl = (sells['price'] * sells['qty']).sum() - (buys['price'] * buys['qty']).sum()

# Group by symbol
by_symbol = trades.groupby('symbol').agg({
    'qty': 'sum',
    'fee': 'sum',
    'event_id': 'count'
}).rename(columns={'event_id': 'trade_count'})

print(by_symbol)
```

## CLI Commands

### Start a Bot

```bash
# Start with default config
fluxum bot start

# Start with custom config
fluxum bot start --bot-id my-bot --symbols BTCUSD,ETHUSD --venues gemini,kraken

# Start with strategy
fluxum bot start --strategy momentum --config ./momentum.json

# Start with dashboard enabled
fluxum bot start --dashboard
```

### Check Status

```bash
# Get bot status
fluxum bot status --bot-id my-bot

# List all bots
fluxum bot list
```

### Stop a Bot

```bash
fluxum bot stop --bot-id my-bot --reason "Manual shutdown"
```

### Replay Events

```bash
# Replay to reconstruct state
fluxum bot replay --events ./bot_events/my-bot/ --output state.json

# Replay with verbose output
fluxum bot replay --events ./bot_events/my-bot/ --verbose
```

### Export Events

```bash
# Export to CSV
fluxum bot export --events ./bot_events/my-bot/ --format csv --output ./exports/

# Export to JSON Lines
fluxum bot export --events ./bot_events/my-bot/ --format jsonl --output ./exports/

# Export only orders
fluxum bot export --events ./bot_events/my-bot/ --filter orders --output ./exports/

# Export only trades
fluxum bot export --events ./bot_events/my-bot/ --filter trades --output ./exports/
```

### List Strategies

```bash
fluxum bot strategies
```

## Complete Examples

### Full Trading Bot

```ocaml
open Core
open Async
open Bot

let main () =
  printf "Starting trading bot...\n";

  (* Configuration *)
  let config = Engine.Config.{
    default with
    bot_id = "momentum-trader";
    symbols = ["BTCUSD"];
    venues = [Event.Venue.Gemini];
    event_store_path = "./bot_events";
    tick_interval = Time_float.Span.of_sec 1.0;
  } in

  (* Create engine *)
  let%bind engine = Engine.create config in

  (* Set strategy *)
  let strategy = Engine.Strategy_wrapper.wrap
    (module Momentum_strategy)
    Momentum_strategy.default_config
  in
  Engine.set_strategy engine strategy;

  (* Create dashboard *)
  let dashboard = Dashboard.create Dashboard.Config.default in

  (* Start engine *)
  let%bind result = Engine.start engine in
  (match result with
  | Ok () -> printf "Bot started!\n"
  | Error e -> failwith (Error.to_string_hum e));

  (* Simulate market data feed *)
  don't_wait_for (
    let prices = [67000.; 67100.; 67050.; 67200.; 67150.; 66900.; 66800.] in
    Deferred.List.iter prices ~how:`Sequential ~f:(fun price ->
      let%bind () = Clock.after (Time_float.Span.of_sec 1.0) in
      Engine.on_book_update engine
        ~symbol:"BTCUSD"
        ~venue:Event.Venue.Gemini
        ~bids:[{ Event.Price_level.price = price -. 1.; qty = 1.0 }]
        ~asks:[{ price = price +. 1.; qty = 1.0 }]
        ~is_snapshot:false
    )
  );

  (* Run dashboard *)
  let%bind () = Dashboard.run_with_engine dashboard engine in

  (* Cleanup *)
  Engine.stop engine ~reason:"Completed"

let () = Command.async ~summary:"Run momentum trading bot" (Command.Param.return main)
         |> Command_unix.run
```

### Event Analysis Script

```ocaml
open Core
open Async
open Bot

let analyze_events ~path =
  printf "Analyzing events from %s...\n" path;

  let%bind reader = Event_store.Reader.open_ ~path in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  (* Statistics *)
  let stats = Parquet_export.Stats.of_events events in
  printf "\n%s\n" (Parquet_export.Stats.to_string stats);

  (* Reconstruct final state *)
  let state = State.reconstruct ~events ~bot_id:"analysis" in
  printf "\nFinal State:\n%s\n" (State.summary state);

  (* Export for Python analysis *)
  let%bind () = Parquet_export.export_csv
    ~events
    ~output_path:"./analysis/events.csv"
    ~include_header:true
  in

  let%bind () = Parquet_export.write_python_helper ~output_dir:"./analysis" in

  printf "\nExported to ./analysis/events.csv\n";
  printf "Run: python analysis/load_events.py\n";
  return ()

let () =
  Command.async
    ~summary:"Analyze bot events"
    (let%map_open.Command path =
       flag "--path" (required string) ~doc:"PATH Path to event store"
     in
     fun () -> analyze_events ~path)
  |> Command_unix.run
```

### Backtesting with Event Replay

```ocaml
open Core
open Async
open Bot

let backtest ~events_path ~strategy_config =
  (* Load historical events *)
  let%bind reader = Event_store.Reader.open_ ~path:events_path in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in

  printf "Loaded %d events for backtest\n" (List.length events);

  (* Create engine for backtest *)
  let config = Engine.Config.{
    default with
    bot_id = "backtest";
    event_store_path = "./backtest_events";
  } in
  let%bind engine = Engine.create config in

  (* Set strategy *)
  let strategy = Engine.Strategy_wrapper.wrap
    (module Momentum_strategy) strategy_config
  in
  Engine.set_strategy engine strategy;
  let%bind _ = Engine.start engine in

  (* Replay events *)
  let%bind () = Deferred.List.iter events ~how:`Sequential ~f:(fun env ->
    match env.Event.event with
    | Market (Book_update { symbol; venue; bids; asks; is_snapshot }) ->
      Engine.on_book_update engine ~symbol ~venue ~bids ~asks ~is_snapshot
    | Market (Trade { symbol; venue; price; qty; side; _ }) ->
      Engine.on_trade engine ~symbol ~venue ~price ~qty ~side
    | _ -> return ()
  ) in

  (* Get results *)
  let state = Engine.state engine in
  printf "\n=== Backtest Results ===\n";
  printf "Total P&L: %.2f\n" (Unified_ledger.Ledger.total_pnl state.ledger);
  printf "Realized P&L: %.2f\n" (Unified_ledger.Ledger.total_realized state.ledger);
  printf "Total Fees: %.2f\n" (Unified_ledger.Ledger.total_fees state.ledger);

  (* Cleanup *)
  Engine.stop engine ~reason:"Backtest complete"
```

## Best Practices

### 1. Always Use Event Sourcing

All state changes should flow through events. Never modify state directly:

```ocaml
(* Good: Emit event, let state update handle it *)
let%bind () = Engine.on_fill engine ~order_id ~venue ~fill_qty ~fill_price ~fee ~is_maker in
(* State is now updated via event sourcing *)

(* Bad: Directly modifying state *)
(* DON'T DO THIS *)
```

### 2. Keep Strategies Pure

Strategies should be pure functions without side effects:

```ocaml
(* Good: Return signals, don't execute directly *)
let on_book_update state ~book ~context =
  let signals = (* compute signals *) in
  (signals, new_state)

(* Bad: Side effects in strategy *)
(* DON'T: place_order_directly () *)
```

### 3. Handle All Event Types

Your strategy should handle all relevant events gracefully:

```ocaml
let on_fill state ~order_id ~symbol ~venue ~side ~fill_qty ~fill_price ~context =
  (* Update internal state to reflect filled order *)
  let state = remove_pending_order state order_id in
  ([], state)
```

### 4. Export Regularly for Analysis

Export events regularly for offline analysis:

```ocaml
let periodic_export engine =
  Clock.every (Time_float.Span.of_hr 1.0) (fun () ->
    don't_wait_for (
      let state = Engine.state engine in
      let timestamp = Time_ns.to_string_trimmed (Time_ns.now ()) in
      let path = sprintf "./exports/snapshot_%s.csv" timestamp in
      (* Export current state *)
      return ()
    )
  )
```

### 5. Validate State Consistency

Periodically validate state:

```ocaml
let validate_periodically engine =
  Clock.every (Time_float.Span.of_min 5.0) (fun () ->
    let state = Engine.state engine in
    match State.validate state with
    | Ok () -> ()
    | Error e ->
      Log.Global.error "State validation failed: %s" (Error.to_string_hum e)
  )
```

## See Also

- [Architecture Guide](ARCHITECTURE.md) - Overall system design
- [Order Book Guide](ORDER_BOOK.md) - Order book usage
- [Exchange Docs](../exchanges/) - Exchange-specific documentation
- [Examples](../../examples/) - Working code examples

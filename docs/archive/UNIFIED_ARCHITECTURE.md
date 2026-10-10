# Unified Market Data, Order Book, Ledger & Session Architecture

**Goal:** Create extensible, exchange-agnostic abstractions for real-time market data, order books, ledger access, and session management.

## **Layer 1: Common Types**

### Market Data Types
```ocaml
module Types = struct
  (* Already exists, extend with: *)

  module Ticker = struct
    type t = {
      symbol : Symbol.t;
      bid : Price.t;
      ask : Price.t;
      bid_size : Qty.t;
      ask_size : Qty.t;
      last_price : Price.t;
      last_size : Qty.t;
      timestamp : Time_float_unix.t;
      (* Exchange-specific extras in variant *)
      extras : extra list;
    }
    and extra =
      | Volume_24h of float
      | Vwap of Price.t
      | High_24h of Price.t
      | Low_24h of Price.t
      | Open_price of Price.t
      | Num_trades of int
    [@@deriving sexp]
  end

  module Trade = struct
    type t = {
      symbol : Symbol.t;
      price : Price.t;
      size : Qty.t;
      side : Side.t;  (* Buy or Sell - taker side *)
      timestamp : Time_float_unix.t;
      trade_id : string option;
    }
    [@@deriving sexp]
  end

  module Ohlc = struct
    type t = {
      symbol : Symbol.t;
      interval : Span.t;  (* 1m, 5m, 1h, etc *)
      open_ : Price.t;
      high : Price.t;
      low : Price.t;
      close : Price.t;
      volume : float;
      start_time : Time_float_unix.t;
      end_time : Time_float_unix.t;
    }
    [@@deriving sexp]
  end

  module Book_level = struct
    type t = {
      price : Price.t;
      size : Qty.t;
      num_orders : int option;  (* Some exchanges provide this *)
    }
    [@@deriving sexp]
  end

  module Book_snapshot = struct
    type t = {
      symbol : Symbol.t;
      bids : Book_level.t list;  (* Sorted descending by price *)
      asks : Book_level.t list;  (* Sorted ascending by price *)
      timestamp : Time_float_unix.t;
    }
    [@@deriving sexp]
  end

  module Book_update = struct
    type action = Add | Update | Remove [@@deriving sexp]

    type t = {
      symbol : Symbol.t;
      side : [`Bid | `Ask];
      action : action;
      level : Book_level.t;
      timestamp : Time_float_unix.t;
    }
    [@@deriving sexp]
  end

  module Ledger_entry = struct
    type entry_type =
      | Deposit
      | Withdrawal
      | Trade
      | Fee
      | Margin
      | Adjustment
      | Other of string
    [@@deriving sexp]

    type t = {
      id : string;
      entry_type : entry_type;
      asset : string;
      amount : float;
      balance : float;
      timestamp : Time_float_unix.t;
      ref_id : string option;  (* Order ID, trade ID, etc *)
    }
    [@@deriving sexp]
  end
end
```

---

## **Layer 2: Transport & Session Abstractions**

### Session Management
```ocaml
module Session_intf = sig
  module State = struct
    type t =
      | Disconnected
      | Connecting
      | Connected
      | Authenticated
      | Subscribing
      | Ready
      | Reconnecting of { attempt : int; backoff : Time_float_unix.Span.t }
      | Failed of Error.t
    [@@deriving sexp]
  end

  module type S = sig
    type t
    type subscription

    (** Current session state *)
    val state : t -> State.t

    (** State change events *)
    val state_changes : t -> State.t Pipe.Reader.t

    (** Connect (or reconnect) *)
    val connect : t -> unit Deferred.t

    (** Graceful disconnect *)
    val disconnect : t -> unit Deferred.t

    (** Subscribe to channels *)
    val subscribe : t -> subscription list -> unit Deferred.t

    (** Unsubscribe from channels *)
    val unsubscribe : t -> subscription list -> unit Deferred.t

    (** Auto-reconnect on failure *)
    val with_auto_reconnect :
      t ->
      max_attempts:int ->
      backoff:Time_float_unix.Span.t ->
      t
  end
end
```

### Transport Layer
```ocaml
module Transport = sig
  module type WEBSOCKET = sig
    type t
    type message

    val connect : Uri.t -> (t, Error.t) Deferred.Result.t
    val send : t -> message -> unit Deferred.t
    val receive : t -> message Pipe.Reader.t
    val close : t -> unit Deferred.t
    val is_open : t -> bool
  end

  module type REST = sig
    type request
    type response

    val get : Uri.t -> (response, Error.t) Deferred.Result.t
    val post : Uri.t -> request -> (response, Error.t) Deferred.Result.t
  end
end
```

---

## **Layer 3: Market Data Abstraction**

### Unified Market Data Interface
```ocaml
module Market_data_intf = sig
  module Subscription = struct
    type channel =
      | Ticker
      | Trades
      | Ohlc of Time_float_unix.Span.t  (* interval *)
      | Book of int option  (* depth, None = full book *)
    [@@deriving sexp]

    type t = {
      symbols : Symbol.t list;
      channel : channel;
    }
    [@@deriving sexp]
  end

  module type S = sig
    type t
    type native_message  (* Exchange-specific message type *)

    (** Create market data session *)
    val create :
      ?url:string ->
      subscriptions:Subscription.t list ->
      unit ->
      (t, Error.t) Deferred.Result.t

    (** Unified data streams *)
    val tickers : t -> Types.Ticker.t Pipe.Reader.t
    val trades : t -> Types.Trade.t Pipe.Reader.t
    val ohlc : t -> Types.Ohlc.t Pipe.Reader.t
    val book_snapshots : t -> Types.Book_snapshot.t Pipe.Reader.t
    val book_updates : t -> Types.Book_update.t Pipe.Reader.t

    (** Raw exchange messages (for debugging/custom handling) *)
    val raw_messages : t -> native_message Pipe.Reader.t

    (** Session management *)
    val session : t -> (module Session_intf.S with type t = t)

    (** Close connection *)
    val close : t -> unit Deferred.t
  end
end
```

---

## **Layer 4: Order Book Manager**

### Maintains book state from updates
```ocaml
module Book_manager_intf = sig
  module type S = sig
    type t

    (** Create book manager from update stream *)
    val create :
      symbols:Symbol.t list ->
      snapshots:Types.Book_snapshot.t Pipe.Reader.t ->
      updates:Types.Book_update.t Pipe.Reader.t ->
      t

    (** Get current book snapshot *)
    val snapshot : t -> Symbol.t -> Types.Book_snapshot.t option

    (** Get top of book (best bid/ask) *)
    val top : t -> Symbol.t -> (Types.Book_level.t * Types.Book_level.t) option

    (** Get N levels *)
    val levels : t -> Symbol.t -> depth:int -> Types.Book_snapshot.t option

    (** Subscribe to book changes *)
    val changes : t -> Symbol.t -> Types.Book_snapshot.t Pipe.Reader.t

    (** Get spread *)
    val spread : t -> Symbol.t -> float option

    (** Get midpoint *)
    val midpoint : t -> Symbol.t -> Price.t option
  end
end
```

---

## **Layer 5: Ledger Abstraction**

### Unified Ledger Access
```ocaml
module Ledger_intf = sig
  module Query = struct
    type t = {
      asset : string option;  (* Filter by asset *)
      entry_type : Types.Ledger_entry.entry_type option;
      start_time : Time_float_unix.t option;
      end_time : Time_float_unix.t option;
      limit : int option;
    }
    [@@deriving sexp]
  end

  module type S = sig
    type t

    (** Query ledger entries *)
    val query :
      t ->
      Query.t ->
      (Types.Ledger_entry.t list, Error.t) Deferred.Result.t

    (** Stream ledger updates in real-time *)
    val stream : t -> Types.Ledger_entry.t Pipe.Reader.t Deferred.t

    (** Get balance for asset *)
    val balance : t -> asset:string -> (float, Error.t) Deferred.Result.t
  end
end
```

---

## **Layer 6: Exchange Adapter Pattern**

### How exchanges implement the unified interface
```ocaml
module Exchange_adapter = sig
  module type S = sig
    module Native : sig
      (* Exchange-specific types *)
      module Ticker : sig type t [@@deriving sexp, of_yojson] end
      module Trade : sig type t [@@deriving sexp, of_yojson] end
      module Book : sig type t [@@deriving sexp, of_yojson] end
      module Ledger : sig type t [@@deriving sexp, of_yojson] end
      module Message : sig type t [@@deriving sexp, of_yojson] end
    end

    module Normalize : sig
      (* Native → Common conversions *)
      val ticker : Native.Ticker.t -> Types.Ticker.t
      val trade : Native.Trade.t -> Types.Trade.t
      val book_snapshot : Native.Book.t -> Types.Book_snapshot.t
      val book_update : Native.Book.t -> Types.Book_update.t list
      val ledger_entry : Native.Ledger.t -> Types.Ledger_entry.t
    end

    module Protocol : sig
      (* Exchange-specific protocol details *)
      val websocket_url : public:bool -> string
      val encode_subscription : Market_data_intf.Subscription.t -> string
      val parse_message : string -> (Native.Message.t, Error.t) result
      val route_message :
        Native.Message.t ->
        [ `Ticker of Native.Ticker.t
        | `Trade of Native.Trade.t
        | `Book of Native.Book.t
        | `Ledger of Native.Ledger.t
        | `System of string
        | `Error of Error.t
        ]
    end

    (* Implement market data interface *)
    include Market_data_intf.S

    (* Implement ledger interface *)
    module Ledger : Ledger_intf.S
  end
end
```

---

## **Example: Kraken Adapter Implementation**

```ocaml
module Kraken_adapter : Exchange_adapter.S = struct
  module Native = struct
    (* Use existing Kraken.Ws.Public.* types *)
    module Ticker = Kraken.Ws.Public.Ticker_data
    module Trade = Kraken.Ws.Public.Trade_item
    module Book = Kraken.Ws.Public.Book_snapshot
    (* ... *)
    module Message = Kraken.Ws.Public.message
  end

  module Normalize = struct
    let ticker (t : Native.Ticker.t) : Types.Ticker.t =
      { symbol = parse_kraken_symbol t.pair;
        bid = Price.of_string t.bid;
        ask = Price.of_string t.ask;
        bid_size = Qty.of_string t.bid_volume;
        ask_size = Qty.of_string t.ask_volume;
        last_price = Price.of_string t.close;
        last_size = Qty.of_string t.close_volume;
        timestamp = Time_float_unix.now ();
        extras = [
          Volume_24h (Float.of_string t.volume);
          Vwap (Price.of_string t.vwap);
          High_24h (Price.of_string t.high);
          Low_24h (Price.of_string t.low);
        ];
      }

    (* ... other conversions *)
  end

  module Protocol = struct
    let websocket_url ~public =
      if public then "wss://ws.kraken.com/"
      else "wss://ws-auth.kraken.com/"

    let encode_subscription sub =
      match sub.Market_data_intf.Subscription.channel with
      | Ticker -> Kraken.Ws.Public.V2.subscribe_ticker sub.symbols
      | Trades -> Kraken.Ws.Public.V2.subscribe_trade sub.symbols
      | Ohlc interval ->
          Kraken.Ws.Public.V2.subscribe_ohlc
            ~interval:(Span.to_int_sec interval)
            sub.symbols
      | Book depth ->
          Kraken.Ws.Public.V2.subscribe_book
            ?depth
            sub.symbols

    let parse_message msg =
      Kraken.Ws.Public.parse_message (Yojson.Safe.from_string msg)

    let route_message = function
      | Kraken.Ws.Public.Ticker (pair, data) -> `Ticker data
      | Kraken.Ws.Public.Trade (pair, trades) -> `Trade trades
      | Kraken.Ws.Public.Book (pair, snapshot) -> `Book snapshot
      | Kraken.Ws.Public.Heartbeat -> `System "heartbeat"
      (* ... *)
  end

  type t = {
    ws : Kraken.Market_data.t;
    ticker_pipe : Types.Ticker.t Pipe.Writer.t * Types.Ticker.t Pipe.Reader.t;
    trade_pipe : Types.Trade.t Pipe.Writer.t * Types.Trade.t Pipe.Reader.t;
    (* ... *)
  }

  let create ?url ~subscriptions () =
    Kraken.Market_data.connect ?url ~subscriptions ()
    >>| Result.map ~f:(fun ws ->
      let ticker_pipe = Pipe.create () in
      let trade_pipe = Pipe.create () in

      (* Start message router *)
      don't_wait_for (
        Pipe.iter (Kraken.Market_data.messages ws) ~f:(fun msg ->
          match Protocol.parse_message msg with
          | Ok native_msg ->
            (match Protocol.route_message native_msg with
            | `Ticker t ->
                Pipe.write (fst ticker_pipe) (Normalize.ticker t)
            | `Trade t ->
                Pipe.write (fst trade_pipe) (Normalize.trade t)
            | _ -> Deferred.unit)
          | Error _ -> Deferred.unit)
      );

      { ws; ticker_pipe; trade_pipe })

  let tickers t = snd t.ticker_pipe
  let trades t = snd t.trade_pipe
  (* ... *)
end
```

---

## **Example: Gemini Adapter Implementation**

```ocaml
module Gemini_adapter : Exchange_adapter.S = struct
  (* Same structure, different implementations *)
  module Native = struct
    module Ticker = Gemini.Ws.MarketData.Change
    module Trade = Gemini.Ws.MarketData.Trade
    (* ... *)
  end

  module Normalize = struct
    let ticker (t : Native.Ticker.t) : Types.Ticker.t =
      (* Gemini-specific conversion *)
      { symbol = Gemini.symbol_to_common t.symbol;
        bid = Price.of_string t.bid;
        ask = Price.of_string t.ask;
        (* ... *)
      }
    (* ... *)
  end

  module Protocol = struct
    let websocket_url ~public = "wss://api.gemini.com/v2/marketdata"
    (* ... Gemini-specific protocol *)
  end

  (* Rest of implementation follows same pattern as Kraken *)
end
```

---

## **Usage Example: Multi-Exchange Aggregation**

```ocaml
(* Application code that works with ANY exchange *)

let aggregate_market_data () =
  (* Connect to multiple exchanges *)
  let%bind kraken =
    (module Kraken_adapter : Market_data_intf.S)
    |> create ~subscriptions:[{symbols = ["BTC/USD"]; channel = Ticker}]
  in

  let%bind gemini =
    (module Gemini_adapter : Market_data_intf.S)
    |> create ~subscriptions:[{symbols = ["BTCUSD"]; channel = Ticker}]
  in

  (* Merge ticker streams from both exchanges *)
  let all_tickers =
    Pipe.interleave [
      Kraken_adapter.tickers kraken;
      Gemini_adapter.tickers gemini;
    ]
  in

  (* Process unified ticker stream *)
  Pipe.iter all_tickers ~f:(fun ticker ->
    printf "Ticker: %s @ %s\n"
      (Symbol.to_string ticker.symbol)
      (Price.to_string ticker.last_price);
    Deferred.unit)
```

---

## **Implementation Roadmap**

### Phase 1: Core Types & Interfaces ✅ (Design above)
- Define common types in `lib/types.ml`
- Define interfaces in `lib/*_intf.ml`

### Phase 2: Session Management
- Implement `lib/session.ml` with reconnection logic
- WebSocket wrapper with lifecycle management

### Phase 3: Market Data Adapters
- Refactor `lib/exchange/kraken/market_data.ml` → adapter pattern
- Refactor `lib/exchange/gemini/ws.ml` → adapter pattern
- Test with existing implementations

### Phase 4: Order Book Manager
- Implement `lib/book_manager.ml`
- Snapshot + incremental update reconciliation
- Book state validation

### Phase 5: Ledger Integration
- Add ledger endpoints to REST frameworks
- Implement real-time ledger streams (if supported)
- Add query builders

### Phase 6: CLI Integration
- Update CLI to use unified interfaces
- Multi-exchange commands: `fluxum stream ticker --exchange kraken,gemini --symbol BTC/USD`

---

## **Benefits of This Design**

✅ **Exchange-agnostic** - Application code doesn't know about Kraken/Gemini
✅ **Type-safe** - Compiler enforces correct conversions
✅ **Extensible** - New exchange = implement adapter, reuse everything else
✅ **Testable** - Mock adapters for unit tests
✅ **Composable** - Mix REST + WebSocket, multiple exchanges
✅ **Resilient** - Session management handles reconnections
✅ **Observable** - State changes, errors exposed as streams

---

## **File Structure**

```
lib/
  types.ml                      # Common types (extend existing)
  session_intf.ml               # Session interface
  session.ml                    # Session implementation
  market_data_intf.ml           # Market data interface
  book_manager_intf.ml          # Book manager interface
  book_manager.ml               # Book manager implementation
  ledger_intf.ml                # Ledger interface
  exchange_adapter.ml           # Adapter pattern signature

  exchange/
    kraken/
      adapter.ml                # Kraken adapter (uses existing ws.ml, rest.ml)
      ws.ml                     # Keep existing (used by adapter)
      rest.ml                   # Keep existing
      v1.ml                     # Keep existing

    gemini/
      adapter.ml                # Gemini adapter
      ws.ml                     # Keep existing
      rest.ml                   # Keep existing
      v1.ml                     # Keep existing
```

---

## **Next Steps**

1. Review & refine this design
2. Implement Phase 1 (types & interfaces)
3. Create Kraken adapter as reference implementation
4. Migrate existing code to use adapters
5. Add Gemini adapter
6. Build book manager
7. Add ledger support

Thoughts? Any modifications to this design you'd like to see?

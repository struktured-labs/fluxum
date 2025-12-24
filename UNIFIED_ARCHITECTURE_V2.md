# Unified Architecture V2 - Based on Gemini's Proven Patterns

**Goal:** Extract and generalize Gemini's excellent order_book, ledger, and session implementations into exchange-agnostic abstractions.

## **Key Observations from Gemini's Implementation**

### **What Gemini Got Right** ✅

1. **Order Book** (`lib/exchange/gemini/order_book.ml`):
   - Separate comparators for Bid (descending) and Ask (ascending) using Map
   - `epoch` field for versioning updates
   - Advanced queries: `best_n`, `market_price` for volume, `mid_market_price`
   - `on_market_data` integration for WebSocket events
   - `pipe` function returning `Book.t Pipe.Reader.t`
   - Beautiful TUI with ANSI colors, throttling, tick aggregation
   - `Books` type for multi-symbol management

2. **Ledger** (`lib/exchange/gemini/ledger.ml`):
   - Rich `Entry` type tracking P&L, positions, avg prices, cost basis
   - `Update_source` enum (`Market_data`, `Trade`, `External_trade`)
   - `on_trade` - handles buy/sell, fees, short positions
   - `update_from_book` - unrealized P&L from market prices
   - `from_mytrades` - bootstrap from trade history
   - `from_balances` - initialize from account snapshot
   - `pipe` combining order_book + order_events for real-time
   - CSV export support

3. **Session** (`lib/exchange/gemini/session.ml`):
   - `auto_restart_pipe` - auto-reconnecting WebSocket streams!
   - `Events.t` - unified container for all streams
   - Multi-symbol management with `Symbol.Map`
   - Concurrent order tracking with `Concurrent_order_map`

4. **Adapter** (`lib/exchange/gemini/fluxum_adapter.ml`):
   - Already implements `Exchange_intf.S`!
   - Clean Native → Common type mapping

---

## **Proposed Unified Architecture**

### **Layer 1: Common Types** (extend `lib/types.ml`)

```ocaml
module Types = struct
  (* Price with proper ordering *)
  module Price = struct
    include Float
    module Ascending = Float  (* For asks *)
    module Descending = struct  (* For bids *)
      include Float
      include Comparator.Make(struct
        type t = float [@@deriving sexp, compare]
        let compare a b = Float.compare b a  (* reversed *)
      end)
    end
  end

  (* Unified across exchanges *)
  module Price_level = struct
    type t = {
      price : float;
      volume : float;
      num_orders : int option;  (* Some exchanges provide this *)
    }
    [@@deriving sexp, fields]
  end

  module Book_side = [`Bid | `Ask] [@@deriving sexp]

  (* Generic enough for any exchange *)
  module Book_snapshot = struct
    type t = {
      symbol : Symbol.t;
      bids : Price_level.t list;  (* Descending by price *)
      asks : Price_level.t list;  (* Ascending by price *)
      timestamp : Time_float_unix.t;
      epoch : int;  (* Version counter for updates *)
    }
    [@@deriving sexp]
  end

  module Book_update = struct
    type action = Set | Add | Remove [@@deriving sexp]

    type t = {
      symbol : Symbol.t;
      side : Book_side.t;
      action : action;
      price : float;
      size : float;
      timestamp : Time_float_unix.t;
    }
    [@@deriving sexp]
  end

  (* Ledger entry - based on Gemini's comprehensive design *)
  module Ledger_entry = struct
    type update_source =
      | Market_data
      | Trade
      | External_trade
      | Balance_snapshot
    [@@deriving sexp]

    type t = {
      symbol : Symbol.t;
      (* P&L fields *)
      pnl : float;  (* Total P&L *)
      position : float;  (* Net position (+ = long, - = short) *)
      spot : float;  (* Current market price *)
      pnl_spot : float;  (* Unrealized P&L at spot *)
      notional : float;  (* Notional value *)

      (* Execution tracking *)
      avg_buy_price : float;
      avg_sell_price : float;
      avg_price : float;
      total_buy_qty : float;
      total_sell_qty : float;
      buy_notional : float;
      sell_notional : float;

      (* Cost basis accounting *)
      cost_basis : float;
      running_price : float;
      running_qty : float;

      (* Order tracking *)
      total_original : float;
      total_executed : float;
      total_remaining : float;

      (* Metadata *)
      update_time : Time_float_unix.t;
      update_source : update_source;
    }
    [@@deriving sexp, fields]
  end
end
```

---

### **Layer 2: Order Book Interface** (`lib/order_book_intf.ml`)

Based on Gemini's `order_book.ml`:

```ocaml
module Order_book_intf = sig
  module type BOOK = sig
    type t [@@deriving sexp]

    val empty : ?timestamp:Time_float_unix.t -> ?epoch:int -> Symbol.t -> t

    (** Update operations *)
    val set : ?timestamp:Time_float_unix.t -> t -> side:Types.Book_side.t -> price:float -> size:float -> t
    val add : ?timestamp:Time_float_unix.t -> t -> side:Types.Book_side.t -> price:float -> size:float -> t
    val remove : ?timestamp:Time_float_unix.t -> t -> side:Types.Book_side.t -> price:float -> size:float -> t

    (** Queries *)
    val best_bid : t -> Types.Price_level.t
    val best_ask : t -> Types.Price_level.t
    val best_n_bids : t -> n:int -> unit -> Types.Price_level.t list
    val best_n_asks : t -> n:int -> unit -> Types.Price_level.t list

    (** Market queries *)
    val market_price : t -> side:Types.Side.t -> volume:float -> Types.Price_level.t
    val mid_market_price : t -> volume:float -> Types.Price_level.t
    val spread : t -> float option
    val midpoint : t -> float option

    (** Accessors *)
    val symbol : t -> Symbol.t
    val epoch : t -> int
    val update_time : t -> Time_float_unix.t

    (** TUI rendering (Gemini's awesome feature!) *)
    val pretty_print :
      ?max_depth:int ->
      ?refresh_ms:float ->
      ?tick_size:float option ->
      t -> unit
  end

  module type BOOKS = sig
    type t [@@deriving sexp]
    type book

    val empty : t
    val book : t -> Symbol.t -> book option
    val symbols : t -> Symbol.t list
    val set_book : ?timestamp:Time_float_unix.t -> t -> book -> t
  end

  module type S = sig
    include BOOK
    module Books : BOOKS with type book := t

    (** Create live book from exchange-specific market data *)
    val pipe :
      (module Cfg.S) ->
      symbol:Symbol.t ->
      unit ->
      ([ `Ok of t | `Error of Error.t ] Pipe.Reader.t) Deferred.t

    (** Multi-symbol books *)
    val pipe_multi :
      (module Cfg.S) ->
      symbols:Symbol.t list ->
      unit ->
      ([ `Ok of t | `Error of Error.t ] Pipe.Reader.t Symbol.Map.t) Deferred.t
  end
end
```

---

### **Layer 3: Ledger Interface** (`lib/ledger_intf.ml`)

Based on Gemini's `ledger.ml`:

```ocaml
module Ledger_intf = sig
  module type ENTRY = sig
    type t = Types.Ledger_entry.t [@@deriving sexp]

    val create :
      ?update_time:Time_float_unix.t ->
      symbol:Symbol.t ->
      ?position:float ->
      ?spot:float ->
      unit -> t

    (** Trade updates *)
    val on_trade :
      ?update_source:Types.Ledger_entry.update_source ->
      ?timestamp:Time_float_unix.t ->
      ?avg_trade_price:float ->
      ?fee_usd:float ->
      t ->
      price:float ->
      side:Types.Side.t ->
      qty:float ->
      t

    (** Market data updates *)
    val update_spot : ?timestamp:Time_float_unix.t -> t -> float -> t
    val update_from_book : t -> Order_book.Book.t -> t

    (** Real-time pipe combining order book + trade events *)
    val pipe :
      init:t ->
      ?num_values:int ->
      ?behavior:[`Alternate | `Priority | `Random] ->
      Order_book.Book.t Pipe.Reader.t ->
      trade_events:t Pipe.Reader.t ->
      t Pipe.Reader.t Deferred.t
  end

  module type S = sig
    type t = Types.Ledger_entry.t Symbol.Map.t [@@deriving sexp]
    module Entry : ENTRY

    (** Bootstrap from account state *)
    val from_balances :
      ?notional_currency:Types.Currency.t ->
      balance list ->
      t

    (** Bootstrap from trade history *)
    val from_trades :
      ?init:t ->
      ?avg_trade_prices:float Symbol.Map.t ->
      trade list ->
      t * t Pipe.Reader.t Symbol.Map.t

    (** Update from market data *)
    val update_from_books : t -> books:Order_book.Books.t -> t
    val update_spots : ?timestamp:Time_float_unix.t -> t -> float Symbol.Map.t -> t

    (** Real-time multi-symbol ledger *)
    val pipe :
      ?num_values:int ->
      ?behavior:[`Alternate | `Priority | `Random] ->
      init:Entry.t Symbol.Map.t ->
      Order_book.Book.t Pipe.Reader.t Symbol.Map.t ->
      trade_events:t Pipe.Reader.t ->
      Entry.t Pipe.Reader.t Symbol.Map.t Deferred.t
  end
end
```

---

### **Layer 4: Session Interface** (`lib/session_intf.ml`)

Based on Gemini's `session.ml`:

```ocaml
module Session_intf = sig
  module State = struct
    type t =
      | Disconnected
      | Connecting
      | Connected
      | Ready
      | Reconnecting of { attempt : int }
      | Failed of Error.t
    [@@deriving sexp]
  end

  (** Auto-restarting pipe wrapper (Gemini's killer feature!) *)
  module Auto_restart : sig
    val pipe :
      name:string ->
      create_pipe:(unit -> 'a Pipe.Reader.t Deferred.t) ->
      unit ->
      'a Pipe.Reader.t
  end

  module type EVENTS = sig
    type t = {
      (* Account streams *)
      balances : balance Pipe.Reader.t;
      trades : trade Pipe.Reader.t Symbol.Map.t;

      (* Market data streams *)
      market_data : market_event Pipe.Reader.t Symbol.Map.t;
      order_books : Order_book.Book.t Pipe.Reader.t Symbol.Map.t;

      (* Derived streams *)
      ledger : Ledger.Entry.t Pipe.Reader.t Symbol.Map.t;

      (* Metadata *)
      symbols : Symbol.t list;
    }

    val create :
      ?symbols:Symbol.t list ->
      (module Cfg.S) ->
      nonce:Nonce.reader ->
      unit ->
      t Deferred.t
  end

  module type S = sig
    type t
    module Events : EVENTS

    val create : ?symbols:Symbol.t list -> (module Cfg.S) -> t Deferred.t
    val events : t -> Events.t
    val state : t -> State.t
    val close : t -> unit Deferred.t
  end
end
```

---

### **Layer 5: Exchange Adapter Pattern**

Each exchange implements these interfaces:

```ocaml
module Exchange_adapter = sig
  module type S = sig
    (** Native exchange-specific types *)
    module Native : sig
      module Market_data_event : sig
        type t [@@deriving sexp, of_yojson]
      end

      module Trade_event : sig
        type t [@@deriving sexp, of_yojson]
      end

      module Balance : sig
        type t [@@deriving sexp, of_yojson]
      end
    end

    (** Conversion to common types *)
    module Normalize : sig
      val book_update : Native.Market_data_event.t -> Types.Book_update.t option
      val trade : Native.Trade_event.t -> Types.Trade.t
      val balance : Native.Balance.t -> Types.Balance.t
    end

    (** Implement unified interfaces *)
    module Order_book : Order_book_intf.S
    module Ledger : Ledger_intf.S
    module Session : Session_intf.S

    (** Exchange-specific implementations of common interfaces *)
    include Exchange_intf.S
  end
end
```

---

### **Implementation: Refactor Gemini** (`lib/exchange/gemini/adapter.ml`)

Gemini already has most pieces - just need to expose them properly:

```ocaml
module Gemini_adapter : Exchange_adapter.S = struct
  module Native = struct
    module Market_data_event = V1.Market_data
    module Trade_event = V1.Order_events.Order_event
    module Balance = V1.Balances
  end

  module Normalize = struct
    let book_update (event : V1.Market_data.event) =
      match event with
      | `Change { price; side; remaining; _ } ->
        Some {
          Types.Book_update.
          symbol = (* extract from event *);
          side = (side :> Types.Book_side.t);
          action = Set;
          price = Float.of_string price;
          size = Float.of_string remaining;
          timestamp = Time_float_unix.now ();
        }
      | _ -> None

    let trade (event : V1.Order_events.Order_event.t) =
      { Types.Trade.
        symbol = event.symbol;
        price = (* extract *);
        size = (* extract *);
        side = event.side;
        timestamp = event.timestampms;
        trade_id = Some event.order_id;
      }

    let balance (b : V1.Balances.balance) =
      { Types.Balance.
        currency = b.currency;
        amount = Float.of_string b.amount;
        available = Float.of_string b.available;
      }
  end

  (* Expose existing implementations *)
  module Order_book = Order_book  (* lib/exchange/gemini/order_book.ml *)
  module Ledger = Ledger          (* lib/exchange/gemini/ledger.ml *)
  module Session = Session        (* lib/exchange/gemini/session.ml *)

  (* Already implemented in fluxum_adapter.ml *)
  include Fluxum_adapter.Adapter
end
```

---

### **Implementation: Create Kraken Adapter** (`lib/exchange/kraken/adapter.ml`)

Follow Gemini's patterns:

```ocaml
module Kraken_adapter : Exchange_adapter.S = struct
  module Native = struct
    module Market_data_event = Ws.Public.message
    module Trade_event = (* from private WebSocket *)
    module Balance = V1.Balances
  end

  module Normalize = struct
    let book_update (msg : Ws.Public.message) =
      match msg with
      | Book (pair, snapshot) ->
        (* Convert Kraken book snapshot to series of updates *)
        Some {
          Types.Book_update.
          symbol = parse_kraken_pair pair;
          (* ... *)
        }
      | _ -> None

    let trade event = (* ... *)
    let balance b = (* ... *)
  end

  (* Implement Order_book following Gemini's structure *)
  module Order_book = struct
    module Book = struct
      type t = {
        symbol : Symbol.t;
        bids : Types.Price_level.t Price.Descending.Map.t;
        asks : Types.Price_level.t Price.Ascending.Map.t;
        epoch : int;
        update_time : Time_float_unix.t;
      }
      [@@deriving sexp]

      (* Implement same interface as Gemini.Order_book.Book *)
      (* ... copy Gemini's logic, adapt to Kraken *)
    end

    (* Adapt Kraken WebSocket to Book updates *)
    let pipe cfg ~symbol () =
      Market_data.connect ~url:Ws.Endpoint.public_url_v2
        ~subscriptions:[{channel="book"; pairs=[symbol]; ...}] ()
      >>| Result.ok_exn
      >>| fun client ->
      Market_data.messages client
      |> Pipe.filter_map ~f:(fun msg ->
        match Ws.Public.parse_message msg with
        | Ok (Book (pair, data)) ->
          (* Convert to Book.t update *)
          Some (Book.on_kraken_book_event ...)
        | _ -> None)
  end

  (* Implement Ledger following Gemini's structure *)
  module Ledger = struct
    (* Use same Entry.t type from Types.Ledger_entry *)
    (* Implement same functions as Gemini.Ledger *)
    (* Adapt to Kraken's trade/balance formats *)
  end

  (* Implement Session with auto_restart_pipe *)
  module Session = struct
    (* Copy Gemini's auto_restart_pipe logic *)
    (* Adapt to Kraken WebSocket specifics *)
  end

  (* Implement Exchange_intf.S *)
  type t = {
    cfg : (module Cfg.S);
    symbols : Symbol.t list;
  }

  module Venue = struct
    let t = Types.Venue.Kraken
  end

  (* ... rest of Exchange_intf.S implementation *)
end
```

---

## **Migration Plan**

### **Phase 1: Extract Common Interfaces** (1-2 days)
- [ ] Create `lib/order_book_intf.ml` - extract from Gemini
- [ ] Create `lib/ledger_intf.ml` - extract from Gemini
- [ ] Create `lib/session_intf.ml` - extract from Gemini
- [ ] Update `lib/types.ml` with common types

### **Phase 2: Refactor Gemini** (1 day)
- [ ] Update `lib/exchange/gemini/adapter.ml` to implement new interfaces
- [ ] Ensure existing functionality still works
- [ ] Add any missing Normalize functions

### **Phase 3: Implement Kraken Adapter** (2-3 days)
- [ ] Create `lib/exchange/kraken/order_book.ml` (copy Gemini's structure)
- [ ] Create `lib/exchange/kraken/ledger.ml` (copy Gemini's structure)
- [ ] Create `lib/exchange/kraken/session.ml` (copy auto_restart_pipe)
- [ ] Create `lib/exchange/kraken/adapter.ml`
- [ ] Wire up Kraken WebSocket v2 to Order_book

### **Phase 4: Testing & CLI** (1-2 days)
- [ ] Test Gemini adapter with existing code
- [ ] Test Kraken adapter
- [ ] Update CLI to use adapters
- [ ] Multi-exchange commands: `fluxum stream --exchange kraken,gemini`

### **Phase 5: Advanced Features** (optional)
- [ ] Port Gemini's TUI pretty_print to Kraken
- [ ] CSV export for Kraken ledger
- [ ] Cross-exchange P&L aggregation
- [ ] Arbitrage detection (spread comparison)

---

## **Key Decisions**

### **Preserve Gemini's Excellent Features**
✅ Keep `auto_restart_pipe` - auto-reconnecting is critical
✅ Keep TUI `pretty_print` - visual order book is awesome
✅ Keep CSV export - essential for analysis
✅ Keep Ledger's comprehensive P&L tracking
✅ Keep multi-symbol management patterns

### **Make Exchange-Agnostic**
✅ Extract interfaces, not implementations
✅ Use `Symbol.t`, `Currency.t`, etc. from common Types
✅ Normalize functions per exchange
✅ Adapter pattern for pluggability

### **Reuse Existing Code**
✅ Don't rewrite Gemini's order_book.ml - just expose it
✅ Don't rewrite Gemini's ledger.ml - just expose it
✅ Copy patterns to Kraken (don't abstract too early)
✅ Prove it works with 2 exchanges before generalizing further

---

## **File Structure**

```
lib/
  types.ml                      # Common types (extend existing)
  order_book_intf.ml            # Order book interfaces
  ledger_intf.ml                # Ledger interfaces
  session_intf.ml               # Session interfaces
  exchange_adapter.ml           # Adapter signature

  exchange/
    gemini/
      adapter.ml                # Gemini adapter (NEW - ties everything together)
      order_book.ml             # Keep as-is (already great!)
      ledger.ml                 # Keep as-is (already great!)
      session.ml                # Keep as-is (already great!)
      fluxum_adapter.ml         # Keep (already implements Exchange_intf.S)
      v1.ml                     # Keep (REST operations)
      ws.ml                     # Keep (WebSocket)

    kraken/
      adapter.ml                # Kraken adapter (NEW)
      order_book.ml             # NEW - follow Gemini's pattern
      ledger.ml                 # NEW - follow Gemini's pattern
      session.ml                # NEW - follow Gemini's pattern
      market_data.ml            # Keep (WebSocket client)
      ws.ml                     # Keep (message types)
      v1.ml                     # Keep (REST operations)
```

---

## **Benefits of This Approach**

✅ **Proven patterns** - Gemini's code already works well
✅ **Minimal refactoring** - Extract interfaces, don't rewrite
✅ **Feature parity** - Kraken gets TUI, CSV, auto-restart, etc.
✅ **Type safety** - Compiler enforces conversions
✅ **Extensible** - New exchange = implement adapter
✅ **Testable** - Mock adapters for tests
✅ **Multi-exchange** - Aggregate Gemini + Kraken streams

---

## **Next Steps**

1. ✅ Review this design
2. Implement Phase 1 (interfaces)
3. Test refactored Gemini adapter
4. Implement Kraken adapter
5. Build cross-exchange features (aggregation, arbitrage)

**Questions:**
- Should we preserve Gemini's exact types or normalize more aggressively?
- Any Gemini features we should NOT port to Kraken?
- CLI design for multi-exchange operations?

This architecture is grounded in real, working code! 🚀

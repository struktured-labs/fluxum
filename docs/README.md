# Fluxum Documentation

Comprehensive documentation for the Fluxum multi-exchange cryptocurrency trading library.

## Quick Links

- [Main README](../README.md) - Project overview and quick start
- [Examples](../examples/README.md) - Working code examples
- [API Reference](#api-reference) - Module documentation
- [Guides](#guides) - Detailed topic guides
- [Exchange Docs](#exchange-specific-documentation) - Exchange-specific information

## Table of Contents

1. [Getting Started](#getting-started)
2. [Architecture](#architecture)
3. [Guides](#guides)
4. [Exchange-Specific Documentation](#exchange-specific-documentation)
5. [API Reference](#api-reference)
6. [Contributing](#contributing)

## Getting Started

### Installation

See the [main README](../README.md#installation) for installation instructions.

### Your First Order Book

```ocaml
open Core
open Async
open Fluxum

(* Stream Kraken BTC/USD order book *)
let%bind book_pipe =
  Kraken.Order_book.Book.pipe
    (module Kraken.Cfg.Production)
    ~symbol:"BTC/USD"
    ()
in

Pipe.iter_without_pushback book_pipe ~f:(fun book ->
  printf "Mid: $%.2f | Spread: $%.2f\n"
    (Kraken.Order_book.Book.mid_price book)
    (Kraken.Order_book.Book.spread book)
)
```

### Key Concepts

1. **Unified Order Book**: All exchanges use the same `Order_book_base` interface
2. **Immutability**: Order books are immutable - updates create new instances
3. **Async I/O**: All operations use Jane Street's Async library
4. **Type Safety**: Full OCaml type safety with compile-time guarantees

## Architecture

### High-Level Overview

```
Application Code
       ↓
Fluxum Library
  ├── Consolidated Order Book (multi-exchange)
  ├── Exchange Common (Order_book_base)
  └── Exchange Adapters
        ├── Binance
        ├── Kraken
        ├── Gemini
        ├── MEXC
        ├── Hyperliquid
        ├── Coinbase
        └── Bitrue
```

### Module Structure

Each exchange follows a consistent structure:

- **Cfg**: Configuration (production/testnet URLs)
- **Common**: Common types (Side, Order_type, etc.)
- **Signature**: Authentication and request signing
- **Rest**: REST API client
- **Ws**: WebSocket streaming
- **Order_book**: Order book implementation
- **V{n}**: Versioned API endpoints

See [Architecture Guide](guides/ARCHITECTURE.md) for details.

## Guides

### Core Guides

- **[Architecture](guides/ARCHITECTURE.md)** - System architecture and design patterns
- **[Order Book](guides/ORDER_BOOK.md)** - Complete order book usage guide
- **[Bot Framework](guides/BOT_FRAMEWORK.md)** - Trading bot infrastructure with event sourcing
- **[WebSocket](guides/WEBSOCKET.md)** - WebSocket streaming guide (TODO)
- **[Authentication](guides/AUTHENTICATION.md)** - API authentication guide (TODO)
- **[Testing](guides/TESTING.md)** - Testing guide (TODO)

### Advanced Topics

- **Consolidated Order Books** - Multi-exchange aggregation (TODO)
- **Arbitrage Detection** - Cross-exchange opportunities (TODO)
- **Custom Analytics** - Building custom indicators (TODO)
- **Performance Tuning** - Optimization tips (TODO)

### Bot Framework

The bot framework provides a complete infrastructure for building trading bots:

- **Event Sourcing** - All state changes captured as events for replay/audit
- **Strategy Interface** - Clean separation between strategy logic and infrastructure
- **P&L Tracking** - Unified ledger for position and profit tracking
- **Python Export** - Export events to CSV/JSON for pandas analysis
- **TUI Dashboard** - Real-time monitoring interface

See the [Bot Framework Guide](guides/BOT_FRAMEWORK.md) for details.

## Exchange-Specific Documentation

Each exchange has detailed documentation covering WebSocket, REST API, and unique features:

- **[Binance](exchanges/BINANCE.md)** - Binance/Binance.US integration
- **[Kraken](exchanges/KRAKEN.md)** - Kraken WebSocket and REST
- **[Gemini](exchanges/GEMINI.md)** - Gemini native API (TODO)
- **[MEXC](exchanges/MEXC.md)** - MEXC Global (TODO)
- **[Hyperliquid](exchanges/HYPERLIQUID.md)** - Hyperliquid DEX (TODO)
- **[Coinbase](exchanges/COINBASE.md)** - Coinbase Advanced Trade (TODO)
- **[Bitrue](exchanges/BITRUE.md)** - Bitrue WebSocket (TODO)

### Exchange Comparison

| Feature | Binance | Kraken | Gemini | MEXC | Hyperliquid | Coinbase | Bitrue |
|---------|---------|---------|--------|------|-------------|----------|--------|
| **WebSocket** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **REST API** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Trading** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| **Order Types** | 7 | 8 | 4 | 5 | 4 | 5 | - |
| **Testnet** | ✅ | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |
| **Auth Method** | HMAC-256 | HMAC-512 | HMAC-384 | HMAC-256 | ED25519 | JWT | - |

## API Reference

### Core Modules

#### Exchange_common.Order_book_base

The unified order book implementation used by all exchanges.

```ocaml
module Order_book_base.Make (Config : sig
  type symbol
  type metadata
  val default_metadata : unit -> metadata
end) : sig
  module Book : sig
    type t

    (* Construction *)
    val create : symbol:Config.symbol -> t

    (* Updates *)
    val set : t -> side:[`Bid | `Ask] -> price:float -> size:float -> t
    val set_many : t -> ([`Bid | `Ask] * float * float) list -> t

    (* Queries *)
    val best_bid : t -> Price_level.t
    val best_ask : t -> Price_level.t
    val mid_price : t -> float
    val spread : t -> float

    (* Analytics *)
    val vwap_buy : t -> volume:float -> float option
    val vwap_sell : t -> volume:float -> float option
  end

  module Books : sig
    type t
    val empty : t
    val set_book : t -> Book.t -> t
    val book : t -> Config.symbol -> Book.t option
  end
end
```

See [lib/exchange/common/order_book_base.mli](../lib/exchange/common/order_book_base.mli) for full documentation.

#### Consolidated_order_book

Multi-exchange order book aggregation.

```ocaml
module Consolidated_order_book : sig
  type t

  val create : unit -> t
  val update_from_exchange : t -> exchange:string -> book:'a -> t
  val get_best_bid : t -> Price_level.t option
  val get_best_ask : t -> Price_level.t option
  val detect_arbitrage : t -> threshold:float -> arbitrage_opportunity option
end
```

See [lib/consolidated_order_book.ml](../lib/consolidated_order_book.ml).

### Exchange Modules

Each exchange module exports:

```ocaml
module <Exchange> : sig
  module Cfg : sig
    module Production : S
    module Testnet : S  (* if available *)
  end

  module Common : sig
    type side = BUY | SELL
    type order_type = LIMIT | MARKET | ...
  end

  module Signature : sig
    val sign : api_secret:string -> params:(string * string) list -> string
  end

  module Rest : sig
    module Error : sig type t end
    (* REST client infrastructure *)
  end

  module Ws : sig
    (* WebSocket streaming *)
  end

  module Order_book : sig
    type metadata
    module Book : sig
      type t
      val pipe : ... -> t Pipe.Reader.t Deferred.t
    end
  end

  module V{n} : sig
    (* Versioned API endpoints *)
  end
end
```

### Generating odoc Documentation

```bash
# Install odoc
opam install odoc

# Generate HTML documentation
dune build @doc

# View in browser
open _build/default/_doc/_html/index.html
```

Generated documentation will be available at `_build/default/_doc/_html/`.

## Examples

The [examples/](../examples/) directory contains working examples for all features:

### Order Book Examples
- `binance_orderbook.ml` - Binance order book streaming
- `kraken_orderbook.ml` - Kraken order book with depth
- `gemini_orderbook.ml` - Gemini WebSocket
- `hyperliquid_orderbook.ml` - Hyperliquid L2 book
- `mexc_orderbook.ml` - MEXC order book
- `bitrue_orderbook.ml` - Bitrue order book

### Consolidated Examples
- `consolidated_orderbook.ml` - Multi-exchange aggregation
- `consolidated_orderbook_arb.ml` - Arbitrage detection
- `consolidated_orderbook_enhanced.ml` - Advanced analytics

### Market Data Examples
- `kraken_market_data.ml` - Kraken market data subscriptions

See [examples/README.md](../examples/README.md) for detailed documentation.

## Common Tasks

### Task: Stream Order Book from Exchange

```ocaml
(* 1. Choose exchange and configuration *)
module Cfg = Kraken.Cfg.Production

(* 2. Create order book pipe *)
let%bind book_pipe =
  Kraken.Order_book.Book.pipe
    (module Cfg)
    ~symbol:"BTC/USD"
    ()
in

(* 3. Process updates *)
Pipe.iter book_pipe ~f:(fun book ->
  (* Use book *)
  return ()
)
```

### Task: Calculate VWAP for Order Size

```ocaml
let book = (* get book *) in

(* Calculate VWAP to buy 10 BTC *)
match Kraken.Order_book.Book.vwap_buy book ~volume:10.0 with
| Some price ->
    printf "VWAP: $%.2f\n" price
| None ->
    printf "Insufficient liquidity\n"
```

### Task: Monitor Multiple Exchanges

```ocaml
(* Start all exchange streams *)
let%bind gemini_pipe = Gemini.Order_book.Book.pipe ... in
let%bind kraken_pipe = Kraken.Order_book.Book.pipe ... in
let%bind binance_pipe = Binance.Order_book.Book.pipe ... in

(* Process all in parallel *)
don't_wait_for (Pipe.iter gemini_pipe ~f:process_gemini);
don't_wait_for (Pipe.iter kraken_pipe ~f:process_kraken);
don't_wait_for (Pipe.iter binance_pipe ~f:process_binance);

(* Or use consolidated order book *)
let%bind () = run_consolidated ["gemini"; "kraken"; "binance"] in
return ()
```

### Task: Place Order via REST

```ocaml
(* Binance: Place limit buy order *)
let%bind response =
  Binance.V3.New_order.call
    ~cfg:(module Binance.Cfg.Production)
    ~api_key
    ~api_secret
    {
      symbol = "BTCUSDT";
      side = BUY;
      order_type = LIMIT;
      quantity = Some 0.001;
      price = Some 50000.0;
      time_in_force = Some GTC;
      (* ... *)
    }
in
match response with
| Ok order -> printf "Order placed: %s\n" order.client_order_id
| Error err -> handle_error err
```

## Contributing

### Documentation

To improve documentation:

1. **Inline docs**: Add to `.mli` files using odoc format
2. **Guides**: Create new guides in `docs/guides/`
3. **Examples**: Add examples to `examples/` with documentation
4. **Exchange docs**: Update exchange-specific docs in `docs/exchanges/`

### Documentation Style

- Use odoc format for inline documentation
- Include code examples in guides
- Provide working examples in `examples/`
- Keep guides practical and example-focused

### Building Docs Locally

```bash
# Install dependencies
opam install odoc

# Build documentation
dune build @doc

# View in browser
open _build/default/_doc/_html/fluxum/index.html
```

## Support

- **Issues**: [GitHub Issues](https://github.com/struktured-labs/fluxum/issues)
- **Examples**: [examples/](../examples/)
- **Source**: [GitHub](https://github.com/struktured-labs/fluxum)

## License

MIT License - see [LICENSE](../LICENSE) for details.

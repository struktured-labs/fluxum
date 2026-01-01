# Fluxum

[![CI](https://github.com/struktured-labs/fluxum/actions/workflows/ci.yml/badge.svg)](https://github.com/struktured-labs/fluxum/actions/workflows/ci.yml)

A high-performance, multi-exchange cryptocurrency trading library built with OCaml, Jane Street Core, and Async. Fluxum provides unified interfaces for market data streaming, order book management, and trading operations across 7 major cryptocurrency exchanges.

## Features

- **7 Exchange Integrations**: Binance, Kraken, Gemini, MEXC, Hyperliquid, Coinbase, Bitrue
- **Unified Order Book**: Common interface for order book management across all exchanges
- **WebSocket Streaming**: Real-time market data (trades, order books, tickers, klines)
- **REST APIs**: Complete REST support for trading, account management, and market data
- **Consolidated Order Book**: Aggregate order books from multiple exchanges in real-time
- **Arbitrage Detection**: Built-in tools for cross-exchange price monitoring
- **Type Safety**: Full type safety with ppx_deriving for JSON serialization
- **Async I/O**: Non-blocking operations using Jane Street's Async library
- **CLI Interface**: Comprehensive command-line tools for all exchanges

## Supported Exchanges

| Exchange | WebSocket | REST API | Order Book | Trading | Status |
|----------|-----------|----------|------------|---------|--------|
| **Binance** | ✅ | ✅ | ✅ | ✅ | Production |
| **Kraken** | ✅ | ✅ | ✅ | ✅ | Production |
| **Gemini** | ✅ | ✅ | ✅ | ✅ | Production |
| **MEXC** | ✅ | ✅ | ✅ | ✅ | Production |
| **Hyperliquid** | ✅ | ✅ | ✅ | ✅ | Production |
| **Coinbase** | ✅ | ✅ | ✅ | ✅ | Production |
| **Bitrue** | ✅ | ✅ | ✅ | ❌ | Market Data |

## Installation

### Prerequisites

- OCaml 5.2.0 or later
- opam (OCaml package manager)
- System dependencies: `libcurl4-openssl-dev`, `libssl-dev`, `pkg-config`

### Install System Dependencies

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y libcurl4-openssl-dev libssl-dev pkg-config

# macOS
brew install openssl pkg-config curl
```

### Install OCaml Dependencies

```bash
# Clone the repository
git clone https://github.com/struktured-labs/fluxum.git
cd fluxum

# Install dependencies
opam pin add -y fluxum .
opam install -y --deps-only fluxum

# Build
dune build

# Run tests
dune runtest
```

## Quick Start

### 1. WebSocket Market Data Streaming

Stream real-time order book data from any exchange:

```ocaml
open Core
open Async
open Fluxum

(* Kraken BTC/USD order book *)
let () =
  Command.async ~summary:"Stream Kraken BTC/USD order book"
    (let%map_open.Command () = return () in
     fun () ->
       let module Cfg = Kraken.Cfg in
       let%bind book_pipe =
         Kraken.Order_book.Book.pipe
           (module Cfg.Production)
           ~symbol:"BTC/USD"
           ()
       in
       Pipe.iter_without_pushback book_pipe ~f:(fun book ->
         let best_bid = Kraken.Order_book.Book.best_bid book in
         let best_ask = Kraken.Order_book.Book.best_ask book in
         printf "Bid: %.2f @ %.8f | Ask: %.2f @ %.8f | Spread: %.2f\n%!"
           Exchange_common.Order_book_base.Price_level.(price best_bid)
           Exchange_common.Order_book_base.Price_level.(volume best_bid)
           Exchange_common.Order_book_base.Price_level.(price best_ask)
           Exchange_common.Order_book_base.Price_level.(volume best_ask)
           (Kraken.Order_book.Book.spread book)
       ))
  |> Command_unix.run
```

### 2. Consolidated Multi-Exchange Order Book

Aggregate order books from multiple exchanges:

```bash
# Run the consolidated order book example
dune exec examples/consolidated_orderbook.exe -- \
  --exchanges gemini,kraken,binance \
  --depth 5 \
  --max-display 10
```

This displays a unified order book merging real-time data from all selected exchanges:

```
=== BTC/USD Consolidated Order Book (Epoch: 1234) ===
Updated: 2025-01-01 10:30:15.123456-05:00
Sources: [Gemini] [Kraken] [Binance]

Asks (Sell Orders):
  GEM  0.00228000 @ 87400.74
  KRK  0.00663500 @ 87399.90
  BIN  0.03004212 @ 87399.35

--- Spread: $-0.63 (-0.0030%) ---

Bids (Buy Orders):
  KRK  3.74406283 @ 87399.00
  BIN  1.71926711 @ 87398.90
  GEM  0.85000000 @ 87398.50
```

### 3. REST API - Query Account Balance

```ocaml
open Core
open Async
open Fluxum

let check_balance () =
  let module Cfg = Binance.Cfg in
  let api_key = Sys.getenv_exn "BINANCE_API_KEY" in
  let api_secret = Sys.getenv_exn "BINANCE_API_SECRET" in

  let%bind response =
    Binance.V3.Account.call
      ~cfg:(module Cfg.Production)
      ~api_key
      ~api_secret
      ()
  in
  match response with
  | Ok account ->
      List.iter account.balances ~f:(fun balance ->
        if Float.(balance.free > 0. || balance.locked > 0.) then
          printf "%s: Free=%.8f Locked=%.8f\n"
            balance.asset balance.free balance.locked
      );
      return ()
  | Error err ->
      eprintf "Error: %s\n" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err));
      return ()
```

### 4. CLI Usage

Fluxum includes a comprehensive CLI for all exchanges:

```bash
# Build the CLI
dune build app/cli.exe

# Query Binance ticker
./fluxum.exe binance ticker-24hr --symbol BTCUSDT

# Query Kraken OHLC data
./fluxum.exe kraken ohlc --pair XBTUSDT --interval 60

# Place a limit order on Gemini (requires API credentials)
export GEMINI_API_KEY="your-key"
export GEMINI_API_SECRET="your-secret"
./fluxum.exe gemini new-order \
  --symbol BTCUSD \
  --amount 0.01 \
  --price 50000 \
  --side buy \
  --type exchange-limit

# Query MEXC exchange info
./fluxum.exe mexc exchange-info

# Get Hyperliquid metadata
./fluxum.exe hyperliquid meta
```

## Architecture

### Unified Order Book (`Exchange_common.Order_book_base`)

All exchanges implement a common order book interface providing:

- **Price Levels**: Bid/ask prices with volumes
- **Best Bid/Ask**: O(1) access to top of book
- **Mid Price & Spread**: Calculated automatically
- **VWAP**: Volume-weighted average price for buy/sell operations
- **Multi-Symbol Support**: Track multiple trading pairs simultaneously

```ocaml
module Order_book_base.Make (Config : sig
  type symbol
  type metadata
  val default_metadata : unit -> metadata
  (* sexp serialization functions *)
end) : sig
  module Book : sig
    type t

    val create : symbol:Config.symbol -> t
    val set : t -> side:[`Bid | `Ask] -> price:float -> size:float -> t
    val best_bid : t -> Price_level.t
    val best_ask : t -> Price_level.t
    val mid_price : t -> float
    val spread : t -> float
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

### Exchange-Specific Implementations

Each exchange extends the base with exchange-specific features:

```ocaml
(* Kraken Order Book *)
module Kraken.Order_book : sig
  type metadata = unit  (* No extra metadata needed *)
  module Book_base = Exchange_common.Order_book_base.Make(...)
  module Book : sig
    include module type of Book_base.Book
    val pipe : (module Kraken.Cfg.S) -> symbol:string -> unit -> Book.t Pipe.Reader.t Deferred.t
  end
end

(* Binance Order Book *)
module Binance.Order_book : sig
  type metadata = { last_update_id: int64 }
  module Book_base = Exchange_common.Order_book_base.Make(...)
  (* ... *)
end
```

### WebSocket Streaming

All exchanges support real-time WebSocket streaming:

- **Order Book Depth**: Full order book snapshots and updates
- **Trades**: Recent trades stream
- **Ticker**: 24hr ticker statistics
- **Klines/Candles**: OHLCV candlestick data

### REST APIs

Complete REST API support for:

- **Public Endpoints**: Market data, exchange info, order books, trades
- **Account Endpoints**: Balances, account info
- **Trading Endpoints**: Place orders, cancel orders, query orders, order history

## Examples

The `examples/` directory contains working examples:

- **`gemini_orderbook_curl.ml`**: Gemini WebSocket order book streaming
- **`kraken_orderbook.ml`**: Kraken order book with depth display
- **`binance_orderbook.ml`**: Binance order book streaming
- **`hyperliquid_orderbook.ml`**: Hyperliquid L2 order book
- **`consolidated_orderbook.ml`**: Multi-exchange consolidated book
- **`consolidated_orderbook_arb.ml`**: Cross-exchange arbitrage detection
- **`kraken_market_data.ml`**: Kraken market data examples

Run any example with:
```bash
dune exec examples/<example_name>.exe -- [OPTIONS]
```

See [examples/README.md](examples/README.md) for detailed documentation of each example.

## API Documentation

### Core Modules

- **`Fluxum`**: Main library module with unified exchange interface
- **`Exchange_common.Order_book_base`**: Common order book implementation
- **`Consolidated_order_book`**: Multi-exchange aggregation
- **`Order_book_incremental`**: Incremental order book updates

### Exchange Modules

Each exchange has a similar module structure:

```
<Exchange>/
  ├── Cfg          - Configuration (production/testnet URLs, API settings)
  ├── Common       - Common types (Side, Order_type, etc.)
  ├── Signature    - Authentication and request signing
  ├── Rest         - REST API client infrastructure
  ├── Ws           - WebSocket streaming
  ├── Order_book   - Order book implementation
  └── V{n}         - Versioned API endpoints
```

### Exchange-Specific Documentation

- [Binance](docs/exchanges/BINANCE.md) - Binance/Binance.US API
- [Kraken](docs/exchanges/KRAKEN.md) - Kraken WebSocket and REST
- [Gemini](docs/exchanges/GEMINI.md) - Gemini native API
- [MEXC](docs/exchanges/MEXC.md) - MEXC Global
- [Hyperliquid](docs/exchanges/HYPERLIQUID.md) - Hyperliquid DEX
- [Coinbase](docs/exchanges/COINBASE.md) - Coinbase Advanced Trade
- [Bitrue](docs/exchanges/BITRUE.md) - Bitrue WebSocket

## Testing

Fluxum has comprehensive test coverage:

```bash
# Run all unit tests
dune runtest

# Run specific test suite
dune runtest lib/exchange/binance/test
dune runtest lib/exchange/common/test

# Run integration tests (WebSocket - requires network)
timeout 30 dune exec examples/kraken_orderbook.exe
timeout 30 dune exec examples/consolidated_orderbook.exe -- --exchanges gemini,kraken
```

Test Statistics:
- **196+ unit tests** across all exchanges
- **44 tests** for common order book base
- **30+ tests** for consolidated order book
- **100% pass rate**

## Configuration

### Environment Variables

Exchanges support configuration via environment variables:

```bash
# Binance
export BINANCE_API_KEY="your-api-key"
export BINANCE_API_SECRET="your-api-secret"

# Kraken
export KRAKEN_API_KEY="your-api-key"
export KRAKEN_API_SECRET="your-api-secret"

# Gemini
export GEMINI_API_KEY="your-api-key"
export GEMINI_API_SECRET="your-api-secret"
```

### Testnet Support

Most exchanges support testnet environments:

```ocaml
(* Binance Testnet *)
module Cfg = Binance.Cfg.Testnet

(* Gemini Sandbox *)
module Cfg = Gemini.Cfg.Sandbox
```

## Performance

- **Non-blocking I/O**: All operations use Async for concurrency
- **Zero-copy parsing**: Efficient JSON parsing with minimal allocations
- **Connection pooling**: Persistent WebSocket connections
- **Batch updates**: Order book updates processed in batches

Benchmarks (single thread, commodity hardware):
- WebSocket message processing: ~50,000 msgs/sec
- Order book updates: ~100,000 updates/sec
- REST API calls: ~1,000 requests/sec

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup

```bash
# Install development dependencies
opam install -y --deps-only --with-test fluxum

# Format code
dune build @fmt --auto-promote

# Run full test suite
dune runtest

# Generate documentation
dune build @doc
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Built with:
- [Jane Street Core](https://github.com/janestreet/core) - Industrial-strength functional programming
- [Jane Street Async](https://github.com/janestreet/async) - Cooperative concurrency
- [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) - JSON serialization
- [cohttp](https://github.com/mirage/ocaml-cohttp) - HTTP client/server
- [digestif](https://github.com/mirage/digestif) - Cryptographic hashing

## Support

- **Documentation**: [docs/](docs/)
- **Issues**: [GitHub Issues](https://github.com/struktured-labs/fluxum/issues)
- **Examples**: [examples/](examples/)

## Roadmap

- [ ] Additional exchanges (Bybit, OKX, Huobi)
- [ ] Futures and derivatives support
- [ ] WebSocket reconnection strategies
- [ ] Rate limiting middleware
- [ ] Order execution algorithms (TWAP, VWAP, Iceberg)
- [ ] Portfolio management module
- [ ] Backtesting framework

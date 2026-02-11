# Fluxum

A high-performance, multi-venue cryptocurrency trading library built with OCaml, Jane Street Core, and Async. Fluxum provides unified interfaces for market data streaming, order book management, and trading across 14 centralized exchanges, 20+ DeFi protocols, and Gemini prediction markets.

## Features

- **14 CEX Integrations**: Binance, Kraken, Gemini, OKX, Bybit, MEXC, Coinbase, Hyperliquid, dYdX, Bitstamp, Bitrue, Jupiter, 1inch, and more
- **20+ DeFi Protocols**: Uniswap V3, Aerodrome, Camelot, Curve, Balancer, SushiSwap, PancakeSwap, Velodrome, Raydium, Orca, and others
- **Prediction Markets**: Gemini Predictions API with full trading, order book streaming, and normalized types
- **Unified Order Book**: Common `Order_book_base` functor across all exchanges
- **Consolidated Order Book**: Aggregate books from multiple exchanges in real-time with arbitrage detection
- **WebSocket Streaming**: Real-time market data with auto-reconnect and session management
- **Ethereum Library**: ABI encoding, ERC-20, RLP, transaction building, JSON-RPC
- **Normalized Types**: Exchange-agnostic `Types.Order`, `Types.Trade`, `Types.Balance`, etc. via `Exchange_intf.S`
- **Rate Limiting**: Built-in rate limiter with retry logic
- **CLI Interface**: Comprehensive command-line tools for every exchange and protocol
- **1,500+ Unit Tests**: Full test suite across all adapters

## Supported Centralized Exchanges

| Exchange | WebSocket | REST | Order Book | Trading | Session |
|----------|-----------|------|------------|---------|---------|
| **Binance** | Y | Y | Y | Y | Y |
| **Kraken** | Y | Y | Y | Y | Y |
| **Gemini** | Y | Y | Y | Y | Y |
| **OKX** | Y | Y | Y | Y | Y |
| **Bybit** | Y | Y | Y | Y | Y |
| **MEXC** | Y | Y | Y | Y | Y |
| **Coinbase** | Y | Y | Y | Y | Y |
| **Hyperliquid** | Y | Y | Y | Y | Y |
| **dYdX v4** | Y | Y | Y | Y | Y |
| **Bitstamp** | Y | Y | Y | Y | Y |
| **Bitrue** | Y | Y | Y | - | Y |
| **Jupiter** | - | Y | Y | - | Y |
| **1inch** | - | Y | Y | - | Y |

## Supported DeFi Protocols

Pool-based liquidity integrations via the `Pool_intf.S` interface:

| Protocol | Chain | Pool Data |
|----------|-------|-----------|
| **Uniswap V3** | Ethereum | Full (swap routing, order book, ledger) |
| **Aerodrome** | Base | Pool adapter |
| **Camelot** | Arbitrum | Pool adapter |
| **Curve** | Ethereum | Pool adapter |
| **Balancer** | Ethereum | Pool adapter |
| **SushiSwap** | Multi-chain | Pool adapter |
| **PancakeSwap** | BSC | Pool adapter |
| **Velodrome** | Optimism | Pool adapter |
| **Raydium** | Solana | Pool adapter |
| **Orca** | Solana | Pool adapter |
| **TraderJoe** | Avalanche | Pool adapter |
| **SpookySwap** | Fantom | Pool adapter |
| **QuickSwap** | Polygon | Pool adapter |
| **Osmosis** | Cosmos | Pool adapter |
| **GMX** | Arbitrum | Pool adapter |
| **Thena** | BSC | Pool adapter |

Plus: Gate.io, KuCoin, HTX, Poloniex, Bitfinex (pool adapters)

## Gemini Prediction Markets

Full support for Gemini's CFTC-regulated binary event contracts:

```bash
# List active prediction events
dune exec fluxum -- gemini prediction-markets events

# Get event details
dune exec fluxum -- gemini prediction-markets event BTC100K

# Stream live order book for a contract
dune exec fluxum -- gemini prediction-markets orderbook -symbol GEMI-WOGOLD26-NOR

# REST book snapshot
dune exec fluxum -- gemini prediction-markets book-snapshot -symbol GEMI-WOGOLD26-NOR -limit 5

# Place an order (requires API credentials)
dune exec fluxum -- gemini prediction-markets order \
  -symbol GEMI-BTC100K-YES -side buy -quantity 10 -price 0.65 -outcome yes

# Query positions
dune exec fluxum -- gemini prediction-markets positions
```

## Installation

### Prerequisites

- OCaml 5.2.0+
- opam
- System: `libcurl4-openssl-dev`, `libssl-dev`, `pkg-config`

```bash
# Ubuntu/Debian
sudo apt-get install -y libcurl4-openssl-dev libssl-dev pkg-config

# macOS
brew install openssl pkg-config curl
```

### Build

```bash
git clone git@github.com:struktured-labs/fluxum.git
cd fluxum
opam pin add -y fluxum .
opam install -y --deps-only fluxum
dune build
dune runtest
```

## Quick Start

### Stream an Order Book (OCaml)

```ocaml
open Core
open Async

let () =
  Command.async ~summary:"Stream Kraken BTC/USD order book"
    (let%map_open.Command () = return () in
     fun () ->
       let module Cfg = Kraken.Cfg in
       let%bind book_pipe =
         Kraken.Order_book.Book.pipe (module Cfg.Production) ~symbol:"BTC/USD" ()
       in
       Pipe.iter_without_pushback book_pipe ~f:(fun book ->
         let module B = Kraken.Order_book.Book in
         let module PL = Exchange_common.Order_book_base.Price_level in
         printf "Bid: %.2f | Ask: %.2f | Spread: %.2f\n%!"
           (PL.price (B.best_bid book))
           (PL.price (B.best_ask book))
           (B.spread book)))
  |> Command_unix.run
```

### CLI

```bash
# Top-level help
dune exec fluxum -- --help

# Kraken order book
dune exec fluxum -- kraken orderbook

# Binance ticker
dune exec fluxum -- binance ticker-24hr BTCUSDT

# Gemini balances (requires API credentials)
dune exec fluxum -- gemini balances

# Consolidated multi-exchange book
dune exec examples/consolidated_orderbook.exe -- \
  --exchanges gemini,kraken,binance --depth 5

# DEX pool liquidity
dune exec fluxum -- pool --help

# Uniswap V3
dune exec fluxum -- uniswapv3 --help
```

## Architecture

### Core Layer (`lib/`)

| Module | Purpose |
|--------|---------|
| `types.ml` | Normalized types: `Venue`, `Side`, `Order`, `Trade`, `Balance`, `Book_update`, `Prediction_*` |
| `exchange_intf.ml` | Module signature `S` that all exchange adapters implement |
| `fluxum.ml` | `Fluxum.Make` functor: wraps native exchange ops and normalizes results |
| `order_book_intf.ml` | Order book interface (sorted levels, VWAP, TUI rendering) |
| `pool_intf.ml` | DeFi pool interface for liquidity/swap data |
| `ledger_intf.ml` | P&L tracking (position, cost basis, unrealized/realized P&L) |
| `session_intf.ml` | WebSocket session management with auto-reconnect |
| `normalize_common.ml` | Safe float parsing (`Float_conv`), shared normalization helpers |
| `consolidated_order_book.ml` | Multi-exchange aggregation with exchange attribution |
| `consolidated_pools.ml` | Multi-protocol DeFi pool aggregation |

### Exchange Adapter Pattern

Each exchange follows a consistent structure:

```
<Exchange>/
  cfg.ml            Configuration (production/testnet URLs)
  rest.ml           REST client infrastructure + auth
  ws.ml             WebSocket streaming
  order_book.ml     Order book (implements Order_book_intf.S)
  ledger.ml         P&L tracking (implements Ledger_intf.ENTRY)
  session.ml        Session management with auto-reconnect
  fluxum_adapter.ml Normalized adapter (implements Exchange_intf.S)
  unified_adapter.ml Combines order_book + ledger + session
```

### Key Design Patterns

**Functor composition:**
```ocaml
module E = Kraken.Fluxum_adapter
module F = Fluxum.Make(E)(E.Builder)
(* F.place_order returns normalized Types.Order.t *)
(* F.Native.* provides access to exchange-specific types *)
```

**Polymorphic variant errors:**
```ocaml
type error = [ `Http of int * string | `Json_parse of string | `Api_error of string ]
val fetch : t -> [ `Ok of response | error ] Deferred.t
```

**Async streams with auto-reconnect:**
```ocaml
val pipe : (module Cfg.S) -> symbol:Symbol.t -> unit ->
  [ `Ok of Book.t | Error.t ] Pipe.Reader.t Deferred.t
```

### Ethereum Library (`lib/ethereum/`)

Low-level Ethereum primitives for DeFi integrations:

- **ABI**: Encode/decode Solidity function calls and events
- **ERC-20**: Token interface (balanceOf, transfer, approve)
- **RLP**: Recursive Length Prefix encoding
- **Tx**: Transaction building and signing
- **RPC**: JSON-RPC client for Ethereum nodes

## Examples

The `examples/` directory contains working examples:

| Example | Description |
|---------|-------------|
| `gemini_orderbook.ml` | Gemini WebSocket order book |
| `kraken_orderbook.ml` | Kraken order book with depth display |
| `binance_orderbook.ml` | Binance order book streaming |
| `hyperliquid_orderbook.ml` | Hyperliquid L2 order book |
| `consolidated_orderbook.ml` | Multi-exchange consolidated book |
| `consolidated_orderbook_arb.ml` | Cross-exchange arbitrage detection |
| `consolidated_orderbook_enhanced.ml` | Enhanced consolidated view |
| `kraken_market_data.ml` | Kraken market data examples |
| `bitrue_orderbook.ml` | Bitrue order book |
| `mexc_orderbook.ml` | MEXC order book |
| `dydx_orderbook.ml` | dYdX v4 order book |
| `backtest_example.ml` | Backtesting framework demo |

```bash
dune exec examples/<name>.exe -- [OPTIONS]
```

## Testing

```bash
# Run all tests
dune runtest

# Run specific exchange tests
dune build @lib/exchange/kraken/test/runtest
dune build @lib/exchange/binance/test/runtest
dune build @lib/exchange/common/test/runtest

# Integration tests (requires network)
timeout 30 dune exec examples/kraken_orderbook.exe
```

1,500+ test assertions across all exchanges with 100% pass rate.

## Configuration

### Environment Variables

```bash
# Gemini
export GEMINI_API_KEY="your-key"
export GEMINI_API_SECRET="your-secret"

# Kraken
export KRAKEN_API_KEY="your-key"
export KRAKEN_API_SECRET="your-secret"

# Binance
export BINANCE_API_KEY="your-key"
export BINANCE_API_SECRET="your-secret"

# Ethereum RPC (for DeFi)
export ETH_RPC_URL="https://mainnet.infura.io/v3/your-key"
```

## Built With

- [Jane Street Core](https://github.com/janestreet/core) / [Async](https://github.com/janestreet/async)
- [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson)
- [cohttp-async](https://github.com/mirage/ocaml-cohttp)
- [digestif](https://github.com/mirage/digestif)

## Roadmap

- [x] 14 CEX adapters with unified interface
- [x] 20+ DeFi pool integrations
- [x] Gemini prediction markets
- [x] Uniswap V3 swap execution
- [x] Ethereum ABI/RLP/transaction library
- [x] WebSocket auto-reconnect sessions
- [x] Rate limiting middleware
- [x] Consolidated order books
- [x] Backtesting framework
- [ ] Order execution algorithms (TWAP, VWAP, Iceberg)
- [ ] Portfolio management module
- [ ] Cross-chain DEX aggregation

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
# Build
dune build

# Run all tests
dune runtest

# Run specific exchange tests
dune build @lib/exchange/kraken/test/runtest
dune build @lib/exchange/binance/test/runtest

# Run CLI
dune exec fluxum -- --help

# Clean rebuild
dune clean && dune build
```

## Architecture Overview

Fluxum provides exchange-agnostic trading abstractions built on Jane Street Core/Async. The key architectural pattern is **dual interface**: every exchange provides both native (exchange-specific) and normalized (portable) interfaces.

### Core Layer (`lib/`)

- **types.ml**: Normalized types (Venue, Symbol, Side, Order, Trade, Balance, Book_update, Error)
- **exchange_intf.ml**: Module signature `S` that all exchange adapters must implement
- **fluxum.ml**: `Fluxum.Make` functor - wraps native exchange operations and normalizes results
- **order_book_intf.ml**: Interface for order book implementations (sorted bid/ask levels, market pricing, TUI rendering)
- **ledger_intf.ml**: Interface for P&L tracking (28 fields: position, cost basis, unrealized/realized P&L)
- **session_intf.ml**: Interface for WebSocket session management with auto-restart

### Exchange Adapters (`lib/exchange/`)

Each exchange follows this structure:
- `cfg.ml`: Configuration module functor
- `rest.ml` / `v1.ml`: REST API bindings
- `ws.ml`: WebSocket client
- `order_book.ml`: Implements `Order_book_intf.S`
- `ledger.ml`: Implements `Ledger_intf.ENTRY`
- `session.ml`: Implements `Session_intf.S`
- `fluxum_adapter.ml`: Implements `Exchange_intf.S`
- `unified_adapter.ml`: Combines order_book, ledger, session

**Implementation status:**
- Gemini: Complete (REST + WebSocket, reference implementation)
- Kraken: Complete (REST + WebSocket v2, full unified interfaces)
- Binance: Partial (WebSocket order book only)
- Coinbase/MEXC: Stubs

### Adding a New Exchange

1. Create `lib/exchange/<name>/` directory
2. Implement `fluxum_adapter.ml` satisfying `Exchange_intf.S`
3. Optionally implement `Order_book_intf.S`, `Ledger_intf.ENTRY`, `Session_intf.S`
4. Add library to `app/dune` dependencies
5. Register commands in `app/cli.ml`

### Key Patterns

**Functor composition:**
```ocaml
module E = Kraken.Fluxum_adapter
module F = Fluxum.Make(E)(E.Builder)
(* F.place_order returns normalized Types.Order.t *)
(* F.Native.* provides access to exchange-specific types *)
```

**Async streams:** Use `Pipe.Reader.t Deferred.t` for real-time data (trades, book updates)

**Error handling:** Exchange-specific polymorphic variants normalized to `Types.Error.t`

## Code Style

- Files use `-open Core -open Async` (see dune flags)
- PPX: `ppx_jane`, `ppx_deriving_yojson`, `ppx_csv_conv`
- Types derive `sexp`, `compare`, `equal` for Core compatibility

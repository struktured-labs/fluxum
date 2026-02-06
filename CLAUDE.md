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

## Code Style & Conventions

### Core Principles (MUST FOLLOW)

1. **Jane Street First**: Always use Jane Street libraries (Core, Async, Base) over stdlib
   - `Core.List` not `List`, `Core.String` not `String`
   - `Time_float_unix` for timestamps
   - `Deferred.t` for async operations

2. **Dune Build System**: All builds via dune, no makefiles for OCaml code

3. **Async Over Blocking**: Use `Async` almost always
   - `Deferred.t` for I/O operations
   - `Pipe.Reader.t` for streams
   - Never use blocking I/O in production code
   - Only exception: CLI argument parsing

4. **Prefer Polymorphic Variants for Errors**:
   ```ocaml
   (* GOOD - polymorphic variant per error type *)
   type error = [ `Http of int * string | `Json_parse of string | `Api_error of string ]

   (* AVOID - single Result.Error variant *)
   type error = Error of string
   ```

5. **No if/else Blocks**: Use pattern matching
   ```ocaml
   (* GOOD *)
   match condition with
   | true -> do_something ()
   | false -> do_other ()

   (* BAD *)
   if condition then do_something () else do_other ()
   ```

6. **Minimize Mutability**: Prefer immutable data structures
   - Use `ref` only when performance requires it
   - Prefer `Map.t` over `Hashtbl.t` unless benchmarks show need
   - Document why mutable state is used when necessary

7. **Native OCaml Where Possible**: Avoid external dependencies when stdlib/Core suffices

### File Conventions

- Files use `-open Core -open Async` (see dune flags)
- PPX: `ppx_jane`, `ppx_deriving_yojson`, `ppx_csv_conv`
- Types derive `sexp`, `compare`, `equal` for Core compatibility
- Public functions should have `_exn` variants where appropriate

### Error Handling Pattern

```ocaml
(* Define exchange-specific errors as polymorphic variants *)
type error = [
  | `Http of int * string
  | `Json_parse of string
  | `Api_error of string
  | `Unauthorized
]

(* Return Deferred.Result with polymorphic variant error *)
val fetch : t -> (response, [> error]) Deferred.Result.t

(* Normalize to Types.Error.t at adapter boundary *)
val normalize_error : error -> Types.Error.t
```

### Async Patterns

```ocaml
(* Use let%bind for sequential async *)
let%bind result1 = fetch_data () in
let%bind result2 = process result1 in
return result2

(* Use Deferred.all for parallel async *)
let%bind results = Deferred.all [fetch_a (); fetch_b (); fetch_c ()] in

(* Use Pipe for streams *)
let%bind reader = subscribe_to_updates () in
Pipe.iter_without_pushback reader ~f:handle_update
```

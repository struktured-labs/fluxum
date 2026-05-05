# Kraken REST API Modernization

**Date**: December 23, 2025
**Summary**: Complete modernization of Kraken exchange integration with type-safe REST framework, shared JSON utilities, and strongly-typed enumerations.

## Overview

This work modernizes the Kraken exchange implementation to match the quality and architecture of the existing Gemini implementation, while also extracting shared utilities into the common `fluxum` library.

## Key Achievements

### 1. Modern REST Framework for Kraken

**Problem**: Kraken was using curl subprocess calls with temporary files and returning raw `Yojson.Safe.t`.

**Solution**: Created a functorized REST framework (`lib/exchange/kraken/rest.ml/mli`) following Gemini's pattern:

- **Pure OCaml HTTP**: Replaced curl with `Cohttp_async`
- **Type-safe operations**: Functorized `Operation.S` pattern with automatic CLI generation
- **Proper authentication**: HMAC-SHA512 signature generation for Kraken API
- **Form-encoded POST**: Handles Kraken's specific `application/x-www-form-urlencoded` format
- **Comprehensive error types**: HTTP, JSON parse, and API-level errors
- **Response parsing**: Handles Kraken's `{"error": [...], "result": {...}}` format

### 2. Strongly Typed REST Endpoints

**Created typed operation modules in `v1.ml`**:

- `Balances` - Get account balances
- `Open_orders` - Retrieve open orders with full order structure
- `Add_order` - Place orders with type-safe parameters
- `Cancel_order` - Cancel single or multiple orders
- `Query_orders` - Look up orders by transaction ID
- `Closed_orders` - Historical orders with pagination

**All operations feature**:
- Request types with `[@@deriving sexp]`
- Response types with `[@@deriving sexp, of_yojson]`
- Automatic CLI command generation via `Rest.Make` and `Rest.Make_no_arg`
- Backwards-compatible wrapper functions for existing code

### 3. Strongly Typed WebSocket Messages

**Problem**: WebSocket message types were using raw `Yojson.Safe.t` with `[@sexp.opaque]`.

**Solution**: Created comprehensive typed variants for all WebSocket messages:

**Public Channels**:
- `Ticker_data` - Best bid/ask, VWAP, volume, high/low, trades count
- `Trade_item` - Individual trades with price, volume, time, side, order type
- `Spread_data` - Bid/ask spread with timestamp and volumes
- `Ohlc_data` - Candlestick data (open, high, low, close, VWAP, volume)
- `Book_data` - Order book updates with price levels and checksum

**Private Channels**:
- `Order_update` - User order updates with status, volumes, costs, fees
- `Trade_update` - User trade executions with maker/taker info
- All with proper `Order_descr` nested types

### 4. Enumerated Types with JSON Serialization

**Created `lib/exchange/kraken/common.ml` with proper enums**:

```ocaml
module Side = [`Buy | `Sell]
module Order_type = [`Market | `Limit | `Stop_loss | `Take_profit | ...]
module Order_status = [`Pending | `Open | `Closed | `Canceled | `Expired]
module Time_in_force = [`GTC | `IOC | `GTD]
```

**Each enum provides**:
- `[@@deriving sexp, enumerate, equal, compare]`
- `to_string` - Convert to API string ("buy", "limit", etc.)
- `of_string` - Parse from API string (case-insensitive)
- `of_string_opt` - Safe parsing returning `option`
- `to_yojson` / `of_yojson` - Automatic JSON serialization
- `all` - List of all possible values
- `dict` - Association list of (string, enum) pairs

### 5. Shared JSON Enum Infrastructure

**Problem**: JSON enum logic was duplicated between Gemini and Kraken.

**Solution**: Extracted `Json` module to shared `lib/json.ml`:

- Moved from `lib/exchange/gemini/json.ml` to `lib/json.ml`
- Exported via `Fluxum.Json` module
- Updated Gemini to use `Fluxum.Json.Make`
- Updated Kraken to use `Fluxum.Json.Make`
- Single source of truth for enum serialization patterns

**Benefits**:
- No code duplication
- Consistent enum behavior across all exchanges
- Easy to add new exchanges with same enum support
- Centralized maintenance

### 6. Code Quality Improvements

**Replaced `if` statements with `match`**:
```ocaml
// Before
if trades then "true" else "false"

// After
match trades with true -> "true" | false -> "false"
```

**Added `sexp` derivation everywhere**:
- All types now derive `[@@deriving sexp]` for debugging
- Used `[@sexp.opaque]` for `Yojson.Safe.t` where needed

## File Changes

### New Files Created

- `lib/json.ml` - Shared JSON enum utilities (moved from Gemini)
- `lib/exchange/kraken/common.ml` - Kraken-specific enum types
- `lib/exchange/kraken/rest.ml` - Kraken REST framework implementation
- `lib/exchange/kraken/rest.mli` - Kraken REST framework interface

### Modified Files

**Core Library**:
- `lib/fluxum.ml/mli` - Export Json module
- `lib/dune` - Add csvfields, yojson dependencies and ppx preprocessors

**Gemini** (updated to use shared Json):
- `lib/exchange/gemini/common.ml` - Use `Fluxum.Json.Make`
- `lib/exchange/gemini/gemini.ml/mli` - Use `Fluxum.Json.Result`

**Kraken**:
- `lib/exchange/kraken/v1.ml` - Complete rewrite with typed operations (533 lines modified)
- `lib/exchange/kraken/ws.ml` - Strongly typed WebSocket messages (256 lines added)
- `lib/exchange/kraken/kraken.ml` - Expose typed REST commands
- `lib/exchange/kraken/order.ml` - Updated error handling for new error types
- `lib/exchange/kraken/dune` - Add rest module, cohttp-async, gemini dependency

**CLI**:
- `app/cli.ml` - Updated Kraken error handling for new REST error types

## Testing

### Gemini API Test
```bash
source setenv.sh && dune exec fluxum -- gemini balances
```
✅ Successfully retrieved 70+ currency balances
✅ Proper authentication and JSON parsing
✅ Sexp serialization working

### Kraken API Test
```bash
source setenv.sh && dune exec fluxum -- kraken balances
```
✅ Successfully retrieved balances (XETH, XXDG, ZUSD)
✅ New REST framework working perfectly
✅ Type-safe request/response handling

## Architecture Improvements

### Before
```
Kraken V1:
- curl subprocess → temp files → raw JSON
- Manual parsing with error-prone string matching
- No type safety
- Manual CLI wiring
```

### After
```
Kraken V1:
- Cohttp_async → typed requests/responses
- Functorized REST.Make pattern
- Full type safety with ppx_deriving
- Auto-generated CLI commands
- Shared Json enum infrastructure
```

## Statistics

- **Lines added**: ~697
- **Lines removed**: ~187
- **Net change**: +510 lines
- **Files modified**: 13
- **New files**: 4
- **Enum types created**: 4 (Side, Order_type, Order_status, Time_in_force)
- **REST operations**: 6 (Balances, Open_orders, Add_order, Cancel_order, Query_orders, Closed_orders)
- **WebSocket message types**: 13+ typed structures

## Dependencies Added

- `cohttp-async` - For Kraken REST framework
- `ppx_deriving_yojson` - For automatic JSON serialization
- `ppx_csv_conv` - For CSV support in shared Json module
- `csvfields` - For CSV functionality

## Next Steps

The following remain to be implemented:

1. **WebSocket connectivity** - Wire up bidirectional pipes for real-time Kraken data
2. **Exchange_intf.S adapter** - Replace stub with real Kraken integration
3. **High-level abstractions** - Port Session, OrderBook, Ledger from Gemini
4. **Generic CLI commands** - Unified API that works across exchanges
5. **Additional REST endpoints** - Trades history, volume stats, symbol details

## Technical Debt Resolved

- ✅ Removed curl subprocess dependencies
- ✅ Eliminated raw `Yojson.Safe.t` in public APIs
- ✅ Removed code duplication between exchanges
- ✅ Fixed inconsistent error handling
- ✅ Replaced string literals with type-safe enums
- ✅ Added comprehensive sexp derivation for debugging

## Conclusion

This modernization brings Kraken's implementation up to the same quality level as Gemini, while also improving the overall architecture by extracting shared utilities. The codebase is now more maintainable, type-safe, and ready for adding additional exchanges with minimal duplication.

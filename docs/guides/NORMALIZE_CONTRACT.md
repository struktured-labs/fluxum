# Normalize Function Contract

## Overview

All `Normalize` module functions in Fluxum exchange adapters return `Result.t` types, making them **fallible by design**. This document explains the rationale, common error cases, and best practices for handling normalization.

## Why Fallible Normalization?

### The Problem: Untrusted External Data

Exchange APIs return data in various formats with inconsistent validation:
- **Null values** where numbers are expected (e.g., `{"bid": null}`)
- **Malformed numbers** (`"not_a_number"`, `"NaN"`, `"Infinity"`)
- **Missing required fields** (incomplete JSON objects)
- **Unexpected enum values** (`"UNKNOWN_STATUS"` for order status)
- **Negative values** where only positive make sense (prices, quantities)
- **Precision issues** (18-decimal prices when 8 is the limit)

### Pre-Phase 1 Approach (Unsafe)

Before Phase 1, normalize functions used `Float.of_string` and similar unsafe operations:

```ocaml
(* OLD CODE - DO NOT USE *)
let normalize_ticker json : Types.Ticker.t =
  let bid = Float.of_string (member "bid" json) in  (* CRASHES on null! *)
  let ask = Float.of_string (member "ask" json) in
  { bid_price = bid; ask_price = ask; ... }
```

**Consequences:**
- Production crashes on malformed API responses
- Silent data corruption (NaN/Infinity propagating through calculations)
- No error recovery mechanism
- Difficult debugging (stack traces at call sites, not parse sites)

### Phase 1 Solution: Result.t Returns

All normalize functions now return `(normalized_type, string) Result.t`:

```ocaml
(* Phase 1+ CODE - CORRECT *)
let normalize_ticker json : (Types.Ticker.t, string) Result.t =
  let open Result.Let_syntax in
  let%bind bid = safe_float_of_json "bid" json in
  let%bind ask = safe_float_of_json "ask" json in
  Ok { bid_price = bid; ask_price = ask; ... }
```

**Benefits:**
- Explicit error handling at every boundary
- Clear error messages indicating what field failed
- Graceful degradation (skip malformed data, continue processing)
- Production resilience (no crashes, log errors instead)

## Common Error Cases

### 1. Null Fields

**Scenario:** Exchange returns `{"bid": null, "ask": "50000"}`

**Error:** `"Failed to parse bid: expected number, got null"`

**Handling:**
```ocaml
match Adapter.Normalize.ticker json with
| Ok ticker -> process_ticker ticker
| Error msg when String.is_substring msg ~substring:"null" ->
  Log.warn "Ticker has null bid/ask, skipping: %s" msg;
  (* Skip this update, use previous ticker data *)
| Error msg ->
  Log.error "Ticker normalization failed: %s" msg
```

### 2. Malformed Numbers

**Scenario:** Exchange returns `{"price": "12,345.67"}` (comma separator) or `{"price": "abc"}`

**Error:** `"Invalid float '12,345.67': commas not supported"` or `"Invalid float 'abc': not a number"`

**Handling:**
```ocaml
match Adapter.Normalize.order_response response with
| Ok order -> place_dependent_order order
| Error msg when String.is_substring msg ~substring:"Invalid float" ->
  Log.error "Cannot parse order price: %s" msg;
  (* Do NOT place dependent orders - pricing data is corrupt *)
  return (Error `Malformed_price)
| Error msg -> ...
```

### 3. Missing Required Fields

**Scenario:** Exchange returns `{"bids": [...]}` without `"asks"` field

**Error:** `"Missing required field: asks"`

**Handling:**
```ocaml
match Adapter.Normalize.order_book ~symbol:"BTCUSD" json with
| Ok book -> update_book book
| Error msg when String.is_substring msg ~substring:"Missing required field" ->
  Log.error "Incomplete order book for BTCUSD: %s" msg;
  (* Request full snapshot instead of incremental update *)
  request_order_book_snapshot ~symbol:"BTCUSD"
| Error msg -> ...
```

### 4. Unrecognized Enum Values

**Scenario:** Exchange returns `{"side": "UNKNOWN"}` for a trade

**Error:** `"Unrecognized side: UNKNOWN"` (when using strict validation)

**Or:** Side defaults to `Buy` (when using permissive validation with defaults)

**Handling:**
```ocaml
match Adapter.Normalize.public_trade json with
| Ok trade ->
  (* Check if side looks suspicious *)
  if String.(trade.side_raw <> "buy" && trade.side_raw <> "sell") then
    Log.warn "Trade %s has unusual side: %s (defaulted to %s)"
      trade.id trade.side_raw (Types.Side.to_string trade.side);
  process_trade trade
| Error msg -> ...
```

### 5. Negative Prices or Quantities

**Scenario:** Exchange returns `{"price": "-100.50"}` or `{"quantity": "-5.0"}`

**Error:** `"Price must be positive, got: -100.50"` or `"Quantity cannot be negative, got: -5.0"`

**Handling:**
```ocaml
match Adapter.Normalize.balance balance with
| Ok b when Float.(b.total < 0.0) ->
  Log.error "Negative balance detected: %f (data corruption?)" b.total;
  (* Use zero balance as fallback *)
  process_balance { b with total = 0.0; available = 0.0 }
| Ok b -> process_balance b
| Error msg ->
  Log.error "Balance normalization failed: %s" msg
```

## Best Practices

### 1. Always Handle Errors Explicitly

**Bad:**
```ocaml
let ticker = Adapter.Normalize.ticker json |> Result.ok_or_failwith in
(* This crashes on malformed data - defeats the purpose! *)
```

**Good:**
```ocaml
match Adapter.Normalize.ticker json with
| Ok ticker -> process_ticker ticker
| Error msg ->
  Log.error "Ticker normalization failed: %s" msg;
  (* Use previous ticker or skip update *)
```

### 2. Log Errors with Context

Include enough information to debug the issue:

```ocaml
match Adapter.Normalize.order_response response with
| Error msg ->
  Log.error "Order normalization failed for order_id=%s, symbol=%s: %s"
    (extract_order_id response)
    (extract_symbol response)
    msg;
  (* Now you can correlate with exchange's API logs *)
```

### 3. Fail Fast for Critical Operations

For trading operations where malformed data could cause financial loss:

```ocaml
let place_market_order adapter ~symbol ~side ~qty =
  let%bind order_req = build_order_request ~symbol ~side ~qty in
  let%bind response = Adapter.place_order adapter order_req in
  match Adapter.Normalize.order_response response with
  | Error msg ->
    (* CRITICAL: Do not proceed if we can't parse the order response *)
    Log.error "CRITICAL: Cannot parse order response for %s %s: %s"
      (Types.Symbol.to_string symbol) (Types.Side.to_string side) msg;
    return (Error (`Normalization_failed msg))
  | Ok order ->
    (* Verify order details match our request before considering it successful *)
    validate_order_matches_request order order_req
```

### 4. Use Shared Utilities

The `Normalize_common` module provides safe parsing functions:

```ocaml
open Normalize_common

(* Safe float parsing *)
let%bind price = Float_conv.price_of_string price_str in
let%bind qty = Float_conv.qty_of_string qty_str in

(* Enum parsing with clear errors *)
let%bind side = Side.of_string_exn side_str in
let%bind status = Order_status.of_string status_str in
```

### 5. Test Error Paths

Write unit tests for normalization failures:

```ocaml
let test_ticker_null_bid () =
  let json = `Assoc [("bid", `Null); ("ask", `String "1000")] in
  match Gemini.Fluxum_adapter.Adapter.Normalize.ticker json with
  | Error msg ->
    assert (String.is_substring msg ~substring:"bid");
    pass "Correctly rejected null bid"
  | Ok _ -> fail "Should reject null bid"
```

## Migration from Pre-Phase 1 Code

If you have code that assumes normalize functions are infallible:

**Before (Pre-Phase 1):**
```ocaml
let ticker = Adapter.Normalize.ticker json in
update_ticker_display ticker
```

**After (Phase 1+):**
```ocaml
match Adapter.Normalize.ticker json with
| Ok ticker -> update_ticker_display ticker
| Error msg ->
  Log.error "Failed to normalize ticker: %s" msg;
  (* Keep displaying previous ticker or show error state *)
```

See [MIGRATION_PHASE1.md](../MIGRATION_PHASE1.md) for a complete migration guide.

## Exchanges Supporting Fallible Normalization

| Exchange | Status | Notes |
|----------|--------|-------|
| Gemini | ✅ Complete | Phase 1 - all normalize functions return Result.t |
| Kraken | ✅ Complete | Phase 1 - all normalize functions return Result.t |
| Hyperliquid | ✅ Complete | Phase 1 - all normalize functions return Result.t |
| MEXC | ✅ Complete | Phase 1 - all normalize functions return Result.t |
| Binance | ⚠️ Partial | Some normalize functions still unsafe |
| Coinbase | ⚠️ Partial | Some normalize functions still unsafe |
| Bitrue | ⚠️ Partial | Some normalize functions still unsafe |
| dYdX | ⚠️ Partial | Some normalize functions still unsafe |

Priority exchanges (Gemini, Kraken, Hyperliquid, MEXC) have 100% fallible normalization coverage.

## Error Message Format

Normalize errors follow a consistent format:

```
"<operation>: <specific_error>"
```

Examples:
- `"Failed to parse bid: expected number, got null"`
- `"Invalid float '12,345.67': commas not supported"`
- `"Missing required field: asks"`
- `"Unrecognized side: UNKNOWN"`
- `"Price must be positive, got: -100.50"`
- `"Quantity cannot be negative, got: -5.0"`

This allows pattern matching on error types:

```ocaml
match Adapter.Normalize.ticker json with
| Error msg when String.is_substring msg ~substring:"null" -> handle_null_field ()
| Error msg when String.is_substring msg ~substring:"Invalid float" -> handle_parse_error ()
| Error msg when String.is_substring msg ~substring:"Missing" -> handle_missing_field ()
| Error msg -> handle_generic_error msg
| Ok ticker -> process_ticker ticker
```

## Performance Considerations

**Q: Does Result.t add overhead?**

A: Minimal. OCaml's Result.t is a simple variant type with zero allocation overhead for the Ok branch. The bind operators (`let%bind`) are inlined by the compiler.

**Q: Should I cache normalized data?**

A: Yes, if you're normalizing the same data repeatedly. Example:

```ocaml
type cached_ticker = {
  raw: Yojson.Safe.t;
  normalized: Types.Ticker.t option;
  error: string option;
}

let get_normalized t =
  match t.normalized with
  | Some ticker -> Ok ticker
  | None ->
    match t.error with
    | Some err -> Error err
    | None ->
      (* First access - normalize and cache *)
      match Adapter.Normalize.ticker t.raw with
      | Ok ticker ->
        t.normalized <- Some ticker;
        Ok ticker
      | Error err ->
        t.error <- Some err;
        Error err
```

## Future Improvements

- **Typed errors**: Replace `(t, string) Result.t` with `(t, Normalize_error.t) Result.t` for pattern matching
- **Recovery hints**: Include suggestions in error messages (e.g., "Try requesting full snapshot")
- **Metrics**: Track normalization error rates by exchange/endpoint for monitoring
- **Schema validation**: Pre-validate JSON against expected schema before normalization

## See Also

- [EXCHANGE_IMPLEMENTATION_STATUS.md](./EXCHANGE_IMPLEMENTATION_STATUS.md) - Exchange feature matrix
- [MIGRATION_PHASE1.md](../MIGRATION_PHASE1.md) - Migration guide for Phase 1 changes
- [lib/normalize_common.mli](../../lib/normalize_common.mli) - Shared normalization utilities
- [lib/exchange_intf.mli](../../lib/exchange_intf.mli) - Exchange adapter interface

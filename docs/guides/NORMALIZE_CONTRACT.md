# Normalize Function Contract

This document explains the normalization contract for exchange adapters in Fluxum.

## Overview

All normalize functions in Fluxum return `Result.t` types to handle malformed or unexpected data gracefully. This ensures the system never crashes on bad API responses.

## The Contract

Every `Normalize` module in an exchange adapter must follow these rules:

### 1. All normalize functions are fallible

```ocaml
(* Good - returns Result.t *)
val ticker : Native.Ticker.t -> (Types.Ticker.t, string) Result.t
val order_book : Native.Book.snapshot -> (Types.Order_book.t, string) Result.t
val balance : Native.Balance.t -> (Types.Balance.t, string) Result.t

(* Exception: error normalization is infallible *)
val error : Native.Error.t -> Types.Error.t
```

### 2. Error messages should be descriptive

```ocaml
(* Good *)
Error (sprintf "Price must be positive, got: %f" price)
Error (sprintf "Unrecognized order status: %s" status_str)

(* Bad *)
Error "invalid"
Error "parse error"
```

### 3. Use Normalize_common utilities

```ocaml
open Fluxum.Normalize_common

(* Safe float conversion *)
let%bind price = Float_conv.price_of_string price_str in
let%bind qty = Float_conv.qty_of_string qty_str in

(* Safe side/status conversion *)
let%bind side = Side.of_string side_str in
let%bind status = Order_status.of_string status_str in
```

## Common Error Cases

### Float Conversion Errors

| Input | Error |
|-------|-------|
| `"not_a_number"` | `Float conversion error 'not_a_number': Invalid_argument` |
| `"NaN"` | `Non-finite float: NaN` |
| `"Infinity"` | `Non-finite float: Infinity` |
| `"-50000"` (for price) | `Price must be positive, got: -50000` |
| `"-1.5"` (for qty) | `Quantity cannot be negative, got: -1.5` |

### JSON Type Errors

| Input | Error |
|-------|-------|
| `null` where string expected | `Expected string, got null` |
| `[]` where object expected | `Expected object, got array` |
| Missing field | `Expected string, got null` (from member lookup) |

### Domain Errors

| Input | Error |
|-------|-------|
| `"INVALID_STATUS"` | `Unrecognized order status: INVALID_STATUS` |
| `"UNKNOWN_SIDE"` | `Unrecognized side: UNKNOWN_SIDE` |

## Handling Errors in Callers

### CLI Commands

```ocaml
match Adapter.Normalize.ticker native_ticker with
| Ok ticker -> print_ticker ticker
| Error msg ->
  eprintf "Normalization failed: %s\n" msg;
  (* Don't crash, handle gracefully *)
```

### Batch Operations

```ocaml
(* Use Result_util.transpose for lists *)
let results = List.map orders ~f:Normalize.order_from_status in
match Normalize_common.Result_util.transpose results with
| Ok orders -> process_orders orders
| Error first_error -> handle_error first_error
```

### Streaming Data

```ocaml
(* Filter out bad data, log errors *)
Pipe.filter_map stream ~f:(fun native ->
  match Normalize.trade native with
  | Ok trade -> Some trade
  | Error msg ->
    Log.Global.error "Trade normalization failed: %s" msg;
    None)
```

## Testing Normalize Functions

Every exchange adapter should have tests covering:

1. **Happy path** - Valid data normalizes correctly
2. **Error paths** - Invalid data returns descriptive errors
3. **Edge cases** - Very large numbers, very small numbers, empty strings
4. **Round-trip** - Data survives normalize without loss

Example test structure:

```ocaml
let test_ticker_malformed_float () =
  let json = \`Assoc [("bid", \`String "not_a_number"); ...] in
  match Normalize.ticker json with
  | Error msg ->
    assert (String.is_substring msg ~substring:"Float conversion error");
    pass "Correctly rejected malformed float"
  | Ok _ ->
    fail "Should reject non-numeric bid"
```

## Migration from Pre-Result.t Code

If you have code that assumed normalize functions were infallible:

```ocaml
(* Old code (crashes on bad data) *)
let ticker = Normalize.ticker native in
use_ticker ticker

(* New code (handles errors) *)
match Normalize.ticker native with
| Ok ticker -> use_ticker ticker
| Error msg -> handle_error msg
```

See [MIGRATION_PHASE1.md](../MIGRATION_PHASE1.md) for detailed migration guide.

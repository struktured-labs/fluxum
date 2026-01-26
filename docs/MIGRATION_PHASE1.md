# Phase 1 Migration Guide

This guide helps migrate code from pre-Phase 1 Fluxum (infallible normalize functions) to Phase 1+ (fallible normalize functions returning `Result.t`).

## Breaking Changes

### Normalize Functions Now Return Result.t

**Before (pre-Phase 1):**
```ocaml
(* These functions could crash on malformed data *)
val ticker : Native.Ticker.t -> Types.Ticker.t
val order_book : Native.Book.snapshot -> Types.Order_book.t
val balance : Native.Balance.t -> Types.Balance.t
val trade : Native.Trade.t -> Types.Trade.t
```

**After (Phase 1+):**
```ocaml
(* These functions safely return errors *)
val ticker : Native.Ticker.t -> (Types.Ticker.t, string) Result.t
val order_book : Native.Book.snapshot -> (Types.Order_book.t, string) Result.t
val balance : Native.Balance.t -> (Types.Balance.t, string) Result.t
val trade : Native.Trade.t -> (Types.Trade.t, string) Result.t
```

## Migration Patterns

### Pattern 1: Simple Value Access

**Before:**
```ocaml
let ticker = Normalize.ticker native_ticker in
printf "Price: %f\n" ticker.last_price
```

**After:**
```ocaml
match Normalize.ticker native_ticker with
| Ok ticker -> printf "Price: %f\n" ticker.last_price
| Error msg -> eprintf "Error: %s\n" msg
```

### Pattern 2: List Processing

**Before:**
```ocaml
let orders = List.map native_orders ~f:Normalize.order_from_status in
process_orders orders
```

**After:**
```ocaml
let results = List.map native_orders ~f:Normalize.order_from_status in
match Normalize_common.Result_util.transpose results with
| Ok orders -> process_orders orders
| Error msg -> handle_error msg
```

Or filter out errors:
```ocaml
let orders = List.filter_map native_orders ~f:(fun native ->
  match Normalize.order_from_status native with
  | Ok order -> Some order
  | Error _ -> None) in
process_orders orders
```

### Pattern 3: Chained Operations

**Before:**
```ocaml
let ticker = Normalize.ticker native in
let processed = process ticker in
save processed
```

**After:**
```ocaml
let open Result.Let_syntax in
let%bind ticker = Normalize.ticker native in
let processed = process ticker in
Ok (save processed)
```

### Pattern 4: Deferred + Result

**Before:**
```ocaml
get_native_ticker () >>| fun native ->
let ticker = Normalize.ticker native in
ticker
```

**After:**
```ocaml
get_native_ticker () >>| fun native ->
match Normalize.ticker native with
| Ok ticker -> Ok ticker
| Error msg -> Error (Types.Error.Normalization_error msg)
```

## Using Normalize_common

The `Normalize_common` module provides safe utilities:

```ocaml
open Fluxum.Normalize_common

(* Safe float conversions *)
Float_conv.of_string "123.45"        (* -> Ok 123.45 *)
Float_conv.price_of_string "0"       (* -> Error "Price must be positive" *)
Float_conv.qty_of_string "-1"        (* -> Error "Quantity cannot be negative" *)

(* Safe enum conversions *)
Side.of_string "buy"                 (* -> Ok Types.Side.Buy *)
Side.of_string "invalid"             (* -> Error "Unrecognized side: invalid" *)

Order_status.of_string "filled"      (* -> Ok Types.Order_status.Filled *)
Order_status.of_string "?"           (* -> Error "Unrecognized order status: ?" *)

(* List transposition *)
Result_util.transpose [Ok 1; Ok 2]   (* -> Ok [1; 2] *)
Result_util.transpose [Ok 1; Error "e"] (* -> Error "e" *)
```

## Error Handling Strategies

### Strategy 1: Fail Fast
Stop processing on first error:
```ocaml
match Normalize.ticker native with
| Ok ticker -> continue_processing ticker
| Error msg -> failwith msg  (* Or return error to caller *)
```

### Strategy 2: Collect All Errors
Process everything, report all errors:
```ocaml
let results = List.map items ~f:Normalize.item in
let errors = List.filter_map results ~f:(function
  | Error e -> Some e
  | Ok _ -> None) in
match errors with
| [] -> (* all succeeded *)
| errs -> Log.Global.error "Errors: %s" (String.concat ~sep:", " errs)
```

### Strategy 3: Best Effort
Skip bad data, continue with good:
```ocaml
let items = List.filter_map native_items ~f:(fun native ->
  match Normalize.item native with
  | Ok item -> Some item
  | Error msg ->
    Log.Global.warn "Skipping item: %s" msg;
    None)
```

## Testing After Migration

Ensure your tests cover error cases:

```ocaml
let test_handles_malformed_data () =
  let bad_native = create_malformed_native () in
  match Normalize.item bad_native with
  | Error _ -> pass "Correctly rejected malformed data"
  | Ok _ -> fail "Should have rejected malformed data"
```

## Checklist

- [ ] Update all direct calls to normalize functions to handle Result.t
- [ ] Replace List.map + normalize with Result_util.transpose or filter_map
- [ ] Add error logging where appropriate
- [ ] Update tests to cover error cases
- [ ] Remove any try/with blocks that were catching normalize crashes

## Getting Help

- See [NORMALIZE_CONTRACT.md](guides/NORMALIZE_CONTRACT.md) for the full contract
- See [EXCHANGE_IMPLEMENTATION_STATUS.md](guides/EXCHANGE_IMPLEMENTATION_STATUS.md) for exchange status
- Check test files for examples: `lib/exchange/*/test/run_tests.ml`

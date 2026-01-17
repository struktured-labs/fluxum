# Phase 1 Migration Guide

## Overview

Phase 1 introduced **fallible normalize functions** across all priority exchanges (Gemini, Kraken, Hyperliquid, MEXC). All `Normalize` module functions now return `Result.t` types instead of direct values.

**Breaking Change:** Code that assumes normalize functions never fail will need to be updated.

**Affected Exchanges:**
- ✅ Gemini (100% complete)
- ✅ Kraken (100% complete)
- ✅ Hyperliquid (100% complete)
- ✅ MEXC (100% complete)
- ⚠️ Binance (partial - not all functions migrated yet)
- ⚠️ Coinbase (partial - not all functions migrated yet)
- ⚠️ Bitrue (partial - not all functions migrated yet)

**Date:** Phase 1 completed January 2026

---

## What Changed

### Before Phase 1 (Unsafe)

Normalize functions returned direct types and could crash on malformed data:

```ocaml
module Normalize = struct
  val ticker : Native.Ticker.t -> Types.Ticker.t
  val order_response : Native.Order.response -> Types.Order.t
  val balance : Native.Balance.t -> Types.Balance.t
  val public_trade : Native.Trade.t -> Types.Trade.t
  val order_book : symbol:string -> Native.Order_book.t -> Types.Order_book.t
end
```

**Problem:** `Float.of_string`, `List.hd_exn`, and similar operations would raise exceptions on unexpected data.

### After Phase 1 (Fallible)

Normalize functions return `Result.t` and handle errors explicitly:

```ocaml
module Normalize = struct
  val ticker : Native.Ticker.t -> (Types.Ticker.t, string) Result.t
  val order_response : Native.Order.response -> (Types.Order.t, string) Result.t
  val balance : Native.Balance.t -> (Types.Balance.t, string) Result.t
  val public_trade : Native.Trade.t -> (Types.Trade.t, string) Result.t
  val order_book : symbol:string -> Native.Order_book.t -> (Types.Order_book.t, string) Result.t
end
```

**Benefit:** Malformed data returns `Error "descriptive message"` instead of crashing.

---

## Migration Steps

### Step 1: Update Function Calls

**Before:**
```ocaml
let ticker = Gemini.Fluxum_adapter.Adapter.Normalize.ticker native_ticker in
update_ui ticker
```

**After:**
```ocaml
match Gemini.Fluxum_adapter.Adapter.Normalize.ticker native_ticker with
| Ok ticker -> update_ui ticker
| Error msg ->
  Log.error "Failed to normalize ticker: %s" msg;
  (* Keep previous ticker or show error state *)
```

### Step 2: Handle Errors in Deferred Context

**Before:**
```ocaml
let%bind response = Gemini.Fluxum_adapter.Adapter.place_order adapter request in
let order = Gemini.Fluxum_adapter.Adapter.Normalize.order_response response in
process_order order
```

**After:**
```ocaml
let%bind response = Gemini.Fluxum_adapter.Adapter.place_order adapter request in
match Gemini.Fluxum_adapter.Adapter.Normalize.order_response response with
| Ok order -> process_order order
| Error msg ->
  Log.error "Order normalization failed: %s" msg;
  return (Error (`Normalization_failed msg))
```

### Step 3: Update List Processing

**Before:**
```ocaml
let balances = List.map native_balances ~f:Adapter.Normalize.balance in
display_balances balances
```

**After (Option 1 - Filter failures):**
```ocaml
let balances =
  List.filter_map native_balances ~f:(fun native ->
    match Adapter.Normalize.balance native with
    | Ok balance -> Some balance
    | Error msg ->
      Log.warn "Skipping malformed balance: %s" msg;
      None)
in
display_balances balances
```

**After (Option 2 - Fail on first error):**
```ocaml
match Result.all (List.map native_balances ~f:Adapter.Normalize.balance) with
| Ok balances -> display_balances balances
| Error msg ->
  Log.error "Balance normalization failed: %s" msg;
  display_error "Failed to load balances"
```

**After (Option 3 - Use Normalize_common.Result_list):**
```ocaml
open Normalize_common

match Result_list.transpose (List.map native_balances ~f:Adapter.Normalize.balance) with
| Ok balances -> display_balances balances
| Error msg ->
  Log.error "Balance normalization failed: %s" msg;
  display_error "Failed to load balances"
```

### Step 4: Update Pipe Processing

**Before:**
```ocaml
let%bind ticker_pipe = Gemini.V1.Market_data.client (module Cfg) ?query:None ~uri_args:symbol () in
Pipe.iter ticker_pipe ~f:(function
  | `Ok response ->
    let ticker = Gemini.Fluxum_adapter.Adapter.Normalize.ticker response in
    update_ticker ticker
  | `Error e -> Log.error "WebSocket error: %s" (Gemini.Ws.Error.to_string e))
```

**After:**
```ocaml
let%bind ticker_pipe = Gemini.V1.Market_data.client (module Cfg) ?query:None ~uri_args:symbol () in
Pipe.iter ticker_pipe ~f:(function
  | `Ok response ->
    (match Gemini.Fluxum_adapter.Adapter.Normalize.ticker response with
     | Ok ticker -> update_ticker ticker
     | Error msg -> Log.error "Ticker normalization failed: %s" msg)
  | `Error e -> Log.error "WebSocket error: %s" (Gemini.Ws.Error.to_string e))
```

---

## Common Migration Patterns

### Pattern 1: Critical Operations (Fail Fast)

For trading operations where malformed data could cause financial loss:

```ocaml
let place_limit_order adapter ~symbol ~side ~price ~qty =
  let%bind order_req = build_limit_order ~symbol ~side ~price ~qty in
  let%bind response = Adapter.place_order adapter order_req in

  (* CRITICAL: Must parse the response to verify order was placed *)
  match Adapter.Normalize.order_response response with
  | Error msg ->
    Log.error "CRITICAL: Cannot parse order response: %s" msg;
    return (Error (`Normalization_failed msg))
  | Ok order ->
    (* Verify order matches our request *)
    if not (verify_order_matches order order_req) then
      return (Error (`Order_mismatch order))
    else
      return (Ok order)
```

### Pattern 2: Best-Effort Operations (Continue on Failure)

For market data where individual failures shouldn't stop the system:

```ocaml
let process_trade_stream trades_pipe =
  Pipe.iter trades_pipe ~f:(fun native_trade ->
    match Adapter.Normalize.public_trade native_trade with
    | Ok trade ->
      update_trade_history trade;
      notify_subscribers trade
    | Error msg ->
      Log.warn "Skipping malformed trade: %s" msg;
      (* Continue processing other trades *)
      Deferred.unit)
```

### Pattern 3: Fallback Values

Use previous data when normalization fails:

```ocaml
type ticker_state = {
  mutable current: Types.Ticker.t option;
}

let update_ticker_state state native_ticker =
  match Adapter.Normalize.ticker native_ticker with
  | Ok ticker ->
    state.current <- Some ticker;
    display_ticker ticker
  | Error msg ->
    Log.warn "Ticker normalization failed, keeping previous: %s" msg;
    (* Display keeps showing previous ticker *)
    match state.current with
    | Some previous -> display_ticker previous
    | None -> display_error "No ticker data available"
```

### Pattern 4: Accumulate Errors

When you need to report all failures:

```ocaml
let normalize_order_book_levels levels =
  let errors = ref [] in
  let normalized = List.filter_map levels ~f:(fun level ->
    match Adapter.Normalize.order_book_level level with
    | Ok level -> Some level
    | Error msg ->
      errors := msg :: !errors;
      None)
  in
  match !errors with
  | [] -> Ok normalized
  | errs ->
    Log.warn "Order book had %d malformed levels: %s"
      (List.length errs)
      (String.concat ~sep:"; " errs);
    (* Return partial book or fail completely *)
    if Float.(List.length normalized /. List.length levels > 0.9) then
      Ok normalized  (* >90% success rate, use partial book *)
    else
      Error (sprintf "Too many malformed levels (%d/%d)" (List.length errs) (List.length levels))
```

---

## Using Normalize_common Utilities

Phase 3 introduced shared normalization utilities to reduce code duplication:

### Safe Float Conversions

**Before:**
```ocaml
let price = Float.of_string price_str in  (* UNSAFE! *)
let qty = Float.of_string qty_str in
```

**After:**
```ocaml
open Normalize_common

let%bind price = Float_conv.price_of_string price_str in  (* validates positive *)
let%bind qty = Float_conv.qty_of_string qty_str in       (* validates non-negative *)
```

### Side/Status/Type Conversions

**Before (duplicated across exchanges):**
```ocaml
let side_of_string s =
  match String.lowercase s with
  | "buy" | "b" -> Types.Side.Buy
  | "sell" | "s" -> Types.Side.Sell
  | _ -> Types.Side.Buy  (* Silent default - dangerous! *)
```

**After (shared utility):**
```ocaml
open Normalize_common

let%bind side = Side.of_string_exn side_str in  (* Returns Error on unknown *)
```

**Or with default:**
```ocaml
let side = Side.of_string ~default:Types.Side.Buy side_str  (* Explicit default *)
```

### Order Status Parsing

**After:**
```ocaml
open Normalize_common

let%bind status = Order_status.of_string status_str in
(* Handles: "new", "filled", "canceled", "partially_filled", "rejected" *)
```

### Result List Transposition

**Before:**
```ocaml
let rec transpose_results = function
  | [] -> Ok []
  | Error e :: _ -> Error e
  | Ok x :: xs ->
    match transpose_results xs with
    | Ok rest -> Ok (x :: rest)
    | Error e -> Error e
```

**After:**
```ocaml
open Normalize_common

match Result_list.transpose result_list with
| Ok values -> process_values values
| Error e -> handle_error e
```

---

## Error Handling Strategies

### Strategy 1: Log and Skip (Market Data)

```ocaml
Pipe.iter trade_pipe ~f:(function
  | `Ok native_trade ->
    (match Adapter.Normalize.public_trade native_trade with
     | Ok trade -> process_trade trade
     | Error msg -> Log.warn "Skipping trade: %s" msg)
  | `Error e -> Log.error "WebSocket error: %s" e)
```

**Use when:** Individual failures don't affect overall system health

### Strategy 2: Fail Fast (Trading)

```ocaml
let%bind response = place_order adapter request in
match Adapter.Normalize.order_response response with
| Error msg ->
  Log.error "CRITICAL: Order normalization failed: %s" msg;
  return (Error `Normalization_failed)
| Ok order -> verify_and_process order
```

**Use when:** Proceeding with malformed data could cause financial loss

### Strategy 3: Retry with Snapshot (Order Books)

```ocaml
let handle_order_book_update ~symbol update =
  match Adapter.Normalize.order_book_update update with
  | Ok book_update -> apply_update book_update
  | Error msg when String.is_substring msg ~substring:"Missing" ->
    Log.warn "Incremental update missing data: %s" msg;
    (* Incremental update corrupted, request full snapshot *)
    request_order_book_snapshot ~symbol
  | Error msg ->
    Log.error "Order book normalization failed: %s" msg
```

**Use when:** Incremental updates can fail but full snapshots will recover

### Strategy 4: Partial Success with Warnings

```ocaml
let normalize_balances balances =
  let successes, failures =
    List.partition_map balances ~f:(fun b ->
      match Adapter.Normalize.balance b with
      | Ok balance -> First balance
      | Error msg -> Second (b, msg))
  in
  match failures with
  | [] -> Ok successes
  | failures ->
    List.iter failures ~f:(fun (native, msg) ->
      Log.warn "Failed to normalize %s balance: %s"
        (extract_asset native) msg);
    (* Return partial results if most succeeded *)
    if Float.(List.length successes /. List.length balances > 0.8) then
      Ok successes
    else
      Error (sprintf "%d/%d balances failed normalization"
        (List.length failures) (List.length balances))
```

**Use when:** Partial data is still useful

---

## Testing Migration

### Add Error Path Tests

**Before (only happy path):**
```ocaml
let test_ticker_normalization () =
  let native = valid_ticker_json () in
  let ticker = Adapter.Normalize.ticker native in
  assert_equal ticker.bid_price 50000.0
```

**After (test error paths too):**
```ocaml
let test_ticker_normalization () =
  (* Happy path *)
  let native = valid_ticker_json () in
  (match Adapter.Normalize.ticker native with
   | Ok ticker -> assert_equal ticker.bid_price 50000.0
   | Error msg -> failwith msg);

  (* Error path: null bid *)
  let bad_native = ticker_with_null_bid () in
  (match Adapter.Normalize.ticker bad_native with
   | Error msg ->
     assert (String.is_substring msg ~substring:"bid");
     pass "Correctly rejected null bid"
   | Ok _ -> fail "Should reject null bid");

  (* Error path: malformed number *)
  let bad_native = ticker_with_invalid_number () in
  (match Adapter.Normalize.ticker bad_native with
   | Error msg ->
     assert (String.is_substring msg ~substring:"Invalid float");
     pass "Correctly rejected malformed number"
   | Ok _ -> fail "Should reject malformed number")
```

See [NORMALIZE_CONTRACT.md](guides/NORMALIZE_CONTRACT.md#best-practices) for more testing guidance.

---

## Performance Considerations

### Q: Does Result.t add overhead?

**A:** Minimal. OCaml's `Result.t` is a lightweight variant type:

```ocaml
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e
```

- **Ok branch:** Zero allocation overhead (the value is inlined)
- **Error branch:** Single allocation for error message string
- **Bind operators:** Inlined by compiler (no function call overhead)

### Q: Should I cache normalized data?

**A:** Yes, if normalizing the same data repeatedly:

```ocaml
type normalized_cache = {
  raw: Yojson.Safe.t;
  mutable normalized: Types.Ticker.t option;
  mutable error: string option;
}

let get_or_normalize t =
  match t.normalized, t.error with
  | Some ticker, _ -> Ok ticker
  | None, Some err -> Error err
  | None, None ->
    (* First access - normalize and cache *)
    match Adapter.Normalize.ticker t.raw with
    | Ok ticker ->
      t.normalized <- Some ticker;
      Ok ticker
    | Error err ->
      t.error <- Some err;
      Error err
```

---

## Exchange-Specific Notes

### Gemini

- **All normalize functions** return Result.t (100% complete)
- Error messages reference specific field names
- Null handling is strict (returns Error)

**Special case:** `balance` can return negative values from API, normalized to Error

### Kraken

- **All normalize functions** return Result.t (100% complete)
- Symbol normalization handles XBT/BTC variants
- Timestamp conversions validate range

**Special case:** Symbol prefixes (XX, Z) are stripped in normalization

### Hyperliquid

- **All normalize functions** return Result.t (100% complete)
- Perpetuals-only (no spot symbols)
- Blockchain-specific fields may be null

**Special case:** Account queries work but orders can't be placed (blockchain signing needed)

### MEXC

- **All normalize functions** return Result.t (100% complete)
- Binance-compatible API structure
- Safe float conversions (Phase 3)

**Special case:** Symbol format uses underscore (`BTC_USDT`)

### Binance (Partial)

- **~60% of normalize functions** return Result.t
- WebSocket order book is complete
- Some legacy REST endpoints still unsafe

**Migration status:** In progress (Phase 2 target)

### Coinbase (Partial)

- **~60% of normalize functions** return Result.t
- Advanced Trade API is primary focus
- Legacy Pro API has incomplete migration

**Migration status:** In progress (Phase 2 target)

---

## Troubleshooting

### Issue: Code won't compile after update

**Error:**
```
Error: This expression has type (Types.Ticker.t, string) result
       but an expression was expected of type Types.Ticker.t
```

**Solution:** Add `match` or `Result.bind` to handle the Result.t:

```ocaml
(* Before *)
let ticker = normalize_ticker native in

(* After *)
match normalize_ticker native with
| Ok ticker -> ...
| Error msg -> ...
```

### Issue: Too many nested matches

**Before:**
```ocaml
match normalize_ticker native with
| Ok ticker ->
  match normalize_balance native_balance with
  | Ok balance ->
    match normalize_orders native_orders with
    | Ok orders -> process_all ticker balance orders
    | Error msg -> handle_error msg
  | Error msg -> handle_error msg
| Error msg -> handle_error msg
```

**Solution:** Use `Result.Let_syntax`:

```ocaml
let open Result.Let_syntax in
let%bind ticker = normalize_ticker native in
let%bind balance = normalize_balance native_balance in
let%bind orders = normalize_orders native_orders in
Ok (process_all ticker balance orders)
```

### Issue: Want to ignore errors and continue

**Don't do this:**
```ocaml
let ticker = normalize_ticker native |> Result.ok_or_failwith  (* DEFEATS THE PURPOSE! *)
```

**Do this instead:**
```ocaml
match normalize_ticker native with
| Ok ticker -> process_ticker ticker
| Error msg ->
  Log.warn "Skipping malformed ticker: %s" msg;
  (* Explicitly decide what to do: skip, use default, retry, etc. *)
```

---

## Checklist

Use this checklist to ensure complete migration:

- [ ] Updated all `Normalize.*` function calls to handle `Result.t`
- [ ] Added error logging for normalization failures
- [ ] Implemented appropriate error handling strategy (fail fast vs continue)
- [ ] Replaced manual float parsing with `Normalize_common.Float_conv`
- [ ] Replaced manual enum parsing with `Normalize_common.Side/Order_status`
- [ ] Added unit tests for error paths
- [ ] Verified no `Result.ok_or_failwith` calls (defeats the purpose)
- [ ] Reviewed logs for normalization errors in production
- [ ] Updated documentation/comments to reflect Result.t returns

---

## Additional Resources

- [NORMALIZE_CONTRACT.md](guides/NORMALIZE_CONTRACT.md) - Deep dive into fallible normalization
- [EXCHANGE_IMPLEMENTATION_STATUS.md](guides/EXCHANGE_IMPLEMENTATION_STATUS.md) - Which exchanges are migrated
- [normalize_common.mli](../lib/normalize_common.mli) - Shared utility functions
- [exchange_intf.mli](../lib/exchange_intf.mli) - Exchange adapter interface

---

## Getting Help

If you encounter migration issues:

1. Check error message format in [NORMALIZE_CONTRACT.md](guides/NORMALIZE_CONTRACT.md#error-message-format)
2. Review common patterns above for your use case
3. Look at Phase 1 exchange implementations (Gemini, Kraken, MEXC, Hyperliquid) for examples
4. Check unit tests in `lib/exchange/*/test/run_tests.ml` for error handling patterns

---

**Last Updated:** 2026-01-17 (Phase 4, Priority 3)

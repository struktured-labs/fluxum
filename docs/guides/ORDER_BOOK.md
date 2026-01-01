# Order Book Guide

Complete guide to using Fluxum's unified order book interface.

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Creating Order Books](#creating-order-books)
- [Updating Order Books](#updating-order-books)
- [Querying Order Books](#querying-order-books)
- [WebSocket Streaming](#websocket-streaming)
- [Multi-Symbol Books](#multi-symbol-books)
- [Analytics](#analytics)
- [Advanced Usage](#advanced-usage)

## Overview

Fluxum provides a unified order book implementation (`Exchange_common.Order_book_base`) that all exchanges use. This ensures consistent behavior and APIs across all 7 supported exchanges.

### Key Features

- **Immutable**: Updates create new book instances
- **Type-Safe**: Compile-time guarantees
- **Efficient**: O(log n) updates, O(1) best bid/ask access
- **Unified**: Same API for all exchanges
- **Analytics**: Built-in VWAP, spread, volume calculations

## Quick Start

### Single Exchange

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
  let best_bid = Kraken.Order_book.Book.best_bid book in
  let best_ask = Kraken.Order_book.Book.best_ask book in

  printf "Bid: %.2f (%.8f) | Ask: %.2f (%.8f) | Spread: %.2f\n"
    Exchange_common.Order_book_base.Price_level.(price best_bid)
    Exchange_common.Order_book_base.Price_level.(volume best_bid)
    Exchange_common.Order_book_base.Price_level.(price best_ask)
    Exchange_common.Order_book_base.Price_level.(volume best_ask)
    (Kraken.Order_book.Book.spread book)
)
```

## Creating Order Books

### Empty Book

```ocaml
(* Binance *)
let book = Binance.Order_book.Book.create ~symbol:"BTCUSDT" in

(* Kraken *)
let book = Kraken.Order_book.Book.create ~symbol:"BTC/USD" in

(* Gemini *)
let book = Gemini.Order_book.Book.create ~symbol:"btcusd" in
```

### With Initial Data

```ocaml
let book = Binance.Order_book.Book.create ~symbol:"BTCUSDT" in
let book = Binance.Order_book.Book.set_many book [
  (`Bid, 50000., 1.5);
  (`Bid, 49999., 2.0);
  (`Bid, 49998., 1.0);
  (`Ask, 50001., 1.2);
  (`Ask, 50002., 0.8);
  (`Ask, 50003., 2.5);
]
```

## Updating Order Books

### Single Update

```ocaml
(* Add/update a bid at price 50000 with size 1.5 *)
let book = Book.set book
  ~side:`Bid
  ~price:50000.
  ~size:1.5
in

(* Remove a bid at price 50000 (size = 0) *)
let book = Book.set book
  ~side:`Bid
  ~price:50000.
  ~size:0.
in
```

### Batch Updates

```ocaml
(* More efficient than multiple single updates *)
let book = Book.set_many book [
  (`Bid, 50000., 1.5);
  (`Bid, 49999., 0.);    (* Remove *)
  (`Ask, 50001., 2.0);
]
```

### With Metadata

```ocaml
(* Binance: Track last update ID *)
let metadata = { Binance.Order_book.last_update_id = 12345L } in
let book = Book.set book
  ~metadata
  ~side:`Bid
  ~price:50000.
  ~size:1.5
in

(* Hyperliquid: Track last update time *)
let metadata = { Hyperliquid.Order_book.last_update_time = timestamp } in
let book = Book.set book
  ~metadata
  ~side:`Ask
  ~price:50001.
  ~size:2.0
in
```

## Querying Order Books

### Best Bid/Ask

```ocaml
let best_bid = Book.best_bid book in
let best_ask = Book.best_ask book in

(* Access price and volume *)
let bid_price = Exchange_common.Order_book_base.Price_level.price best_bid in
let bid_volume = Exchange_common.Order_book_base.Price_level.volume best_bid in
```

### Mid Price and Spread

```ocaml
(* Mid price: (best_bid + best_ask) / 2 *)
let mid = Book.mid_price book in

(* Spread: best_ask - best_bid *)
let spread = Book.spread book in

(* Spread percentage *)
let spread_pct = spread /. mid *. 100. in
printf "Spread: %.2f (%.4f%%)\n" spread spread_pct
```

### Top N Levels

```ocaml
(* Get best 10 bids *)
let top_bids = Book.best_n_bids book ~n:10 () in

(* Get best 10 asks *)
let top_asks = Book.best_n_asks book ~n:10 () in

(* Print top of book *)
List.iter top_asks ~f:(fun level ->
  printf "  Ask: %.2f @ %.8f\n"
    Price_level.(price level)
    Price_level.(volume level)
);

printf "--- Spread: %.2f ---\n" (Book.spread book);

List.iter top_bids ~f:(fun level ->
  printf "  Bid: %.2f @ %.8f\n"
    Price_level.(price level)
    Price_level.(volume level)
);
```

### All Levels

```ocaml
(* Get all bids (sorted: best first) *)
let all_bids = Book.all_bids book in

(* Get all asks (sorted: best first) *)
let all_asks = Book.all_asks book in

(* As association lists *)
let bids_alist = Book.bids_alist book in
let asks_alist = Book.asks_alist book in
(* Returns: (float * Price_level.t) list *)
```

### Book Metadata

```ocaml
let symbol = Book.symbol book in
let epoch = Book.epoch book in  (* Update counter *)
let update_time = Book.update_time book in  (* Unix timestamp *)
let metadata = Book.metadata book in  (* Exchange-specific *)

printf "Symbol: %s | Epoch: %d | Last update: %.6f\n"
  symbol epoch update_time
```

## WebSocket Streaming

### Basic Streaming

```ocaml
let%bind book_pipe =
  Kraken.Order_book.Book.pipe
    (module Cfg)
    ~symbol:"BTC/USD"
    ()
in

(* Pipe yields updated books on each message *)
Pipe.iter book_pipe ~f:(fun book ->
  process_book book;
  return ()
)
```

### Processing Updates

```ocaml
let%bind book_pipe = Binance.Order_book.Book.pipe (module Cfg) ~symbol:"BTCUSDT" () in

Pipe.fold book_pipe ~init:None ~f:(fun prev_book book ->
  (* Calculate spread change *)
  let curr_spread = Book.spread book in
  let () = match prev_book with
    | None -> printf "Initial spread: %.2f\n" curr_spread
    | Some prev ->
        let prev_spread = Book.spread prev in
        let delta = curr_spread -. prev_spread in
        if Float.(abs delta > 1.0) then
          printf "Large spread change: %.2f -> %.2f (%.2f)\n"
            prev_spread curr_spread delta
  in
  return (Some book)
)
```

### Multiple Symbols

```ocaml
(* Binance: BTCUSDT + ETHUSDT *)
let%bind btc_pipe =
  Binance.Order_book.Book.pipe (module Cfg) ~symbol:"BTCUSDT" ()
in
let%bind eth_pipe =
  Binance.Order_book.Book.pipe (module Cfg) ~symbol:"ETHUSDT" ()
in

(* Process both streams *)
don't_wait_for (Pipe.iter btc_pipe ~f:(fun book -> ...));
don't_wait_for (Pipe.iter eth_pipe ~f:(fun book -> ...));
```

## Multi-Symbol Books

Use `Books` module to manage multiple symbols:

```ocaml
(* Create empty collection *)
let books = Binance.Order_book.Book_base.Books.empty in

(* Add books *)
let btc_book = Binance.Order_book.Book.create ~symbol:"BTCUSDT" in
let eth_book = Binance.Order_book.Book.create ~symbol:"ETHUSDT" in

let books = Books.set_book books btc_book in
let books = Books.set_book books eth_book in

(* Query *)
let btc_book = Books.book books "BTCUSDT" in
match btc_book with
| Some book -> process_book book
| None -> printf "Book not found\n"

(* Get all symbols *)
let symbols = Books.symbols books in
List.iter symbols ~f:(fun sym -> printf "%s\n" sym)

(* Iterate all books *)
Books.iter books ~f:(fun book ->
  printf "%s: mid=%.2f spread=%.2f\n"
    (Book.symbol book)
    (Book.mid_price book)
    (Book.spread book)
)

(* Fold over books *)
let total_volume = Books.fold books ~init:0. ~f:(fun acc book ->
  let vol = Book.total_volume_n book ~side:`Bid ~n:10 in
  acc +. vol
)
```

## Analytics

### VWAP (Volume-Weighted Average Price)

```ocaml
(* VWAP to buy 10 BTC *)
let vwap_buy = Book.vwap_buy book ~volume:10.0 in
match vwap_buy with
| Some price ->
    printf "VWAP to buy 10 BTC: $%.2f\n" price
| None ->
    printf "Insufficient liquidity\n"

(* VWAP to sell 5 BTC *)
let vwap_sell = Book.vwap_sell book ~volume:5.0 in
match vwap_sell with
| Some price ->
    printf "VWAP to sell 5 BTC: $%.2f\n" price
| None ->
    printf "Insufficient liquidity\n"
```

**Algorithm**:
```ocaml
(* VWAP Buy: walk asks from best to worst *)
let rec calculate_vwap asks ~remaining_volume ~total_cost =
  match asks with
  | [] -> None  (* Insufficient liquidity *)
  | level :: rest ->
      let level_volume = Price_level.volume level in
      let level_price = Price_level.price level in

      if Float.(level_volume >= remaining_volume) then
        (* This level satisfies remaining volume *)
        let cost = total_cost +. (remaining_volume *. level_price) in
        Some (cost /. original_volume)
      else
        (* Consume this level, continue *)
        let cost = total_cost +. (level_volume *. level_price) in
        calculate_vwap rest
          ~remaining_volume:(remaining_volume -. level_volume)
          ~total_cost:cost
```

### Total Volume

```ocaml
(* Total bid volume in top 10 levels *)
let bid_volume = Book.total_volume_n book ~side:`Bid ~n:10 in

(* Total ask volume in top 10 levels *)
let ask_volume = Book.total_volume_n book ~side:`Ask ~n:10 in

(* Liquidity imbalance *)
let imbalance = (bid_volume -. ask_volume) /. (bid_volume +. ask_volume) in
printf "Liquidity imbalance: %.4f\n" imbalance
```

### Market Depth

```ocaml
let depth_at_price book ~side ~target_price =
  let levels = match side with
    | `Bid -> Book.bids_alist book
    | `Ask -> Book.asks_alist book
  in

  List.fold levels ~init:0. ~f:(fun acc (price, level) ->
    let include_level = match side with
      | `Bid -> Float.(price >= target_price)
      | `Ask -> Float.(price <= target_price)
    in
    if include_level then
      acc +. Price_level.volume level
    else
      acc
  )

(* Volume available to buy up to $51000 *)
let volume = depth_at_price book ~side:`Ask ~target_price:51000. in
printf "Can buy %.8f BTC up to $51000\n" volume
```

## Advanced Usage

### Custom Aggregation

```ocaml
(* Group levels by $100 buckets *)
let aggregate_by_bucket book ~side ~bucket_size =
  let levels = match side with
    | `Bid -> Book.all_bids book
    | `Ask -> Book.all_asks book
  in

  let buckets = Map.empty (module Float) in
  List.fold levels ~init:buckets ~f:(fun buckets level ->
    let price = Price_level.price level in
    let volume = Price_level.volume level in
    let bucket = Float.round_down (price /. bucket_size) *. bucket_size in

    Map.update buckets bucket ~f:(function
      | None -> volume
      | Some existing -> existing +. volume
    )
  )
```

### Spread Alerts

```ocaml
let monitor_spread book_pipe ~threshold =
  Pipe.iter book_pipe ~f:(fun book ->
    let spread = Book.spread book in
    let mid = Book.mid_price book in
    let spread_pct = spread /. mid *. 100. in

    if Float.(spread_pct > threshold) then begin
      printf "[ALERT] Wide spread: %.2f (%.4f%%)\n" spread spread_pct;
      printf "  Best bid: %.2f\n" Price_level.(price (Book.best_bid book));
      printf "  Best ask: %.2f\n" Price_level.(price (Book.best_ask book));
    end;

    return ()
  )
```

### Order Impact Estimation

```ocaml
let estimate_impact book ~side ~volume =
  match side with
  | `Buy ->
      (* Market buy: walk asks *)
      let vwap = Book.vwap_buy book ~volume in
      let mid = Book.mid_price book in
      (match vwap with
       | Some avg_price ->
           let slippage = (avg_price -. mid) /. mid *. 100. in
           printf "Buy %.8f: VWAP=%.2f, slippage=%.4f%%\n"
             volume avg_price slippage
       | None ->
           printf "Insufficient liquidity for buy %.8f\n" volume)

  | `Sell ->
      (* Market sell: walk bids *)
      let vwap = Book.vwap_sell book ~volume in
      let mid = Book.mid_price book in
      (match vwap with
       | Some avg_price ->
           let slippage = (mid -. avg_price) /. mid *. 100. in
           printf "Sell %.8f: VWAP=%.2f, slippage=%.4f%%\n"
             volume avg_price slippage
       | None ->
           printf "Insufficient liquidity for sell %.8f\n" volume)
```

## Exchange-Specific Differences

### Metadata

Each exchange has different metadata:

```ocaml
(* Binance *)
type Binance.Order_book.metadata = {
  last_update_id: int64;
}

(* Hyperliquid *)
type Hyperliquid.Order_book.metadata = {
  last_update_time: int64;
}

(* Kraken *)
type Kraken.Order_book.metadata = unit  (* No extra metadata *)
```

### Symbol Formats

- **Binance**: `BTCUSDT`, `ETHUSDT`
- **Kraken**: `BTC/USD`, `ETH/USD`
- **Gemini**: `btcusd`, `ethusd` (lowercase)
- **MEXC**: `BTCUSDT`, `ETHUSDT`
- **Hyperliquid**: `BTC`, `ETH` (coin only, no quote)
- **Coinbase**: `BTC-USD`, `ETH-USD`
- **Bitrue**: `BTCUSDT`, `ETHUSDT`

### WebSocket Depth

- **Binance**: 100ms or 1000ms updates
- **Kraken**: Depth: 10, 25, 100, 500, 1000
- **Gemini**: Full book with incremental updates
- **MEXC**: Depth20, Depth5
- **Hyperliquid**: L2 book
- **Coinbase**: L2 updates
- **Bitrue**: Depth stream

## Performance Tips

### 1. Use Batch Updates

```ocaml
(* Bad: Multiple single updates *)
let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
let book = Book.set book ~side:`Bid ~price:49999. ~size:2.0 in
let book = Book.set book ~side:`Ask ~price:50001. ~size:1.5 in

(* Good: Single batch update *)
let book = Book.set_many book [
  (`Bid, 50000., 1.0);
  (`Bid, 49999., 2.0);
  (`Ask, 50001., 1.5);
]
```

### 2. Limit Queries

```ocaml
(* Only get what you need *)
let top_5 = Book.best_n_bids book ~n:5 () in  (* O(5) *)
(* vs *)
let all = Book.all_bids book in  (* O(n) *)
```

### 3. Reuse Books

```ocaml
(* Book updates share structure - no need to avoid *)
let book1 = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
let book2 = Book.set book1 ~side:`Bid ~price:50001. ~size:1.5 in
(* book, book1, book2 share unchanged portions *)
```

## See Also

- [Architecture Guide](ARCHITECTURE.md)
- [WebSocket Guide](WEBSOCKET.md)
- [Consolidated Order Book](CONSOLIDATED_ORDERBOOK.md)
- [Exchange Guides](../exchanges/)

# Binance Exchange Integration

Complete Binance/Binance.US API integration with WebSocket streaming and REST API support.

## Overview

- **Exchange**: Binance / Binance.US
- **API Version**: v3 (Spot)
- **WebSocket**: Supported
- **REST API**: Supported
- **Order Book**: Unified interface
- **Trading**: Full support
- **Authentication**: HMAC-SHA256

## Quick Start

### WebSocket Order Book

```ocaml
open Core
open Async
open Fluxum

let () =
  let%bind book_pipe =
    Binance.Order_book.Book.pipe
      (module Binance.Cfg.Production)
      ~symbol:"BTCUSDT"
      ()
  in
  Pipe.iter_without_pushback book_pipe ~f:(fun book ->
    printf "Mid: %.2f | Spread: %.2f\n"
      (Binance.Order_book.Book.mid_price book)
      (Binance.Order_book.Book.spread book)
  )
  |> Command_unix.run
```

### REST API - Account Balance

```ocaml
let check_balance ~api_key ~api_secret =
  let%bind response =
    Binance.V3.Account.call
      ~cfg:(module Binance.Cfg.Production)
      ~api_key
      ~api_secret
      ()
  in
  match response with
  | Ok account ->
      List.iter account.balances ~f:(fun b ->
        if Float.(b.free > 0.) then
          printf "%s: %.8f\n" b.asset b.free
      );
      return ()
  | Error err -> handle_error err
```

### Place Market Order

```ocaml
let place_order ~api_key ~api_secret ~symbol ~side ~quantity =
  let%bind response =
    Binance.V3.New_order.call
      ~cfg:(module Binance.Cfg.Production)
      ~api_key
      ~api_secret
      {
        symbol;
        side;
        order_type = MARKET;
        quantity = Some quantity;
        quote_order_qty = None;
        price = None;
        time_in_force = None;
        new_client_order_id = None;
        recv_window = Some 5000L;
      }
  in
  match response with
  | Ok order ->
      printf "Order placed: %s\n" order.client_order_id;
      return ()
  | Error err -> handle_error err
```

## Configuration

### Production (Binance.com)

```ocaml
module Cfg = Binance.Cfg.Production

(* Settings *)
- Base URL: https://api.binance.com
- WebSocket: wss://stream.binance.com:9443
```

### Binance.US

```ocaml
module Cfg = Binance.Cfg.Binance_us

(* Settings *)
- Base URL: https://api.binance.us
- WebSocket: wss://stream.binance.us:9443
```

### Testnet

```ocaml
module Cfg = Binance.Cfg.Testnet

(* Settings *)
- Base URL: https://testnet.binance.vision
- WebSocket: wss://testnet.binance.vision
```

### Environment Variables

```bash
export BINANCE_API_KEY="your-api-key"
export BINANCE_API_SECRET="your-api-secret"
```

## WebSocket API

### Supported Streams

#### 1. Depth Stream (Order Book)

```ocaml
Binance.Order_book.Book.pipe
  (module Cfg)
  ~symbol:"BTCUSDT"
  ()
```

**Update Frequency**: 100ms or 1000ms
**Stream Name**: `<symbol>@depth` or `<symbol>@depth@100ms`

#### 2. Trade Stream

```ocaml
Binance.Ws.trades
  ~cfg:(module Cfg)
  ~symbol:"BTCUSDT"
  ()
```

**Stream Name**: `<symbol>@trade`
**Data**: Individual trade events

#### 3. Kline/Candlestick

```ocaml
Binance.Ws.klines
  ~cfg:(module Cfg)
  ~symbol:"BTCUSDT"
  ~interval:"1m"
  ()
```

**Intervals**: 1m, 3m, 5m, 15m, 30m, 1h, 2h, 4h, 6h, 8h, 12h, 1d, 3d, 1w, 1M
**Stream Name**: `<symbol>@kline_<interval>`

#### 4. 24hr Ticker

```ocaml
Binance.Ws.ticker_24hr
  ~cfg:(module Cfg)
  ~symbol:"BTCUSDT"
  ()
```

**Stream Name**: `<symbol>@ticker`
**Update**: Real-time

## REST API (v3)

### Public Endpoints

#### Server Time
```ocaml
Binance.V3.Server_time.call ~cfg:(module Cfg) ()
```

Returns server timestamp (for clock synchronization).

#### Exchange Info
```ocaml
Binance.V3.Exchange_info.call ~cfg:(module Cfg) ()
```

Returns trading rules, symbol info, filters.

#### Order Book Depth
```ocaml
Binance.V3.Depth.call
  ~cfg:(module Cfg)
  { symbol = "BTCUSDT"; limit = Some 100 }
```

**Limits**: 5, 10, 20, 50, 100, 500, 1000, 5000

#### Recent Trades
```ocaml
Binance.V3.Recent_trades.call
  ~cfg:(module Cfg)
  { symbol = "BTCUSDT"; limit = Some 100 }
```

**Limit**: Max 1000

#### 24hr Ticker
```ocaml
Binance.V3.Ticker_24hr.call
  ~cfg:(module Cfg)
  { symbol = Some "BTCUSDT" }
```

### Account Endpoints

#### Account Information
```ocaml
Binance.V3.Account.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  ()
```

Returns:
- Balances (free + locked)
- Account permissions
- Maker/taker fees

### Trading Endpoints

#### Place New Order
```ocaml
Binance.V3.New_order.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    symbol = "BTCUSDT";
    side = BUY;
    order_type = LIMIT;
    quantity = Some 0.001;
    price = Some 50000.0;
    time_in_force = Some GTC;
    (* ... *)
  }
```

**Order Types**:
- `LIMIT` - Limit order (requires price, timeInForce)
- `MARKET` - Market order (requires quantity OR quoteOrderQty)
- `STOP_LOSS` - Stop loss
- `STOP_LOSS_LIMIT` - Stop loss limit
- `TAKE_PROFIT` - Take profit
- `TAKE_PROFIT_LIMIT` - Take profit limit
- `LIMIT_MAKER` - Limit maker (post-only)

**Time in Force**:
- `GTC` - Good Till Cancel
- `IOC` - Immediate or Cancel
- `FOK` - Fill or Kill

#### Cancel Order
```ocaml
Binance.V3.Cancel_order.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    symbol = "BTCUSDT";
    order_id = Some 12345L;
    orig_client_order_id = None;
  }
```

#### Query Order
```ocaml
Binance.V3.Query_order.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    symbol = "BTCUSDT";
    order_id = Some 12345L;
    orig_client_order_id = None;
  }
```

#### Open Orders
```ocaml
Binance.V3.Open_orders.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  { symbol = Some "BTCUSDT" }
```

#### All Orders (History)
```ocaml
Binance.V3.All_orders.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    symbol = "BTCUSDT";
    order_id = None;
    start_time = None;
    end_time = None;
    limit = Some 100;
  }
```

#### Trade History
```ocaml
Binance.V3.My_trades.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    symbol = "BTCUSDT";
    start_time = None;
    end_time = None;
    from_id = None;
    limit = Some 100;
  }
```

## Authentication

Binance uses HMAC-SHA256 for request signing:

### Signature Process

1. **Build Query String**:
   ```
   symbol=BTCUSDT&side=BUY&type=LIMIT&quantity=0.001&timestamp=1234567890000
   ```

2. **Sign with HMAC-SHA256**:
   ```ocaml
   let signature = Binance.Signature.sign ~api_secret ~params in
   (* Returns hex-encoded signature *)
   ```

3. **Add to Request**:
   - Header: `X-MBX-APIKEY: <api_key>`
   - Query param: `signature=<signature>`

### Timestamp Requirements

- All signed requests require `timestamp` parameter
- Timestamp must be within ±5000ms of server time (configurable via `recvWindow`)
- Use `Server_time` endpoint to sync clocks

```ocaml
let%bind server_time = Binance.V3.Server_time.call ~cfg () in
let timestamp = server_time.server_time in
(* Use this timestamp in signed requests *)
```

## Order Book

The Binance order book implements the unified `Order_book_base` interface:

### Metadata

```ocaml
type metadata = {
  last_update_id: int64;  (* Last update ID from exchange *)
}
```

### Usage

```ocaml
let book = Binance.Order_book.Book.create ~symbol:"BTCUSDT" in

(* Get best bid/ask *)
let best_bid = Binance.Order_book.Book.best_bid book in
let best_ask = Binance.Order_book.Book.best_ask book in

(* Get mid price and spread *)
let mid = Binance.Order_book.Book.mid_price book in
let spread = Binance.Order_book.Book.spread book in

(* Calculate VWAP *)
let vwap = Binance.Order_book.Book.vwap_buy book ~volume:1.0 in
```

## CLI Usage

```bash
# Query ticker
./fluxum.exe binance ticker-24hr --symbol BTCUSDT

# Get account balances
./fluxum.exe binance account \
  --api-key $BINANCE_API_KEY \
  --api-secret $BINANCE_API_SECRET

# Place limit order
./fluxum.exe binance new-order \
  --symbol BTCUSDT \
  --side BUY \
  --type LIMIT \
  --quantity 0.001 \
  --price 50000 \
  --time-in-force GTC \
  --api-key $BINANCE_API_KEY \
  --api-secret $BINANCE_API_SECRET

# Cancel order
./fluxum.exe binance cancel-order \
  --symbol BTCUSDT \
  --order-id 12345 \
  --api-key $BINANCE_API_KEY \
  --api-secret $BINANCE_API_SECRET

# Query open orders
./fluxum.exe binance open-orders \
  --symbol BTCUSDT \
  --api-key $BINANCE_API_KEY \
  --api-secret $BINANCE_API_SECRET
```

## Rate Limits

### REST API
- **Weight-based system**
- Default: 1200 requests/minute
- Header: `X-MBX-USED-WEIGHT-1M`
- Order endpoints have higher weights

### WebSocket
- Max 5 connections per IP
- Max 300 messages/second per connection
- Automatic ping/pong required

## Error Handling

```ocaml
match response with
| Ok data -> (* Success *)
| Error err ->
    match err with
    | `Http_error (code, msg) ->
        (* HTTP errors: 400, 401, 403, 429, 500, etc. *)
    | `Json_parse_error msg ->
        (* JSON parsing failed *)
    | `Signature_error msg ->
        (* Signature generation failed *)
    | `Network_error msg ->
        (* Connection issues *)
```

### Common Error Codes

- `-1000`: Unknown error
- `-1001`: Disconnected
- `-1002`: Unauthorized
- `-1003`: Too many requests
- `-1010`: Invalid signature
- `-1021`: Timestamp out of window
- `-1022`: Invalid signature
- `-2010`: Insufficient funds
- `-2011`: Unknown order

## Testing

### Unit Tests

```bash
dune runtest lib/exchange/binance/test
```

### Integration Test

```bash
timeout 30 dune exec examples/binance_orderbook.exe
```

## Performance

- **WebSocket latency**: ~50-100ms
- **REST latency**: ~100-300ms
- **Order book updates**: 100ms or 1000ms intervals
- **Max throughput**: ~10,000 updates/second

## See Also

- [Official Binance API Docs](https://developers.binance.com/docs/binance-spot-api-docs)
- [Binance.US API Docs](https://docs.binance.us/)
- [Order Book Base](../guides/ORDER_BOOK.md)
- [WebSocket Guide](../guides/WEBSOCKET.md)

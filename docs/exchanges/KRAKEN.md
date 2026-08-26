# Kraken Exchange Integration

Complete Kraken WebSocket and REST API integration with order book management.

## Overview

- **Exchange**: Kraken
- **WebSocket**: v2 (public), v1 (private)
- **REST API**: v1 and v0 (legacy)
- **Low-latency API**: Unified FIX 4.4 message codec (Spot)
- **Order Book**: Unified interface with configurable depth
- **Trading**: Full support (spot)
- **Authentication**: HMAC-SHA512

## Unified FIX 4.4 fast path

`Kraken.Fix` provides validated FIX framing, exact Kraken Logon signing, session
messages, L2 subscriptions, Spot market/limit orders, cancels, typed L2 updates,
and inbound execution-report classification. Prices and quantities use exact
fixed-point decimals; the API does not accept floats. `Kraken.Fix_session` adds
an Async TLS client with verified certificates and hostnames, reconnects,
heartbeats, TestRequest responses, and durable inbound/outbound sequence state.

FIX access is separate from an ordinary Kraken API key. Before connecting,
request Unified FIX access and a SenderCompID from your Kraken account manager,
create a FIX-specific key, and whitelist the machine's public IP. Resolve DNS at
connection time rather than pinning an address.

Production uses `fix.kraken.com`; UAT uses `fix.uat.kraken.com`. Spot L2 market
data is on port 4000, Spot trading is on port 4001, and Spot L3 data is on port
4005. Hostnames are resolved on every reconnect.

```ocaml
module Fix = Kraken.Fix
module Fix_session = Kraken.Fix_session

let session_or_fail = function
  | Ok value -> value
  | Error error ->
      failwith (Sexp.to_string_hum (Fix_session.sexp_of_error error))

let endpoint =
  Fix.Endpoint.create ~environment:Uat ~service:Spot_market_data_l2

let config =
  Fix_session.Config.create ~endpoint ~sender_comp_id
    ~authentication:Market_data
    ~state_path:"var/kraken-fix-l2.sexp" ()
  |> session_or_fail

let%bind client = Fix_session.Client.create config >>| session_or_fail
let events = Fix_session.Client.events client

(* Run this concurrently with an event consumer. [run] reconnects until stopped. *)
let session = Fix_session.Client.run client
```

For a trading endpoint, construct `Fix.Credentials.t` from the FIX-specific key
and use `Trading { credentials; cancel_on_disconnect; client_id }` as the
authentication value. Keep secrets out of the state path; the state file stores
only sequence numbers. `cancel_on_disconnect` is explicit. Single-order cancels
are session-scoped on Kraken FIX, so preserve the session that originated each
live order.

The runner asks Kraken for missing inbound messages when it detects a sequence
gap, buffers bounded out-of-order frames, and delivers recovered messages in
sequence. When Kraken sends a ResendRequest, the runner replays application
messages from its bounded in-process journal with `PossDupFlag=Y` and gap-fills
administrative messages. The journal intentionally is not restored after a
process restart. Missing or evicted history terminates the session with
`Replay_unavailable`; reconcile exchange state and coordinate a sequence reset
instead of guessing at past economic intent. Use `reset_on_start` only for a
coordinated reset, such as a fresh UAT session, not as routine reconnect policy.

Sequence state and its containing directory are fsynced after every message by
default. For latency testing, raising `checkpoint_every` batches those fsyncs;
clean disconnects still save state, but a process or machine crash can roll
sequence numbers back to the last checkpoint. If a socket flush succeeds but
the checkpoint fails, `send` returns `Sent_but_not_checkpointed`; the message is
ambiguous and must not be blindly retried. That is an explicit durability and
latency tradeoff, not a safe default for unattended trading.

Logon and inbound-silence timeouts fail the connection and trigger reconnect.
Event, inbound-gap, and replay-journal capacities are hard memory bounds; an
overflow fails closed instead of allowing an unbounded fast-path queue.

Run the local codec benchmark with:

```bash
dune exec --profile=release ./bench/kraken_fix_codec.exe -- 200000
```

## Quick Start

### WebSocket Order Book

```ocaml
open Core
open Async
open Fluxum

let () =
  let%bind book_pipe =
    Kraken.Order_book.Book.pipe
      (module Kraken.Cfg.Production)
      ~symbol:"BTC/USD"
      ()
  in
  Pipe.iter_without_pushback book_pipe ~f:(fun book ->
    let best_bid = Kraken.Order_book.Book.best_bid book in
    let best_ask = Kraken.Order_book.Book.best_ask book in
    printf "%s: Bid %.2f | Ask %.2f | Spread %.2f\n"
      (Kraken.Order_book.Book.symbol book)
      Exchange_common.Order_book_base.Price_level.(price best_bid)
      Exchange_common.Order_book_base.Price_level.(price best_ask)
      (Kraken.Order_book.Book.spread book)
  )
  |> Command_unix.run
```

### REST API - Account Balance

```ocaml
let check_balance ~api_key ~api_secret =
  let%bind response =
    Kraken.V1.Balance.call
      ~cfg:(module Kraken.Cfg.Production)
      ~api_key
      ~api_secret
      ()
  in
  match response with
  | Ok balances ->
      Map.iteri balances ~f:(fun ~key ~data ->
        printf "%s: %s\n" key data
      );
      return ()
  | Error err -> handle_error err
```

## Configuration

### Production

```ocaml
module Cfg = Kraken.Cfg.Production

(* Settings *)
- REST API: https://api.kraken.com
- WebSocket (public): wss://ws.kraken.com
- WebSocket (auth): wss://ws-auth.kraken.com
```

### Environment Variables

```bash
export KRAKEN_API_KEY="your-api-key"
export KRAKEN_API_SECRET="your-api-secret"  # Base64-encoded
```

## WebSocket API

Kraken uses WebSocket API v2 for public channels and v1 for authenticated channels.

### Supported Public Channels

#### 1. Book (Order Book)

```ocaml
Kraken.Order_book.Book.pipe
  (module Cfg)
  ~symbol:"BTC/USD"
  ()
```

**Depth Levels**: 10, 25, 100, 500, 1000
**Update Type**: Snapshot + incremental updates
**Channel**: `book`

**Subscription Message**:
```json
{
  "method": "subscribe",
  "params": {
    "channel": "book",
    "symbol": ["BTC/USD"],
    "depth": 10
  }
}
```

#### 2. Ticker

```ocaml
Kraken.Ws.ticker
  ~cfg:(module Cfg)
  ~pairs:["BTC/USD"; "ETH/USD"]
  ()
```

**Channel**: `ticker`
**Data**: Best bid/ask, last trade, volume, VWAP

#### 3. Trades

```ocaml
Kraken.Ws.trades
  ~cfg:(module Cfg)
  ~pairs:["BTC/USD"]
  ()
```

**Channel**: `trade`
**Data**: Price, volume, time, side

#### 4. OHLC (Candles)

```ocaml
Kraken.Ws.ohlc
  ~cfg:(module Cfg)
  ~pairs:["BTC/USD"]
  ~interval:1
  ()
```

**Intervals**: 1, 5, 15, 30, 60, 240, 1440, 10080, 21600 (minutes)
**Channel**: `ohlc`

#### 5. Spread

```ocaml
Kraken.Ws.spread
  ~cfg:(module Cfg)
  ~pairs:["BTC/USD"]
  ()
```

**Channel**: `spread`
**Data**: Best bid, best ask, timestamp

### WebSocket Message Format

```ocaml
(* Subscription confirmation *)
type subscription_status = {
  channel: string;
  depth: int option;
  interval: int option;
  snapshot: bool;
}

(* Book update *)
type book_update = {
  channel: string;
  type_: [`Snapshot | `Update];
  data: book_data list;
}
```

## REST API

### Public Endpoints (v0)

#### Server Time
```ocaml
Kraken.V1.Server_time.call ~cfg:(module Cfg) ()
```

#### Asset Pairs
```ocaml
Kraken.V1.Asset_pairs.call
  ~cfg:(module Cfg)
  { pair = Some "XXBTZUSD" }
```

Returns trading rules, fees, limits.

#### Ticker
```ocaml
Kraken.V1.Ticker.call
  ~cfg:(module Cfg)
  { pair = "XXBTZUSD" }
```

**Response**: 24hr stats, bid/ask, volume, VWAP

#### Order Book (Depth)
```ocaml
Kraken.V1.Depth.call
  ~cfg:(module Cfg)
  {
    pair = "XXBTZUSD";
    count = Some 100;
  }
```

**Count**: Max 500

#### Recent Trades
```ocaml
Kraken.V1.Trades.call
  ~cfg:(module Cfg)
  {
    pair = "XXBTZUSD";
    since = None;
    count = Some 100;
  }
```

#### OHLC Data
```ocaml
Kraken.V1.OHLC.call
  ~cfg:(module Cfg)
  {
    pair = "XXBTZUSD";
    interval = Some 60;  (* minutes *)
    since = None;
  }
```

**Intervals**: 1, 5, 15, 30, 60, 240, 1440, 10080, 21600

### Private Endpoints (v0)

All private endpoints require authentication.

#### Balance
```ocaml
Kraken.V1.Balance.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  ()
```

Returns asset balances as `(string, string) Map.t`.

#### Trade Balance
```ocaml
Kraken.V1.Trade_balance.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  { asset = Some "ZUSD" }
```

Returns margin info, equity, free margin.

#### Open Orders
```ocaml
Kraken.V1.Open_orders.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    trades = Some true;
    userref = None;
  }
```

#### Closed Orders
```ocaml
Kraken.V1.Closed_orders.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    trades = Some false;
    userref = None;
    start = None;
    end_ = None;
    ofs = None;
    closetime = Some `Both;
  }
```

#### Query Orders
```ocaml
Kraken.V1.Query_orders.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    txid = "ORDER-ID";
    trades = Some true;
  }
```

#### Trades History
```ocaml
Kraken.V1.Trades_history.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    type_ = Some `All;
    trades = Some true;
    start = None;
    end_ = None;
    ofs = None;
  }
```

#### Add Order
```ocaml
Kraken.V1.Add_order.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  {
    pair = "XXBTZUSD";
    type_ = `Buy;
    ordertype = `Limit;
    price = Some 50000.0;
    volume = 0.001;
    leverage = None;
    oflags = None;
    starttm = None;
    expiretm = None;
    userref = None;
    validate = Some false;
  }
```

**Order Types**:
- `Limit` - Limit order
- `Market` - Market order
- `Stop_loss` - Stop loss
- `Take_profit` - Take profit
- `Stop_loss_limit` - Stop loss with limit
- `Take_profit_limit` - Take profit with limit
- `Settle_position` - Settle position

#### Cancel Order
```ocaml
Kraken.V1.Cancel_order.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  { txid = "ORDER-ID" }
```

#### Cancel All Orders
```ocaml
Kraken.V1.Cancel_all.call
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  ()
```

## Authentication

Kraken uses HMAC-SHA512 for request signing.

### Signature Process

1. **Build POST Data**:
   ```
   nonce=1234567890000&ordertype=limit&pair=XXBTZUSD&price=50000&type=buy&volume=0.001
   ```

2. **Create Message**:
   ```
   message = path + SHA256(nonce + postdata)
   ```
   Where `path` = `/0/private/AddOrder`

3. **Sign with HMAC-SHA512**:
   ```ocaml
   let signature = Kraken.Signature.sign
     ~api_secret  (* base64-decoded *)
     ~uri_path
     ~nonce
     ~post_data
   in
   (* Returns base64-encoded signature *)
   ```

4. **Add to Headers**:
   ```
   API-Key: <api_key>
   API-Sign: <signature>
   ```

### Nonce Requirements

- Nonce must be strictly increasing
- Typically use Unix timestamp in milliseconds
- Kraken allows nonce window configuration

```ocaml
let nonce = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64 in
(* nonce = 1234567890123456 *)
```

## Order Book

### Metadata

```ocaml
type metadata = unit  (* Kraken doesn't need extra metadata *)
```

### Usage

```ocaml
(* Create order book *)
let book = Kraken.Order_book.Book.create ~symbol:"BTC/USD" in

(* Update with set *)
let book = Kraken.Order_book.Book.set book
  ~side:`Bid
  ~price:50000.0
  ~size:1.5
in

(* Get market data *)
let mid = Kraken.Order_book.Book.mid_price book in
let spread = Kraken.Order_book.Book.spread book in

(* Get best N levels *)
let top_5_bids = Kraken.Order_book.Book.best_n_bids book ~n:5 () in
let top_5_asks = Kraken.Order_book.Book.best_n_asks book ~n:5 () in

(* VWAP calculation *)
let vwap = Kraken.Order_book.Book.vwap_buy book ~volume:10.0 in
```

### WebSocket Integration

The order book automatically processes WebSocket updates:

```ocaml
let%bind book_pipe = Kraken.Order_book.Book.pipe (module Cfg) ~symbol:"BTC/USD" () in

(* Pipe yields updated books on each WebSocket message *)
Pipe.iter book_pipe ~f:(fun book ->
  (* book is already updated with latest data *)
  process_book book
)
```

## Pair Naming

Kraken uses specific naming conventions:

### WebSocket Symbols
- Format: `BASE/QUOTE`
- Examples: `BTC/USD`, `ETH/USD`, `XRP/EUR`

### REST API Pairs
- Format: `X<BASE>Z<QUOTE>` or `<BASE><QUOTE>`
- Examples: `XXBTZUSD`, `XETHZUSD`, `XXRPZEUR`
- Some pairs: `BTCUSD`, `ETHUSD` (no X/Z prefix)

### Conversion

```ocaml
(* WebSocket to REST *)
"BTC/USD" -> "XXBTZUSD"
"ETH/USD" -> "XETHZUSD"

(* REST to WebSocket *)
"XXBTZUSD" -> "BTC/USD"
```

Use `Kraken.Common.normalize_pair` for conversion.

## CLI Usage

```bash
# Query ticker
./fluxum.exe kraken ticker --pair XXBTZUSD

# Get OHLC data
./fluxum.exe kraken ohlc --pair XXBTZUSD --interval 60

# Account balance
./fluxum.exe kraken balance \
  --api-key $KRAKEN_API_KEY \
  --api-secret $KRAKEN_API_SECRET

# Place limit order
./fluxum.exe kraken add-order \
  --pair XXBTZUSD \
  --type buy \
  --ordertype limit \
  --price 50000 \
  --volume 0.001 \
  --api-key $KRAKEN_API_KEY \
  --api-secret $KRAKEN_API_SECRET

# Cancel order
./fluxum.exe kraken cancel-order \
  --txid ORDER-ID \
  --api-key $KRAKEN_API_KEY \
  --api-secret $KRAKEN_API_SECRET

# Open orders
./fluxum.exe kraken open-orders \
  --api-key $KRAKEN_API_KEY \
  --api-secret $KRAKEN_API_SECRET
```

## Rate Limits

### REST API
- **Tier-based system**
- Counter increases with each call
- Counter decreases over time
- Exceeded: HTTP 429 (rate limit)

### WebSocket
- Max 50 subscriptions per connection
- Max 5 connections per IP
- Heartbeat required (automatic)

## Error Handling

```ocaml
match response with
| Ok data -> (* Success *)
| Error err ->
    match err with
    | `Api_error (code, msg) ->
        (* Kraken error codes *)
    | `Http_error (status, body) ->
        (* HTTP errors *)
    | `Json_parse_error msg ->
        (* Parsing failed *)
```

### Common Error Codes

- `EAPI:Invalid key`: Bad API key
- `EAPI:Invalid signature`: Signature mismatch
- `EAPI:Invalid nonce`: Nonce too low or too high
- `EGeneral:Permission denied`: Insufficient permissions
- `EOrder:Insufficient funds`: Not enough balance
- `EOrder:Rate limit exceeded`: Too many requests

## Testing

### Unit Tests

```bash
dune runtest lib/exchange/kraken/test
```

Tests include:
- Signature generation (80+ tests)
- Order book operations
- Message parsing
- REST API types

### Integration Test

```bash
timeout 30 dune exec examples/kraken_orderbook.exe
```

## Performance

- **WebSocket latency**: ~30-80ms
- **REST latency**: ~100-200ms
- **Order book updates**: Real-time (as fast as market moves)
- **Max subscriptions**: 50 per connection

## Advanced Features

### Ledger Management

```ocaml
(* Kraken.Ledger module for position tracking *)
let ledger = Kraken.Ledger.create () in

(* Update from order events *)
let%bind ledger = Kraken.Ledger.update_from_order_events
  ledger
  order_events_pipe
in

(* Get position *)
let position = Kraken.Ledger.position ledger ~symbol:"BTC/USD" in
```

### Session Management

```ocaml
(* Kraken.Session for authenticated WebSocket *)
let%bind session = Kraken.Session.create
  ~cfg:(module Cfg)
  ~api_key
  ~api_secret
  ()
in

(* Subscribe to private channels *)
let%bind orders_pipe = Kraken.Session.open_orders session in
```

## See Also

- [Official Kraken API Docs](https://docs.kraken.com/rest/)
- [Kraken WebSocket API](https://docs.kraken.com/websockets/)
- [Order Book Guide](../guides/ORDER_BOOK.md)
- [Authentication Guide](../guides/AUTHENTICATION.md)

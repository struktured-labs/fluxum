# Fluxum vs CCXT Feature Gap Analysis

**Goal:** Identify what features CCXT (Python's standard exchange library) has that fluxum is missing, prioritized by impact.

**Last Updated:** February 2026

---

## Implementation Progress

| Phase | Feature | Status | Version |
|-------|---------|--------|---------|
| 1 | Rate Limiting Infrastructure | ✅ Complete | v0.20.0 |
| 2 | Time-in-Force + Stop Orders | ✅ Complete | v0.20.0 |
| 3 | Historical OHLCV/Candles | ✅ Complete | v0.21.0 |
| 4 | Account Operations (Deposits/Withdrawals) | ⏳ Pending | - |
| 5 | Perpetuals/Futures Types | ⏳ Deprioritized | - |

**Note:** Phase 4 and 5 swapped priorities. US retail traders (especially PA) cannot legally trade perpetuals/futures on offshore exchanges (Bybit, OKX, Hyperliquid). Account operations are more immediately useful.

---

## Performance Comparison: Fluxum vs CCXT

| Metric | CCXT (Python) | Fluxum (OCaml) | Notes |
|--------|---------------|----------------|-------|
| **ECDSA Signing** | ~45ms (pure Python) | <1ms (native) | CCXT can use Coincurve for 0.05ms |
| **Request Overhead** | ~5-10ms (async) | <1ms (Async) | Jane Street Async is very efficient |
| **Memory per Connection** | ~10-50MB (Python runtime) | ~1-5MB | OCaml has lower baseline |
| **WebSocket Reconnect** | Manual/varies | Auto with heartbeat | Fluxum has RFC 6455 ping/pong |
| **Rate Limit Handling** | Built-in token bucket | Built-in token bucket | Parity (v0.20.0) |
| **Concurrent Streams** | GIL-limited | True parallel | OCaml multicore/Async |

**CCXT Rate Limits:** The `rateLimit` property is ms between requests. E.g., rateLimit=100 → 10 req/sec.

**Latency Observations (CCXT issues):**
- Proxy overhead: ~0.006s vs ~0.054s direct
- Order placement: 5ms ping but ~700ms order latency (exchange-dependent)
- Signing: Coincurve reduces from 45ms to 0.05ms

**Fluxum Advantages:**
- No GIL - true parallelism for multi-exchange arbitrage
- Lower memory footprint for long-running processes
- Type safety catches errors at compile time
- Better WebSocket architecture with auto-reconnect

---

## Current Fluxum State

### Exchange Coverage (35 total)
- **Production CEX (11):** Gemini, Kraken, Coinbase, Binance, OKX, Bybit, Bitstamp, MEXC, Bitrue, Hyperliquid, dYdX
- **DEX Adapters (15):** Uniswap V3, Curve, Balancer, SushiSwap, etc.
- **Pool Discovery (9):** Various AMMs with REST-only adapters

### What Fluxum Does Well
- ✅ Spot trading (place/cancel orders, balances, order status)
- ✅ Real-time WebSocket streams (order books, trades, tickers)
- ✅ Normalized types across all exchanges
- ✅ P&L tracking (28-field ledger with FIFO cost basis)
- ✅ Multi-exchange order book consolidation + arbitrage detection
- ✅ On-chain DEX execution (Uniswap V3)
- ✅ Auto-reconnecting sessions with heartbeat

---

## Gap Analysis: What CCXT Has That Fluxum Lacks

### ✅ Phase 1: Rate Limiting Infrastructure (COMPLETE)

**CCXT:** Built-in rate limiter with per-exchange configs, automatic backoff, request queuing
**Fluxum (v0.20.0+):** Token bucket rate limiter with:
- Per-exchange configs (Gemini, Kraken, Binance, Bybit, OKX, etc.)
- Exponential backoff with jitter
- `with_rate_limit` and `with_rate_limit_retry` functions

**Files:** `lib/exchange/common/rate_limiter.ml`

### ✅ Phase 2: Time-in-Force + Conditional Orders (COMPLETE)

**CCXT:** GTC, IOC, FOK, PO (post-only), GTD (good-til-date), Stop-loss, Take-profit, Trailing stop
**Fluxum (v0.20.0+):**
- `Time_in_force.t`: GTC, IOC, FOK, GTD
- `Order_kind.t` restructured with:
  - `Basic`: Market, Limit, Post_only
  - `Conditional`: Stop_market, Stop_limit, Take_profit_market, Take_profit_limit, Trailing_stop

**Files:** `lib/types.ml`, all 12 exchange adapters updated

### ✅ Phase 3: Historical OHLCV/Candles (COMPLETE)

**CCXT:** `fetch_ohlcv(symbol, timeframe, since, limit)` with pagination
**Fluxum (v0.21.0+):**
- `Timeframe.t`: M1, M3, M5, M15, M30, H1, H2, H4, H6, H8, H12, D1, D3, W1, MO1
- `Candle.t`: venue, symbol, timeframe, open_time, OHLCV, quote_volume, trades, closed
- `get_candles` in `Exchange_intf.S` with since/until/limit parameters
- Full implementation: Binance, Kraken
- Stub implementation: All other adapters

**Files:** `lib/types.ml`, `lib/exchange_intf.ml`, all 14 exchange adapters

---

### ⏳ Phase 4: Account Operations (PENDING)

**CCXT:** `fetch_deposit_address()`, `withdraw()`, `fetch_deposits()`, `fetch_withdrawals()`
**Fluxum:** None

**Impact:** Manual fund transfers required; can't automate rebalancing across exchanges
**Target Exchanges:** Gemini, Kraken, Coinbase (US-friendly, regulated)

#### New Types Required
- `Transfer_status.t`: Pending, Processing, Completed, Failed, Cancelled
- `Deposit_address.t`: venue, currency, address, tag, network
- `Deposit.t`: venue, id, currency, amount, status, address, tx_id, timestamps
- `Withdrawal.t`: venue, id, currency, amount, fee, status, address, tag, tx_id, timestamps

#### Exchange API Endpoints

| Exchange | Deposit Address | Withdraw | Deposit History | Withdrawal History |
|----------|-----------------|----------|-----------------|-------------------|
| Gemini | `POST /v1/deposit/{currency}/newAddress` | `POST /v1/withdraw/{currency}` | `POST /v1/transfers` | `POST /v1/transfers` |
| Kraken | `POST /0/private/DepositAddresses` | `POST /0/private/Withdraw` | `POST /0/private/DepositStatus` | `POST /0/private/WithdrawStatus` |
| Coinbase | `POST /v2/accounts/:id/addresses` | `POST /v2/accounts/:id/transactions` | `GET /v2/accounts/:id/deposits` | `GET /v2/accounts/:id/withdrawals` |

**Effort:** ~200-300 lines per exchange (3 exchanges = ~800 lines total)

---

### ⏳ Phase 5: Perpetuals/Futures (DEPRIORITIZED)

**Reason:** US retail traders cannot legally trade perpetuals on offshore exchanges. PA specifically has strict regulations. Hyperliquid, Bybit, OKX all geo-block US users.

**CCXT:** Position with entry_price, liquidation_price, leverage, margin_mode, unrealized_pnl
**Fluxum:** No Position type; Hyperliquid has funding rates but not exposed uniformly

**If needed later:**
- Add `Position.t` to `lib/types.ml`
- Add `Funding_rate.t` and `get_funding_rate` to interface
- Implement for: Hyperliquid, Bybit, OKX, dYdX

---

## Feature Comparison Matrix

| Feature | CCXT | Fluxum | Status |
|---------|------|--------|--------|
| **Spot Orders** | ✅ | ✅ | Parity |
| **Order Types** | Market/Limit/Stop/OCO | Market/Limit/Post_only/Stop/TP/Trailing | ✅ v0.20.0 |
| **Time-in-Force** | GTC/IOC/FOK/PO/GTD | GTC/IOC/FOK/GTD | ✅ v0.20.0 |
| **Rate Limiter** | ✅ Built-in | ✅ Token bucket | ✅ v0.20.0 |
| **Balances** | ✅ | ✅ | Parity |
| **Order Book** | ✅ | ✅ | Parity |
| **Ticker** | ✅ | ✅ | Parity |
| **Public Trades** | ✅ | ✅ | Parity |
| **User Trades** | ✅ | ✅ | Parity |
| **OHLCV REST** | ✅ | ✅ Binance/Kraken | ✅ v0.21.0 |
| **OHLCV WS** | ⚠️ Some | ✅ | Fluxum better |
| **Futures Position** | ✅ | ❌ | Phase 5 (deprioritized) |
| **Funding Rates** | ✅ | ⚠️ Hyperliquid only | Phase 5 (deprioritized) |
| **Open Interest** | ✅ | ⚠️ Hyperliquid only | Phase 5 (deprioritized) |
| **Deposits/Withdrawals** | ✅ | ❌ | Phase 4 |
| **Sub-Accounts** | ✅ | ❌ | Future |
| **Margin/Leverage** | ✅ | ❌ | Future |
| **WebSocket Streams** | ⚠️ Some | ✅ | Fluxum better |
| **Multi-Exchange Arb** | ❌ | ✅ | Fluxum better |
| **P&L Tracking** | ❌ | ✅ | Fluxum better |
| **DEX Execution** | ❌ | ✅ | Fluxum better |

---

## Fluxum Strengths vs CCXT

Features where Fluxum exceeds CCXT:

1. **Better WebSocket Architecture** - Auto-reconnect with heartbeat, RFC 6455 ping/pong
2. **Real-time P&L Tracking** - 28-field ledger with FIFO cost basis
3. **Multi-Exchange Arbitrage** - Consolidated order books with spread detection
4. **On-chain DEX Execution** - Native Uniswap V3 swap execution
5. **OCaml Type Safety** - Compile-time guarantees, exhaustive pattern matching
6. **Normalized Types** - Consistent interface across 35+ exchanges
7. **No GIL** - True parallelism for multi-exchange operations
8. **Lower Memory** - ~5x less memory than Python for long-running processes

---

## Remaining Gaps to Close

### High Priority (Phase 4)
1. **Deposits/Withdrawals** - For automated fund management and rebalancing
   - Target: Gemini, Kraken, Coinbase (US-friendly)

### Lower Priority (Phase 5+)
2. **Perpetuals Position type** - If US regulations change or for non-US users
3. **Unified funding rates** - For funding cost analysis
4. **Sub-account transfers** - For multi-strategy setups
5. **Margin settings** - For margin trading

---

## Version History

| Version | Features Added |
|---------|---------------|
| v0.19.1 | WebSocket RFC 6455 ping/pong |
| v0.20.0 | Rate limiter, Time-in-force, Stop orders |
| v0.21.0 | Historical OHLCV candles |

---

## References

- [CCXT GitHub](https://github.com/ccxt/ccxt)
- [CCXT Documentation](https://docs.ccxt.com/)
- [CCXT Rate Limiting Discussion](https://github.com/ccxt/ccxt/issues/13949)
- [CCXT Latency Meter](https://github.com/DenisKolodin/ccxt-latency)

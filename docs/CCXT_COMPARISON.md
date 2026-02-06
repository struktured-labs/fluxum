# Fluxum vs CCXT Feature Gap Analysis

**Goal:** Identify what features CCXT (Python's standard exchange library) has that fluxum is missing, prioritized by impact.

**Last Updated:** February 2026

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

## Implementation Progress

| Phase | Feature | Status | Version |
|-------|---------|--------|---------|
| 1 | Rate Limiting Infrastructure | ✅ Complete | v0.20.0 |
| 2 | Time-in-Force + Stop Orders | ✅ Complete | v0.20.0 |
| 3 | Historical OHLCV/Candles | ✅ Complete | v0.21.0 |
| 4 | Perpetuals/Futures Types | ⏳ Pending | - |
| 5 | Account Operations | ⏳ Pending | - |

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

### ⏳ Phase 4: Perpetuals/Futures (PENDING)

#### Futures/Perpetuals Position Types
**CCXT:** Position with entry_price, liquidation_price, leverage, margin_mode, unrealized_pnl
**Fluxum:** No Position type; Hyperliquid has funding rates but not exposed uniformly

**Impact:** Can't properly track perpetual positions
**Effort:** Medium (~200 lines)
**Files:** `lib/types.ml` (add `Position.t`), update Hyperliquid/Bybit/OKX adapters

#### Funding Rates (Unified)
**CCXT:** `fetch_funding_rate(symbol)`, `fetch_funding_rate_history()`
**Fluxum:** Only Hyperliquid has funding rates; Bybit/OKX have perpetuals but no rate exposure

**Impact:** Can't analyze funding costs across exchanges
**Effort:** Low (~100 lines)
**Files:** `lib/types.ml` (add `Funding_rate.t`), update perpetual adapters

---

### ⏳ Phase 5: Account Operations (PENDING)

#### Deposit/Withdrawal Operations
**CCXT:** `fetch_deposit_address()`, `withdraw()`, `fetch_deposits()`, `fetch_withdrawals()`
**Fluxum:** None

**Impact:** Manual fund transfers required
**Effort:** Medium-High (~200 lines per exchange)

#### Sub-Account Management
**CCXT:** `fetch_accounts()`, `transfer()` between accounts
**Fluxum:** Single account only

#### Lending/Borrowing
**CCXT:** `fetch_borrow_rate()`, `borrow()`, `repay()`
**Fluxum:** None (despite Aave/Compound in venues)

#### Margin Trading Types
**CCXT:** Isolated vs cross margin, leverage settings
**Fluxum:** Spot-only types

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
| **Futures Position** | ✅ | ❌ | Phase 4 |
| **Funding Rates** | ✅ | ⚠️ Hyperliquid only | Phase 4 |
| **Open Interest** | ✅ | ⚠️ Hyperliquid only | Phase 4 |
| **Deposits/Withdrawals** | ✅ | ❌ | Phase 5 |
| **Sub-Accounts** | ✅ | ❌ | Phase 5 |
| **Margin/Leverage** | ✅ | ❌ | Phase 5 |
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

---

## Remaining Gaps to Close

### High Priority (Phase 4)
1. **Perpetuals Position type** - Needed for derivatives trading
2. **Unified funding rates** - Needed for funding cost analysis

### Lower Priority (Phase 5)
3. **Deposits/Withdrawals** - For automated fund management
4. **Sub-account transfers** - For multi-strategy setups
5. **Margin settings** - For margin trading

---

## Version History

| Version | Features Added |
|---------|---------------|
| v0.19.1 | WebSocket RFC 6455 ping/pong |
| v0.20.0 | Rate limiter, Time-in-force, Stop orders |
| v0.21.0 | Historical OHLCV candles |

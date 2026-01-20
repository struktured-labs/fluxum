# Exchange Implementation Status

## Overview

Fluxum supports 10 exchanges with varying levels of integration. This document provides a comprehensive status matrix of implemented features for each exchange.

**Last Updated:** 2026-01-17 (Phase 4, Priority 3 completion)

## Feature Matrix

| Exchange | Type | Trading | Market Data | WebSocket | Order Book | Ledger | Session | Fallible Normalize | Status |
|----------|------|---------|-------------|-----------|------------|--------|---------|-------------------|--------|
| **Gemini** | CEX | ✅ REST | ✅ REST | ✅ Curl | ✅ L2 | ✅ P&L | ✅ Auto-reconnect | ✅ Phase 1 | **Production** |
| **Kraken** | CEX | ✅ REST | ✅ REST | ✅ v2 Curl | ✅ L2 | ✅ P&L | ✅ Auto-reconnect | ✅ Phase 1 | **Production** |
| **MEXC** | CEX | ✅ REST | ✅ REST | ✅ Curl | ✅ L2 | ❌ | ❌ | ✅ Phase 1 | **Beta** |
| **Hyperliquid** | L1 DEX | ❌ Blockchain | ✅ REST | ✅ Curl | ✅ L2 | ❌ | ❌ | ✅ Phase 1 | **Market Data Only** |
| **Binance** | CEX | ✅ REST | ✅ REST | ✅ Curl | ✅ L2 | ✅ P&L | ✅ Auto-reconnect | ✅ Phase 1 | **Production** |
| **Coinbase** | CEX | ✅ REST | ✅ REST | ✅ Curl | ✅ L2 | ❌ | ❌ | ⚠️ Partial | **Partial** |
| **Bitrue** | CEX | ✅ REST | ✅ REST | ✅ Curl | ✅ L2 | ❌ | ❌ | ⚠️ Partial | **Market Data Primary** |
| **dYdX** | L1 DEX | ❌ Blockchain | ✅ REST | ✅ | ✅ L3 | ❌ | ❌ | ⚠️ Partial | **Market Data Only** |
| **Jupiter** | Solana DEX | ❌ On-chain | ✅ Aggregator | ❌ | ❌ | ❌ | ❌ | ⚠️ Partial | **Experimental** |
| **1inch** | DEX Aggregator | ❌ On-chain | ✅ Aggregator | ❌ | ❌ | ❌ | ❌ | ⚠️ Partial | **Experimental** |

### Legend

- ✅ **Implemented** - Feature is complete and tested
- ⚠️ **Partial** - Feature is partially implemented or has limitations
- ❌ **Not Implemented** - Feature is planned but not yet available
- **CEX** - Centralized Exchange
- **DEX** - Decentralized Exchange
- **L1** - Layer 1 blockchain-based
- **Curl** - Using websocket_curl library (rock solid)
- **v2** - Using exchange's v2 API

## Detailed Exchange Information

### Gemini (Production Ready) ✅

**Implementation:** Complete reference implementation of Exchange_intf.S

**Features:**
- ✅ REST trading (spot only, no derivatives)
- ✅ WebSocket market data (trades, order book, L2 updates)
- ✅ Order book tracking with incremental updates
- ✅ P&L ledger (28 fields: position, cost basis, realized/unrealized P&L)
- ✅ Session management with auto-reconnect
- ✅ Fallible normalization (Phase 1 complete)

**Authentication:**
- API key/secret via environment variables (GEMINI_API_KEY, GEMINI_SECRET)
- HMAC-SHA384 signatures
- Nonce management with file-based tracking

**Rate Limits:**
- Public: 120 requests/minute
- Private: 600 requests/minute
- WebSocket: No documented limit

**Symbol Format:** Lowercase, no separator (`btcusd`, `ethusd`)

**Known Limitations:**
- Spot trading only (no margin, no derivatives)
- No batch order placement
- Authenticated WebSocket needs custom header support (websocket_curl limitation)

**Documentation:** See [fluxum_adapter.ml](../../lib/exchange/gemini/fluxum_adapter.ml) module docstring

---

### Kraken (Production Ready) ✅

**Implementation:** Complete implementation with WebSocket v2 support

**Features:**
- ✅ REST trading (spot, margin, futures via different APIs)
- ✅ WebSocket market data (trades, order book, ticker, OHLC)
- ✅ WebSocket v2 with authenticated feeds
- ✅ Order book tracking with safe float conversions
- ✅ P&L ledger with comprehensive tracking
- ✅ Session management with auto-restart
- ✅ Fallible normalization (Phase 1 complete)

**Authentication:**
- API key/secret via environment variables (KRAKEN_API_KEY, KRAKEN_SECRET)
- HMAC-SHA512 signatures
- Nonce: Unix timestamp in milliseconds

**Rate Limits (Tier-dependent):**
- Starter: 15 requests/second, burst up to 20 orders
- Intermediate: 20 requests/second
- Pro: 20 requests/second with higher call limits
- Max tier: 60+ requests/second

**Symbol Format:** Uppercase with prefixes (`XBTUSD`, `XXBTZUSD`, `ETHUSD`)
- XBT = Bitcoin (Kraken's convention)
- Pairs may have XX prefix for base, Z prefix for fiat quote

**Known Limitations:**
- Symbol naming can be inconsistent (e.g., BTC vs XBT)
- Futures require separate API integration

**Documentation:** See [fluxum_adapter.ml](../../lib/exchange/kraken/fluxum_adapter.ml) module docstring

---

### MEXC (Beta) ⚠️

**Implementation:** Complete basic features, missing advanced integrations

**Features:**
- ✅ REST trading (spot only)
- ✅ WebSocket market data (trades, depth, kline, 24hr ticker)
- ✅ Order book tracking with incremental updates
- ✅ Safe float conversions (Phase 3 complete)
- ✅ Fallible normalization (Phase 1 complete)
- ✅ Binance-compatible API structure
- ❌ Ledger tracking (not yet implemented)
- ❌ Session management (not yet implemented)

**Authentication:**
- API key/secret via environment variables (MEXC_API_KEY, MEXC_SECRET)
- HMAC-SHA256 signatures
- Timestamp-based nonce

**Rate Limits:**
- Public: 20 requests/second
- Private: 10 requests/second
- WebSocket: 10 connections per IP

**Symbol Format:** Uppercase with underscore (`BTC_USDT`, `ETH_USDT`)

**Known Limitations:**
- No P&L ledger tracking
- No automatic session recovery
- Binance compatibility not 100% (some endpoints differ)

**Documentation:** See [fluxum_adapter.ml](../../lib/exchange/mexc/fluxum_adapter.ml) module docstring

---

### Hyperliquid (Market Data Only) 🔷

**Implementation:** L1 blockchain DEX with read-only market data access

**Features:**
- ✅ REST market data (order books, trades, ticker-like data)
- ✅ REST account queries (positions, balances, open orders, fills)
- ✅ WebSocket market data (L2 book, trades, all mids)
- ✅ Order book tracking with safe float conversions
- ✅ Fallible normalization (Phase 1 complete)
- ❌ Trading operations (requires blockchain signing - not yet implemented)
- ❌ Ledger tracking (not yet implemented)
- ❌ Session management (not yet implemented)

**Architecture:** Hyperliquid L1 blockchain (perpetuals-only)

**Trading Implementation Roadmap:**
- Phase 1: Market data ✅ (complete)
- Phase 2: Account queries ✅ (complete)
- Phase 3: Order signing (requires eth-crypto integration)
- Phase 4: Order placement via REST/WebSocket

**Symbol Format:** Uppercase, no separator (`BTC`, `ETH`) - perpetuals only

**Known Limitations:**
- Trading requires blockchain signing (not standard REST)
- Perpetuals only (no spot markets)
- Account queries work but order placement not implemented

**Documentation:** See [fluxum_adapter.ml](../../lib/exchange/hyperliquid/fluxum_adapter.ml) module docstring

---

### Binance (Production Ready) ✅

**Implementation:** Complete implementation with Ledger and Session support

**Features:**
- ✅ REST trading (spot, margin, futures)
- ✅ WebSocket market data (trades, depth, ticker, klines)
- ✅ Order book tracking with websocket_curl
- ✅ P&L ledger with comprehensive accounting (28 fields)
- ✅ Session management with auto-reconnecting streams
- ✅ Fallible normalization (Phase 1 complete)

**Authentication:**
- API key/secret via environment variables (BINANCE_API_KEY, BINANCE_SECRET)
- HMAC-SHA256 signatures
- Timestamp-based nonce
- Recv window support for clock skew

**Rate Limits (Weight-Based System):**
- Public: 1200 requests/minute per IP
- Private: 1200 requests/minute per UID
- Order placement: 10/second per account, 100K/day
- WebSocket: 5 connections per IP, 300 streams per connection

**Symbol Format:** Uppercase, no separator (`BTCUSDT`, `ETHUSDT`, `BNBUSDT`)

**Order Types:** Market, Limit, Stop-Loss, Stop-Loss-Limit, Take-Profit, Take-Profit-Limit, Iceberg, OCO

**Known Limitations:**
- Separate credentials required for spot/margin/futures
- Some advanced order types not yet exposed
- Futures API integration pending

**Production Readiness:**
- Largest exchange by volume (critical for arbitrage)
- All normalize functions return Result.t
- Comprehensive P&L tracking
- Auto-reconnecting session management
- Battle-tested in production systems

**Documentation:** See [fluxum_adapter.ml](../../lib/exchange/binance/fluxum_adapter.ml) module docstring

---

### Coinbase (Partial Implementation) ⚠️

**Implementation:** Advanced Trade API with basic features

**Features:**
- ✅ REST trading (Advanced Trade API)
- ✅ WebSocket market data (level2, market_trades, ticker, candles)
- ✅ Order book tracking with websocket_curl
- ⚠️ Partial fallible normalization (some unsafe operations remain)
- ❌ Ledger tracking (not yet implemented)
- ❌ Session management (not yet implemented)

**Symbol Format:** Hyphenated (`BTC-USD`, `ETH-USD`)

**Status:** Functional but needs Phase 1 completion for all normalize functions

---

### Bitrue (Market Data Primary) ⚠️

**Implementation:** WebSocket market data focus

**Features:**
- ✅ REST market data and basic trading
- ✅ WebSocket market data (trades, depth, ticker, klines)
- ✅ Order book tracking with websocket_curl
- ✅ Automatic ping/pong handling
- ⚠️ Partial fallible normalization (some unsafe operations remain)
- ❌ Ledger tracking (not yet implemented)
- ❌ Session management (not yet implemented)

**Symbol Format:** Uppercase, no separator (`BTCUSDT`)

**Status:** Market data is solid, trading is basic

---

### dYdX (Market Data Only) 🔷

**Implementation:** L1 blockchain DEX (v4 decentralized)

**Features:**
- ✅ REST market data
- ✅ WebSocket market data
- ✅ Order book tracking (L3 - order-by-order)
- ⚠️ Partial fallible normalization
- ❌ Trading (requires blockchain signing)
- ❌ Ledger tracking
- ❌ Session management

**Architecture:** dYdX v4 on custom blockchain (Cosmos SDK)

**Known Limitations:**
- Trading requires blockchain integration (not standard REST)
- Account operations need wallet signing

---

### Jupiter (Experimental) 🧪

**Implementation:** Solana DEX aggregator integration

**Features:**
- ✅ REST aggregator API (quote, swap routes)
- ⚠️ Partial normalization
- ❌ On-chain execution (requires Solana wallet integration)
- ❌ WebSocket market data
- ❌ Order book tracking

**Status:** Experimental - quote fetching works, execution requires Solana integration

---

### 1inch (Experimental) 🧪

**Implementation:** Multi-chain DEX aggregator

**Features:**
- ✅ REST aggregator API (quotes, swap routes)
- ⚠️ Partial normalization
- ❌ On-chain execution (requires Web3 wallet integration)
- ❌ WebSocket market data
- ❌ Order book tracking

**Status:** Experimental - routing works, execution requires wallet integration

---

## Phase 1 Completion Status

**Fallible Normalize Functions (Result.t returns):**

| Exchange | Normalize Functions | Phase 1 Complete | Notes |
|----------|-------------------|------------------|-------|
| Gemini | All ✅ | ✅ Yes | Reference implementation |
| Kraken | All ✅ | ✅ Yes | Complete with safe conversions |
| Hyperliquid | All ✅ | ✅ Yes | Market data only |
| MEXC | All ✅ | ✅ Yes | Complete with safe conversions |
| Binance | All ✅ | ✅ Yes | Complete with safe conversions |
| Coinbase | Partial ⚠️ | ❌ No | ~60% complete |
| Bitrue | Partial ⚠️ | ❌ No | ~50% complete |
| dYdX | Partial ⚠️ | ❌ No | ~40% complete |
| Jupiter | Partial ⚠️ | ❌ No | ~30% complete |
| 1inch | Partial ⚠️ | ❌ No | ~30% complete |

**Priority exchanges** (Gemini, Kraken, Hyperliquid, MEXC) have 100% fallible normalization coverage.

See [NORMALIZE_CONTRACT.md](./NORMALIZE_CONTRACT.md) for details on fallible normalization.

---

## WebSocket Implementation Status

**Post-Migration Status (2026-01-17):**

All exchanges now use `websocket_curl` for WebSocket connections. The old `cohttp_async_websocket` dependency has been completely removed.

| Exchange | WebSocket Library | Status | Notes |
|----------|------------------|--------|-------|
| Gemini | websocket_curl | ✅ Migrated | Auth channels may need header support |
| Kraken | websocket_curl | ✅ Native | v2 API support |
| MEXC | websocket_curl | ✅ Native | Binance-compatible |
| Hyperliquid | websocket_curl | ✅ Native | L1 blockchain feeds |
| Binance | websocket_curl | ✅ Migrated | Use Market_data module |
| Coinbase | websocket_curl | ✅ Migrated | Use Market_data module |
| Bitrue | websocket_curl | ✅ Migrated | Use Market_data_curl module |
| dYdX | (varies) | ⚠️ Legacy | Needs review |

**CI Status:** 🟢 All builds passing with clean dependencies

---

## Recommended Exchanges by Use Case

### Production Trading
**Binance**, **Gemini**, or **Kraken**
- Complete feature set (trading, market data, P&L tracking, session management)
- Fallible normalization (robust error handling)
- Well-tested and documented
- **Binance**: Largest exchange by volume (best liquidity)

### Market Data Only
**MEXC** or **Hyperliquid**
- Reliable WebSocket feeds
- Good order book tracking
- No trading required

### High-Frequency Trading
**Kraken**
- Lowest latency (WebSocket v2)
- High rate limits (60+ req/s on Pro tier)
- Batch order support

### Multi-Exchange Arbitrage
**Binance + Gemini + Kraken** or **Binance + Kraken + MEXC**
- Use `Consolidated_order_book` for aggregated L2 data
- All have complete fallible normalization
- **Binance**: Critical for arbitrage (largest volume, best liquidity)
- Compatible symbol formats

### DEX Integration (Experimental)
**Hyperliquid** (perpetuals) or **Jupiter** (Solana spot)
- Market data available
- Trading requires blockchain integration (future work)

---

## Development Priorities

### Next Phase Targets

**Phase 2: Complete Fallible Normalization**
- Binance: Migrate remaining normalize functions to Result.t
- Coinbase: Migrate remaining normalize functions to Result.t
- Bitrue: Migrate remaining normalize functions to Result.t

**Phase 3: Expand Production Coverage**
- Binance: Add Ledger and Session modules
- Coinbase: Add Ledger and Session modules
- MEXC: Add Ledger module

**Phase 4: Blockchain Trading**
- Hyperliquid: Implement order signing and placement
- dYdX: Implement blockchain signing
- Jupiter: Add Solana wallet integration

---

## Testing Status

| Exchange | Unit Tests | Integration Tests | WebSocket Tests | Coverage |
|----------|------------|------------------|----------------|----------|
| Gemini | ✅ 400+ lines | ✅ Public endpoints | ✅ Error paths | ~80% |
| Kraken | ✅ 527 lines | ✅ Public endpoints | ✅ v2 feeds | ~75% |
| MEXC | ✅ 598 lines | ✅ Public endpoints | ✅ Error handling | ~70% |
| Hyperliquid | ✅ 471 lines | ✅ Public endpoints | ✅ L1 feeds | ~65% |
| Binance | ⚠️ Partial | ⚠️ Basic | ❌ Needed | ~40% |
| Coinbase | ⚠️ Partial | ⚠️ Basic | ❌ Needed | ~40% |
| Others | ❌ Minimal | ❌ Minimal | ❌ Minimal | <20% |

---

## Contributing

To add support for a new exchange or complete an existing implementation:

1. Review [Exchange_intf.S](../../lib/exchange_intf.mli) interface
2. Follow the pattern from Gemini (reference implementation)
3. Ensure all normalize functions return Result.t
4. Add comprehensive unit tests (400+ lines recommended)
5. Add integration tests for public endpoints
6. Update this status document

See [CLAUDE.md](../../CLAUDE.md) for detailed implementation guidance.

---

## See Also

- [NORMALIZE_CONTRACT.md](./NORMALIZE_CONTRACT.md) - Fallible normalization guide
- [MIGRATION_PHASE1.md](../MIGRATION_PHASE1.md) - Breaking changes from Phase 1
- [Exchange_intf.mli](../../lib/exchange_intf.mli) - Exchange adapter interface
- [Consolidated APIs](../../lib/) - Cross-exchange aggregation (order book, balance)

---

**Maintenance:** This document should be updated whenever:
- A new exchange is added
- An exchange implementation is completed
- Major features are added/removed
- Phase completion milestones are reached

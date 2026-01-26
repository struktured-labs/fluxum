# Exchange Implementation Status

This document tracks the implementation status of all exchange adapters in Fluxum.

## CEX (Centralized Exchanges)

| Exchange | Trading | Market Data | WebSocket | Session | Ledger | Pool Adapter | Status |
|----------|---------|-------------|-----------|---------|--------|--------------|--------|
| Gemini | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | **Production** |
| Kraken | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | **Production** |
| Binance | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| MEXC | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Beta |
| Hyperliquid | ❌* | ✅ | ✅ | ❌ | ❌ | ✅ | *Blockchain only |
| Coinbase | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| Bitrue | ❌ | ✅ | ✅ | ❌ | ❌ | ✅ | Data only |
| Bybit | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| Bitfinex | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| Bitstamp | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | Partial |
| Gate.io | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| HTX | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| KuCoin | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ | Partial |
| Poloniex | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | Partial |

**Notes:**
- *Hyperliquid: Trading via blockchain signing only (L1 transactions, not REST)
- Pool Adapters for CEXes model order book liquidity as synthetic pools

## DEX (Decentralized Exchanges)

### EVM DEXes

| Exchange | Chain | Pool Type | Pool Adapter | Status |
|----------|-------|-----------|--------------|--------|
| SushiSwap | Multi | Constant Product | ✅ | Production |
| Uniswap V3 | Multi | Concentrated | ✅ | Production |
| Curve | Multi | Stable | ✅ | Production |
| Balancer | Multi | Weighted | ✅ | Production |
| PancakeSwap | BSC | Constant Product | ✅ | Production |
| Thena | BSC | Constant Product | ✅ | Production |
| Aerodrome | Base | Concentrated | ✅ | Production |
| Velodrome | Optimism | Concentrated | ✅ | Production |
| QuickSwap | Polygon | Constant Product | ✅ | Production |
| Camelot | Arbitrum | Concentrated | ✅ | Production |
| SpookySwap | Fantom | Constant Product | ✅ | Production |
| TraderJoe | Avalanche | Liquidity Bin | ✅ | Production |

### Solana DEXes

| Exchange | Pool Type | Pool Adapter | Status |
|----------|-----------|--------------|--------|
| Orca | Concentrated | ✅ | Production |
| Raydium | Constant Product | ✅ | Production |
| Jupiter | Aggregator | ✅ | Production |

### Other Chain DEXes

| Exchange | Chain | Pool Type | Pool Adapter | Status |
|----------|-------|-----------|--------------|--------|
| Osmosis | Cosmos | Weighted | ✅ | Production |
| dYdX | Cosmos | Order Book | ✅ | Production |
| 1inch | Multi | Aggregator | ✅ | Production |

### Perpetuals DEXes

| Exchange | Chain | Pool Type | Pool Adapter | Status |
|----------|-------|-----------|--------------|--------|
| GMX | Arbitrum/Avalanche | GLP (Weighted) | ✅ | Production |

## Feature Legend

- **Trading**: REST API for placing/canceling orders
- **Market Data**: REST API for ticker, order book, trades
- **WebSocket**: Real-time streaming data
- **Session**: Auto-reconnecting session management (Session_intf.S)
- **Ledger**: P&L tracking (Ledger_intf.ENTRY)
- **Pool Adapter**: Unified pool interface (Pool_intf.S)

## Implementation Priority

### Tier 1 (Complete)
- Gemini, Kraken - Full implementation with all features

### Tier 2 (Production Ready)
- MEXC, Hyperliquid - Trading + market data, tested

### Tier 3 (Partial)
- Binance, Coinbase, Bybit - Core functionality, needs Session/Ledger

### Tier 4 (Data Only)
- Bitrue, dYdX - Market data only, no trading

## Adding a New Exchange

See [CLAUDE.md](/CLAUDE.md) for detailed instructions on implementing a new exchange adapter.

Key steps:
1. Create `lib/exchange/<name>/` directory
2. Implement `fluxum_adapter.ml` satisfying `Exchange_intf.S`
3. Implement `pool_adapter.ml` satisfying `Pool_intf.S`
4. Add tests in `lib/exchange/<name>/test/`
5. Register in CLI (`app/cli.ml`)

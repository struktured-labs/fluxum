# Fluxum Examples

This directory contains working examples demonstrating how to use Fluxum with various cryptocurrency exchanges.

## Table of Contents

- [Order Book Examples](#order-book-examples)
- [Consolidated Order Books](#consolidated-order-books)
- [Market Data Examples](#market-data-examples)
- [Running Examples](#running-examples)

## Order Book Examples

### Binance Order Book (`binance_orderbook.ml`)

Real-time Binance order book streaming via WebSocket.

**Features:**
- WebSocket depth stream (`depth@100ms`)
- Real-time bid/ask updates
- Best 10 levels display
- Automatic reconnection

**Usage:**
```bash
dune exec examples/binance_orderbook.exe -- --symbol BTCUSDT --depth 10
```

**Options:**
- `--symbol`: Trading pair (default: BTCUSDT)
- `--depth`: Number of levels to display (default: 10)

**Example Output:**
```
=== Binance BTC/USDT Order Book (Epoch: 523) ===
Updated: 2025-01-01 10:30:45.123456-05:00

Asks (Sell Orders):
  87401.50 @ 0.15230000
  87401.00 @ 0.08450000
  87400.75 @ 1.23450000

--- Spread: $0.50 (0.0006%) ---

Bids (Buy Orders):
  87400.00 @ 2.45123000
  87399.50 @ 0.67800000
  87399.00 @ 1.89340000

Epoch: 523 | Spread: $0.50
```

---

### Kraken Order Book (`kraken_orderbook.ml`)

Kraken WebSocket order book with depth levels.

**Features:**
- WebSocket book subscription
- Configurable depth (10, 25, 100, 500, 1000)
- Spread calculation
- Mid-price tracking

**Usage:**
```bash
dune exec examples/kraken_orderbook.exe -- --symbol BTC/USD --depth 10
```

**Options:**
- `--symbol`: Trading pair (default: BTC/USD)
- `--depth`: Depth level - 10, 25, 100, 500, or 1000 (default: 10)

**Debug Version:**
```bash
# Shows raw WebSocket messages
dune exec examples/kraken_orderbook_debug.exe
```

---

### Gemini Order Book (`gemini_orderbook.ml`)

Gemini real-time order book using libcurl WebSocket backend.

**Features:**
- Market data WebSocket v2 API
- Events: change, trade, auction
- High-performance libcurl backend
- Automatic snapshot reconstruction

**Usage:**
```bash
dune exec examples/gemini_orderbook.exe -- --symbol btcusd
```

**Options:**
- `--symbol`: Trading pair (default: btcusd)
- Supports: btcusd, ethusd, ethbtc, etc.

**Note:** Gemini symbols are lowercase (e.g., `btcusd`, not `BTC-USD`)

---

### Hyperliquid Order Book (`hyperliquid_orderbook.ml`)

Hyperliquid DEX L2 order book streaming.

**Features:**
- WebSocket L2 book subscription
- Coin-based symbols (e.g., "BTC" not "BTCUSD")
- High-frequency updates
- Full depth reconstruction

**Usage:**
```bash
dune exec examples/hyperliquid_orderbook.exe -- --symbol BTC
```

**Options:**
- `--symbol`: Coin symbol (default: BTC)
- Available: BTC, ETH, SOL, AVAX, etc.

---

### MEXC Order Book (`mexc_orderbook.ml`)

MEXC Global order book streaming.

**Features:**
- Spot market WebSocket
- Incremental updates
- Full snapshot support

**Usage:**
```bash
dune exec examples/mexc_orderbook.exe -- --symbol BTCUSDT
```

---

### Bitrue Order Book (`bitrue_orderbook.ml`)

Bitrue exchange order book with automatic ping/pong.

**Features:**
- WebSocket depth stream
- 15-second ping/pong heartbeat
- Event-driven updates

**Usage:**
```bash
dune exec examples/bitrue_orderbook.exe -- --symbol BTCUSDT
```

**Debug Version:**
```bash
# Shows WebSocket connection details
dune exec examples/bitrue_orderbook_debug.exe
```

---

## Consolidated Order Books

### Basic Consolidated Order Book (`consolidated_orderbook.ml`)

Aggregate order books from multiple exchanges in real-time.

**Features:**
- Multi-exchange aggregation
- Exchange attribution tags (GEM, KRK, BIN, etc.)
- Unified spread calculation
- Volume aggregation at matching price levels

**Usage:**
```bash
# Aggregate Gemini and Kraken
dune exec examples/consolidated_orderbook.exe -- \
  --exchanges gemini,kraken \
  --depth 5

# All available exchanges
dune exec examples/consolidated_orderbook.exe -- \
  --exchanges gemini,kraken,binance,hyperliquid,bitrue,coinbase \
  --depth 10 \
  --max-display 20
```

**Options:**
- `--exchanges`: Comma-separated list (gemini, kraken, binance, hyperliquid, bitrue, coinbase)
- `--depth`: Book depth per exchange (default: 5)
- `--max-display`: Maximum levels to display (default: 10)

**Example Output:**
```
=== BTC/USD Consolidated Order Book (Epoch: 1234) ===
Updated: 2025-01-01 10:30:15.123456-05:00
Sources: [Gemini] [Kraken] [Binance]

Asks (Sell Orders):
  GEM  0.00228000 @ 87400.74
  KRK  0.00663500 @ 87399.90
  BIN  0.03004212 @ 87399.35
  KRK  3.16166915 @ 87399.10

--- Spread: $-0.63 (-0.0030%) ---

Bids (Buy Orders):
  KRK  3.74406283 @ 87399.00
  BIN  1.71926711 @ 87398.90
  GEM  0.85000000 @ 87398.50
  KRK  0.02318456 @ 87398.00
```

---

### Arbitrage Detection (`consolidated_orderbook_arb.ml`)

Detect cross-exchange arbitrage opportunities.

**Features:**
- Real-time arbitrage detection
- Configurable minimum spread threshold
- Buy/sell exchange identification
- Profit calculation
- Alert system

**Usage:**
```bash
dune exec examples/consolidated_orderbook_arb.exe -- \
  --exchanges gemini,kraken,binance \
  --min-spread 0.1
```

**Options:**
- `--exchanges`: Exchanges to monitor
- `--min-spread`: Minimum spread % for alert (default: 0.05)
- `--volume`: Minimum volume for opportunity (default: 0.1 BTC)

**Example Output:**
```
[ARBITRAGE ALERT] 2025-01-01 10:30:15
Buy on:  Gemini @ 87398.50 (0.850 BTC available)
Sell on: Kraken @ 87401.00 (1.234 BTC available)
Spread:  $2.50 (0.0029%)
Potential profit (1 BTC): $2.50
```

---

### Enhanced Consolidated Book (`consolidated_orderbook_enhanced.ml`)

Advanced consolidated order book with statistics.

**Features:**
- Volume aggregation
- Liquidity metrics
- Exchange contribution analysis
- Historical spread tracking
- VWAP calculation

**Usage:**
```bash
dune exec examples/consolidated_orderbook_enhanced.exe -- \
  --exchanges gemini,kraken,binance \
  --stats-window 60
```

**Options:**
- `--exchanges`: Exchanges to aggregate
- `--stats-window`: Statistics window in seconds (default: 60)

---

## Market Data Examples

### Kraken Market Data (`kraken_market_data.ml`)

Demonstrates Kraken's WebSocket subscription API.

**Features:**
- Multiple channel subscriptions (ticker, trade, ohlc, spread)
- Real-time event processing
- Channel management

**Usage:**
```bash
dune exec examples/kraken_market_data.exe
```

This example subscribes to:
- Ticker: BTC/USD, ETH/USD
- Trades: BTC/USD
- OHLC: BTC/USD (1min candles)
- Spread: ETH/USD

---

## Running Examples

### General Pattern

All examples follow this pattern:

```bash
dune exec examples/<example>.exe -- [OPTIONS]
```

### Build All Examples

```bash
dune build examples/
```

### Run with Timeout (for integration tests)

```bash
# Run for 30 seconds then exit
timeout 30 dune exec examples/kraken_orderbook.exe

# Run for 2 minutes
timeout 120 dune exec examples/consolidated_orderbook.exe -- --exchanges gemini,kraken
```

### Debug Output

Enable debug logging:

```bash
# Async debug logging
ASYNC_CONFIG="log_level=Debug" dune exec examples/kraken_orderbook.exe

# Increase thread pool size
ASYNC_CONFIG='((max_num_threads 32))' dune exec examples/consolidated_orderbook.exe
```

---

## Configuration

### Environment Variables

Some examples support configuration via environment variables:

```bash
# Gemini (for authenticated endpoints)
export GEMINI_API_KEY="your-api-key"
export GEMINI_API_SECRET="your-api-secret"

# Kraken (for authenticated endpoints)
export KRAKEN_API_KEY="your-api-key"
export KRAKEN_API_SECRET="your-api-secret"
```

### Network Requirements

All WebSocket examples require:
- Active internet connection
- Firewall allows outbound WSS (port 443)
- Sufficient file descriptors (usually 1024+)

Check file descriptor limits:
```bash
ulimit -n
```

Increase if needed:
```bash
ulimit -n 4096
```

---

## Common Issues

### Issue: WebSocket Connection Timeout

**Solution:**
- Check internet connection
- Verify exchange API status
- Try with longer timeout: `timeout 60 dune exec ...`

### Issue: "Too many open files"

**Solution:**
```bash
ulimit -n 4096
```

Or permanently in `/etc/security/limits.conf`:
```
* soft nofile 4096
* hard nofile 8192
```

### Issue: High CPU Usage

**Solution:**
- Reduce number of exchanges in consolidated examples
- Reduce display update frequency
- Increase `--depth` parameter (less frequent updates)

---

## Integration Testing

The CI system runs these examples as integration tests:

```bash
# CI test command (from .github/workflows/ci.yml)
timeout 30 dune exec examples/gemini_orderbook.exe || true
timeout 30 dune exec examples/kraken_orderbook.exe || true
timeout 30 dune exec examples/hyperliquid_orderbook.exe || true
```

The `|| true` prevents build failure on network issues.

---

## Performance Tips

### For High-Frequency Updates:

1. **Increase Thread Pool:**
   ```bash
   ASYNC_CONFIG='((max_num_threads 32))' dune exec examples/...
   ```

2. **Reduce Display Updates:**
   Only update display every N epochs instead of every update

3. **Use Specific Depth:**
   Lower depth = fewer messages = better performance

### For Low Latency:

1. **Minimize Processing:**
   Remove console output for production use

2. **Batch Updates:**
   Process updates in batches rather than one-by-one

3. **Pin to CPU:**
   ```bash
   taskset -c 0 dune exec examples/...
   ```

---

## Contributing Examples

To add a new example:

1. Create `examples/new_example.ml`
2. Add entry to `examples/dune`:
   ```lisp
   (executable
    (name new_example)
    (libraries core async fluxum <exchange>)
    (preprocess (pps ppx_jane)))
   ```
3. Document in this README
4. Add to CI if applicable

---

## See Also

- [Main README](../README.md) - Library overview
- [API Documentation](../docs/) - Detailed API docs
- [Exchange Guides](../docs/exchanges/) - Exchange-specific guides

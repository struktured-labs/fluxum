# Hyperliquid Testnet Setup Guide

This guide will help you set up and test Hyperliquid trading with the native OCaml implementation.

## Prerequisites

- MetaMask browser extension installed
- Basic understanding of cryptocurrency wallets

## Step-by-Step Setup

### 1. Create a Testnet Wallet in MetaMask

**IMPORTANT: Create a NEW wallet specifically for testnet - NEVER use your mainnet wallet!**

1. Open MetaMask extension
2. Click the account icon (top right)
3. Click "Create Account" or "Add account or hardware wallet"
4. Name it "Hyperliquid Testnet" (or similar)
5. Click "Create"

### 2. Export Your Private Key

1. In MetaMask, select your testnet account
2. Click the three dots (⋮) next to the account name
3. Select "Account details"
4. Click "Show private key"
5. Enter your MetaMask password
6. **Copy the private key** (64-character hex string)
   - Example: `1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef`
   - Do NOT include the `0x` prefix

### 3. Get Your Wallet Address

1. In MetaMask, your address is shown at the top
2. Click to copy it
   - Example: `0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb`
   - INCLUDE the `0x` prefix

### 4. Fund Your Testnet Wallet

1. Visit the Hyperliquid testnet: https://app.hyperliquid-testnet.xyz/
2. Click "Connect Wallet" (top right)
3. Select "MetaMask"
4. Approve the connection in MetaMask popup
5. Once connected, look for the faucet/testnet tokens option
6. Request testnet tokens (usually free, may require Discord verification)

### 5. Set Up Environment Variables

Create a file `~/tmp/hyperliquid_testnet.env` (this is gitignored):

```bash
# Hyperliquid Testnet Credentials
# NEVER commit this file to git!

export HYPERLIQUID_TESTNET_ADDRESS="0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb"  # Your wallet address
export HYPERLIQUID_TESTNET_PRIVATE_KEY="1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"  # NO 0x prefix!
```

**Security Notes:**
- Replace the example values with your actual credentials
- Keep this file secure - it contains your private key
- Never commit private keys to version control
- The `tmp/` folder is already gitignored

### 6. Run the Testnet Example

```bash
# Load environment variables
source ~/tmp/hyperliquid_testnet.env

# Build the example
dune build examples/hyperliquid_testnet.exe

# Run it
dune exec examples/hyperliquid_testnet.exe

# Or run with flags (alternative to environment variables)
dune exec examples/hyperliquid_testnet.exe -- \
  --address "0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb" \
  --private-key "1234567890abcdef..."
```

## What the Example Does

The testnet example script will:

1. ✅ **Connect to Hyperliquid testnet** using your credentials
2. ✅ **Fetch account state** (balance, positions, margin)
3. ✅ **Get market data** (available assets, current prices)
4. ✅ **Place a test order** using native OCaml EIP-712 signing:
   - BUY 0.001 BTC contracts
   - Limit price 20% below market (won't fill - safe test)
   - Demonstrates complete signing flow without execution risk

## Expected Output

```
═══════════════════════════════════════════════════════════════
  Hyperliquid Testnet Trading - Native OCaml Implementation
═══════════════════════════════════════════════════════════════

Wallet Address: 0x742d35Cc6634C0532925a3b844Bc9e7595f0bEb
Private Key:    12345678...

Step 1: Fetching account state...
✓ Account state retrieved

Account Value:  1000.00 USD
Total Position: 0.00 USD
Margin Used:    0.00 USD
Withdrawable:   1000.00 USD

No open positions

Step 2: Fetching market data (BTC)...
✓ Market data retrieved

Available Assets:
  0. BTC (decimals: 5, max leverage: 50)
  1. ETH (decimals: 4, max leverage: 50)
  ...

Step 3: Fetching current BTC price...
✓ Current BTC price: $42000.00

═══════════════════════════════════════════════════════════════
  Test Order (Safe - Will Not Fill)
═══════════════════════════════════════════════════════════════

Asset:       BTC-USD Perpetual
Side:        BUY
Type:        LIMIT
Size:        0.001 contracts (~$33.60)
Limit Price: $33600.00 (20% below market)
TIF:         GTC (Good-Til-Cancel)

This order is intentionally priced far from market
so it will NOT execute - it's just a signing test.

Step 4: Placing test order with EIP-712 signature...

✅ Order placed successfully!

Status: OK
Response:
{"status":"ok","response":{"type":"order","data":{"statuses":[{"resting":{"oid":123456}}]}}}

═══════════════════════════════════════════════════════════════
  🎉 Native OCaml EIP-712 Signing Works!
═══════════════════════════════════════════════════════════════

Successfully demonstrated:
  ✓ Keccak-256 hashing
  ✓ MessagePack serialization
  ✓ EIP-712 structured data signing
  ✓ secp256k1 ECDSA signatures
  ✓ Blockchain transaction submission

All without external JavaScript or Python!
```

## Troubleshooting

### "Failed to fetch account"

**Problem:** Can't connect to testnet or fetch account data

**Solutions:**
- Verify your address is correct (includes `0x` prefix)
- Check testnet is accessible: https://app.hyperliquid-testnet.xyz/
- Ensure you've connected MetaMask to the testnet site at least once

### "Order placement failed: insufficient funds"

**Problem:** Your testnet wallet has no tokens

**Solutions:**
- Visit https://app.hyperliquid-testnet.xyz/
- Connect your wallet
- Request testnet tokens from the faucet
- Wait a few minutes for tokens to arrive

### "Invalid signature" or "Signing failed"

**Problem:** Private key format issue

**Solutions:**
- Remove `0x` prefix from private key if present
- Ensure private key is exactly 64 hex characters
- Verify you copied the complete private key from MetaMask

### "Address required" or "Private key required"

**Problem:** Environment variables not set

**Solutions:**
```bash
# Check if variables are set
echo $HYPERLIQUID_TESTNET_ADDRESS
echo $HYPERLIQUID_TESTNET_PRIVATE_KEY

# Re-source the environment file
source ~/tmp/hyperliquid_testnet.env

# Or use command-line flags instead
dune exec examples/hyperliquid_testnet.exe -- \
  --address "0x..." \
  --private-key "..."
```

## Security Best Practices

1. ✅ **Use a dedicated testnet wallet** - Never use your mainnet private key
2. ✅ **Keep private keys secure** - Don't commit to git, don't share publicly
3. ✅ **Use environment variables** - Store credentials in `~/tmp/hyperliquid_testnet.env`
4. ✅ **Gitignore sensitive files** - `tmp/` folder is already ignored
5. ✅ **Test with small amounts** - The example uses 0.001 BTC (~$42 worth)
6. ✅ **Verify testnet URL** - Make sure you're on testnet, not mainnet!

## Next Steps

Once the testnet example works, you can:

1. **Modify the order parameters** in `examples/hyperliquid_testnet.ml`
2. **Try different order types** (market, post-only, etc.)
3. **Experiment with position management** (reduce-only orders)
4. **Build your own trading strategies** using the adapter

## Resources

- **Hyperliquid Testnet:** https://app.hyperliquid-testnet.xyz/
- **Hyperliquid Docs:** https://hyperliquid.gitbook.io/hyperliquid-docs/
- **API Reference:** https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api
- **MetaMask Guide:** https://support.metamask.io/

## Questions?

If you encounter issues not covered here, check:
- Hyperliquid Discord for testnet status
- GitHub issues for known problems
- Code comments in `examples/hyperliquid_testnet.ml`

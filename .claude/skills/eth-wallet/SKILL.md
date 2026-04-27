---
name: eth-wallet
description: Read-only Ethereum wallet introspection — balance, ERC-20 holdings, recent transfer history for any 0x address.
user_invocable: true
args: <subcommand> [flags]
---

# Ethereum Wallet (read-only)

Wraps `fluxum eth-wallet` for quick on-chain wallet queries from a single Claude Code invocation. No keys, no signing — pure JSON-RPC reads.

## Subcommands

| Subcommand | Purpose | Required flags |
|------------|---------|----------------|
| `balance` | Native ETH balance | `-address 0x...` |
| `erc20` | ERC-20 token balance | `-address 0x...` `-token 0x...` |
| `transfers` | Recent Transfer events for the address | `-address 0x...` `-token 0x...` `[-since 7d \| -last-blocks N]` |

All subcommands accept `-rpc-url URL` (defaults to `https://1rpc.io/eth`; override with `ETH_RPC_URL` env var if you have an Alchemy/Infura key).

## Run

```bash
cd ~/projects/fluxum && eval "$(opam env)" && dune exec ./app/cli.exe -- eth-wallet $ARGS
```

`$ARGS` is everything after `/eth-wallet` — pass through unchanged so the user's flags reach the CLI.

## Examples

```
/eth-wallet balance -address 0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045
/eth-wallet erc20 -address 0x... -token 0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48
/eth-wallet transfers -address 0x... -token 0x... -since 24h
```

## Reference: known mainnet contracts

These are public knowledge but useful as sanity-check defaults:

- USDC: `0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48` (6 decimals)
- USDT: `0xdAC17F958D2ee523a2206206994597C13D831ec7` (6 decimals)
- WETH: `0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2` (18 decimals)
- HOT (Holo): `0x6c6EE5e31d828De241282B9606C8e98Ea48526E2` (18 decimals)

## Output format

- `balance` / `erc20` print one line: `<amount> <symbol>` (or just `<amount>` for tokens with no symbol metadata).
- `transfers` prints one line per event: `blk=N  IN/OUT/SLF  <amount> <symbol>  <counterparty>  tx=0x...` sorted oldest-first.

If the RPC returns an error, the command exits non-zero with `ERROR: <sexp>` on stderr — show that to the user verbatim, since the sexp form preserves the polymorphic-variant tag for diagnosis.

## Limitations

- Mainnet only by default. For L2s, override `-rpc-url` (block-time heuristic in `transfers --since` assumes 12s/block — pass `-last-blocks N` directly on faster chains).
- Free public RPCs rate-limit aggressively. Long `transfers` lookbacks paginate in 10k-block chunks with a 200ms pause; if you see `Max_logs_per_call_exceeded`, narrow the window. As of 2026-04-27: `1rpc.io/eth` (default), `ethereum-rpc.publicnode.com`, `eth.drpc.org` all handle batching cleanly; `eth.llamarpc.com` 429s on Multicall3 follow-ups; `cloudflare-eth.com` errors on batches; `ankr.com/eth` now requires an API key.
- No signing. For transactions, see `fluxum`'s exchange or DEX adapters.

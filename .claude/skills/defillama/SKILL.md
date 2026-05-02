---
name: defillama
description: Read-only DefiLlama TVL/protocol aggregator — list protocols by category, fetch one protocol's TVL + metadata. 7000+ protocols across 200+ chains, no auth.
user_invocable: true
args: <subcommand> [flags]
---

# DefiLlama TVL aggregator

Wraps `fluxum defillama` for read-only access to DefiLlama's free public API. Coverage: 7000+ protocols across 200+ chains. No auth required.

## Subcommands

| Subcommand | Purpose | Required flags |
|------------|---------|----------------|
| `list` | List protocols (filter by category, sorted by TVL) | optional `-category`, `-limit` |
| `protocol` | Show one protocol's TVL + metadata | `-slug <protocol_slug>` |

## Run

```bash
cd ~/projects/fluxum && eval "$(opam env)" && dune exec ./app/cli.exe -- defillama $ARGS
```

## Examples

```
/defillama list -limit 20                       # top 20 protocols by TVL
/defillama list -category DEX -limit 10         # top 10 DEXes
/defillama list -category Lending -limit 10     # top 10 lending protocols
/defillama protocol -slug uniswap               # Uniswap details
/defillama protocol -slug aave                  # Aave details
```

## Common categories

DEX, Lending, CDP, Yield, Yield Aggregator, Bridge, Liquid Staking, Liquid Restaking, Perps, Derivatives, CEX, Synthetics, NFT Lending, Insurance, Cross Chain, Algo-Stables.

## Composability

DefiLlama's `Aggregate_feed.S` shape gives consumers:
- Cross-protocol TVL benchmarking (compare Uniswap vs Curve vs Balancer)
- Multi-chain protocol presence (`venue_metadata.chains` lists all chains a protocol is on)
- Sanity checks against on-chain queries (does my Pool reading match DefiLlama's TVL?)

## Notes

- TVL values are in USD, computed by DefiLlama's pricing oracle (typically CoinGecko-derived). Some protocols have TVL = `null` if DefiLlama hasn't priced their tokens; sort treats these as last.
- The `/protocol/{slug}` endpoint returns richer per-chain TVL breakdown which the adapter sums for the canonical TVL value. For the per-chain breakdown, hit DefiLlama's API directly.
- DefiLlama updates TVL on a roughly hourly cadence — this is not a live tick stream.

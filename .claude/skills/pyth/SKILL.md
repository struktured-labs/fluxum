---
name: pyth
description: Read-only Pyth Network oracle price feeds — list available symbols, fetch latest price by feed_id or symbol. Sub-second updates from on-chain oracle, no auth required.
user_invocable: true
args: <subcommand> [flags]
---

# Pyth Network oracle feed

Wraps `fluxum pyth` for read-only access to Pyth's Hermes price-update API. Sub-second oracle updates across crypto, equities, FX, commodities. No auth.

## Subcommands

| Subcommand | Purpose | Required flags |
|------------|---------|----------------|
| `list` | List available price feeds (filter by query) | optional `-query`, `-limit` |
| `price` | Latest price for one feed | `-symbol <feed_id_or_symbol>` |

`feed_id` is a 64-char hex string identifying a specific price stream. Use `list -query BTC` to discover BTC-related feed IDs and copy the one you want.

## Run

```bash
cd ~/projects/fluxum && eval "$(opam env)" && dune exec ./app/cli.exe -- pyth $ARGS
```

## Examples

```
/pyth list -query BTC -limit 10                                 # discover BTC-related feeds
/pyth price -symbol e62df6c8b4a85fe1a67db44dc12de5db330f7ac66b72dc658afedf0f4a415b43  # BTC/USD
/pyth list -query ETH                                           # ETH feeds
```

## Composability

Pyth prices feed naturally into:
- `Analysis.Returns.simple` — historical price series → returns
- Cross-venue basis tracking — Pyth canonical price vs CEX/DEX
- `Microstructure.Spread.roll` (with `~tick_size:0.0` since Pyth is continuous-priced)

## Notes

- Confidence interval (`conf`) is Pyth's price uncertainty — use as a soft "skip if too wide" gate before consuming the price for anything sensitive.
- `expo` field in venue_metadata records the original price scaling exponent for callers who want the raw scaled-int form.

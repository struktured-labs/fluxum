---
name: manifold
description: Read-only Manifold Markets data feed — list active play-money prediction markets, fetch event details. Tens of thousands of resolved markets available for calibration research.
user_invocable: true
args: <subcommand> [flags]
---

# Manifold Markets data feed

Wraps `fluxum manifold` for read-only access to Manifold's API. All trading on Manifold is play-money mana (no real-money risk; legal anywhere). Particularly valuable for empirical calibration research — Manifold has tens of thousands of resolved markets across many years.

## Subcommands

| Subcommand | Purpose | Required flags |
|------------|---------|----------------|
| `list` | List currently active markets | optional `-limit`, `-include-resolved` |
| `event` | Show one market with outcomes + probabilities | `-id <market_id>` |

## Run

```bash
cd ~/projects/fluxum && eval "$(opam env)" && dune exec ./app/cli.exe -- manifold $ARGS
```

## Examples

```
/manifold list -limit 5
/manifold list -include-resolved -limit 10
/manifold event -id yLz2dSRd52
```

## Composability

Manifold's `Feed_only_adapter.S` shape composes directly with `Prediction_analysis`:
- Cross-venue arb with Polymarket/Kalshi via `Arbitrage.categorical_check`
- Calibration analysis on resolved-market history → `Calibration.calibration_bins` + `expected_calibration_error`
- Probability normalization via `Probability.implied_overround` (Manifold's CPMM doesn't have venue overround the same way, but the API still works)

## Limitations

- Manifold has BINARY, MULTIPLE_CHOICE, FREE_RESPONSE, NUMERIC market types. The adapter handles BINARY (2 outcomes) and MULTIPLE_CHOICE (N outcomes) cleanly. NUMERIC and FREE_RESPONSE return a single placeholder outcome (full handling deferred).
- Manifold uses a CPMM (constant product market maker), not a CLOB — `get_book` returns a single-level "book" derived from the implied probability, not real depth.
- `outcome_id` is encoded as `<event_id>:<outcome_label_or_id>` — copy-paste from `event` output rather than constructing manually.

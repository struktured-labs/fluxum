---
name: polymarket
description: Read-only Polymarket data feed — list active events, fetch event details, query orderbook, recent trades. Trading is US-geo-restricted; data is unrestricted.
user_invocable: true
args: <subcommand> [flags]
---

# Polymarket data feed

Wraps `fluxum polymarket` for quick read-only Polymarket queries from a single Claude Code invocation. No keys required — Polymarket's Gamma + CLOB + Data APIs are unauthenticated for read access.

**Important: trading on Polymarket is geo-blocked from US persons** (CFTC Jan 2022 settlement, ~$1.4M fine for unregistered swap exchange). This adapter is feed-only by design. Querying data is unrestricted.

## Subcommands

| Subcommand | Purpose | Required flags |
|------------|---------|----------------|
| `list` | List currently active events | (none; optional `-limit`, `-category`, `-include-resolved`) |
| `event` | Full details of one event | `-id <event_id>` |
| `book` | Orderbook for one outcome | `-outcome-id <token_id>` |
| `trades` | Recent trades for one outcome | `-outcome-id <token_id>` |

`event_id` comes from the `list` output (Gamma market id, e.g. `540816`). `outcome_id` is the CLOB token_id, shown in the `event` output's `(token_id=...)` column.

## Run

```bash
cd ~/projects/fluxum && eval "$(opam env)" && dune exec ./app/cli.exe -- polymarket $ARGS
```

`$ARGS` is everything after `/polymarket` — pass through unchanged so the user's flags reach the CLI.

## Examples

```
/polymarket list -limit 5
/polymarket list -category "Politics" -limit 20
/polymarket event -id 540816
/polymarket book -outcome-id 8501497159083948713316135768103773293754490207922884688769443031624417212426
/polymarket trades -outcome-id 8501497... -limit 50
```

## Workflow: investigating a market

```
/polymarket list -limit 10              # find an event of interest
/polymarket event -id <event_id>        # see outcomes + token_ids
/polymarket book -outcome-id <token_id> # see orderbook depth on one outcome
/polymarket trades -outcome-id <token_id> -limit 20  # see recent activity
```

## Composing with other fluxum surfaces

Because Polymarket implements `Feed_only_adapter.S`, its data composes with fluxum's `Prediction_analysis` library directly:

- Cross-venue arbitrage with Kalshi: feed Polymarket + Kalshi prices for the same event into `Prediction_analysis.Arbitrage.categorical_check` (with `?min_coverage_fraction` per the bluxit-gemini empirical work — many categorical events don't have full coverage on either venue alone).
- Calibration analysis: pull `Polymarket.get_resolved_events` + actual outcomes, feed into `Prediction_analysis.Calibration.calibration_bins` and `expected_calibration_error`.
- Implied-probability conversion: Polymarket prices ARE probabilities directly (already in [0,1]); no need for `Probability.from_*` conversion. For Kalshi cents → probability use `Probability.from_kalshi_cents`.

## Limitations

- `outcome_id` in this adapter is the Polymarket CLOB token_id (78-digit decimal string). It looks unwieldy but it's what their API expects; copy-paste it from `event` output rather than typing it.
- `get_resolved_events` returns `is_resolved=true` events but doesn't directly populate `resolved_outcome_id` — Polymarket signals resolution via `closed=true` on the market but the winning outcome lookup requires UMA oracle data not in the basic Gamma response. Future enhancement.
- WebSocket subscriptions are not yet supported — REST only for v0.15.0.
- Polymarket may rate-limit aggressive scrapers; the adapter doesn't implement rate-limit backoff. Use `?limit:N` parameters and pace your queries.

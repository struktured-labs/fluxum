# liquidity_rewards

Pure (no I/O, no Async, no order entry) scoring functions for maker
liquidity-reward programs on prediction-market venues:

| Module | Program | Source |
|---|---|---|
| `Gemini` | Gemini Predictions Liquidity Rewards Program | https://developer.gemini.com/prediction-markets/liquidity-rewards-program |
| `Polymarket` | Polymarket Liquidity Rewards | https://docs.polymarket.com/market-makers/liquidity-rewards |
| `Kalshi` | Kalshi Liquidity Incentive Program | https://help.kalshi.com/en/articles/13823851-liquidity-incentive-program ; CFTC rule filings https://www.cftc.gov/filings/orgrules/rules07152610358.pdf (2026-07-15) and the 2026-07-30 modification |

Run tests: `dune build @lib/liquidity_rewards/runtest`

## Limitations — read before using these numbers for decisions

1. **Gemini's exact spread-weight curve is not public.** Gemini documents only a
   "quadratic curve based on quote distance from the midpoint". `Gemini.spread_weight`
   defaults to Polymarket's documented `((v - d) / v)^2` with `v` = max spread (10c).
   On Gemini's own worked example (makers A/B/C, mid $0.505) this curve gives a
   snapshot split of **81 / 1 / 18 %** versus Gemini's published **74 / 5 / 21 %**.
   A `d^-2.4` curve fits the example better (73 / 5 / 23 %) but contradicts the
   documented zero score beyond 10c, and Gemini's "~50x" (1.5c vs 10c) claim matches
   neither exactly. Treat Gemini reward estimates as model-dependent.
2. **Competitor scores are unobservable.** Gemini publishes only a qualifying-maker
   count per event; maker IDs, quote sizes per maker, and scores are hidden. Any
   share estimate must infer competitors from public books (aggregated by price
   level) and is a bound, not a measurement.
3. **Pool splitting across instances is assumed.** Contract-level pools cover all
   instances of a recurring contract type. Callers decide how to slice the pool;
   the fluxit shadow scorer assumes `daily_pool / pool_event_count` per event.
4. **Sampling granularity.** Gemini scores every minute (Kalshi: once per second at
   a random offset; Polymarket: random sample each minute). The fluxit shadow
   recorder samples top-5 books every ~5 minutes, so intra-interval quote changes,
   uptime, and competitor flicker are not observed.
5. **Uptime and eligibility details** (post-only exclusion, one-sided reference =
   best price on your side) are implemented as documented but not validated
   against real payouts; Gemini's authenticated daily/lifetime summary endpoints
   would be required for that.

## Research result (2026-10-16)

The fluxit Gemini rewards shadow (`fluxit/research/gemini_rewards_shadow`, public data
only, hypothetical 10-contract quotes at the touch, 60 bps maker fee, inventory
marked to settlement) scored **-$28.03 over 4 days (Oct 13-16, 2026)** under this
formula model, versus +$136.31 under a naive equal-share assumption and -$41.07
under a pessimistic competitor bound. Rewards were ~$18 against ~$46 of fee and
inventory/settlement loss; larger size (50, 250) or improving one tick inside the
touch made results worse. Kill criteria were not passed. Do not treat this module's
outputs as evidence of a profitable reward-farming strategy.

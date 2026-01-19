(** 1inch EVM DEX Aggregator

    1inch is the leading DEX aggregator across EVM chains, routing trades
    through 400+ liquidity sources for optimal pricing.

    Supported chains:
    - Ethereum (chain_id: 1)
    - BNB Smart Chain (chain_id: 56)
    - Polygon (chain_id: 137)
    - Arbitrum (chain_id: 42161)
    - Optimism (chain_id: 10)
    - Base (chain_id: 8453)

    @see <https://portal.1inch.dev/documentation>

    Features:
    - Best price routing across 400+ DEXs
    - Pathfinder algorithm for optimal splits
    - Fusion mode for gas-free, MEV-protected swaps
    - Cross-chain aggregation

    API Key:
    - Required for all endpoints
    - Get one at https://portal.1inch.dev
    - Set via ONEINCH_API_KEY environment variable
*)

module Cfg = Cfg
module Rest = Rest
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter

(** Common EVM token addresses *)
module Tokens = Rest.Tokens

let command =
  Core.Command.group ~summary:"1inch EVM DEX Aggregator Commands"
    [ Ledger.command
    ]

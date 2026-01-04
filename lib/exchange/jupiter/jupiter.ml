(** Jupiter Solana DEX Aggregator

    Jupiter is the dominant DEX aggregator on Solana, routing trades
    through all major DEXs (Raydium, Orca, Meteora, etc.) for optimal
    pricing.

    @see <https://dev.jup.ag/docs>

    Features:
    - Best price routing across 20+ Solana DEXs
    - Low slippage through split routing
    - Priority fee estimation
    - Versioned transactions support

    API Key:
    - Required for swap/v1 endpoints
    - Get one at https://dev.jup.ag
    - Set via JUPITER_API_KEY environment variable
*)

module Cfg = Cfg
module Rest = Rest
module Fluxum_adapter = Fluxum_adapter

(** Common Solana token mint addresses *)
module Tokens = Rest.Tokens

let command =
  Core.Command.group ~summary:"Jupiter Solana DEX Aggregator Commands"
    []

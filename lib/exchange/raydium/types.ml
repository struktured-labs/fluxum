(** Raydium DEX Types (Solana) *)

open Core

type swap_quote = {
  inputMint : string;
  outputMint : string;
  inAmount : string;
  outAmount : string;
  priceImpact : float;
  minOutAmount : string;
  swapMode : string;
} [@@deriving yojson { strict = false }, sexp]

type pool_info = {
  id : string;
  baseMint : string;
  quoteMint : string;
  lpMint : string;
  baseDecimals : int;
  quoteDecimals : int;
  lpDecimals : int;
  version : int;
  programId : string;
  authority : string;
  openOrders : string;
  targetOrders : string;
  baseVault : string;
  quoteVault : string;
  marketVersion : int;
  marketProgramId : string;
  marketId : string;
  marketAuthority : string;
  marketBaseVault : string;
  marketQuoteVault : string;
  marketBids : string;
  marketAsks : string;
  marketEventQueue : string;
} [@@deriving yojson { strict = false }, sexp]

type pair_info = {
  name : string;
  ammId : string;
  lpMint : string;
  price : float;
  volume24h : float;
  liquidity : float;
} [@@deriving yojson { strict = false }, sexp]

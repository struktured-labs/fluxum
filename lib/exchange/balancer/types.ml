(** Balancer DEX Types *)

open Core

type token = {
  address : string;
  symbol : string;
  name : string;
  decimals : int;
  balance : string;
  weight : string option; [@default None]
} [@@deriving yojson { strict = false }, sexp]

type pool = {
  id : string;
  address : string;
  poolType : string;
  swapFee : string;
  totalLiquidity : string;
  totalShares : string;
  tokens : token list;
  swapsCount : string;
  holdersCount : string;
} [@@deriving yojson { strict = false }, sexp]

type swap = {
  id : string;
  timestamp : string;
  tokenIn : string;
  tokenOut : string;
  tokenAmountIn : string;
  tokenAmountOut : string;
  valueUSD : string;
  userAddress : string;
} [@@deriving yojson { strict = false }, sexp]

type pool_snapshot = {
  id : string;
  timestamp : string;
  liquidity : string;
  swapVolume : string;
  swapFees : string;
} [@@deriving yojson { strict = false }, sexp]

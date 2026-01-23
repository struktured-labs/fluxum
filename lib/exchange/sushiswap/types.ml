(** SushiSwap DEX Types (Multi-chain) *)

open Core

type swap_quote = {
  tokenIn : string;
  tokenOut : string;
  amountIn : string;
  amountOut : string;
  priceImpact : float;
  route : string list;
} [@@deriving yojson { strict = false }, sexp]

type pool_info = {
  address : string;
  token0 : string;
  token1 : string;
  reserve0 : string;
  reserve1 : string;
  totalSupply : string;
} [@@deriving yojson { strict = false }, sexp]

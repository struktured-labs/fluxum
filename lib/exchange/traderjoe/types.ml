(** Trader Joe DEX Types *)

open Core

type token = {
  address : string;
  symbol : string;
  decimals : int;
} [@@deriving yojson { strict = false }, sexp]

type pair = {
  address : string;
  token0 : token;
  token1 : token;
  reserve0 : string;
  reserve1 : string;
  totalSupply : string;
  volumeUSD : float;
  txCount : int;
} [@@deriving yojson { strict = false }, sexp]

type liquidity_bin = {
  binId : int;
  price : float;
  reserveX : string;
  reserveY : string;
  liquidity : string;
} [@@deriving yojson { strict = false }, sexp]

type lb_pair = {
  address : string;
  token0 : token;
  token1 : token;
  activeId : int;
  binStep : int;
  bins : liquidity_bin list;
  volumeUSD : float;
  feesUSD : float;
} [@@deriving yojson { strict = false }, sexp]

type swap = {
  id : string;
  timestamp : string;
  amountIn : string;
  amountOut : string;
  tokenIn : string;
  tokenOut : string;
  sender : string;
} [@@deriving yojson { strict = false }, sexp]

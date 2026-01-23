(** Uniswap V3 DEX Types *)

open Core

type token = {
  id : string;
  symbol : string;
  name : string;
  decimals : int;
} [@@deriving yojson { strict = false }, sexp]

type pool = {
  id : string;
  token0 : token;
  token1 : token;
  feeTier : int;
  liquidity : string;
  sqrtPrice : string;
  tick : int;
  volumeUSD : float;
  txCount : int;
} [@@deriving yojson { strict = false }, sexp]

type swap = {
  id : string;
  timestamp : string;
  amount0 : string;
  amount1 : string;
  amountUSD : float;
  sender : string;
  recipient : string;
} [@@deriving yojson { strict = false }, sexp]

type tick = {
  tickIdx : int;
  liquidityGross : string;
  liquidityNet : string;
  price0 : float;
  price1 : float;
} [@@deriving yojson { strict = false }, sexp]

type position = {
  id : string;
  owner : string;
  liquidity : string;
  tickLower : tick;
  tickUpper : tick;
} [@@deriving yojson { strict = false }, sexp]

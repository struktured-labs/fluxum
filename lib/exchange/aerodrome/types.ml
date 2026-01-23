(** Aerodrome DEX Types *)

open Core

type token = {
  id : string;
  symbol : string;
  decimals : string;
} [@@deriving yojson { strict = false }, sexp]

type pool = {
  id : string;
  token0 : token;
  token1 : token;
  liquidity : string;
  sqrtPrice : string;
  tick : string;
} [@@deriving yojson { strict = false }, sexp]

type swap = {
  id : string;
  timestamp : string;
  amount0 : string;
  amount1 : string;
  recipient : string;
} [@@deriving yojson { strict = false }, sexp]

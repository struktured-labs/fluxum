(** SpookySwap DEX Types *)

open Core

type token = {
  id : string;
  symbol : string;
  name : string;
  decimals : string;
} [@@deriving yojson { strict = false }, sexp]

type pair = {
  id : string;
  token0 : token;
  token1 : token;
  reserve0 : string;
  reserve1 : string;
  totalSupply : string;
  reserveUSD : string;
  volumeUSD : string;
  txCount : string;
} [@@deriving yojson { strict = false }, sexp]

type swap = {
  id : string;
  timestamp : string;
  amount0In : string;
  amount1In : string;
  amount0Out : string;
  amount1Out : string;
  amountUSD : string;
  to_ : string; [@key "to"]
} [@@deriving yojson { strict = false }, sexp]

type liquidity_position = {
  id : string;
  user : string;
  liquidityTokenBalance : string;
} [@@deriving yojson { strict = false }, sexp]

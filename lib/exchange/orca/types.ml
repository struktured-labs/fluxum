(** Orca DEX Types (Solana) *)

open Core

type whirlpool = {
  address : string;
  tokenA : string;
  tokenB : string;
  tickSpacing : int;
  price : float;
  liquidity : string;
  volume24h : float;
  volumeWeek : float;
  feeRate : float;
} [@@deriving yojson { strict = false }, sexp]

type token_price = {
  mint : string;
  price : float;
  decimals : int;
} [@@deriving yojson { strict = false }, sexp]

(** PancakeSwap DEX Types (BSC) *)

open Core

type token_data = {
  name : string;
  symbol : string;
  price : string;
  price_BNB : string;
} [@@deriving yojson { strict = false }, sexp]

type pair_data = {
  pair_address : string;
  base_name : string;
  base_symbol : string;
  quote_name : string;
  quote_symbol : string;
  price : string;
  base_volume : string;
  quote_volume : string;
} [@@deriving yojson { strict = false }, sexp]

(** Raydium DEX Configuration (Solana)

    @see <https://docs.raydium.io/raydium/traders/trade-api>
*)

open Core

type t = {
  swap_url : string;
} [@@deriving sexp]

let production = {
  swap_url = "https://api.raydium.io/v2";
}

let mainnet = production

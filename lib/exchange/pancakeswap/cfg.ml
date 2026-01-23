(** PancakeSwap DEX Configuration (BSC)

    @see <https://docs.pancakeswap.finance>
*)

open Core

type t = {
  api_url : string;
} [@@deriving sexp]

let production = {
  api_url = "https://api.pancakeswap.info/api/v2";
}

let mainnet = production

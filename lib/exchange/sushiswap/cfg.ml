(** SushiSwap DEX Configuration (Multi-chain)

    @see <https://docs.sushi.com/api/examples/swap>
*)

open Core

type t = {
  api_url : string;
  api_key : string option;
} [@@deriving sexp]

let production = {
  api_url = "https://api.sushi.com/swap/v7";
  api_key = None;
}

let mainnet = production

let with_api_key ~api_key cfg = {
  cfg with api_key = Some api_key;
}

let api_key_opt t = t.api_key

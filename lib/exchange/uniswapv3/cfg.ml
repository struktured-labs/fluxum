(** Uniswap V3 DEX Configuration

    @see <https://docs.uniswap.org/api/subgraph/overview>
*)

type t = {
  subgraph_url : string;
  api_key : string option;
} [@@deriving sexp]

let ethereum_mainnet = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v3";
  api_key = None;
}

let production = ethereum_mainnet

let polygon = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/uniswap-v3-polygon";
  api_key = None;
}

let arbitrum = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/arbitrum-minimal";
  api_key = None;
}

let optimism = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/optimism-post-regenesis";
  api_key = None;
}

let with_api_key ~api_key cfg = {
  cfg with api_key = Some api_key;
}

let api_key_opt t = t.api_key

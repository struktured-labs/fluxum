(** Balancer DEX Configuration

    @see <https://docs.balancer.fi/reference/subgraph/>
*)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let ethereum_mainnet = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/balancer-labs/balancer-v2";
}

let production = ethereum_mainnet

let polygon = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/balancer-labs/balancer-polygon-v2";
}

let arbitrum = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/balancer-labs/balancer-arbitrum-v2";
}

let optimism = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/balancer-labs/balancer-optimism-v2";
}

let gnosis = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/balancer-labs/balancer-gnosis-chain-v2";
}

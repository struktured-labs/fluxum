(** GMX DEX Configuration (Arbitrum/Avalanche) *)
type t = { subgraph_url : string; } [@@deriving sexp]
let arbitrum = { subgraph_url = "https://api.thegraph.com/subgraphs/name/gmx-io/gmx-stats"; }
let avalanche = { subgraph_url = "https://api.thegraph.com/subgraphs/name/gmx-io/gmx-avalanche-stats"; }
let production = arbitrum

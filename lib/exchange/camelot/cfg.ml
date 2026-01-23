(** Camelot DEX Configuration (Arbitrum) *)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let arbitrum = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/camelotlabs/camelot-amm";
}

let production = arbitrum

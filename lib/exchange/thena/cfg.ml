(** Thena DEX Configuration (BSC) *)
type t = { subgraph_url : string; } [@@deriving sexp]
let bsc = { subgraph_url = "https://api.thegraph.com/subgraphs/name/thenaursa/thena-fusion"; }
let production = bsc

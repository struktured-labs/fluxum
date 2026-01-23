(** QuickSwap DEX Configuration (Polygon)

    @see <https://docs.quickswap.exchange/>
*)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let polygon = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/sameepsi/quickswap06";
}

let production = polygon

let polygon_zkevm = {
  subgraph_url = "https://api.studio.thegraph.com/query/48211/quickswap-v3-zkevm/version/latest";
}

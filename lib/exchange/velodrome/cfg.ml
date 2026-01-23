(** Velodrome DEX Configuration (Optimism)

    @see <https://docs.velodrome.finance/>
*)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let optimism = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/velodrome-finance/velodrome";
}

let production = optimism

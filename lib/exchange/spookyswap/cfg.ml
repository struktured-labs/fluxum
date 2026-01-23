(** SpookySwap DEX Configuration (Fantom)

    @see <https://docs.spooky.fi/>
*)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let fantom = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/eerieeight/spookyswap";
}

let production = fantom

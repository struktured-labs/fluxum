(** Aerodrome DEX Configuration (Base) *)

type t = {
  subgraph_url : string;
} [@@deriving sexp]

let base = {
  subgraph_url = "https://api.studio.thegraph.com/query/48211/aerodrome-slipstream/version/latest";
}

let production = base

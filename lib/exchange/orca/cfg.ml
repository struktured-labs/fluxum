(** Orca DEX Configuration (Solana)

    @see <https://docs.orca.so/>
*)

open Core

type t = {
  api_url : string;
} [@@deriving sexp]

let production = {
  api_url = "https://api.mainnet.orca.so/v1";
}

let mainnet = production

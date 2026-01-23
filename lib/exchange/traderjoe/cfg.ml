(** Trader Joe DEX Configuration (Avalanche)

    @see <https://docs.traderjoexyz.com/>
*)

type t = {
  api_url : string;
  network : string;
} [@@deriving sexp]

let avalanche_mainnet = {
  api_url = "https://barn.traderjoexyz.com";
  network = "avalanche";
}

let production = avalanche_mainnet

let arbitrum = {
  api_url = "https://barn.traderjoexyz.com";
  network = "arbitrum";
}

let bsc = {
  api_url = "https://barn.traderjoexyz.com";
  network = "bsc";
}

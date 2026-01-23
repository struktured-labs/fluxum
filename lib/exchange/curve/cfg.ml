(** Curve Finance DEX Configuration

    @see <https://curve.readthedocs.io/ref-api.html>
*)

type t = {
  api_url : string;
  network : string;
} [@@deriving sexp]

let ethereum_mainnet = {
  api_url = "https://api.curve.fi/api";
  network = "ethereum";
}

let production = ethereum_mainnet

let polygon = {
  api_url = "https://api.curve.fi/api";
  network = "polygon";
}

let arbitrum = {
  api_url = "https://api.curve.fi/api";
  network = "arbitrum";
}

let optimism = {
  api_url = "https://api.curve.fi/api";
  network = "optimism";
}

let avalanche = {
  api_url = "https://api.curve.fi/api";
  network = "avalanche";
}

let fantom = {
  api_url = "https://api.curve.fi/api";
  network = "fantom";
}

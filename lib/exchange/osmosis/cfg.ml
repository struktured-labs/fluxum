(** Osmosis DEX Configuration (Cosmos) *)
type t = { api_url : string; } [@@deriving sexp]
let mainnet = { api_url = "https://lcd.osmosis.zone"; }
let production = mainnet

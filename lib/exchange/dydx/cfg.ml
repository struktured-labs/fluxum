open Core

module type S = sig
  val rest_url : string
  val ws_url : string
end

module Production = struct
  let rest_url = "https://indexer.dydx.trade/v4"
  let ws_url = "wss://indexer.dydx.trade/v4/ws"
end

module Testnet = struct
  let rest_url = "https://indexer.v4testnet.dydx.exchange"
  let ws_url = "wss://indexer.v4testnet.dydx.exchange/v4/ws"
end

let or_default cfg_opt =
  Option.value cfg_opt ~default:(module Production : S)

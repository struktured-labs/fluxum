(** Hyperliquid Exchange Module

    Hyperliquid is a decentralized perpetual futures exchange with a
    fully on-chain order book. This module provides REST and WebSocket
    API access.

    @see <https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api>
*)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Order_book = Order_book

let command : Command.t =
  Command.group ~summary:"Hyperliquid Exchange Commands"
    []

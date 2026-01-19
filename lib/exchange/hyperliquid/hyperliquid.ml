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
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter
module Market_data = Market_data

let command : Command.t =
  Command.group ~summary:"Hyperliquid Exchange Commands"
    [ Ledger.command
    ]

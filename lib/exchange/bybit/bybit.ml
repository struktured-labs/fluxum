open Core

(** Bybit Exchange Module - Main entry point for Bybit integration *)

module Cfg = Cfg
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter

(** Main Bybit command group

    Note: This is a minimal command group. Once REST API and WebSocket
    clients are implemented, additional commands should be added:
    - Balance queries
    - Order placement/cancellation
    - Order status queries
    - WebSocket streaming commands
*)
let command : Command.t =
  Command.group ~summary:"Bybit Exchange Commands"
    [ Ledger.command
    ]

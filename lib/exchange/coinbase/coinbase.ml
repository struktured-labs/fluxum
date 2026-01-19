(** Coinbase Advanced Trade API Module *)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Order_book = Order_book
module Signature = Signature
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group ~summary:"Coinbase Advanced Trade Commands"
    [ Ledger.command
    ]

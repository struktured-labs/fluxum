(** Bitrue Exchange Module *)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Market_data_curl = Market_data_curl
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group ~summary:"Bitrue Exchange Commands"
    [ Ledger.command
    ]

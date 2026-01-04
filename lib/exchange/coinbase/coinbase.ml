(** Coinbase Advanced Trade API Module *)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Order_book = Order_book
module Signature = Signature
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group ~summary:"Coinbase Advanced Trade Commands"
    []

open Core

module Cfg = Cfg
module V1 = V1
module Order = Order
module Ws = Ws
module Ws_cmd = Ws_cmd
module Market_data = Market_data

let command : Command.t =
  Command.group ~summary:"Kraken Exchange Commands"
    [ Order.command; Ws_cmd.command ]

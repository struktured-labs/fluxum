open Core

module Cfg = Cfg
module Common = Common
module Rest = Rest
module V1 = V1
module Order = Order
module Ws = Ws
module Ws_cmd = Ws_cmd
module Market_data = Market_data

let command : Command.t =
  Command.group ~summary:"Kraken Exchange Commands"
    [ V1.Balances.command
    ; V1.Open_orders.command
    ; V1.Add_order.command
    ; V1.Cancel_order.command
    ; V1.Query_orders.command
    ; V1.Closed_orders.command
    ; Order.command
    ; Ws_cmd.command
    ]

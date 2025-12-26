(** MEXC Exchange Module *)

open Core

module Cfg = Cfg
module Common = Common
module Signature = Signature
module Rest = Rest
module V1 = V1
module Order_book = Order_book
module Ws = Ws

let command : Command.t =
  Command.group
    ~summary:"MEXC Exchange Commands"
    [ (* Public endpoints *)
      V1.Server_time.command
    ; V1.Exchange_info.command
    ; V1.Depth.command
    ; V1.Ticker_24hr.command
    ; V1.Recent_trades.command
      (* Account endpoints *)
    ; V1.Account.command
      (* Trading endpoints *)
    ; V1.New_order.command
    ; V1.Cancel_order.command
    ; V1.Query_order.command
    ; V1.Open_orders.command
    ; V1.All_orders.command
    ; V1.My_trades.command
    ; V1.Cancel_all_orders.command
    ]

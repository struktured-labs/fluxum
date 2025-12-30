open Core

module Cfg = Cfg
module Common = Common
module Signature = Signature
module Rest = Rest
module V3 = V3
module Ws = Ws
module Order_book = Order_book

let test_command =
  Command.basic
    ~summary:"Test command"
    (Command.Param.return (fun () ->
      printf "Test command works!\n%!"))

let command : Command.t =
  Command.group
    ~summary:"Binance Exchange Commands"
    [ (* Public endpoints *)
      V3.Server_time.command
    ; V3.Exchange_info.command
    ; V3.Depth.command
    ; V3.Ticker_24hr.command
    ; V3.Recent_trades.command
      (* Account endpoints *)
    ; V3.Account.command
      (* Trading endpoints *)
    ; V3.New_order.command
    ; V3.Cancel_order.command
    ; V3.Query_order.command
    ; V3.Open_orders.command
    ; V3.All_orders.command
    ; V3.My_trades.command
    ; V3.Cancel_all_orders.command
    ]

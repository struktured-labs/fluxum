open Core

(** Bybit Exchange Module - Main entry point for Bybit integration *)

module Cfg = Cfg
module Rest = Rest
module V5 = V5
module Ws = Ws
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Fluxum_adapter = Fluxum_adapter
module Unified_adapter = Unified_adapter

(** Main Bybit command group *)
let command : Command.t =
  Command.group ~summary:"Bybit Exchange Commands"
    [ Ledger.command
    ; V5.Market_tickers.command
    ; V5.Instruments_info.command
    ; V5.Orderbook.command
    ; V5.Recent_trade.command
    ; V5.Wallet_balance.command
    ; V5.Create_order.command
    ; V5.Cancel_order.command
    ; V5.Order_realtime.command
    ; V5.Execution_list.command
    ]

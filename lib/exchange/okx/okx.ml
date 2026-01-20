open Core

(** OKX Exchange Module - Main entry point for OKX integration *)

module Cfg = Cfg
module Rest = Rest
module V5 = V5
module Ws = Ws
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
(* TODO: Uncomment when implemented *)
(* module Fluxum_adapter = Fluxum_adapter *)
(* module Unified_adapter = Unified_adapter *)

(** Main OKX command group *)
let command : Command.t =
  Command.group ~summary:"OKX Exchange Commands"
    [ ("ledger", Ledger.command)
    ; ("market-tickers", V5.Market_tickers.command)
    ; ("instruments", V5.Instruments.command)
    ; ("orderbook", V5.Orderbook.command)
    ; ("recent-trades", V5.Recent_trades.command)
    ; ("account-balance", V5.Account_balance.command)
    ; ("place-order", V5.Place_order.command)
    ; ("cancel-order", V5.Cancel_order.command)
    ; ("order-details", V5.Order_details.command)
    ; ("orders-history", V5.Orders_history.command)
    ]

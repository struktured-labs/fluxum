(** Unified Uniswap V3 Adapter - Consolidates order book, ledger, and session

    Status:
    - Order Book: Virtual from concentrated liquidity tick data
    - Ledger: P&L tracking for DEX trades
    - Session: Polling-based (subgraph doesn't support WebSocket)
*)

module Order_book = Order_book
module Ledger = Ledger
module Session = Session

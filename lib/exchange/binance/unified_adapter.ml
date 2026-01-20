(** Unified Binance Adapter - Consolidates Binance's order book, ledger, and session implementations

    Binance is now production-ready with:
    - Complete fallible normalize functions (Phase 1 ✅)
    - Order book tracking with safe float conversions
    - P&L ledger with comprehensive accounting (28 fields)
    - Session management with auto-reconnecting streams

    This module provides convenient access to all three unified interfaces.
*)

(** Binance's Order Book implementation *)
module Order_book = Order_book

(** Binance's Ledger implementation *)
module Ledger = Ledger

(** Binance's Session implementation *)
module Session = Session

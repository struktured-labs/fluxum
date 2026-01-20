(** Unified MEXC Adapter - Consolidates MEXC's order book, ledger, and session implementations

    MEXC is now production-ready with:
    - Complete fallible normalize functions (Phase 1 ✅)
    - Order book tracking with safe float conversions
    - P&L ledger with comprehensive accounting (28 fields)
    - Session management with auto-reconnecting streams
    - Binance-compatible API structure

    This module provides convenient access to all three unified interfaces.
*)

(** MEXC's Order Book implementation *)
module Order_book = Order_book

(** MEXC's Ledger implementation *)
module Ledger = Ledger

(** MEXC's Session implementation *)
module Session = Session

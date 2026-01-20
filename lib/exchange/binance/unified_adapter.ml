(** Unified Binance Adapter - Consolidates Binance's order book, ledger, and session implementations

    Binance is now production-ready with:
    - ✅ Order Book: Complete with safe float conversions
    - ✅ Ledger: Complete with P&L tracking and cost basis accounting (28 fields)
    - ✅ Session: Complete with auto-restart streams
    - ✅ Complete fallible normalize functions (Phase 1)

    The order book follows the unified Order_book_intf.S interface.
    The ledger follows the unified Ledger_intf.S interface with 28-field P&L tracking.
    The session follows the unified Session_intf.S interface with auto-restart pipes.

    This module provides convenient access to all three unified interfaces.
*)

(** Binance's Order Book implementation *)
module Order_book = Order_book

(** Binance's Ledger implementation *)
module Ledger = Ledger

(** Binance's Session implementation *)
module Session = Session

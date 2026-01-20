(** Unified OKX Adapter - Consolidates OKX's order book, ledger, and session implementations

    OKX is now production-ready with:
    - ✅ Order Book: Complete with safe float conversions and WebSocket integration
    - ✅ Ledger: Complete with P&L tracking and cost basis accounting (28 fields)
    - ✅ Session: Complete with auto-restart streams
    - ✅ Complete fallible normalize functions (Phase 1)
    - ✅ Unified V5 API across SPOT, FUTURES, SWAP, OPTION

    The order book follows the unified Order_book_intf.S interface.
    The ledger follows the unified Ledger_intf.S interface with 28-field P&L tracking.
    The session follows the unified Session_intf.S interface with auto-restart pipes.

    This module provides convenient access to all three unified interfaces.
*)

(** OKX's Order Book implementation *)
module Order_book = Order_book

(** OKX's Ledger implementation *)
module Ledger = Ledger

(** OKX's Session implementation *)
module Session = Session

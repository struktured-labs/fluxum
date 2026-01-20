(** Unified Jupiter Adapter - Consolidates Jupiter's order book, ledger, and session implementations

    Status:
    - ✅ Order Book: Complete
    - ✅ Ledger: Complete with P&L tracking and cost basis accounting
    - ✅ Session: Complete with auto-restart streams

    The order book follows the unified Order_book_intf.S interface.
    The ledger follows the unified Ledger_intf.S interface with 28-field P&L tracking.
    The session follows the unified Session_intf.S interface with auto-restart pipes.
*)

(** Jupiter's Order Book implementation *)
module Order_book = Order_book

(** Jupiter's Ledger implementation *)
module Ledger = Ledger

(** Jupiter's Session implementation *)
module Session = Session

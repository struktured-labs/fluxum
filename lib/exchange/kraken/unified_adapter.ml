(** Unified Kraken Adapter - Consolidates Kraken's order book, ledger, and session implementations

    Status:
    - ✅ Order Book: Complete with WebSocket v2 integration
    - ⏳ Ledger: TODO - Implement P&L tracking
    - ⏳ Session: TODO - Implement auto-restart streams

    The order book follows the unified Order_book_intf.S interface and provides:
    - Sorted bid/ask maps (bid descending, ask ascending)
    - Market price calculations with volume-weighted averaging
    - WebSocket v2 real-time updates via pipe function
    - TUI pretty_print with ANSI colors
    - Multi-symbol Books manager
*)

(** Kraken's Order Book implementation - COMPLETE *)
module Order_book = Order_book

(** Kraken's Ledger implementation - TODO *)
(* module Ledger = Ledger *)

(** Kraken's Session implementation - TODO *)
(* module Session = Session *)

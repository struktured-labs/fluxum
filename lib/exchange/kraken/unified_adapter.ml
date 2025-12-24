(** Unified Kraken Adapter - Consolidates Kraken's order book, ledger, and session implementations

    Status:
    - ✅ Order Book: Complete with WebSocket v2 integration
    - ✅ Ledger: Complete with P&L tracking and cost basis accounting
    - ✅ Session: Complete with auto-restart streams

    The order book follows the unified Order_book_intf.S interface and provides:
    - Sorted bid/ask maps (bid descending, ask ascending)
    - Market price calculations with volume-weighted averaging
    - WebSocket v2 real-time updates via pipe function
    - TUI pretty_print with ANSI colors
    - Multi-symbol Books manager

    The ledger follows the unified Ledger_intf.S interface and provides:
    - 28-field P&L tracking (pnl, position, spot, cost_basis, etc.)
    - Trade execution updates with fee handling
    - Short position support via external trade unwinding
    - Average price calculations (buy, sell, overall)
    - Cost basis accounting
    - Multi-symbol tracking via Symbol.Map

    The session follows the unified Session_intf.S interface and provides:
    - Auto-restart pipes for reliable WebSocket connections
    - State tracking (Disconnected, Connecting, Connected, Ready, Reconnecting, Failed)
    - Multi-stream Events container (balance, trades, market_data, order_books, ledger, order_events)
    - State change notifications
    - Automatic reconnection on WebSocket EOF/errors
*)

(** Kraken's Order Book implementation - COMPLETE *)
module Order_book = Order_book

(** Kraken's Ledger implementation - COMPLETE *)
module Ledger = Ledger

(** Kraken's Session implementation - COMPLETE *)
module Session = Session

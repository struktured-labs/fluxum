(** Unified Gemini Adapter - Consolidates Gemini's order book, ledger, and session implementations

    Note: Gemini uses strongly-typed symbol enums (Common.Symbol.t) rather than strings,
    so this adapter exposes Gemini's implementations as-is rather than forcing exact
    interface compliance. The unified interfaces (Order_book_intf, Ledger_intf, Session_intf)
    serve as design templates for new exchange implementations like Kraken.
*)

(** Gemini's Order Book implementation *)
module Order_book = Order_book

(** Gemini's Ledger implementation *)
module Ledger = Ledger

(** Gemini's Session implementation *)
module Session = Session

(** Bot Framework - Top-level Module

    Provides infrastructure for building trading bots with:
    - Event sourcing for deterministic state reconstruction
    - Persistent event storage with bin_prot serialization
    - Real-time TUI dashboard
    - Python interoperability via parquet/CSV export
    - Strategy interface for signal-based trading

    {1 Architecture}

    The bot framework follows an event-sourcing pattern where all state
    changes are captured as events, enabling:
    - Deterministic replay and state reconstruction
    - Audit trail for all trading activity
    - Debugging via event replay
    - Analysis via export to Python

    {1 Quick Start}

    {[
      open Bot

      (* Create engine with config *)
      let%bind engine = Engine.create Engine.Config.{
        default with
        bot_id = "my-bot";
        symbols = ["BTCUSD"];
        venues = [Event.Venue.Gemini];
      } in

      (* Optional: Set a strategy *)
      let strategy = Engine.Strategy_wrapper.wrap
        (module Strategy_intf.Noop) ()
      in
      Engine.set_strategy engine strategy;

      (* Start the bot *)
      let%bind _ = Engine.start engine in

      (* Feed market data *)
      let%bind () = Engine.on_book_update engine
        ~symbol:"BTCUSD"
        ~venue:Gemini
        ~bids:[{ price = 67000.; qty = 1.0 }]
        ~asks:[{ price = 67001.; qty = 1.0 }]
        ~is_snapshot:true
      in

      (* Run dashboard *)
      let dashboard = Dashboard.create Dashboard.Config.default in
      Dashboard.run_with_engine dashboard engine
    ]}

    {1 Modules}
*)

(** Event types with bin_prot serialization *)
module Event = Event

(** Persistent event storage *)
module Event_store = Event_store

(** Bot state with event sourcing *)
module State = State

(** Strategy interface for signal-based trading *)
module Strategy_intf = Strategy_intf

(** Main bot engine *)
module Engine = Engine

(** TUI dashboard for monitoring *)
module Dashboard = Dashboard

(** Export to CSV/JSON for Python analysis *)
module Parquet_export = Parquet_export

(** Unified ledger (re-export) *)
module Ledger = Unified_ledger.Ledger
module Entry = Unified_ledger.Entry

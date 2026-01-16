(** Session Interface - Auto-Reconnecting WebSocket Sessions

    Extracted from Gemini's production implementation. Provides automatic
    reconnection with exponential backoff and state management.

    {b Key Features:}
    - Automatic reconnection on connection loss
    - State change notifications
    - Multi-stream event aggregation
    - Clean resource management

    {b Design Pattern:}
    This interface follows the "session" pattern where a single connection
    manages multiple event streams (trades, order books, balances, etc.).
    Inspired by Gemini's robust WebSocket implementation.

    @see <https://docs.gemini.com/websocket-api/> for reference implementation
*)

open Core
open Async

(** {1 Session State Machine} *)

module State : sig
  (** Session lifecycle states

      State transitions:
      - Disconnected → Connecting → Connected → Ready
      - Any state → Reconnecting → Connecting (on error)
      - Any state → Failed (on fatal error)
  *)
  type t =
    | Disconnected                      (** Initial state, not yet started *)
    | Connecting                        (** Attempting to establish connection *)
    | Connected                         (** TCP connection established *)
    | Ready                             (** Authenticated and subscribed *)
    | Reconnecting of { attempt : int } (** Reconnecting after disconnect (with attempt number) *)
    | Failed of Error.t                 (** Fatal error - manual intervention required *)
  [@@deriving sexp, compare, equal]
end

(** {1 Auto-Restart Utilities} *)

module Auto_restart : sig
  (** Automatic pipe reconnection utilities

      This is Gemini's "killer feature" - pipes that never close.
      On EOF/disconnect, automatically creates a new connection and resumes streaming.
  *)

  val pipe :
    name:string ->
    create_pipe:(unit -> 'a Pipe.Reader.t Deferred.t) ->
    unit ->
    'a Pipe.Reader.t
  (** Create a pipe that automatically reconnects on EOF.

      @param name Identifier for logging (e.g., "trades:BTCUSD")
      @param create_pipe Function to create a new pipe connection
      @return A pipe that never closes - auto-reconnects indefinitely

      Reconnection strategy:
      - Detects EOF on source pipe
      - Waits 1 second (prevents tight loop)
      - Calls create_pipe again
      - Transfers data to same output pipe

      Example:
      {[
        let trades_stream =
          Auto_restart.pipe
            ~name:"gemini_trades"
            ~create_pipe:(fun () -> Gemini.Ws.connect ~symbols:["BTCUSD"] ())
            ()
        in
        (* Stream never closes - reconnects on disconnect *)
        Pipe.iter trades_stream ~f:process_trade
      ]}

      Warning: This creates a background loop. Use [close] to stop it.
  *)
end

(** {1 Multi-Stream Events} *)

module type EVENTS = sig
  (** Event streams aggregated from a session

      Provides typed access to different data streams from a single connection.
      Inspired by Gemini's multi-channel WebSocket API.
  *)

  (** {2 Exchange-Specific Types} *)

  type balance
  (** Exchange's native balance type *)

  type trade
  (** Exchange's native trade type *)

  type market_event
  (** Exchange's native market data event *)

  type book
  (** Exchange's native order book type *)

  type ledger_entry
  (** Exchange's native ledger entry *)

  type order_event
  (** Exchange's native order event *)

  type order_id
  (** Exchange's native order ID *)

  (** {2 Event Container} *)

  type t
  (** Opaque event container - aggregates all streams *)

  (** {2 Stream Accessors} *)

  val symbols : t -> Types.Symbol.t list
  (** List of symbols this session is subscribed to *)

  val balance : t -> balance Pipe.Reader.t
  (** Real-time balance updates (single stream for all currencies) *)

  val trades : t -> trade Pipe.Reader.t Types.Symbol.Map.t
  (** User's filled trades, per symbol

      Returns: Map from symbol to trade stream
      Use: P&L tracking, fill notifications
  *)

  val market_data : t -> market_event Pipe.Reader.t Types.Symbol.Map.t
  (** Market data events (tickers, etc.), per symbol *)

  val order_books : t -> book Pipe.Reader.t Types.Symbol.Map.t
  (** Order book updates, per symbol

      Typically incremental updates. See Order_book_intf for full book management.
  *)

  val ledger : t -> ledger_entry Pipe.Reader.t Types.Symbol.Map.t
  (** Ledger entries (position changes, fees, etc.), per symbol *)

  val order_events : t -> order_event Pipe.Reader.t
  (** Order lifecycle events (new, fill, cancel) - single stream for all symbols *)
end

(** {1 Session Management} *)

module type S = sig
  (** Session interface for exchange connections

      Manages WebSocket connection lifecycle with automatic reconnection.
      Provides state change notifications and clean shutdown.
  *)

  type t
  (** Opaque session handle *)

  module Events : EVENTS
  (** Event streams for this exchange *)

  val events : t -> Events.t
  (** Get event streams from this session

      Access all data streams (trades, books, orders, etc.) from single session.

      Example:
      {[
        let session = Gemini.Session.connect ~symbols:["BTCUSD"; "ETHUSD"] () in
        let events = events session in
        let trade_stream = Events.trades events |> Symbol.Map.find_exn "BTCUSD" in
        Pipe.iter trade_stream ~f:(fun trade ->
          print_s [%message "Trade" (trade : Events.trade)];
          return ()
        )
      ]}
  *)

  val state : t -> State.t
  (** Get current connection state

      Returns: Current state (Disconnected, Connecting, Connected, Ready, etc.)
      Use: UI status indicators, health checks
  *)

  val state_changes : t -> State.t Pipe.Reader.t
  (** Subscribe to state change notifications

      Emits new state whenever connection state changes.

      Example:
      {[
        Pipe.iter (state_changes session) ~f:(fun new_state ->
          match new_state with
          | State.Ready -> Log.info "Session ready"
          | State.Reconnecting { attempt } ->
            Log.warn "Reconnecting (attempt %d)" attempt
          | State.Failed err ->
            Log.error "Session failed: %s" (Error.to_string_hum err)
          | _ -> return ()
        )
      ]}
  *)

  val close : t -> unit Deferred.t
  (** Close session and release resources

      - Closes WebSocket connection
      - Stops auto-reconnect loops
      - Closes all event pipes
      - Waits for cleanup to complete

      Warning: After close, all pipes will close (EOF).
      Do not use session after calling close.
  *)
end

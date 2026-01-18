(** Bitrue Session - Implements unified Session_intf with auto-reconnecting streams

    Placeholder session management for Bitrue exchange.
    Full implementation will support market data, trades, and order book tracking.

    TODO: Implement full WebSocket session when REST API is complete.
*)

open Async

(** Re-export State from interface *)
module State = Fluxum.Session_intf.State

(** Placeholder session - basic structure for future implementation *)
module Session = struct
  type t =
    { mutable state : State.t
    ; symbols : string list
    }

  let create ?(symbols = []) () =
    { state = Disconnected
    ; symbols
    }

  let state t = t.state

  let set_state t state =
    t.state <- state

  (** Placeholder: Connect to exchange *)
  let connect t =
    t.state <- Connecting;
    t.state <- Connected;
    Deferred.unit

  (** Placeholder: Disconnect from exchange *)
  let disconnect t =
    t.state <- Disconnected;
    Deferred.unit

  (** Placeholder: Subscribe to symbol *)
  let subscribe _t ~symbol:_ =
    Deferred.unit

  (** Placeholder: Unsubscribe from symbol *)
  let unsubscribe _t ~symbol:_ =
    Deferred.unit

  (** Placeholder: Get market data pipe *)
  let market_data_pipe _t =
    let r, _w = Pipe.create () in
    Deferred.return r

  (** Placeholder: Get trades pipe *)
  let trades_pipe _t =
    let r, _w = Pipe.create () in
    Deferred.return r

  (** Placeholder: Get order book updates pipe *)
  let book_updates_pipe _t =
    let r, _w = Pipe.create () in
    Deferred.return r
end

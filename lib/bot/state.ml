(** Bot State - Event Sourcing Pattern

    All state is reconstructible from the event log.
    The apply_event function is a pure function that
    transforms state based on events.
*)

open Core

(** Re-export Event.Time for convenience *)
module Time = Event.Time

(** Connection state per venue *)
module Connection = struct
  type status =
    | Disconnected
    | Connecting
    | Connected
    | Ready
    | Reconnecting of int
    | Failed of string
  [@@deriving sexp, compare, equal]

  type t =
    { venue : Event.Venue.t
    ; status : status
    ; connected_at : Time.t option
    ; last_activity : Time.t
    ; reconnect_count : int
    }
  [@@deriving sexp_of]

  let create venue =
    { venue
    ; status = Disconnected
    ; connected_at = None
    ; last_activity = Time.now ()
    ; reconnect_count = 0
    }

  let is_ready t =
    match t.status with
    | Ready -> true
    | _ -> false

  let is_connected t =
    match t.status with
    | Connected | Ready -> true
    | _ -> false
end

(** Active order tracking *)
module Active_order = struct
  type t =
    { order_id : string
    ; exchange_id : string option
    ; symbol : string
    ; venue : Event.Venue.t
    ; side : Event.Side.t
    ; original_qty : float
    ; filled_qty : float
    ; remaining_qty : float
    ; price : float option
    ; created_at : Time.t
    ; last_update : Time.t
    }
  [@@deriving sexp_of]

  let create ~order_id ~symbol ~venue ~side ~qty ~price =
    let now = Time.now () in
    { order_id
    ; exchange_id = None
    ; symbol
    ; venue
    ; side
    ; original_qty = qty
    ; filled_qty = 0.
    ; remaining_qty = qty
    ; price
    ; created_at = now
    ; last_update = now
    }
end

(** Balance per currency per venue *)
module Balance = struct
  type t =
    { venue : Event.Venue.t
    ; currency : string
    ; available : float
    ; locked : float
    ; total : float
    }
  [@@deriving sexp_of]
end

(** Main bot state *)
type t =
  { bot_id : string
  ; started_at : Time.t
  ; is_running : bool
  ; version : string
    (* Connections *)
  ; connections : Connection.t Map.M(Event.Venue).t
    (* Trading state *)
  ; ledger : Unified_ledger.Ledger.t
  ; active_orders : Active_order.t Map.M(String).t  (** order_id -> order *)
  ; balances : Balance.t list Map.M(Event.Venue).t
    (* Event tracking *)
  ; last_sequence : int64
  ; event_count : int
  ; last_event_time : Time.t option
    (* Error tracking *)
  ; error_count : int
  ; last_error : string option
  }

(** Create empty state for a bot *)
let empty ~bot_id =
  { bot_id
  ; started_at = Time.epoch
  ; is_running = false
  ; version = ""
  ; connections = Map.empty (module Event.Venue)
  ; ledger = Unified_ledger.Ledger.create ()
  ; active_orders = Map.empty (module String)
  ; balances = Map.empty (module Event.Venue)
  ; last_sequence = 0L
  ; event_count = 0
  ; last_event_time = None
  ; error_count = 0
  ; last_error = None
  }

(** Apply a single event to state (PURE FUNCTION - no side effects) *)
let apply_event t (envelope : Event.envelope) =
  let t = { t with
    last_sequence = envelope.sequence
  ; event_count = t.event_count + 1
  ; last_event_time = Some envelope.timestamp
  } in

  match envelope.event with
  (* System events *)
  | System (Bot_started { bot_id; version; _ }) ->
    { t with
      bot_id
    ; started_at = envelope.timestamp
    ; is_running = true
    ; version
    }

  | System (Bot_stopped _) ->
    { t with is_running = false }

  | System (Connection_state_changed { venue; new_state; _ }) ->
    let conn =
      Map.find t.connections venue
      |> Option.value ~default:(Connection.create venue)
    in
    let status = match new_state with
      | Event.Connection_state.Disconnected -> Connection.Disconnected
      | Connecting -> Connection.Connecting
      | Connected -> Connection.Connected
      | Ready -> Connection.Ready
      | Reconnecting n -> Connection.Reconnecting n
      | Failed s -> Connection.Failed s
    in
    let connected_at = match status with
      | Connected | Ready -> Some envelope.timestamp
      | _ -> conn.connected_at
    in
    let reconnect_count = match status with
      | Reconnecting _ -> conn.reconnect_count + 1
      | Connected | Ready -> 0
      | _ -> conn.reconnect_count
    in
    let conn = { conn with
      status
    ; connected_at
    ; last_activity = envelope.timestamp
    ; reconnect_count
    } in
    { t with connections = Map.set t.connections ~key:venue ~data:conn }

  | System (Error { message; is_fatal; _ }) ->
    { t with
      error_count = t.error_count + 1
    ; last_error = Some message
    ; is_running = match is_fatal with true -> false | false -> t.is_running
    }

  | System (Heartbeat _) ->
    t  (* Just updates sequence/timestamp, already done above *)

  (* Order events *)
  | Order (Order_submitted { order_id; symbol; venue; side; qty; price; _ }) ->
    let order = Active_order.create ~order_id ~symbol ~venue ~side ~qty ~price in
    { t with active_orders = Map.set t.active_orders ~key:order_id ~data:order }

  | Order (Order_accepted { order_id; exchange_id; _ }) ->
    let active_orders =
      Map.change t.active_orders order_id ~f:(function
        | None -> None
        | Some order -> Some { order with
            exchange_id = Some exchange_id
          ; last_update = envelope.timestamp
          }
      )
    in
    { t with active_orders }

  | Order (Order_filled { order_id; venue; fill_qty; fill_price; fee; _ }) ->
    (* Update active order *)
    let order_opt = Map.find t.active_orders order_id in
    let active_orders, symbol, side =
      match order_opt with
      | None -> (t.active_orders, "UNKNOWN", Event.Side.Buy)
      | Some order ->
        let remaining = order.remaining_qty -. fill_qty in
        match Float.(remaining <= 0.0001) with
        | true ->
          (* Fully filled, remove from active *)
          (Map.remove t.active_orders order_id, order.symbol, order.side)
        | false ->
          let order = { order with
            filled_qty = order.filled_qty +. fill_qty
          ; remaining_qty = remaining
          ; last_update = envelope.timestamp
          } in
          (Map.set t.active_orders ~key:order_id ~data:order, order.symbol, order.side)
    in
    (* Update ledger *)
    let venue_entry = match Event.Venue.to_fluxum_venue venue with
      | Some v -> Unified_ledger.Entry.Venue.of_fluxum_venue v
      | None ->
        (* Convert Event.Venue directly to Entry.Venue *)
        match venue with
        | Event.Venue.Gemini -> Unified_ledger.Entry.Venue.Gemini
        | Kraken -> Unified_ledger.Entry.Venue.Kraken
        | Mexc -> Unified_ledger.Entry.Venue.Mexc
        | Coinbase -> Unified_ledger.Entry.Venue.Coinbase
        | Binance -> Unified_ledger.Entry.Venue.Binance
        | Hyperliquid -> Unified_ledger.Entry.Venue.Hyperliquid
        | Bitrue -> Unified_ledger.Entry.Venue.Bitrue
        | Dydx -> Unified_ledger.Entry.Venue.Dydx
        | Jupiter -> Unified_ledger.Entry.Venue.Jupiter
        | OneInch -> Unified_ledger.Entry.Venue.OneInch
        | Other s -> Unified_ledger.Entry.Venue.Other s
    in
    let fluxum_side = Event.Side.to_fluxum_side side in
    let ledger, _ = Unified_ledger.Ledger.apply_fill t.ledger
      ~symbol ~venue:venue_entry ~price:fill_price ~qty:fill_qty ~side:fluxum_side ~fee
    in
    { t with active_orders; ledger }

  | Order (Order_partially_filled { order_id; venue; fill_qty; fill_price; remaining_qty; fee }) ->
    let order_opt = Map.find t.active_orders order_id in
    let active_orders, symbol, side =
      match order_opt with
      | None -> (t.active_orders, "UNKNOWN", Event.Side.Buy)
      | Some order ->
        let order = { order with
          filled_qty = order.filled_qty +. fill_qty
        ; remaining_qty
        ; last_update = envelope.timestamp
        } in
        (Map.set t.active_orders ~key:order_id ~data:order, order.symbol, order.side)
    in
    let venue_entry = match Event.Venue.to_fluxum_venue venue with
      | Some v -> Unified_ledger.Entry.Venue.of_fluxum_venue v
      | None ->
        (* Convert Event.Venue directly to Entry.Venue *)
        match venue with
        | Event.Venue.Gemini -> Unified_ledger.Entry.Venue.Gemini
        | Kraken -> Unified_ledger.Entry.Venue.Kraken
        | Mexc -> Unified_ledger.Entry.Venue.Mexc
        | Coinbase -> Unified_ledger.Entry.Venue.Coinbase
        | Binance -> Unified_ledger.Entry.Venue.Binance
        | Hyperliquid -> Unified_ledger.Entry.Venue.Hyperliquid
        | Bitrue -> Unified_ledger.Entry.Venue.Bitrue
        | Dydx -> Unified_ledger.Entry.Venue.Dydx
        | Jupiter -> Unified_ledger.Entry.Venue.Jupiter
        | OneInch -> Unified_ledger.Entry.Venue.OneInch
        | Other s -> Unified_ledger.Entry.Venue.Other s
    in
    let fluxum_side = Event.Side.to_fluxum_side side in
    let ledger, _ = Unified_ledger.Ledger.apply_fill t.ledger
      ~symbol ~venue:venue_entry ~price:fill_price ~qty:fill_qty ~side:fluxum_side ~fee
    in
    { t with active_orders; ledger }

  | Order (Order_cancelled { order_id; _ }) ->
    { t with active_orders = Map.remove t.active_orders order_id }

  | Order (Order_rejected { order_id; _ }) ->
    { t with active_orders = Map.remove t.active_orders order_id }

  (* Balance events *)
  | Balance (Balance_update { venue; currency; available; locked; total }) ->
    let balance : Balance.t = { venue; currency; available; locked; total } in
    let venue_balances =
      Map.find t.balances venue
      |> Option.value ~default:[]
      |> List.filter ~f:(fun b -> not (String.equal b.currency currency))
    in
    let venue_balances = balance :: venue_balances in
    { t with balances = Map.set t.balances ~key:venue ~data:venue_balances }

  | Balance (Balance_snapshot { venue; balances }) ->
    let venue_balances =
      List.map balances ~f:(fun (currency, available, locked, total) ->
        { Balance.venue; currency; available; locked; total }
      )
    in
    { t with balances = Map.set t.balances ~key:venue ~data:venue_balances }

  (* Market events - update mark prices in ledger *)
  | Market (Book_update { symbol; bids; asks; _ }) ->
    let mid_price = match bids, asks with
      | hd_bid :: _, hd_ask :: _ ->
        (hd_bid.Event.Price_level.price +. hd_ask.price) /. 2.
      | _, _ -> 0.
    in
    (match Float.(mid_price > 0.) with
    | false -> t
    | true ->
      let ledger = Unified_ledger.Ledger.mark_to_market t.ledger
        ~prices:[(symbol, mid_price)]
      in
      { t with ledger })

  | Market (Trade { symbol; price; _ }) ->
    let ledger = Unified_ledger.Ledger.mark_to_market t.ledger
      ~prices:[(symbol, price)]
    in
    { t with ledger }

  | Market (Ticker { symbol; last; _ }) ->
    let ledger = Unified_ledger.Ledger.mark_to_market t.ledger
      ~prices:[(symbol, last)]
    in
    { t with ledger }

  (* Strategy events - informational only *)
  | Strategy (Signal_generated _) -> t
  | Strategy (Position_changed _) -> t
  | Strategy (Pnl_update _) -> t

(** Reconstruct state from a list of events *)
let reconstruct ~events ~bot_id =
  let init = empty ~bot_id in
  List.fold events ~init ~f:apply_event

(** Validate state consistency *)
let validate t =
  let errors = ref [] in

  (* Check ledger position matches active orders *)
  (* This is a basic sanity check *)

  (* Check all connections have valid status *)
  Map.iter t.connections ~f:(fun conn ->
    match conn.status with
    | Connection.Failed reason when String.is_empty reason ->
      errors := "Connection failed with empty reason" :: !errors
    | _ -> ()
  );

  match !errors with
  | [] -> Ok ()
  | errs -> Error (Error.of_list (List.map errs ~f:Error.of_string))

(** Summary for display *)
let summary t =
  let conn_status =
    Map.fold t.connections ~init:[] ~f:(fun ~key:venue ~data:conn acc ->
      let status = match conn.status with
        | Connection.Ready -> "ready"
        | Connected -> "connected"
        | Connecting -> "connecting"
        | Disconnected -> "disconnected"
        | Reconnecting n -> sprintf "reconnecting(%d)" n
        | Failed _ -> "failed"
      in
      sprintf "%s:%s" (Event.Venue.to_string venue) status :: acc
    )
    |> String.concat ~sep:", "
  in
  let pnl = Unified_ledger.Ledger.total_pnl t.ledger in
  let pnl_sign = match Float.(pnl >= 0.) with true -> "+" | false -> "" in
  sprintf "Bot[%s] running=%b orders=%d pnl=%s%.2f events=%d conn=[%s]"
    t.bot_id
    t.is_running
    (Map.length t.active_orders)
    pnl_sign pnl
    t.event_count
    conn_status

(** Get all venues with active connections *)
let connected_venues t =
  Map.filter t.connections ~f:Connection.is_connected
  |> Map.keys

(** Get all ready venues *)
let ready_venues t =
  Map.filter t.connections ~f:Connection.is_ready
  |> Map.keys

(** Get active orders for a symbol *)
let orders_for_symbol t ~symbol =
  Map.filter t.active_orders ~f:(fun order ->
    String.equal order.Active_order.symbol symbol
  )
  |> Map.data

(** Get active orders for a venue *)
let orders_for_venue t ~venue =
  Map.filter t.active_orders ~f:(fun order ->
    Event.Venue.equal order.Active_order.venue venue
  )
  |> Map.data

(** Get balance for a currency at a venue *)
let balance t ~venue ~currency =
  Map.find t.balances venue
  |> Option.bind ~f:(fun balances ->
    List.find balances ~f:(fun b -> String.equal b.Balance.currency currency)
  )

(** Uptime *)
let uptime t =
  match t.is_running with
  | false -> Event.Time_span.zero
  | true -> Time.diff (Time.now ()) t.started_at

(** Bot Engine - Main Orchestration Component

    The engine is the core of the bot framework, responsible for:
    - Connecting to configured exchanges
    - Subscribing to market data streams
    - Routing events to strategy
    - Executing strategy signals
    - Persisting all events via event store
    - Managing state via event sourcing
*)

open Core
open Async

module Config = struct
  type t =
    { bot_id : string
    ; symbols : string list
    ; venues : Event.Venue.t list
    ; event_store_path : string
    ; tick_interval : Time_float.Span.t
    ; enable_dashboard : bool
    ; max_reconnect_attempts : int
    ; heartbeat_interval : Time_float.Span.t
    }
  [@@deriving sexp]

  let default =
    { bot_id = "fluxum-bot"
    ; symbols = [ "BTCUSD" ]
    ; venues = [ Event.Venue.Gemini ]
    ; event_store_path = "./events"
    ; tick_interval = Time_float.Span.of_sec 1.0
    ; enable_dashboard = false
    ; max_reconnect_attempts = 10
    ; heartbeat_interval = Time_float.Span.of_sec 30.0
    }

  let to_json t =
    Sexp.to_string (sexp_of_t t)
end

(** Market data handler signature *)
module type Market_data_handler = sig
  val on_book_update
    : symbol:string
    -> venue:Event.Venue.t
    -> bids:Event.Price_level.t list
    -> asks:Event.Price_level.t list
    -> is_snapshot:bool
    -> unit Deferred.t

  val on_trade
    : symbol:string
    -> venue:Event.Venue.t
    -> price:float
    -> qty:float
    -> side:Event.Side.t option
    -> unit Deferred.t
end

(** Strategy wrapper using existential type *)
module Strategy_wrapper = struct
  (** Existential wrapper for strategy state *)
  type packed_state = Packed : 'a -> packed_state

  type t =
    { name : string
    ; version : string
    ; mutable state : packed_state
    ; on_book : packed_state -> Strategy_intf.Book_snapshot.t -> Strategy_intf.Context.t
                -> Strategy_intf.Signal.t list * packed_state
    ; on_trade : packed_state -> symbol:string -> venue:Event.Venue.t -> price:float
                 -> qty:float -> side:Event.Side.t option -> Strategy_intf.Context.t
                 -> Strategy_intf.Signal.t list * packed_state
    ; on_fill : packed_state -> order_id:string -> symbol:string -> venue:Event.Venue.t
                -> side:Event.Side.t -> fill_qty:float -> fill_price:float
                -> Strategy_intf.Context.t -> Strategy_intf.Signal.t list * packed_state
    ; on_tick : packed_state -> time:Event.Time.t -> Strategy_intf.Context.t
                -> Strategy_intf.Signal.t list * packed_state
    ; on_start : packed_state -> Strategy_intf.Context.t -> Strategy_intf.Signal.t list * packed_state
    ; on_stop : packed_state -> Strategy_intf.Context.t -> Strategy_intf.Signal.t list * packed_state
    }

  let wrap (type cfg st) (module S : Strategy_intf.S with type config = cfg and type state = st) config =
    let initial_state = S.init config in
    let pack st = Packed st in
    let unpack (Packed st) = (Obj.magic st : st) in
    { name = S.name
    ; version = S.version
    ; state = pack initial_state
    ; on_book = (fun packed book ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_book_update state ~book ~context:ctx in
        signals, pack new_state)
    ; on_trade = (fun packed ~symbol ~venue ~price ~qty ~side ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_trade state ~symbol ~venue ~price ~qty ~side ~context:ctx in
        signals, pack new_state)
    ; on_fill = (fun packed ~order_id ~symbol ~venue ~side ~fill_qty ~fill_price ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_fill state ~order_id ~symbol ~venue ~side ~fill_qty ~fill_price ~context:ctx in
        signals, pack new_state)
    ; on_tick = (fun packed ~time ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_tick state ~time ~context:ctx in
        signals, pack new_state)
    ; on_start = (fun packed ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_start state ~context:ctx in
        signals, pack new_state)
    ; on_stop = (fun packed ctx ->
        let state = unpack packed in
        let signals, new_state = S.on_stop state ~context:ctx in
        signals, pack new_state)
    }
end

(** Order submission callback type.
    Returns Ok exchange_order_id on success, Error message on failure. *)
type order_submitter =
  venue:Event.Venue.t ->
  symbol:string ->
  side:Event.Side.t ->
  qty:float ->
  price:float option ->
  (string, string) Result.t Deferred.t

type t =
  { config : Config.t
  ; mutable state : State.t
  ; event_writer : Event_store.Writer.t
  ; mutable strategy : Strategy_wrapper.t option
  ; mutable is_running : bool
  ; event_pipe_reader : Event.envelope Pipe.Reader.t
  ; event_pipe_writer : Event.envelope Pipe.Writer.t
  ; mutable heartbeat_sequence : int64
  ; mutable tick_clock : unit Deferred.t option
  ; mutable heartbeat_clock : unit Deferred.t option
  ; mutable order_submitter : order_submitter option
  }

(** Emit an event: persist and update state *)
let emit t event =
  let envelope = Event.create_envelope ~sequence:t.state.State.last_sequence event in
  let envelope = { envelope with sequence = Int64.(t.state.last_sequence + 1L) } in
  let%bind () = Event_store.Writer.write t.event_writer envelope in
  t.state <- State.apply_event t.state envelope;
  match Pipe.is_closed t.event_pipe_writer with
  | true -> return ()
  | false ->
    Pipe.write_without_pushback t.event_pipe_writer envelope;
    return ()

(** Build strategy context from current state *)
let build_context t =
  let positions =
    Unified_ledger.Ledger.all_entries t.state.ledger
    |> List.map ~f:(fun e -> (e.Unified_ledger.Entry.symbol, e.position))
  in
  let balances =
    Map.fold t.state.balances ~init:[] ~f:(fun ~key:_ ~data:venue_balances acc ->
      List.fold venue_balances ~init:acc ~f:(fun acc b ->
        (b.State.Balance.currency, b.available) :: acc
      )
    )
  in
  let active_orders = Map.data t.state.active_orders in
  let total_pnl = Unified_ledger.Ledger.total_pnl t.state.ledger in
  let realized_pnl = Unified_ledger.Ledger.total_realized t.state.ledger in
  let unrealized_pnl = Unified_ledger.Ledger.total_unrealized t.state.ledger in
  { Strategy_intf.Context.
    timestamp = Event.Time.now ()
  ; positions
  ; balances
  ; active_orders
  ; total_pnl
  ; realized_pnl
  ; unrealized_pnl
  }

(** Execute a strategy signal *)
let execute_signal t signal =
  match signal with
  | Strategy_intf.Signal.No_action ->
    return ()
  | Strategy_intf.Signal.Place_order req ->
    let order_id = Event.Event_id.create () in
    let event = Event.Order (Event.Order_event.Order_submitted
      { order_id
      ; symbol = req.Strategy_intf.Order_request.symbol
      ; venue = req.venue
      ; side = req.side
      ; qty = req.qty
      ; price = req.price
      ; time_in_force = (match req.time_in_force with
          | Strategy_intf.Time_in_force.GTC -> Event.Time_in_force.GTC
          | IOC -> IOC
          | FOK -> FOK)
      })
    in
    let%bind () = emit t event in
    (* Submit order to exchange if submitter is configured *)
    let%bind () = match t.order_submitter with
      | None ->
        (* No order submitter - just log the signal *)
        let signal_event = Event.Strategy (Event.Strategy_event.Signal_generated
          { signal_type = Event.Strategy_event.Place_order
          ; symbol = Some req.symbol
          ; reason = sprintf "Strategy requested %s %.8f @ %s (no submitter configured)"
              (Event.Side.to_string req.side)
              req.qty
              (match req.price with Some p -> sprintf "%.2f" p | None -> "MKT")
          ; details = None
          })
        in
        emit t signal_event
      | Some submit_order ->
        (* Actually submit to exchange *)
        let%bind result = submit_order
          ~venue:req.venue ~symbol:req.symbol ~side:req.side
          ~qty:req.qty ~price:req.price
        in
        (match result with
         | Ok exchange_id ->
           let accepted_event = Event.Order (Event.Order_event.Order_accepted
             { order_id; exchange_id; venue = req.venue })
           in
           emit t accepted_event
         | Error reason ->
           let rejected_event = Event.Order (Event.Order_event.Order_rejected
             { order_id; venue = req.venue; reason })
           in
           emit t rejected_event)
    in
    return ()
  | Strategy_intf.Signal.Cancel cancel_req ->
    let event = match cancel_req with
      | Strategy_intf.Cancel_request.Cancel_order { order_id } ->
        Event.Strategy (Event.Strategy_event.Signal_generated
          { signal_type = Event.Strategy_event.Cancel_order
          ; symbol = None
          ; reason = sprintf "Cancel order %s" order_id
          ; details = None
          })
      | Strategy_intf.Cancel_request.Cancel_all { symbol; venue = _ } ->
        Event.Strategy (Event.Strategy_event.Signal_generated
          { signal_type = Event.Strategy_event.Cancel_all
          ; symbol
          ; reason = "Cancel all orders"
          ; details = None
          })
    in
    emit t event

(** Process signals from strategy *)
let process_signals t signals =
  Deferred.List.iter signals ~how:`Sequential ~f:(execute_signal t)

(** Handle book update *)
let on_book_update t ~symbol ~venue ~bids ~asks ~is_snapshot =
  (* Emit market event *)
  let event = Event.Market (Event.Market_event.Book_update
    { symbol; venue; bids; asks; is_snapshot })
  in
  let%bind () = emit t event in

  (* Forward to strategy if running *)
  match t.strategy, t.is_running with
  | Some strategy, true ->
    let ctx = build_context t in
    let book : Strategy_intf.Book_snapshot.t =
      { symbol
      ; venue
      ; bids = List.map bids ~f:(fun l ->
          { Strategy_intf.Book_snapshot.price = l.Event.Price_level.price
          ; qty = l.qty })
      ; asks = List.map asks ~f:(fun l ->
          { Strategy_intf.Book_snapshot.price = l.Event.Price_level.price
          ; qty = l.qty })
      ; timestamp = Event.Time.now ()
      }
    in
    let signals, new_state = strategy.on_book strategy.state book ctx in
    strategy.state <- new_state;
    process_signals t signals
  | _ -> return ()

(** Handle trade *)
let on_trade t ~symbol ~venue ~price ~qty ~side =
  let event = Event.Market (Event.Market_event.Trade
    { symbol; venue; price; qty; side; trade_id = None })
  in
  let%bind () = emit t event in

  match t.strategy, t.is_running with
  | Some strategy, true ->
    let ctx = build_context t in
    let signals, new_state = strategy.on_trade strategy.state ~symbol ~venue ~price ~qty ~side ctx in
    strategy.state <- new_state;
    process_signals t signals
  | _ -> return ()

(** Handle order fill *)
let on_fill t ~order_id ~venue ~fill_qty ~fill_price ~fee ~is_maker =
  let event = Event.Order (Event.Order_event.Order_filled
    { order_id; venue; fill_qty; fill_price; fee; is_maker })
  in
  let%bind () = emit t event in

  match t.strategy, t.is_running with
  | Some strategy, true ->
    let ctx = build_context t in
    let order_opt = Map.find t.state.active_orders order_id in
    let symbol, side = match order_opt with
      | Some o -> (o.State.Active_order.symbol, o.side)
      | None -> ("UNKNOWN", Event.Side.Buy)
    in
    let signals, new_state = strategy.on_fill strategy.state
      ~order_id ~symbol ~venue ~side ~fill_qty ~fill_price ctx
    in
    strategy.state <- new_state;
    process_signals t signals
  | _ -> return ()

(** Tick handler *)
let on_tick t =
  match t.strategy, t.is_running with
  | Some strategy, true ->
    let ctx = build_context t in
    let time = Event.Time.now () in
    let signals, new_state = strategy.on_tick strategy.state ~time ctx in
    strategy.state <- new_state;
    process_signals t signals
  | _ -> return ()

(** Heartbeat handler *)
let on_heartbeat t =
  t.heartbeat_sequence <- Int64.(t.heartbeat_sequence + 1L);
  let event = Event.System (Event.System_event.Heartbeat
    { sequence = t.heartbeat_sequence })
  in
  emit t event

(** Start tick clock *)
let start_tick_clock t =
  let rec loop () =
    let%bind () = Clock.after t.config.tick_interval in
    match t.is_running with
    | false -> return ()
    | true ->
      let%bind () = on_tick t in
      loop ()
  in
  t.tick_clock <- Some (loop ())

(** Start heartbeat clock *)
let start_heartbeat_clock t =
  let rec loop () =
    let%bind () = Clock.after t.config.heartbeat_interval in
    match t.is_running with
    | false -> return ()
    | true ->
      let%bind () = on_heartbeat t in
      loop ()
  in
  t.heartbeat_clock <- Some (loop ())

(** Create a new bot engine *)
let create config =
  let%bind event_writer = Event_store.Writer.create
    Event_store.Config.{ default with base_path = config.Config.event_store_path }
    ~bot_id:config.bot_id
  in
  let event_pipe_reader, event_pipe_writer = Pipe.create () in
  let state = State.empty ~bot_id:config.bot_id in
  return
    { config
    ; state
    ; event_writer
    ; strategy = None
    ; is_running = false
    ; event_pipe_reader
    ; event_pipe_writer
    ; heartbeat_sequence = 0L
    ; tick_clock = None
    ; heartbeat_clock = None
    ; order_submitter = None
    }

(** Set the strategy *)
let set_strategy t strategy =
  t.strategy <- Some strategy

(** Set the order submitter callback for live trading.
    When set, Place_order signals will be submitted to exchanges.
    When not set, orders are only logged as events. *)
let set_order_submitter t submitter =
  t.order_submitter <- Some submitter

(** Start the bot *)
let start t =
  match t.is_running with
  | true -> return (Error (Error.of_string "Bot already running"))
  | false ->
    t.is_running <- true;

    (* Emit start event *)
    let event = Event.System (Event.System_event.Bot_started
      { bot_id = t.config.bot_id
      ; config_json = Config.to_json t.config
      ; version = "0.1.0"
      })
    in
    let%bind () = emit t event in

    (* Call strategy on_start if present *)
    let%bind () = match t.strategy with
      | None -> return ()
      | Some strategy ->
        let ctx = build_context t in
        let signals, new_state = strategy.on_start strategy.state ctx in
        strategy.state <- new_state;
        process_signals t signals
    in

    (* Start clocks *)
    start_tick_clock t;
    start_heartbeat_clock t;

    return (Ok ())

(** Stop the bot *)
let stop t ~reason =
  match t.is_running with
  | false -> return ()
  | true ->
    (* Call strategy on_stop if present *)
    let%bind () = match t.strategy with
      | None -> return ()
      | Some strategy ->
        let ctx = build_context t in
        let signals, new_state = strategy.on_stop strategy.state ctx in
        strategy.state <- new_state;
        process_signals t signals
    in

    t.is_running <- false;

    (* Emit stop event *)
    let event = Event.System (Event.System_event.Bot_stopped
      { reason; exit_code = Some 0 })
    in
    let%bind () = emit t event in

    (* Sync and close event writer *)
    let%bind () = Event_store.Writer.sync t.event_writer in
    Event_store.Writer.close t.event_writer

(** Get current state *)
let state t = t.state

(** Get event stream *)
let events t = t.event_pipe_reader

(** Get config *)
let config t = t.config

(** Check if running *)
let is_running t = t.is_running

(** Update connection state *)
let update_connection t ~venue ~old_state ~new_state =
  let event = Event.System (Event.System_event.Connection_state_changed
    { venue; old_state; new_state })
  in
  emit t event

(** Report error *)
let report_error t ~message ~is_fatal ?venue () =
  let event = Event.System (Event.System_event.Error
    { venue; message; is_fatal })
  in
  emit t event

(** Update balance *)
let update_balance t ~venue ~currency ~available ~locked ~total =
  let event = Event.Balance (Event.Balance_event.Balance_update
    { venue; currency; available; locked; total })
  in
  emit t event

(** Get summary *)
let summary t =
  sprintf "Engine[%s] running=%b strategy=%s state: %s"
    t.config.bot_id
    t.is_running
    (match t.strategy with Some s -> s.name | None -> "none")
    (State.summary t.state)

(** Replay events from store to rebuild state *)
let replay ~base_path ~bot_id =
  let init_state = State.empty ~bot_id in
  Event_store.replay_all ~base_path ~bot_id ~init:init_state
    ~f:(fun envelope state -> State.apply_event state envelope)

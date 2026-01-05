(** Strategy Interface - Signal-based trading strategy abstraction

    Strategies are pure functions that:
    - Receive market data and state
    - Return trading signals
    - Have no direct exchange access
*)

open Core

(** Time in force for orders *)
module Time_in_force = struct
  type t =
    | GTC  (** Good til canceled *)
    | IOC  (** Immediate or cancel *)
    | FOK  (** Fill or kill *)
  [@@deriving sexp, compare, equal]

  let to_string = function
    | GTC -> "GTC"
    | IOC -> "IOC"
    | FOK -> "FOK"
end

(** Order request from strategy *)
module Order_request = struct
  type t =
    { symbol : string
    ; venue : Event.Venue.t
    ; side : Event.Side.t
    ; qty : float
    ; price : float option  (** None = market order *)
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp]

  let market ~symbol ~venue ~side ~qty =
    { symbol; venue; side; qty; price = None; time_in_force = IOC }

  let limit ~symbol ~venue ~side ~qty ~price =
    { symbol; venue; side; qty; price = Some price; time_in_force = GTC }

  let post_only ~symbol ~venue ~side ~qty ~price =
    { symbol; venue; side; qty; price = Some price; time_in_force = GTC }
end

(** Cancel request from strategy *)
module Cancel_request = struct
  type t =
    | Cancel_order of { order_id : string }
    | Cancel_all of { symbol : string option; venue : Event.Venue.t option }
  [@@deriving sexp]
end

(** Trading signal from strategy *)
module Signal = struct
  type t =
    | Place_order of Order_request.t
    | Cancel of Cancel_request.t
    | No_action
  [@@deriving sexp]

  let buy_market ~symbol ~venue ~qty =
    Place_order (Order_request.market ~symbol ~venue ~side:Buy ~qty)

  let sell_market ~symbol ~venue ~qty =
    Place_order (Order_request.market ~symbol ~venue ~side:Sell ~qty)

  let buy_limit ~symbol ~venue ~qty ~price =
    Place_order (Order_request.limit ~symbol ~venue ~side:Buy ~qty ~price)

  let sell_limit ~symbol ~venue ~qty ~price =
    Place_order (Order_request.limit ~symbol ~venue ~side:Sell ~qty ~price)

  let cancel ~order_id =
    Cancel (Cancel_request.Cancel_order { order_id })

  let cancel_all ?symbol ?venue () =
    Cancel (Cancel_request.Cancel_all { symbol; venue })
end

(** Order book snapshot for strategy *)
module Book_snapshot = struct
  type level = { price : float; qty : float }
  [@@deriving sexp]

  type t =
    { symbol : string
    ; venue : Event.Venue.t
    ; bids : level list  (** Sorted descending by price *)
    ; asks : level list  (** Sorted ascending by price *)
    ; timestamp : Event.Time.t
    }
  [@@deriving sexp]

  let best_bid t = List.hd t.bids
  let best_ask t = List.hd t.asks

  let mid_price t =
    match best_bid t, best_ask t with
    | Some bid, Some ask -> Some ((bid.price +. ask.price) /. 2.)
    | _ -> None

  let spread t =
    match best_bid t, best_ask t with
    | Some bid, Some ask -> Some (ask.price -. bid.price)
    | _ -> None

  let spread_bps t =
    match mid_price t, spread t with
    | Some mid, Some s when Float.(mid > 0.) ->
      Some (s /. mid *. 10000.)
    | _ -> None
end

(** Context passed to strategy on each update *)
module Context = struct
  type t =
    { timestamp : Event.Time.t
    ; positions : (string * float) list  (** symbol -> position *)
    ; balances : (string * float) list   (** currency -> available *)
    ; active_orders : State.Active_order.t list
    ; total_pnl : float
    ; realized_pnl : float
    ; unrealized_pnl : float
    }
  [@@deriving sexp_of]

  let position t ~symbol =
    List.Assoc.find t.positions ~equal:String.equal symbol
    |> Option.value ~default:0.

  let balance t ~currency =
    List.Assoc.find t.balances ~equal:String.equal currency
    |> Option.value ~default:0.

  let has_position t ~symbol =
    let pos = position t ~symbol in
    Float.(abs pos > 0.00000001)

  let orders_for_symbol t ~symbol =
    List.filter t.active_orders ~f:(fun o ->
      String.equal o.State.Active_order.symbol symbol
    )
end

(** Strategy module signature *)
module type S = sig
  (** Strategy configuration type *)
  type config

  (** Strategy internal state type *)
  type state

  (** Strategy name *)
  val name : string

  (** Strategy version *)
  val version : string

  (** Default configuration *)
  val default_config : config

  (** Initialize strategy state from config *)
  val init : config -> state

  (** Called on order book update
      Returns list of signals and new state *)
  val on_book_update
    : state
    -> book:Book_snapshot.t
    -> context:Context.t
    -> Signal.t list * state

  (** Called on public trade
      Returns list of signals and new state *)
  val on_trade
    : state
    -> symbol:string
    -> venue:Event.Venue.t
    -> price:float
    -> qty:float
    -> side:Event.Side.t option
    -> context:Context.t
    -> Signal.t list * state

  (** Called on order fill (our order)
      Returns list of signals and new state *)
  val on_fill
    : state
    -> order_id:string
    -> symbol:string
    -> venue:Event.Venue.t
    -> side:Event.Side.t
    -> fill_qty:float
    -> fill_price:float
    -> context:Context.t
    -> Signal.t list * state

  (** Called on timer tick
      Returns list of signals and new state *)
  val on_tick
    : state
    -> time:Event.Time.t
    -> context:Context.t
    -> Signal.t list * state

  (** Called on strategy start *)
  val on_start
    : state
    -> context:Context.t
    -> Signal.t list * state

  (** Called on strategy stop *)
  val on_stop
    : state
    -> context:Context.t
    -> Signal.t list * state
end

(** Helper to create a simple stateless strategy *)
module Make_stateless(F : sig
  val name : string
  val version : string
  val on_book : Book_snapshot.t -> context:Context.t -> Signal.t list
end) : S with type config = unit and type state = unit = struct
  type config = unit
  type state = unit

  let name = F.name
  let version = F.version
  let default_config = ()

  let init () = ()

  let on_book_update () ~book ~context =
    (F.on_book book ~context, ())

  let on_trade () ~symbol:_ ~venue:_ ~price:_ ~qty:_ ~side:_ ~context:_ =
    ([], ())

  let on_fill () ~order_id:_ ~symbol:_ ~venue:_ ~side:_ ~fill_qty:_ ~fill_price:_ ~context:_ =
    ([], ())

  let on_tick () ~time:_ ~context:_ =
    ([], ())

  let on_start () ~context:_ =
    ([], ())

  let on_stop () ~context:_ =
    ([], ())
end

(** Example: No-op strategy that does nothing *)
module Noop : S with type config = unit and type state = unit = struct
  type config = unit
  type state = unit

  let name = "noop"
  let version = "1.0.0"
  let default_config = ()

  let init () = ()
  let on_book_update () ~book:_ ~context:_ = ([], ())
  let on_trade () ~symbol:_ ~venue:_ ~price:_ ~qty:_ ~side:_ ~context:_ = ([], ())
  let on_fill () ~order_id:_ ~symbol:_ ~venue:_ ~side:_ ~fill_qty:_ ~fill_price:_ ~context:_ = ([], ())
  let on_tick () ~time:_ ~context:_ = ([], ())
  let on_start () ~context:_ = ([], ())
  let on_stop () ~context:_ = ([], ())
end

(** Example: Simple market maker strategy (for testing) *)
module Simple_mm = struct
  type config =
    { symbol : string
    ; venue : Event.Venue.t
    ; spread_bps : float      (** Spread in basis points *)
    ; order_size : float      (** Order size per side *)
    ; max_position : float    (** Maximum position size *)
    }

  type state =
    { last_mid : float option
    ; bid_order : string option
    ; ask_order : string option
    }

  let name = "simple-mm"
  let version = "0.1.0"

  let default_config =
    { symbol = "BTCUSD"
    ; venue = Gemini
    ; spread_bps = 10.
    ; order_size = 0.001
    ; max_position = 0.01
    }

  let init _ =
    { last_mid = None
    ; bid_order = None
    ; ask_order = None
    }

  let on_book_update state ~book ~context =
    let cfg = default_config in  (* Would be passed in properly *)
    match String.equal book.Book_snapshot.symbol cfg.symbol with
    | false -> ([], state)
    | true ->
      match Book_snapshot.mid_price book with
      | None -> ([], state)
      | Some mid ->
        let position = Context.position context ~symbol:cfg.symbol in
        let half_spread = mid *. cfg.spread_bps /. 10000. /. 2. in
        let bid_price = mid -. half_spread in
        let ask_price = mid +. half_spread in

        let signals = ref [] in

        (* Cancel existing orders if price moved *)
        (match state.last_mid with
        | Some last_mid when Float.(abs (mid -. last_mid) > half_spread /. 2.) ->
          Option.iter state.bid_order ~f:(fun id ->
            signals := Signal.cancel ~order_id:id :: !signals
          );
          Option.iter state.ask_order ~f:(fun id ->
            signals := Signal.cancel ~order_id:id :: !signals
          )
        | _ -> ()
        );

        (* Place new orders if within position limits *)
        (match Float.(position < cfg.max_position) with
        | true ->
          signals := Signal.buy_limit
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size ~price:bid_price
            :: !signals
        | false -> ()
        );
        (match Float.(position > Float.neg cfg.max_position) with
        | true ->
          signals := Signal.sell_limit
            ~symbol:cfg.symbol ~venue:cfg.venue ~qty:cfg.order_size ~price:ask_price
            :: !signals
        | false -> ()
        );

        (!signals, { state with last_mid = Some mid })

  let on_trade state ~symbol:_ ~venue:_ ~price:_ ~qty:_ ~side:_ ~context:_ =
    ([], state)

  let on_fill state ~order_id ~symbol:_ ~venue:_ ~side:_ ~fill_qty:_ ~fill_price:_ ~context:_ =
    let bid_order = match state.bid_order with
      | Some id when String.equal id order_id -> None
      | x -> x
    in
    let ask_order = match state.ask_order with
      | Some id when String.equal id order_id -> None
      | x -> x
    in
    ([], { state with bid_order; ask_order })

  let on_tick state ~time:_ ~context:_ =
    ([], state)

  let on_start state ~context:_ =
    ([], state)

  let on_stop state ~context:_ =
    (* Cancel all orders on stop *)
    let signals = [Signal.cancel_all ~symbol:default_config.symbol ()] in
    (signals, state)
end

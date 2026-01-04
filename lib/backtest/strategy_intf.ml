(** Strategy interface for backtesting *)

open Core

(** Trading signal *)
module Signal = struct
  type t =
    | Buy of { qty : float; limit : float option }
    | Sell of { qty : float; limit : float option }
    | Close_long of { qty : float option }   (** Close long position (qty = None means all) *)
    | Close_short of { qty : float option }  (** Close short position (qty = None means all) *)
    | Hold
  [@@deriving sexp]

  let buy ?limit qty = Buy { qty; limit }
  let sell ?limit qty = Sell { qty; limit }
  let close_long ?qty () = Close_long { qty }
  let close_short ?qty () = Close_short { qty }
  let hold = Hold

  let to_string = function
    | Buy { qty; limit = None } -> sprintf "BUY %.4f (market)" qty
    | Buy { qty; limit = Some p } -> sprintf "BUY %.4f @ %.2f" qty p
    | Sell { qty; limit = None } -> sprintf "SELL %.4f (market)" qty
    | Sell { qty; limit = Some p } -> sprintf "SELL %.4f @ %.2f" qty p
    | Close_long { qty = None } -> "CLOSE LONG (all)"
    | Close_long { qty = Some q } -> sprintf "CLOSE LONG %.4f" q
    | Close_short { qty = None } -> "CLOSE SHORT (all)"
    | Close_short { qty = Some q } -> sprintf "CLOSE SHORT %.4f" q
    | Hold -> "HOLD"
end

(** Position info passed to strategy *)
module Position = struct
  type t =
    { qty           : float  (** Positive = long, negative = short, 0 = flat *)
    ; avg_price     : float  (** Average entry price *)
    ; unrealized_pnl : float (** Current unrealized P&L *)
    }
  [@@deriving sexp, fields]

  let empty = { qty = 0.; avg_price = 0.; unrealized_pnl = 0. }

  let is_long t = Float.(t.qty > 0.)
  let is_short t = Float.(t.qty < 0.)
  let is_flat t = Float.(t.qty = 0.)
end

(** Market context passed to strategy *)
module Context = struct
  type t =
    { candle        : Candle.t
    ; position      : Position.t
    ; balance       : float       (** Available cash *)
    ; equity        : float       (** Balance + unrealized P&L *)
    ; candle_index  : int         (** Index in the candle sequence *)
    ; history       : Candle.t list  (** Previous N candles (most recent first) *)
    }
  [@@deriving sexp, fields]
end

(** Strategy module signature *)
module type S = sig
  (** Strategy name for identification *)
  val name : string

  (** Internal state type *)
  type state

  (** Initial state *)
  val initial_state : state

  (** Called on each candle; returns signal and updated state *)
  val on_candle : state -> Context.t -> Signal.t * state
end

(** Helper module for creating strategies with configurable parameters *)
module type CONFIGURABLE = sig
  include S

  (** Configuration type *)
  type config

  (** Default configuration *)
  val default_config : config

  (** Create initial state from config *)
  val init_with_config : config -> state
end

(** Simple strategy that doesn't need state *)
module type STATELESS = sig
  val name : string
  val on_candle : Context.t -> Signal.t
end

(** Convert a stateless strategy to a full strategy *)
module Of_stateless (S : STATELESS) : S = struct
  let name = S.name
  type state = unit
  let initial_state = ()
  let on_candle () ctx = (S.on_candle ctx, ())
end

(** Registry for strategies *)
module Registry = struct
  type entry =
    { name : string
    ; description : string
    ; create : unit -> (module S)
    }

  let strategies : entry list ref = ref []

  let register ~name ~description ~create =
    strategies := { name; description; create } :: !strategies

  let find name =
    List.find !strategies ~f:(fun e -> String.equal e.name name)

  let list () =
    List.map !strategies ~f:(fun e -> (e.name, e.description))
end

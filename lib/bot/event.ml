(** Bot Event Types with bin_prot Serialization

    All events that can occur in the bot lifecycle, designed for:
    - Efficient binary serialization via bin_prot
    - Event sourcing (deterministic state reconstruction)
    - Streaming persistence
*)

open Core

(** Time type with proper sexp/bin_io support *)
module Time = struct
  type t = Time_ns.t [@@deriving compare, equal, bin_io]

  let sexp_of_t = Time_ns_unix.sexp_of_t
  let t_of_sexp = Time_ns_unix.t_of_sexp

  let now = Time_ns.now
  let epoch = Time_ns.epoch
  let diff = Time_ns.diff
  let to_string = Time_ns_unix.to_string
end

module Time_span = Time_ns.Span

(** Unique event identifier *)
module Event_id = struct
  type t = string [@@deriving sexp, compare, equal, bin_io]

  let counter = ref 0

  let create () =
    let id = !counter in
    incr counter;
    let timestamp = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string in
    sprintf "%s-%d" timestamp id

  let to_string t = t
end

(** Venue identifier - matches Types.Venue but with bin_io *)
module Venue = struct
  module T = struct
    type t =
      | Gemini
      | Kraken
      | Mexc
      | Coinbase
      | Binance
      | Hyperliquid
      | Bitrue
      | Dydx
      | Jupiter
      | OneInch
      | Other of string
    [@@deriving sexp, compare, equal, bin_io]
  end
  include T
  include Comparator.Make(T)

  let of_fluxum_venue : Fluxum.Types.Venue.t -> t = function
    | Gemini -> Gemini
    | Kraken -> Kraken
    | Mexc -> Mexc
    | Coinbase -> Coinbase
    | Binance -> Binance
    | Hyperliquid -> Hyperliquid
    | Bitrue -> Bitrue
    | Dydx -> Dydx
    | Jupiter -> Jupiter
    | OneInch -> OneInch
    | Gmx -> Other "Gmx"
    | Aave -> Other "Aave"
    | Compound -> Other "Compound"

  let to_fluxum_venue : t -> Fluxum.Types.Venue.t option = function
    | Gemini -> Some Gemini
    | Kraken -> Some Kraken
    | Mexc -> Some Mexc
    | Coinbase -> Some Coinbase
    | Binance -> Some Binance
    | Hyperliquid -> Some Hyperliquid
    | Bitrue -> Some Bitrue
    | Dydx -> Some Dydx
    | Jupiter -> Some Jupiter
    | OneInch -> Some OneInch
    | Other _ -> None

  let to_string = function
    | Gemini -> "Gemini"
    | Kraken -> "Kraken"
    | Mexc -> "MEXC"
    | Coinbase -> "Coinbase"
    | Binance -> "Binance"
    | Hyperliquid -> "Hyperliquid"
    | Bitrue -> "Bitrue"
    | Dydx -> "dYdX"
    | Jupiter -> "Jupiter"
    | OneInch -> "1inch"
    | Other s -> s
end

(** Trade side with bin_io *)
module Side = struct
  type t = Buy | Sell [@@deriving sexp, compare, equal, bin_io]

  let of_fluxum_side : Fluxum.Types.Side.t -> t = function
    | Buy -> Buy
    | Sell -> Sell

  let to_fluxum_side : t -> Fluxum.Types.Side.t = function
    | Buy -> Fluxum.Types.Side.Buy
    | Sell -> Fluxum.Types.Side.Sell

  let to_string = function
    | Buy -> "buy"
    | Sell -> "sell"
end

(** Price level for order book *)
module Price_level = struct
  type t = { price : float; qty : float }
  [@@deriving sexp, compare, equal, bin_io]
end

(** Market data events *)
module Market_event = struct
  type t =
    | Book_update of
        { symbol : string
        ; venue : Venue.t
        ; bids : Price_level.t list
        ; asks : Price_level.t list
        ; is_snapshot : bool
        }
    | Trade of
        { symbol : string
        ; venue : Venue.t
        ; price : float
        ; qty : float
        ; side : Side.t option
        ; trade_id : string option
        }
    | Ticker of
        { symbol : string
        ; venue : Venue.t
        ; bid : float
        ; ask : float
        ; last : float
        ; volume_24h : float option
        }
  [@@deriving sexp, compare, bin_io]
end

(** Time in force for orders *)
module Time_in_force = struct
  type t =
    | GTC  (** Good til canceled *)
    | IOC  (** Immediate or cancel *)
    | FOK  (** Fill or kill *)
    | GTD of Time.t  (** Good til date *)
  [@@deriving sexp, compare, bin_io]

  let to_string = function
    | GTC -> "GTC"
    | IOC -> "IOC"
    | FOK -> "FOK"
    | GTD _ -> "GTD"
end

(** Order events *)
module Order_event = struct
  type t =
    | Order_submitted of
        { order_id : string
        ; symbol : string
        ; venue : Venue.t
        ; side : Side.t
        ; qty : float
        ; price : float option  (** None for market orders *)
        ; time_in_force : Time_in_force.t
        }
    | Order_accepted of
        { order_id : string
        ; exchange_id : string
        ; venue : Venue.t
        }
    | Order_filled of
        { order_id : string
        ; venue : Venue.t
        ; fill_qty : float
        ; fill_price : float
        ; fee : float
        ; is_maker : bool option
        }
    | Order_partially_filled of
        { order_id : string
        ; venue : Venue.t
        ; fill_qty : float
        ; fill_price : float
        ; remaining_qty : float
        ; fee : float
        }
    | Order_cancelled of
        { order_id : string
        ; venue : Venue.t
        ; reason : string
        }
    | Order_rejected of
        { order_id : string
        ; venue : Venue.t
        ; reason : string
        }
  [@@deriving sexp, compare, bin_io]
end

(** Balance events *)
module Balance_event = struct
  type t =
    | Balance_update of
        { venue : Venue.t
        ; currency : string
        ; available : float
        ; locked : float
        ; total : float
        }
    | Balance_snapshot of
        { venue : Venue.t
        ; balances : (string * float * float * float) list  (** currency, available, locked, total *)
        }
  [@@deriving sexp, compare, bin_io]
end

(** Connection state *)
module Connection_state = struct
  type t =
    | Disconnected
    | Connecting
    | Connected
    | Ready
    | Reconnecting of int  (** attempt number *)
    | Failed of string
  [@@deriving sexp, compare, bin_io]
end

(** System events *)
module System_event = struct
  type t =
    | Bot_started of
        { bot_id : string
        ; config_json : string
        ; version : string
        }
    | Bot_stopped of
        { reason : string
        ; exit_code : int option
        }
    | Connection_state_changed of
        { venue : Venue.t
        ; old_state : Connection_state.t
        ; new_state : Connection_state.t
        }
    | Error of
        { venue : Venue.t option
        ; message : string
        ; is_fatal : bool
        }
    | Heartbeat of
        { sequence : int64
        }
  [@@deriving sexp, compare, bin_io]
end

(** Strategy events *)
module Strategy_event = struct
  type signal_type =
    | Place_order
    | Cancel_order
    | Cancel_all
    | No_action
  [@@deriving sexp, compare, bin_io]

  type t =
    | Signal_generated of
        { signal_type : signal_type
        ; symbol : string option
        ; reason : string
        ; details : string option
        }
    | Position_changed of
        { symbol : string
        ; venue : Venue.t
        ; old_qty : float
        ; new_qty : float
        ; avg_cost : float
        }
    | Pnl_update of
        { symbol : string
        ; venue : Venue.t
        ; realized : float
        ; unrealized : float
        ; total : float
        }
  [@@deriving sexp, compare, bin_io]
end

(** Top-level event type *)
type t =
  | Market of Market_event.t
  | Order of Order_event.t
  | Balance of Balance_event.t
  | System of System_event.t
  | Strategy of Strategy_event.t
[@@deriving sexp, compare, bin_io]

(** Event envelope with metadata *)
type envelope =
  { id : Event_id.t
  ; timestamp : Time.t
  ; sequence : int64
  ; event : t
  }
[@@deriving sexp, compare, bin_io]

(** Create a new envelope for an event *)
let create_envelope ?(sequence = 0L) event =
  { id = Event_id.create ()
  ; timestamp = Time.now ()
  ; sequence
  ; event
  }

(** Serialize envelope to binary string *)
let serialize envelope =
  Bin_prot.Writer.to_string bin_writer_envelope envelope

(** Deserialize envelope from binary string *)
let deserialize data =
  try Ok (Bin_prot.Reader.of_string bin_reader_envelope data)
  with exn -> Error (Error.of_exn exn)

(** Get event category as string *)
let category = function
  | Market _ -> "market"
  | Order _ -> "order"
  | Balance _ -> "balance"
  | System _ -> "system"
  | Strategy _ -> "strategy"

(** Short description of event for logging *)
let describe = function
  | Market (Book_update { symbol; venue; is_snapshot; _ }) ->
    sprintf "book_%s %s@%s" (match is_snapshot with true -> "snap" | false -> "upd") symbol (Venue.to_string venue)
  | Market (Trade { symbol; venue; price; qty; side; _ }) ->
    let side_str = match side with Some s -> Side.to_string s | None -> "?" in
    sprintf "trade %s@%s %.8f@%.2f %s" symbol (Venue.to_string venue) qty price side_str
  | Market (Ticker { symbol; venue; last; _ }) ->
    sprintf "tick %s@%s last=%.2f" symbol (Venue.to_string venue) last
  | Order (Order_submitted { order_id; symbol; side; qty; price; _ }) ->
    let price_str = match price with Some p -> sprintf "@%.2f" p | None -> "@MKT" in
    sprintf "submit %s %s %.8f%s [%s]" (Side.to_string side) symbol qty price_str order_id
  | Order (Order_accepted { order_id; exchange_id; _ }) ->
    sprintf "accept %s -> %s" order_id exchange_id
  | Order (Order_filled { order_id; fill_qty; fill_price; _ }) ->
    sprintf "fill %s %.8f@%.2f" order_id fill_qty fill_price
  | Order (Order_partially_filled { order_id; fill_qty; remaining_qty; _ }) ->
    sprintf "partial %s %.8f (rem: %.8f)" order_id fill_qty remaining_qty
  | Order (Order_cancelled { order_id; reason; _ }) ->
    sprintf "cancel %s: %s" order_id reason
  | Order (Order_rejected { order_id; reason; _ }) ->
    sprintf "reject %s: %s" order_id reason
  | Balance (Balance_update { venue; currency; total; _ }) ->
    sprintf "bal %s@%s %.8f" currency (Venue.to_string venue) total
  | Balance (Balance_snapshot { venue; balances }) ->
    sprintf "bal_snap %s (%d assets)" (Venue.to_string venue) (List.length balances)
  | System (Bot_started { bot_id; _ }) ->
    sprintf "START %s" bot_id
  | System (Bot_stopped { reason; _ }) ->
    sprintf "STOP: %s" reason
  | System (Connection_state_changed { venue; new_state; _ }) ->
    sprintf "conn %s -> %s" (Venue.to_string venue) (Sexp.to_string (Connection_state.sexp_of_t new_state))
  | System (Error { message; is_fatal; _ }) ->
    sprintf "%s: %s" (match is_fatal with true -> "FATAL" | false -> "error") message
  | System (Heartbeat { sequence }) ->
    sprintf "heartbeat #%Ld" sequence
  | Strategy (Signal_generated { signal_type; reason; _ }) ->
    sprintf "signal %s: %s" (Sexp.to_string (Strategy_event.sexp_of_signal_type signal_type)) reason
  | Strategy (Position_changed { symbol; new_qty; _ }) ->
    sprintf "pos %s -> %.8f" symbol new_qty
  | Strategy (Pnl_update { symbol; total; _ }) ->
    sprintf "pnl %s %.2f" symbol total

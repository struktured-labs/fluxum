open Core

(** Order side *)
type side =
  | BUY
  | SELL
[@@deriving sexp, yojson]

let side_to_string = function
  | BUY -> "BUY"
  | SELL -> "SELL"

let side_of_string = function
  | "BUY" -> BUY
  | "SELL" -> SELL
  | s -> failwith (sprintf "Unknown side: %s" s)

(** Order type *)
type order_type =
  | MARKET
  | LIMIT
  | STOP_LIMIT
  | TAKE_PROFIT_LIMIT
[@@deriving sexp, yojson]

let order_type_to_string = function
  | MARKET -> "MARKET"
  | LIMIT -> "LIMIT"
  | STOP_LIMIT -> "STOP_LIMIT"
  | TAKE_PROFIT_LIMIT -> "TAKE_PROFIT_LIMIT"

(** Time in force *)
type time_in_force =
  | GTT  (** Good-Til-Time *)
  | FOK  (** Fill-or-Kill *)
  | IOC  (** Immediate-or-Cancel *)
[@@deriving sexp, yojson]

let time_in_force_to_string = function
  | GTT -> "GTT"
  | FOK -> "FOK"
  | IOC -> "IOC"

(** Order status *)
type order_status =
  | BEST_EFFORT_OPENED
  | OPEN
  | FILLED
  | CANCELED
  | BEST_EFFORT_CANCELED
  | UNTRIGGERED
[@@deriving sexp, yojson]

let order_status_of_string = function
  | "BEST_EFFORT_OPENED" -> BEST_EFFORT_OPENED
  | "OPEN" -> OPEN
  | "FILLED" -> FILLED
  | "CANCELED" -> CANCELED
  | "BEST_EFFORT_CANCELED" -> BEST_EFFORT_CANCELED
  | "UNTRIGGERED" -> UNTRIGGERED
  | s -> failwith (sprintf "Unknown order status: %s" s)

(** Position status *)
type position_status =
  | OPEN
  | CLOSED
  | LIQUIDATED
[@@deriving sexp, yojson]

(** Candle resolution *)
type candle_resolution =
  | ONE_MINUTE
  | FIVE_MINUTES
  | FIFTEEN_MINUTES
  | THIRTY_MINUTES
  | ONE_HOUR
  | FOUR_HOURS
  | ONE_DAY
[@@deriving sexp, yojson]

let candle_resolution_to_string = function
  | ONE_MINUTE -> "1MIN"
  | FIVE_MINUTES -> "5MINS"
  | FIFTEEN_MINUTES -> "15MINS"
  | THIRTY_MINUTES -> "30MINS"
  | ONE_HOUR -> "1HOUR"
  | FOUR_HOURS -> "4HOURS"
  | ONE_DAY -> "1DAY"

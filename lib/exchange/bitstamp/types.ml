(** Bitstamp API Types

    Native Bitstamp data structures from REST and WebSocket APIs.
*)

open Core

(** {1 Public API Types} *)

(** Ticker data *)
type ticker = {
  high : string;        (** Last 24 hours price high *)
  last : string;        (** Last BTC price *)
  timestamp : string;   (** Unix timestamp *)
  bid : string;         (** Highest buy order *)
  vwap : string;        (** Last 24 hours volume weighted average price *)
  volume : string;      (** Last 24 hours volume *)
  low : string;         (** Last 24 hours price low *)
  ask : string;         (** Lowest sell order *)
  open_price : string;  (** First price of the day *)
  [@key "open"]
} [@@deriving yojson { strict = false }, sexp]

(** Order book entry (price level) *)
type order_book_entry = float * float  (** (price, amount) *)
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`String price; `String amount] ->
    (try
      Ok (Float.of_string price, Float.of_string amount)
    with _ -> Error "Invalid order book entry")
  | _ -> Error "Order book entry must be [price, amount]"

let order_book_entry_to_yojson (price, amount) =
  `List [`String (Float.to_string price); `String (Float.to_string amount)]

(** Order book *)
type order_book = {
  timestamp : string;          (** Unix timestamp *)
  microtimestamp : string;     (** Microsecond Unix timestamp *)
  bids : order_book_entry list; (** Buy orders (descending by price) *)
  asks : order_book_entry list; (** Sell orders (ascending by price) *)
} [@@deriving yojson { strict = false }, sexp]

(** Public trade *)
type trade = {
  date : string;        (** Unix timestamp *)
  tid : int;            (** Transaction ID *)
  price : string;       (** BTC price *)
  amount : string;      (** BTC amount *)
  type_ : int;          (** 0 (buy) or 1 (sell) *)
  [@key "type"]
} [@@deriving yojson { strict = false }, sexp]

(** OHLC candle data *)
type ohlc = {
  high : string;        (** Highest price *)
  timestamp : string;   (** Unix timestamp *)
  volume : string;      (** Volume *)
  low : string;         (** Lowest price *)
  close : string;       (** Close price *)
  open_price : string;  (** Open price *)
  [@key "open"]
} [@@deriving yojson { strict = false }, sexp]

(** Trading pair info *)
type trading_pair_info = {
  name : string;                    (** Pair name (e.g., "BTC/USD") *)
  url_symbol : string;              (** URL-friendly symbol *)
  base_decimals : int;              (** Base currency decimal places *)
  counter_decimals : int;           (** Counter currency decimal places *)
  minimum_order : string;           (** Minimum order size *)
  trading : string;                 (** "Enabled" or "Disabled" *)
  description : string;             (** Human-readable description *)
} [@@deriving yojson { strict = false }, sexp]

(** {1 Private API Types} *)

(** Account balance *)
type balance = {
  (* Available balances *)
  usd_available : string option; [@default None]
  btc_available : string option; [@default None]
  eur_available : string option; [@default None]
  eth_available : string option; [@default None]

  (* Reserved balances (in orders) *)
  usd_reserved : string option; [@default None]
  btc_reserved : string option; [@default None]
  eur_reserved : string option; [@default None]
  eth_reserved : string option; [@default None]

  (* Total balances *)
  usd_balance : string option; [@default None]
  btc_balance : string option; [@default None]
  eur_balance : string option; [@default None]
  eth_balance : string option; [@default None]

  (* Trading fees *)
  fee : string option; [@default None]
} [@@deriving yojson { strict = false }, sexp]

(** Order response *)
type order_response = {
  id : string;          (** Order ID *)
  datetime : string;    (** ISO 8601 datetime *)
  type_ : int;          (** 0 (buy) or 1 (sell) *)
  [@key "type"]
  price : string;       (** Order price *)
  amount : string;      (** Order amount *)
} [@@deriving yojson { strict = false }, sexp]

(** Open order *)
type open_order = {
  id : string;                (** Order ID *)
  datetime : string;          (** ISO 8601 datetime *)
  type_ : int;                (** 0 (buy) or 1 (sell) *)
  [@key "type"]
  price : string;             (** Order price *)
  amount : string;            (** Order amount *)
  currency_pair : string;     (** Trading pair *)
} [@@deriving yojson { strict = false }, sexp]

(** User transaction *)
type user_transaction = {
  datetime : string;          (** ISO 8601 datetime *)
  id : int;                   (** Transaction ID *)
  type_ : int;                (** Transaction type *)
  [@key "type"]
  usd : string option; [@default None]
  btc : string option; [@default None]
  eur : string option; [@default None]
  eth : string option; [@default None]
  fee : string;               (** Transaction fee *)
  order_id : int;             (** Related order ID *)
} [@@deriving yojson { strict = false }, sexp]

(** {1 WebSocket Types} *)

(** WebSocket subscription request *)
type subscription = {
  event : string;       (** "bts:subscribe" *)
  data : Yojson.Safe.t; (** { "channel": "..." } *)
} [@@deriving yojson]

(** WebSocket message *)
type ws_message = {
  event : string;             (** Event type *)
  channel : string option; [@default None]  (** Channel name *)
  data : Yojson.Safe.t option; [@default None] (** Message data *)
} [@@deriving yojson { strict = false }]

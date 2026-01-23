(** Gate.io API Types *)

open Core

(** Ticker *)
type ticker = {
  currency_pair : string;
  last : string;
  lowest_ask : string;
  highest_bid : string;
  high_24h : string;
  low_24h : string;
  base_volume : string;
  quote_volume : string;
} [@@deriving yojson { strict = false }, sexp]

(** Order book entry [price, amount] *)
type order_book_entry = string * string
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`String price; `String amount] -> Ok (price, amount)
  | _ -> Error "Order book entry must be [price, amount]"

let order_book_entry_to_yojson (price, amount) =
  `List [`String price; `String amount]

(** Order book *)
type order_book = {
  current : int64;              (** Timestamp *)
  update : int64;               (** Update ID *)
  asks : order_book_entry list;
  bids : order_book_entry list;
} [@@deriving yojson { strict = false }, sexp]

(** Public trade *)
type trade = {
  id : int64;
  create_time : int64;
  side : string;
  amount : string;
  price : string;
} [@@deriving yojson { strict = false }, sexp]

(** Balance *)
type balance = {
  currency : string;
  available : string;
  locked : string;
} [@@deriving yojson { strict = false }, sexp]

(** Order response *)
type order_response = {
  id : string;
  text : string;
  currency_pair : string;
  type_ : string; [@key "type"]
  account : string;
  side : string;
  amount : string;
  price : string;
  time_in_force : string;
  status : string;
} [@@deriving yojson { strict = false }, sexp]

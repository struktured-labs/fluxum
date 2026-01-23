(** Poloniex API Types *)

open Core

type ticker = {
  symbol : string;
  high : string;
  low : string;
  close : string;
  quantity : string;
  amount : string;
  open_ : string; [@key "open"]
  bid : string;
  ask : string;
} [@@deriving yojson { strict = false }, sexp]

type order_book_entry = string * string
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`String price; `String qty] -> Ok (price, qty)
  | _ -> Error "Order book entry must be [price, qty]"

let order_book_entry_to_yojson (price, qty) =
  `List [`String price; `String qty]

type order_book = {
  time : int64;
  scale : string;
  bids : order_book_entry list;
  asks : order_book_entry list;
} [@@deriving yojson { strict = false }, sexp]

type trade = {
  id : string;
  price : string;
  quantity : string;
  takerSide : string;
  ts : int64;
} [@@deriving yojson { strict = false }, sexp]

type balance = {
  currency : string;
  available : string;
  hold : string;
} [@@deriving yojson { strict = false }, sexp]

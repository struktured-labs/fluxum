(** HTX (Huobi) API Types *)

open Core

type ticker = {
  symbol : string;
  open_ : float; [@key "open"]
  high : float;
  low : float;
  close : float;
  amount : float;
  vol : float;
  count : int;
  bid : float;
  bidSize : float;
  ask : float;
  askSize : float;
} [@@deriving yojson { strict = false }, sexp]

type order_book_entry = float * float
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`Float price; `Float amount] -> Ok (price, amount)
  | _ -> Error "Order book entry must be [price, amount]"

let order_book_entry_to_yojson (price, amount) =
  `List [`Float price; `Float amount]

type order_book = {
  ts : int64;
  bids : order_book_entry list;
  asks : order_book_entry list;
} [@@deriving yojson { strict = false }, sexp]

type trade = {
  id : int64;
  ts : int64;
  data : trade_data list;
} [@@deriving yojson { strict = false }, sexp]

and trade_data = {
  id : int64;
  ts : int64;
  tradeId : int64;
  amount : float;
  price : float;
  direction : string;
} [@@deriving yojson { strict = false }, sexp]

type balance = {
  currency : string;
  type_ : string; [@key "type"]
  balance : string;
} [@@deriving yojson { strict = false }, sexp]

type order_response = {
  status : string;
  data : string;
} [@@deriving yojson { strict = false }, sexp]

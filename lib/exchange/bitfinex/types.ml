(** Bitfinex API Types *)

open Core

(** Ticker [BID, BID_SIZE, ASK, ASK_SIZE, DAILY_CHANGE, DAILY_CHANGE_RELATIVE,
           LAST_PRICE, VOLUME, HIGH, LOW] *)
type ticker = float list
[@@deriving yojson, sexp]

(** Order book entry [PRICE, COUNT, AMOUNT] *)
type order_book_entry = float * int * float
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`Float price; `Int count; `Float amount] -> Ok (price, count, amount)
  | _ -> Error "Order book entry must be [price, count, amount]"

let order_book_entry_to_yojson (price, count, amount) =
  `List [`Float price; `Int count; `Float amount]

type order_book = order_book_entry list
[@@deriving yojson, sexp]

(** Trade [ID, MTS, AMOUNT, PRICE] *)
type trade = int64 * int64 * float * float
[@@deriving sexp]

let trade_of_yojson = function
  | `List [`Int id; `Int mts; `Float amount; `Float price] ->
    Ok (Int64.of_int id, Int64.of_int mts, amount, price)
  | _ -> Error "Trade must be [id, mts, amount, price]"

let trade_to_yojson (id, mts, amount, price) =
  `List [`Int (Int64.to_int_exn id); `Int (Int64.to_int_exn mts); `Float amount; `Float price]

type trades = trade list
[@@deriving yojson, sexp]

(** Wallet [WALLET_TYPE, CURRENCY, BALANCE, UNSETTLED_INTEREST, BALANCE_AVAILABLE] *)
type wallet = string * string * float * float * float option
[@@deriving sexp]

let wallet_of_yojson = function
  | `List [`String wtype; `String currency; `Float balance; `Float unsettled; bal_avail] ->
    let avail = match bal_avail with
      | `Float f -> Some f
      | `Null -> None
      | _ -> None
    in
    Ok (wtype, currency, balance, unsettled, avail)
  | _ -> Error "Wallet must be [type, currency, balance, unsettled, available]"

let wallet_to_yojson (wtype, currency, balance, unsettled, avail) =
  let avail_json = match avail with
    | Some f -> `Float f
    | None -> `Null
  in
  `List [`String wtype; `String currency; `Float balance; `Float unsettled; avail_json]

type wallets = wallet list
[@@deriving yojson, sexp]

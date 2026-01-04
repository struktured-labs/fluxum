(** Coinbase Advanced Trade REST API *)

open Core
open Async

(* ============================================================ *)
(* Error Types *)
(* ============================================================ *)

module Error = struct
  type http = [ `Http of int * string ]
  type json = [ `Json_parse of string ]
  type api = [ `Api_error of string ]
  type t = [ http | json | api ]

  let sexp_of_t = function
    | `Http (code, msg) ->
      Sexp.List [Sexp.Atom "Http"; Sexp.Atom (sprintf "%d: %s" code msg)]
    | `Json_parse msg ->
      Sexp.List [Sexp.Atom "Json_parse"; Sexp.Atom msg]
    | `Api_error msg ->
      Sexp.List [Sexp.Atom "Api_error"; Sexp.Atom msg]
end

(* ============================================================ *)
(* Response Types *)
(* ============================================================ *)

module Types = struct
  (** Product *)
  type product = {
    product_id: string;
    price: string option; [@default None]
    price_percentage_change_24h: string option; [@default None]
    volume_24h: string option; [@default None]
    volume_percentage_change_24h: string option; [@default None]
    base_increment: string option; [@default None]
    quote_increment: string option; [@default None]
    quote_min_size: string option; [@default None]
    quote_max_size: string option; [@default None]
    base_min_size: string option; [@default None]
    base_max_size: string option; [@default None]
    base_name: string option; [@default None]
    quote_name: string option; [@default None]
    status: string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type products_response = {
    products: product list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Order book level *)
  type price_level = {
    price: string;
    size: string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Product book (order book) *)
  type product_book = {
    product_id: string;
    bids: price_level list;
    asks: price_level list;
    time: string;
  } [@@deriving yojson { strict = false }, sexp]

  type product_book_response = {
    pricebook: product_book;
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker *)
  type ticker = {
    trades: trade list;
    best_bid: string;
    best_ask: string;
  } [@@deriving yojson { strict = false }, sexp]

  and trade = {
    trade_id: string;
    product_id: string;
    price: string;
    size: string;
    time: string;
    side: string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Candle *)
  type candle = {
    start: string;
    low: string;
    high: string;
    open_: string; [@key "open"]
    close: string;
    volume: string;
  } [@@deriving yojson { strict = false }, sexp]

  type candles_response = {
    candles: candle list;
  } [@@deriving yojson { strict = false }, sexp]
end

(* ============================================================ *)
(* HTTP Client *)
(* ============================================================ *)

let get ~cfg ~path : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let uri = Uri.of_string (Cfg.rest_url ^ path) in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.get uri
    >>= fun (_response, body) ->
    Cohttp_async.Body.to_string body
  )
  >>| function
  | Error exn ->
    Error (`Http (0, Exn.to_string exn))
  | Ok body_str ->
    match Yojson.Safe.from_string body_str with
    | exception _ -> Error (`Json_parse body_str)
    | json -> Ok json

(* ============================================================ *)
(* API Endpoints *)
(* ============================================================ *)

(** Get all products *)
let products cfg : (Types.products_response, [> Error.t ]) result Deferred.t =
  get ~cfg ~path:"/api/v3/brokerage/products" >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.products_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok products -> Ok products

(** Get product book (order book) *)
let product_book cfg ~product_id : (Types.product_book, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v3/brokerage/product_book?product_id=%s" product_id in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.product_book_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.pricebook

(** Get best bid/ask *)
let best_bid_ask cfg ~product_id : (Types.ticker, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v3/brokerage/best_bid_ask?product_ids=%s" product_id in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    let pricebooks = Yojson.Safe.Util.member "pricebooks" json in
    match pricebooks with
    | `List (first :: _) ->
      (match Types.ticker_of_yojson first with
       | Error e -> Error (`Json_parse e)
       | Ok ticker -> Ok ticker)
    | _ -> Error (`Json_parse "No pricebooks in response")

(** Get product candles *)
let candles cfg ~product_id ~start ~end_ ~granularity
    : (Types.candles_response, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v3/brokerage/products/%s/candles?start=%s&end=%s&granularity=%s"
    product_id start end_ granularity
  in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.candles_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok candles -> Ok candles

(* ============================================================ *)
(* Authenticated Endpoints *)
(* ============================================================ *)

(** Make authenticated GET request *)
let get_authenticated ~cfg ~path : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp = Signature.current_timestamp () in
  match Signature.coinbase_rest_signature
          ~api_secret:Cfg.api_secret
          ~timestamp
          ~method_:"GET"
          ~path
          ~body:""
  with
  | Error _ -> Deferred.return (Error (`Api_error "Failed to generate signature"))
  | Ok signature ->
    let uri = Uri.of_string (Cfg.rest_url ^ path) in
    let headers = Cohttp.Header.of_list
      [ ("CB-ACCESS-KEY", Cfg.api_key)
      ; ("CB-ACCESS-SIGN", signature)
      ; ("CB-ACCESS-TIMESTAMP", timestamp)
      ; ("Content-Type", "application/json")
      ]
    in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.get ~headers uri
      >>= fun (_response, body) ->
      Cohttp_async.Body.to_string body
    )
    >>| function
    | Error exn ->
      Error (`Http (0, Exn.to_string exn))
    | Ok body_str ->
      match Yojson.Safe.from_string body_str with
      | exception _ -> Error (`Json_parse body_str)
      | json -> Ok json

(** Account balance *)
module Account = struct
  type balance = {
    currency: string;
    value: string;
  } [@@deriving yojson { strict = false }, sexp]

  type available_balance = {
    value: string;
    currency: string;
  } [@@deriving yojson { strict = false }, sexp]

  type hold_balance = {
    value: string;
    currency: string;
  } [@@deriving yojson { strict = false }, sexp]

  type account = {
    uuid: string;
    name: string;
    currency: string;
    available_balance: available_balance;
    default: bool; [@default false]
    active: bool; [@default true]
    created_at: string option; [@default None]
    updated_at: string option; [@default None]
    deleted_at: string option; [@default None]
    type_: string option; [@default None] [@key "type"]
    ready: bool; [@default true]
    hold: hold_balance option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type response = {
    accounts: account list;
    has_next: bool; [@default false]
    cursor: string option; [@default None]
    size: int; [@default 0]
  } [@@deriving yojson { strict = false }, sexp]
end

(** Get all accounts (balances) *)
let accounts cfg : (Account.response, [> Error.t ]) result Deferred.t =
  get_authenticated ~cfg ~path:"/api/v3/brokerage/accounts" >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account.response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok accounts -> Ok accounts

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

(** Make authenticated POST request *)
let post_authenticated ~cfg ~path ~body : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp = Signature.current_timestamp () in
  let body_str = Yojson.Safe.to_string body in
  match Signature.coinbase_rest_signature
          ~api_secret:Cfg.api_secret
          ~timestamp
          ~method_:"POST"
          ~path
          ~body:body_str
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
    let cohttp_body = Cohttp_async.Body.of_string body_str in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.post ~headers ~body:cohttp_body uri
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
(* Order Types *)
(* ============================================================ *)

module Order = struct
  (** Order configuration for market orders *)
  type market_ioc = {
    quote_size : string option; [@default None]
    base_size : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Order configuration for limit orders *)
  type limit_gtc = {
    base_size : string;
    limit_price : string;
    post_only : bool; [@default false]
  } [@@deriving yojson { strict = false }, sexp]

  type order_configuration =
    | Market_market_ioc of market_ioc
    | Limit_limit_gtc of limit_gtc

  let order_configuration_to_yojson = function
    | Market_market_ioc m ->
      `Assoc [("market_market_ioc", market_ioc_to_yojson m)]
    | Limit_limit_gtc l ->
      `Assoc [("limit_limit_gtc", limit_gtc_to_yojson l)]

  type create_order_request = {
    client_order_id : string;
    product_id : string;
    side : string;  (* BUY or SELL *)
    order_configuration : order_configuration;
  }

  let create_order_request_to_yojson req =
    `Assoc
      [ ("client_order_id", `String req.client_order_id)
      ; ("product_id", `String req.product_id)
      ; ("side", `String req.side)
      ; ("order_configuration", order_configuration_to_yojson req.order_configuration)
      ]

  type success_response = {
    order_id : string;
    product_id : string option; [@default None]
    side : string option; [@default None]
    client_order_id : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type error_response = {
    error : string option; [@default None]
    message : string option; [@default None]
    error_details : string option; [@default None]
    preview_failure_reason : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type create_order_response = {
    success : bool;
    success_response : success_response option; [@default None]
    error_response : error_response option; [@default None]
    order_id : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type order_status = {
    order_id : string;
    product_id : string;
    side : string;
    status : string;
    order_type : string option; [@default None]
    filled_size : string option; [@default None]
    filled_value : string option; [@default None]
    average_filled_price : string option; [@default None]
    total_fees : string option; [@default None]
    created_time : string option; [@default None]
    completion_percentage : string option; [@default None]
    base_size : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type order_status_response = {
    order : order_status;
  } [@@deriving yojson { strict = false }, sexp]

  type orders_response = {
    orders : order_status list;
    has_next : bool; [@default false]
    cursor : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type cancel_request_item = {
    order_id : string;
  } [@@deriving yojson { strict = false }, sexp]

  type cancel_result = {
    success : bool;
    order_id : string option; [@default None]
    failure_reason : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type cancel_response = {
    results : cancel_result list;
  } [@@deriving yojson { strict = false }, sexp]
end

(** Create an order *)
let create_order cfg (req : Order.create_order_request) : (Order.create_order_response, [> Error.t ]) result Deferred.t =
  let body = Order.create_order_request_to_yojson req in
  post_authenticated ~cfg ~path:"/api/v3/brokerage/orders" ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.create_order_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Cancel orders *)
let cancel_orders cfg ~order_ids : (Order.cancel_response, [> Error.t ]) result Deferred.t =
  let body = `Assoc [("order_ids", `List (List.map order_ids ~f:(fun id -> `String id)))] in
  post_authenticated ~cfg ~path:"/api/v3/brokerage/orders/batch_cancel" ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.cancel_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Get order by ID *)
let get_order cfg ~order_id : (Order.order_status, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v3/brokerage/orders/historical/%s" order_id in
  get_authenticated ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.order_status_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.order

(** List orders *)
let list_orders cfg ?product_id ?status ?limit () : (Order.orders_response, [> Error.t ]) result Deferred.t =
  let params = List.filter_opt
    [ Option.map product_id ~f:(fun p -> sprintf "product_id=%s" p)
    ; Option.map status ~f:(fun s -> sprintf "order_status=%s" s)
    ; Option.map limit ~f:(fun l -> sprintf "limit=%d" l)
    ]
  in
  let path = match params with
    | [] -> "/api/v3/brokerage/orders/historical"
    | _ -> sprintf "/api/v3/brokerage/orders/historical?%s" (String.concat ~sep:"&" params)
  in
  get_authenticated ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.orders_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

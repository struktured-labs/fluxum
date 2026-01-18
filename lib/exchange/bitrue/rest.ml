(** Bitrue REST API *)

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
  (** Exchange info - symbol *)
  type symbol_info = {
    symbol: string;
    status: string;
    baseAsset: string;
    baseAssetPrecision: int;
    quoteAsset: string;
    quotePrecision: int;
    quoteAssetPrecision: int;
  } [@@deriving yojson { strict = false }, sexp]

  type exchange_info = {
    timezone: string;
    serverTime: int64;
    symbols: symbol_info list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Order book *)
  type order_book = {
    lastUpdateId: int64;
    bids: (string * string) list;
    asks: (string * string) list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Recent trades *)
  type trade = {
    id: int64;
    price: string;
    qty: string;
    quoteQty: string option; [@default None]
    time: int64;
    isBuyerMaker: bool;
  } [@@deriving yojson { strict = false }, sexp]

  type recent_trades = trade list
  [@@deriving sexp]

  let recent_trades_of_yojson json =
    match json with
    | `List trades ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | t :: rest ->
          match trade_of_yojson t with
          | Error e -> Error e
          | Ok trade -> parse (trade :: acc) rest
      in
      parse [] trades
    | _ -> Error "Expected list for trades"

  (** 24hr ticker *)
  type ticker_24hr = {
    symbol: string;
    priceChange: string;
    priceChangePercent: string;
    lastPrice: string;
    bidPrice: string option; [@default None]
    bidQty: string option; [@default None]
    askPrice: string option; [@default None]
    askQty: string option; [@default None]
    openPrice: string;
    highPrice: string;
    lowPrice: string;
    volume: string;
    quoteVolume: string;
    openTime: int64;
    closeTime: int64;
    count: int;
  } [@@deriving yojson { strict = false }, sexp]

  (** Kline/Candlestick *)
  type kline = {
    open_time: int64;
    open_: string;
    high: string;
    low: string;
    close: string;
    volume: string;
    close_time: int64;
    quote_volume: string;
    trades: int;
  }

  let kline_of_yojson json =
    match json with
    | `List (open_time_json :: `String open_ :: `String high :: `String low ::
             `String close :: `String volume :: close_time_json :: `String quote_volume ::
             trades_json :: _) ->
      let open_time = match open_time_json with
        | `Int ts -> Int64.of_int ts
        | `Intlit ts -> Int64.of_string ts
        | _ -> 0L
      in
      let close_time = match close_time_json with
        | `Int ts -> Int64.of_int ts
        | `Intlit ts -> Int64.of_string ts
        | _ -> 0L
      in
      let trades = match trades_json with
        | `Int n -> n
        | `Intlit n -> Int.of_string n
        | _ -> 0
      in
      Ok {
        open_time;
        open_;
        high;
        low;
        close;
        volume;
        close_time;
        quote_volume;
        trades;
      }
    | _ -> Error "Invalid kline format"

  let sexp_of_kline k =
    Sexp.List [
      Sexp.List [Sexp.Atom "open_time"; Sexp.Atom (Int64.to_string k.open_time)];
      Sexp.List [Sexp.Atom "open"; Sexp.Atom k.open_];
      Sexp.List [Sexp.Atom "high"; Sexp.Atom k.high];
      Sexp.List [Sexp.Atom "low"; Sexp.Atom k.low];
      Sexp.List [Sexp.Atom "close"; Sexp.Atom k.close];
      Sexp.List [Sexp.Atom "volume"; Sexp.Atom k.volume];
    ]

  type klines = kline list

  let sexp_of_klines klines =
    Sexp.List (List.map klines ~f:sexp_of_kline)

  let klines_of_yojson json =
    match json with
    | `List klines ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | k :: rest ->
          match kline_of_yojson k with
          | Error e -> Error e
          | Ok kline -> parse (kline :: acc) rest
      in
      parse [] klines
    | _ -> Error "Expected list for klines"
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

(** Get exchange info *)
let exchange_info cfg : (Types.exchange_info, [> Error.t ]) result Deferred.t =
  get ~cfg ~path:"/api/v1/exchangeInfo" >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.exchange_info_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok info -> Ok info

(** Get order book depth *)
let depth cfg ~symbol ?(limit = 100) () : (Types.order_book, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v1/depth?symbol=%s&limit=%d" symbol limit in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.order_book_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok book -> Ok book

(** Get recent trades *)
let trades cfg ~symbol ?(limit = 500) () : (Types.recent_trades, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v1/trades?symbol=%s&limit=%d" symbol limit in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.recent_trades_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok trades -> Ok trades

(** Get 24hr ticker *)
let ticker_24hr cfg ~symbol : (Types.ticker_24hr, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v1/ticker/24hr?symbol=%s" symbol in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.ticker_24hr_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok ticker -> Ok ticker

(** Get klines/candlestick data *)
let klines cfg ~symbol ~interval ?(limit = 500) () : (Types.klines, [> Error.t ]) result Deferred.t =
  let path = sprintf "/api/v1/klines?symbol=%s&interval=%s&limit=%d" symbol interval limit in
  get ~cfg ~path >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.klines_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok klines -> Ok klines

(* ============================================================ *)
(* Authentication *)
(* ============================================================ *)

let hmac_sha256 ~secret ~message =
  let key = Digestif.SHA256.hmac_string ~key:secret message in
  Digestif.SHA256.to_hex key

let current_timestamp () =
  Int64.to_string (Int64.of_float (Unix.gettimeofday () *. 1000.0))

let build_query_string params =
  List.map params ~f:(fun (k, v) -> sprintf "%s=%s" k v)
  |> String.concat ~sep:"&"

(** Make authenticated GET request *)
let get_authenticated ~cfg ~path ~params : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp = current_timestamp () in
  let params_with_ts = params @ [("timestamp", timestamp)] in
  let query_string = build_query_string params_with_ts in
  let signature = hmac_sha256 ~secret:Cfg.api_secret ~message:query_string in
  let final_params = params_with_ts @ [("signature", signature)] in
  let query_string_final = build_query_string final_params in
  let uri = Uri.of_string (sprintf "%s%s?%s" Cfg.rest_url path query_string_final) in
  let headers = Cohttp.Header.of_list
    [ ("X-MBX-APIKEY", Cfg.api_key)
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

(* ============================================================ *)
(* Account Endpoints *)
(* ============================================================ *)

module Account = struct
  type balance = {
    asset: string;
    free: string;
    locked: string;
  } [@@deriving yojson { strict = false }, sexp]

  type response = {
    makerCommission: int; [@default 0]
    takerCommission: int; [@default 0]
    buyerCommission: int; [@default 0]
    sellerCommission: int; [@default 0]
    canTrade: bool; [@default true]
    canWithdraw: bool; [@default true]
    canDeposit: bool; [@default true]
    updateTime: int64; [@default 0L]
    accountType: string; [@default "SPOT"]
    balances: balance list;
  } [@@deriving yojson { strict = false }, sexp]
end

(** Get account information (balances) *)
let account cfg : (Account.response, [> Error.t ]) result Deferred.t =
  get_authenticated ~cfg ~path:"/api/v1/account" ~params:[] >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account.response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok account -> Ok account

(* ============================================================ *)
(* Order Endpoints *)
(* ============================================================ *)

module Order = struct
  type side = [ `BUY | `SELL ]
  let sexp_of_side = function
    | `BUY -> Sexp.Atom "BUY"
    | `SELL -> Sexp.Atom "SELL"
  let side_to_string = function
    | `BUY -> "BUY"
    | `SELL -> "SELL"

  type order_type = [ `LIMIT | `MARKET | `LIMIT_MAKER ]
  let sexp_of_order_type = function
    | `LIMIT -> Sexp.Atom "LIMIT"
    | `MARKET -> Sexp.Atom "MARKET"
    | `LIMIT_MAKER -> Sexp.Atom "LIMIT_MAKER"
  let order_type_to_string = function
    | `LIMIT -> "LIMIT"
    | `MARKET -> "MARKET"
    | `LIMIT_MAKER -> "LIMIT_MAKER"

  type time_in_force = [ `GTC | `IOC | `FOK ]
  let sexp_of_time_in_force = function
    | `GTC -> Sexp.Atom "GTC"
    | `IOC -> Sexp.Atom "IOC"
    | `FOK -> Sexp.Atom "FOK"
  let time_in_force_to_string = function
    | `GTC -> "GTC"
    | `IOC -> "IOC"
    | `FOK -> "FOK"

  type new_order_request = {
    symbol: string;
    side: side;
    order_type: order_type;
    quantity: string;
    price: string option;
    time_in_force: time_in_force option;
    new_client_order_id: string option;
  }

  type new_order_response = {
    symbol: string;
    orderId: string;
    clientOrderId: string option; [@default None]
    transactTime: int64;
    price: string;
    origQty: string;
    executedQty: string;
    status: string;
    type_: string; [@key "type"]
    side: string;
  } [@@deriving yojson { strict = false }, sexp]

  type order_status_response = {
    symbol: string;
    orderId: string;
    clientOrderId: string option; [@default None]
    price: string;
    origQty: string;
    executedQty: string;
    status: string;
    type_: string; [@key "type"]
    side: string;
    time: int64;
    updateTime: int64;
  } [@@deriving yojson { strict = false }, sexp]

  type cancel_response = {
    symbol: string;
    orderId: string;
    clientOrderId: string option; [@default None]
    status: string;
  } [@@deriving yojson { strict = false }, sexp]
end

(** Make authenticated POST request *)
let post_authenticated ~cfg ~path ~params : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp = current_timestamp () in
  let params_with_ts = params @ [("timestamp", timestamp)] in
  let query_string = build_query_string params_with_ts in
  let signature = hmac_sha256 ~secret:Cfg.api_secret ~message:query_string in
  let final_params = params_with_ts @ [("signature", signature)] in
  let query_string_final = build_query_string final_params in
  let uri = Uri.of_string (sprintf "%s%s?%s" Cfg.rest_url path query_string_final) in
  let headers = Cohttp.Header.of_list
    [ ("X-MBX-APIKEY", Cfg.api_key)
    ; ("Content-Type", "application/x-www-form-urlencoded")
    ]
  in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.post ~headers uri
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

(** Make authenticated DELETE request *)
let delete_authenticated ~cfg ~path ~params : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp = current_timestamp () in
  let params_with_ts = params @ [("timestamp", timestamp)] in
  let query_string = build_query_string params_with_ts in
  let signature = hmac_sha256 ~secret:Cfg.api_secret ~message:query_string in
  let final_params = params_with_ts @ [("signature", signature)] in
  let query_string_final = build_query_string final_params in
  let uri = Uri.of_string (sprintf "%s%s?%s" Cfg.rest_url path query_string_final) in
  let headers = Cohttp.Header.of_list
    [ ("X-MBX-APIKEY", Cfg.api_key)
    ; ("Content-Type", "application/x-www-form-urlencoded")
    ]
  in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.delete ~headers uri
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

(** Place new order *)
let new_order cfg (req : Order.new_order_request) : (Order.new_order_response, [> Error.t ]) result Deferred.t =
  let params = [
    ("symbol", req.symbol);
    ("side", Order.side_to_string req.side);
    ("type", Order.order_type_to_string req.order_type);
    ("quantity", req.quantity);
  ] in
  let params = match req.price with
    | Some p -> params @ [("price", p)]
    | None -> params
  in
  let params = match req.time_in_force with
    | Some tif -> params @ [("timeInForce", Order.time_in_force_to_string tif)]
    | None -> params
  in
  let params = match req.new_client_order_id with
    | Some id -> params @ [("newClientOrderId", id)]
    | None -> params
  in
  post_authenticated ~cfg ~path:"/api/v1/order" ~params >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.new_order_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok response -> Ok response

(** Query order status *)
let query_order cfg ~symbol ~order_id : (Order.order_status_response, [> Error.t ]) result Deferred.t =
  let params = [
    ("symbol", symbol);
    ("orderId", order_id);
  ] in
  get_authenticated ~cfg ~path:"/api/v1/order" ~params >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.order_status_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok response -> Ok response

(** Cancel order *)
let cancel_order cfg ~symbol ~order_id : (Order.cancel_response, [> Error.t ]) result Deferred.t =
  let params = [
    ("symbol", symbol);
    ("orderId", order_id);
  ] in
  delete_authenticated ~cfg ~path:"/api/v1/order" ~params >>| function
  | Error _ as err -> err
  | Ok json ->
    match Order.cancel_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok response -> Ok response

(** Get open orders *)
let open_orders cfg ~symbol : (Order.order_status_response list, [> Error.t ]) result Deferred.t =
  let params = [("symbol", symbol)] in
  get_authenticated ~cfg ~path:"/api/v1/openOrders" ~params >>| function
  | Error _ as err -> err
  | Ok json ->
    match json with
    | `List orders ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | o :: rest ->
          match Order.order_status_response_of_yojson o with
          | Error e -> Error (`Json_parse e)
          | Ok order -> parse (order :: acc) rest
      in
      parse [] orders
    | _ -> Error (`Json_parse "Expected list for open orders")

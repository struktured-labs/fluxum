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

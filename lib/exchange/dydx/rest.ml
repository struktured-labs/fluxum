(** dYdX v4 REST API - Indexer Endpoints *)

open Core
open Async

(* ============================================================ *)
(* Error Types *)
(* ============================================================ *)

module Error = struct
  type http = [ `Http of int * string ]
  type json = [ `Json_parse of string ]
  type api = [ `Api_error of string ]
  type not_found = [ `Not_found ]
  type t = [ http | json | api | not_found ]

  let sexp_of_t = function
    | `Http (code, msg) ->
      Sexp.List [Sexp.Atom "Http"; Sexp.Atom (sprintf "%d: %s" code msg)]
    | `Json_parse msg ->
      Sexp.List [Sexp.Atom "Json_parse"; Sexp.Atom msg]
    | `Api_error msg ->
      Sexp.List [Sexp.Atom "Api_error"; Sexp.Atom msg]
    | `Not_found ->
      Sexp.List [Sexp.Atom "Not_found"]
end

(* ============================================================ *)
(* Response Types *)
(* ============================================================ *)

module Types = struct
  (** Price level in orderbook *)
  type price_level = {
    price : string;
    size : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Orderbook response *)
  type orderbook = {
    bids : price_level list;
    asks : price_level list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Trade *)
  type trade = {
    id : string;
    side : string;
    size : string;
    price : string;
    createdAt : string;
    createdAtHeight : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Trades response *)
  type trades_response = {
    trades : trade list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Perpetual market info *)
  type perpetual_market = {
    clobPairId : string option; [@default None]
    ticker : string;
    status : string;
    initialMarginFraction : string;
    maintenanceMarginFraction : string;
    openInterest : string;
    atomicResolution : int;
    quantumConversionExponent : int;
    tickSize : string;
    stepSize : string;
    stepBaseQuantums : int;
    subticksPerTick : int;
    marketType : string;
    openInterestLowerCap : string option; [@default None]
    openInterestUpperCap : string option; [@default None]
    baseOpenInterest : string option; [@default None]
    oraclePrice : string option; [@default None]
    priceChange24H : string option; [@default None]
    volume24H : string option; [@default None]
    trades24H : int option; [@default None]
    nextFundingRate : string option; [@default None]
    defaultFundingRate1H : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  let base_asset_of_ticker ticker =
    match String.lsplit2 ticker ~on:'-' with
    | Some (base, _) -> base
    | None -> ticker

  let quote_asset_of_ticker ticker =
    match String.lsplit2 ticker ~on:'-' with
    | Some (_, quote) -> quote
    | None -> "USD"

  (** Markets response - mapping from ticker to market info *)
  type markets_response = {
    markets : (string * perpetual_market) list;
  } [@@deriving sexp]

  let markets_response_of_yojson json =
    match json with
    | `Assoc [("markets", `Assoc markets)] ->
      let parsed = List.filter_map markets ~f:(fun (ticker, market_json) ->
        match perpetual_market_of_yojson market_json with
        | Ok market -> Some (ticker, market)
        | Error _ -> None
      ) in
      Ok { markets = parsed }
    | _ -> Error "Expected {markets: {...}} object"

  (** Candle *)
  type candle = {
    startedAt : string;
    ticker : string;
    resolution : string;
    low : string;
    high : string;
    open_ : string; [@key "open"]
    close : string;
    baseTokenVolume : string;
    usdVolume : string;
    trades : int;
    startingOpenInterest : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Candles response *)
  type candles_response = {
    candles : candle list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker/stats - from sparklines endpoint *)
  type ticker_stats = {
    ticker : string;
    oraclePrice : string option; [@default None]
    priceChange24H : string option; [@default None]
    volume24H : string option; [@default None]
    trades24H : int option; [@default None]
    nextFundingRate : string option; [@default None]
    openInterest : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]
end

(* ============================================================ *)
(* HTTP Client *)
(* ============================================================ *)

let get (module Cfg : Cfg.S) ~path ?params () : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let base_uri = Uri.of_string Cfg.rest_url in
  let uri = Uri.with_path base_uri path in
  let uri = match params with
    | None -> uri
    | Some p -> Uri.add_query_params' uri p
  in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.get uri
    >>= fun (response, body) ->
    Cohttp_async.Body.to_string body
    >>| fun body_str ->
    (response, body_str)
  )
  >>| function
  | Error exn ->
    Error (`Http (0, Exn.to_string exn))
  | Ok (response, body_str) ->
    let status = Cohttp.Response.status response in
    let code = Cohttp.Code.code_of_status status in
    match code with
    | 404 -> Error `Not_found
    | c when c >= 400 -> Error (`Http (code, body_str))
    | _ ->
      match Yojson.Safe.from_string body_str with
      | exception _ -> Error (`Json_parse body_str)
      | json -> Ok json

(* ============================================================ *)
(* API Endpoints *)
(* ============================================================ *)

(** Get all perpetual markets *)
let markets cfg ?market () : (Types.markets_response, [> Error.t ]) result Deferred.t =
  let params = match market with
    | None -> None
    | Some m -> Some [("ticker", m)]
  in
  get cfg ~path:"/v4/perpetualMarkets" ?params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.markets_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Get orderbook for a market *)
let orderbook cfg ~market : (Types.orderbook, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/orderbooks/perpetualMarket/%s" market in
  get cfg ~path () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.orderbook_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok book -> Ok book

(** Get recent trades for a market *)
let trades cfg ~market ?limit () : (Types.trade list, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/trades/perpetualMarket/%s" market in
  let params = match limit with
    | None -> None
    | Some l -> Some [("limit", Int.to_string l)]
  in
  get cfg ~path ?params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.trades_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.trades

(** Get candles for a market *)
let candles cfg ~market ~resolution ?limit () : (Types.candle list, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/candles/perpetualMarkets/%s" market in
  let resolution_str = Common.candle_resolution_to_string resolution in
  let params = [("resolution", resolution_str)] in
  let params = match limit with
    | None -> params
    | Some l -> ("limit", Int.to_string l) :: params
  in
  get cfg ~path ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.candles_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.candles

(** Get market stats/ticker info - uses perpetualMarkets with filtering *)
let ticker cfg ~market : ((string * Types.perpetual_market) option, [> Error.t ]) result Deferred.t =
  markets cfg ~market () >>| function
  | Error _ as err -> err
  | Ok resp ->
    Ok (List.find resp.markets ~f:(fun (ticker, _) -> String.equal ticker market))

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

(* ============================================================ *)
(* Account / Subaccount Types *)
(* ============================================================ *)

module Account_types = struct
  (** Transfer type for deposit/withdrawal *)
  type transfer_type =
    | DEPOSIT
    | WITHDRAWAL
    | TRANSFER_IN
    | TRANSFER_OUT
  [@@deriving sexp]

  let transfer_type_of_string = function
    | "DEPOSIT" -> Ok DEPOSIT
    | "WITHDRAWAL" -> Ok WITHDRAWAL
    | "TRANSFER_IN" -> Ok TRANSFER_IN
    | "TRANSFER_OUT" -> Ok TRANSFER_OUT
    | s -> Error (`Unknown_transfer_type s)

  let transfer_type_of_yojson json =
    match json with
    | `String s -> (
        match transfer_type_of_string s with
        | Ok t -> Ok t
        | Error (`Unknown_transfer_type s) -> Error (sprintf "Unknown transfer type: %s" s))
    | _ -> Error "Expected string for transfer type"

  let transfer_type_to_yojson = function
    | DEPOSIT -> `String "DEPOSIT"
    | WITHDRAWAL -> `String "WITHDRAWAL"
    | TRANSFER_IN -> `String "TRANSFER_IN"
    | TRANSFER_OUT -> `String "TRANSFER_OUT"

  (** Transfer record from dYdX v4 indexer *)
  type transfer = {
    id : string;
    sender : transfer_sender option [@default None];
    recipient : transfer_recipient option [@default None];
    size : string;
    createdAt : string;
    createdAtHeight : string;
    symbol : string option [@default None];
    type_ : transfer_type [@key "type"];
    transactionHash : string option [@default None];
  } [@@deriving yojson { strict = false }, sexp]

  and transfer_sender = {
    sender_address : string [@key "address"];
    sender_subaccountNumber : int option [@default None] [@key "subaccountNumber"];
  } [@@deriving yojson { strict = false }, sexp]

  and transfer_recipient = {
    recipient_address : string [@key "address"];
    recipient_subaccountNumber : int option [@default None] [@key "subaccountNumber"];
  } [@@deriving yojson { strict = false }, sexp]

  (** Transfers list response *)
  type transfers_response = {
    transfers : transfer list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Subaccount balance - USDC balance on dYdX *)
  type asset_position = {
    symbol : string;
    side : string;
    size : string;
    assetId : string;
    subaccountNumber : int;
  } [@@deriving yojson { strict = false }, sexp]

  (** Perpetual position *)
  type perpetual_position = {
    market : string;
    status : string;
    side : string;
    size : string;
    maxSize : string option [@default None];
    entryPrice : string;
    exitPrice : string option [@default None];
    realizedPnl : string;
    unrealizedPnl : string;
    createdAt : string;
    createdAtHeight : string;
    closedAt : string option [@default None];
    sumOpen : string option [@default None];
    sumClose : string option [@default None];
    netFunding : string option [@default None];
    subaccountNumber : int;
  } [@@deriving yojson { strict = false }, sexp]

  (** Subaccount info *)
  type subaccount = {
    address : string;
    subaccountNumber : int;
    equity : string;
    freeCollateral : string;
    marginEnabled : bool option [@default None];
    updatedAtHeight : string option [@default None];
    assetPositions : asset_position list [@default []];
    perpetualPositions : perpetual_position list [@default []];
  } [@@deriving yojson { strict = false }, sexp]

  (** Subaccount response *)
  type subaccount_response = {
    subaccount : subaccount;
  } [@@deriving yojson { strict = false }, sexp]

  (** List of subaccounts response *)
  type subaccounts_response = {
    subaccounts : subaccount list;
  } [@@deriving yojson { strict = false }, sexp]
end

(* ============================================================ *)
(* Account / Transfer Endpoints *)
(* ============================================================ *)

(** Get subaccount info by address and subaccount number *)
let subaccount cfg ~address ~subaccount_number
  : (Account_types.subaccount, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/addresses/%s/subaccountNumber/%d" address subaccount_number in
  get cfg ~path () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account_types.subaccount_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.subaccount

(** Get all subaccounts for an address *)
let subaccounts cfg ~address
  : (Account_types.subaccount list, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/addresses/%s" address in
  get cfg ~path () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account_types.subaccounts_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.subaccounts

(** Get transfers (deposits, withdrawals, internal transfers) for a subaccount *)
let transfers cfg ~address ~subaccount_number ?limit ?created_before_or_at_height ()
  : (Account_types.transfer list, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/transfers" in
  let params = [
    ("address", address);
    ("subaccountNumber", Int.to_string subaccount_number);
  ] in
  let params = match limit with
    | None -> params
    | Some l -> ("limit", Int.to_string l) :: params
  in
  let params = match created_before_or_at_height with
    | None -> params
    | Some h -> ("createdBeforeOrAtHeight", h) :: params
  in
  get cfg ~path ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account_types.transfers_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.transfers

(** Get deposits only - filters transfers by type *)
let deposits cfg ~address ~subaccount_number ?limit ()
  : (Account_types.transfer list, [> Error.t ]) result Deferred.t =
  transfers cfg ~address ~subaccount_number ?limit () >>| function
  | Error _ as err -> err
  | Ok all_transfers ->
    Ok (List.filter all_transfers ~f:(fun t ->
      match t.Account_types.type_ with
      | Account_types.DEPOSIT | Account_types.TRANSFER_IN -> true
      | Account_types.WITHDRAWAL | Account_types.TRANSFER_OUT -> false))

(** Get withdrawals only - filters transfers by type *)
let withdrawals cfg ~address ~subaccount_number ?limit ()
  : (Account_types.transfer list, [> Error.t ]) result Deferred.t =
  transfers cfg ~address ~subaccount_number ?limit () >>| function
  | Error _ as err -> err
  | Ok all_transfers ->
    Ok (List.filter all_transfers ~f:(fun t ->
      match t.Account_types.type_ with
      | Account_types.WITHDRAWAL | Account_types.TRANSFER_OUT -> true
      | Account_types.DEPOSIT | Account_types.TRANSFER_IN -> false))

(** Get parent subaccount transfers (subaccountNumber 0 is typically the main one) *)
let parent_transfers cfg ~address ?limit ()
  : (Account_types.transfer list, [> Error.t ]) result Deferred.t =
  let path = sprintf "/v4/transfers/parentSubaccountNumber" in
  let params = [
    ("address", address);
    ("parentSubaccountNumber", "0");
  ] in
  let params = match limit with
    | None -> params
    | Some l -> ("limit", Int.to_string l) :: params
  in
  get cfg ~path ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Account_types.transfers_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp.transfers

(** Get asset positions (USDC balance) for subaccount *)
let asset_positions cfg ~address ~subaccount_number
  : (Account_types.asset_position list, [> Error.t ]) result Deferred.t =
  subaccount cfg ~address ~subaccount_number >>| function
  | Error _ as err -> err
  | Ok sub -> Ok sub.assetPositions

(** Get perpetual positions for subaccount *)
let perpetual_positions cfg ~address ~subaccount_number
  : (Account_types.perpetual_position list, [> Error.t ]) result Deferred.t =
  subaccount cfg ~address ~subaccount_number >>| function
  | Error _ as err -> err
  | Ok sub -> Ok sub.perpetualPositions

(** Hyperliquid REST API *)

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
  (** Mid prices for all coins *)
  type all_mids = (string * string) list
  [@@deriving sexp]

  let all_mids_of_yojson json =
    match json with
    | `Assoc pairs ->
      Ok (List.map pairs ~f:(fun (coin, price) ->
        (coin, Yojson.Safe.Util.to_string price)))
    | _ -> Error "Expected object for allMids"

  (** Order book level *)
  type level = {
    px : string;  (* price *)
    sz : string;  (* size *)
    n : int;      (* number of orders *)
  } [@@deriving yojson { strict = false }, sexp]

  (** L2 order book *)
  type l2_book = {
    coin : string;
    levels : level list list;  (* [bids, asks] *)
    time : int64;
  } [@@deriving yojson { strict = false }, sexp]

  (** Asset context (from meta) *)
  type asset_ctx = {
    funding : string;
    openInterest : string;
    prevDayPx : string;
    dayNtlVlm : string;
    premium : string option; [@default None]
    oraclePx : string;
    markPx : string;
    midPx : string option; [@default None]
    impactPxs : string list option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Universe/Perpetual info *)
  type perp_info = {
    name : string;
    szDecimals : int;
    maxLeverage : int option; [@default None]
    onlyIsolated : bool option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Meta response (exchange metadata) *)
  type universe_item = {
    name : string;
    szDecimals : int;
    maxLeverage : int option; [@default None]
    onlyIsolated : bool option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type meta = {
    universe : universe_item list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Combined meta and asset contexts *)
  type meta_and_asset_ctxs = meta * asset_ctx list
  [@@deriving sexp]

  let meta_and_asset_ctxs_of_yojson json =
    match json with
    | `List [meta_json; ctxs_json] ->
      (match meta_of_yojson meta_json with
       | Error e -> Error e
       | Ok meta ->
         match ctxs_json with
         | `List ctxs ->
           let rec parse_ctxs acc = function
             | [] -> Ok (List.rev acc)
             | ctx :: rest ->
               match asset_ctx_of_yojson ctx with
               | Error e -> Error e
               | Ok c -> parse_ctxs (c :: acc) rest
           in
           (match parse_ctxs [] ctxs with
            | Error e -> Error e
            | Ok ctxs -> Ok (meta, ctxs))
         | _ -> Error "Expected list for asset contexts")
    | _ -> Error "Expected [meta, asset_ctxs] tuple"

  (** Trade/fill *)
  type trade = {
    coin : string;
    px : string;
    sz : string;
    side : string;
    time : int64;
    hash : string;
    tid : int64;  (* trade id *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Recent trades response *)
  type recent_trades = trade list
  [@@deriving sexp]

  let recent_trades_of_yojson json =
    match json with
    | `List trades ->
      let rec parse_trades acc = function
        | [] -> Ok (List.rev acc)
        | t :: rest ->
          match trade_of_yojson t with
          | Error e -> Error e
          | Ok tr -> parse_trades (tr :: acc) rest
      in
      parse_trades [] trades
    | _ -> Error "Expected list for trades"

  (** Candle (OHLCV) *)
  type candle = {
    t : int64;           (* open time *)
    close_time : int64;  [@key "T"] (* close time *)
    s : string;          (* coin symbol *)
    i : string;          (* interval *)
    o : string;          (* open *)
    c : string;          (* close *)
    h : string;          (* high *)
    l : string;          (* low *)
    v : string;          (* volume *)
    n : int;             (* number of trades *)
  } [@@deriving yojson { strict = false }, sexp]

  type candles = candle list
  [@@deriving sexp]

  let candles_of_yojson json =
    match json with
    | `List cs ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | c :: rest ->
          match candle_of_yojson c with
          | Error e -> Error e
          | Ok candle -> parse (candle :: acc) rest
      in
      parse [] cs
    | _ -> Error "Expected list for candles"

  (** User position *)
  type position = {
    coin : string;
    entryPx : string option; [@default None]
    leverage : leverage_info;
    liquidationPx : string option; [@default None]
    marginUsed : string;
    maxLeverage : int option; [@default None]
    positionValue : string;
    returnOnEquity : string;
    szi : string;  (* signed size *)
    unrealizedPnl : string;
  } [@@deriving yojson { strict = false }, sexp]

  and leverage_info = {
    lev_type : string; [@key "type"]
    value : int;
    rawUsd : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Account state *)
  type margin_summary = {
    accountValue : string;
    totalNtlPos : string;
    totalRawUsd : string;
    totalMarginUsed : string;
  } [@@deriving yojson { strict = false }, sexp]

  type asset_position = {
    position : position;
    account_type : string; [@key "type"]
  } [@@deriving yojson { strict = false }, sexp]

  type clearinghouse_state = {
    marginSummary : margin_summary;
    crossMarginSummary : margin_summary;
    crossMaintenanceMarginUsed : string;
    withdrawable : string;
    assetPositions : asset_position list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Open order *)
  type open_order = {
    coin : string;
    limitPx : string;
    oid : int64;
    side : string;
    sz : string;
    timestamp : int64;
  } [@@deriving yojson { strict = false }, sexp]

  type open_orders = open_order list
  [@@deriving sexp]

  let open_orders_of_yojson json =
    match json with
    | `List orders ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | o :: rest ->
          match open_order_of_yojson o with
          | Error e -> Error e
          | Ok order -> parse (order :: acc) rest
      in
      parse [] orders
    | _ -> Error "Expected list for open orders"

  (** User fill *)
  type user_fill = {
    coin : string;
    px : string;
    sz : string;
    side : string;
    time : int64;
    startPosition : string;
    dir : string;
    closedPnl : string;
    hash : string;
    oid : int64;
    crossed : bool;
    fee : string;
    tid : int64;
    feeToken : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type user_fills = user_fill list
  [@@deriving sexp]

  let user_fills_of_yojson json =
    match json with
    | `List fills ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | f :: rest ->
          match user_fill_of_yojson f with
          | Error e -> Error e
          | Ok fill -> parse (fill :: acc) rest
      in
      parse [] fills
    | _ -> Error "Expected list for user fills"
end

(* ============================================================ *)
(* HTTP Client *)
(* ============================================================ *)

let post ~cfg ~body : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  let uri = Uri.of_string (cfg.Cfg.rest_url ^ "/info") in
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "application/json");
    ]
  in
  let body_str = Yojson.Safe.to_string body in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.post
      ~headers
      ~body:(Cohttp_async.Body.of_string body_str)
      uri
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
    match code >= 400 with
    | true -> Error (`Http (code, body_str))
    | false ->
      match Yojson.Safe.from_string body_str with
      | exception _ -> Error (`Json_parse body_str)
      | json -> Ok json

(* ============================================================ *)
(* API Endpoints *)
(* ============================================================ *)

(** Get all mid prices *)
let all_mids cfg : (Types.all_mids, [> Error.t ]) result Deferred.t =
  let body = `Assoc [("type", `String "allMids")] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.all_mids_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok mids -> Ok mids

(** Get L2 order book for a coin *)
let l2_book cfg ~coin : (Types.l2_book, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "l2Book");
    ("coin", `String coin);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.l2_book_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok book -> Ok book

(** Get exchange metadata *)
let meta cfg : (Types.meta, [> Error.t ]) result Deferred.t =
  let body = `Assoc [("type", `String "meta")] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.meta_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok meta -> Ok meta

(** Get metadata with asset contexts (funding, OI, etc.) *)
let meta_and_asset_ctxs cfg : (Types.meta_and_asset_ctxs, [> Error.t ]) result Deferred.t =
  let body = `Assoc [("type", `String "metaAndAssetCtxs")] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.meta_and_asset_ctxs_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok data -> Ok data

(** Get recent trades for a coin *)
let recent_trades cfg ~coin : (Types.recent_trades, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "recentTrades");
    ("coin", `String coin);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.recent_trades_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok trades -> Ok trades

(** Get candle data *)
let candle_snapshot cfg ~coin ~interval ~start_time ~end_time
    : (Types.candles, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "candleSnapshot");
    ("req", `Assoc [
      ("coin", `String coin);
      ("interval", `String interval);
      ("startTime", `Int (Int64.to_int_exn start_time));
      ("endTime", `Int (Int64.to_int_exn end_time));
    ]);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.candles_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok candles -> Ok candles

(** Get user's clearinghouse state (positions, margin, etc.) *)
let clearinghouse_state cfg ~user : (Types.clearinghouse_state, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "clearinghouseState");
    ("user", `String user);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.clearinghouse_state_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok state -> Ok state

(** Get user's open orders *)
let open_orders cfg ~user : (Types.open_orders, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "openOrders");
    ("user", `String user);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.open_orders_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok orders -> Ok orders

(** Get user's fills *)
let user_fills cfg ~user : (Types.user_fills, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("type", `String "userFills");
    ("user", `String user);
  ] in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.user_fills_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok fills -> Ok fills

(** Get user's fills by time range *)
let user_fills_by_time cfg ~user ~start_time ?end_time ()
    : (Types.user_fills, [> Error.t ]) result Deferred.t =
  let body =
    let base = [
      ("type", `String "userFillsByTime");
      ("user", `String user);
      ("startTime", `Int (Int64.to_int_exn start_time));
    ] in
    let with_end = match end_time with
      | None -> base
      | Some t -> ("endTime", `Int (Int64.to_int_exn t)) :: base
    in
    `Assoc with_end
  in
  post ~cfg ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.user_fills_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok fills -> Ok fills

(* ============================================================ *)
(* Trading API - /exchange endpoint *)
(* ============================================================ *)

(** Exchange action response *)
module ExchangeResponse = struct
  type status_variant =
    | Ok_status
    | Error_status of string
  [@@deriving sexp]

  type t = {
    status : status_variant;
    response : (Yojson.Safe.t [@sexp.opaque]) option; [@default None]
  }

  let of_yojson json =
    match json with
    | `Assoc fields ->
      let status_result =
        List.Assoc.find fields ~equal:String.equal "status"
        |> Option.value ~default:(`String "error")
      in
      let status = match status_result with
        | `String "ok" -> Ok_status
        | `String err -> Error_status err
        | _ -> Error_status "Unknown status"
      in
      let response = List.Assoc.find fields ~equal:String.equal "response" in
      Ok { status; response }
    | _ -> Error "Expected object for exchange response"
end

(** POST /exchange for order placement and cancellation *)
let post_exchange ~cfg ~action ~signature ~nonce ?vault_address ()
    : (ExchangeResponse.t, [> Error.t ]) result Deferred.t =
  let uri = Uri.of_string (cfg.Cfg.rest_url ^ "/exchange") in
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "application/json");
    ]
  in

  (* Build request body *)
  let body_fields = [
    ("action", action);
    ("nonce", `Int (Int64.to_int_exn nonce));
    ("signature", `Assoc [
      ("r", `String (String.sub signature ~pos:0 ~len:64));
      ("s", `String (String.sub signature ~pos:64 ~len:64));
      ("v", `Int (Char.to_int (String.get signature 128)));
    ]);
  ] in

  let body_fields_with_vault = match vault_address with
    | None -> body_fields
    | Some addr -> ("vaultAddress", `String addr) :: body_fields
  in

  let body = `Assoc body_fields_with_vault in
  let body_str = Yojson.Safe.to_string body in

  Monitor.try_with (fun () ->
    Cohttp_async.Client.post
      ~headers
      ~body:(Cohttp_async.Body.of_string body_str)
      uri
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
    match code >= 400 with
    | true -> Error (`Http (code, body_str))
    | false ->
      match Yojson.Safe.from_string body_str with
      | exception _ -> Error (`Json_parse body_str)
      | json ->
        match ExchangeResponse.of_yojson json with
        | Error e -> Error (`Json_parse e)
        | Ok resp -> Ok resp

(** Place order using signed request *)
let place_order ~cfg ~private_key ~(orders : Signing.order_request list) ~grouping ()
    : (ExchangeResponse.t, [> Error.t ]) result Deferred.t =
  let nonce = Int64.of_int (Int.of_float (Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_ms)) in

  (* Sign the order *)
  match Signing.sign_place_order ~private_key ~orders ~grouping ~nonce with
  | Error err -> return (Error (`Api_error err))
  | Ok signature ->
    (* Serialize action to JSON for the API request *)
    let action_json = `Assoc [
      ("type", `String "order");
      ("orders", `List (List.map orders ~f:(fun o ->
        `Assoc [
          ("a", `Int o.asset);
          ("b", `Bool o.is_buy);
          ("p", `String o.limit_px);
          ("s", `String o.sz);
          ("r", `Bool o.reduce_only);
          ("t", `Assoc [
            ("limit", `Assoc [
              ("tif", `String o.time_in_force)
            ])
          ]);
          ("c", match o.cloid with Some c -> `String c | None -> `Null);
        ]
      )));
      ("grouping", `String grouping);
    ] in

    post_exchange ~cfg ~action:action_json ~signature ~nonce ()

(** Cancel order using signed request *)
let cancel_order ~cfg ~private_key ~(cancels : Signing.cancel_request list) ()
    : (ExchangeResponse.t, [> Error.t ]) result Deferred.t =
  let nonce = Int64.of_int (Int.of_float (Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_ms)) in

  (* Sign the cancel *)
  match Signing.sign_cancel_order ~private_key ~cancels ~nonce with
  | Error err -> return (Error (`Api_error err))
  | Ok signature ->
    (* Serialize action to JSON *)
    let action_json = `Assoc [
      ("type", `String "cancel");
      ("cancels", `List (List.map cancels ~f:(fun c ->
        `Assoc [
          ("a", `Int c.asset);
          ("o", `Int (Int64.to_int_exn c.oid));
        ]
      )));
    ] in

    post_exchange ~cfg ~action:action_json ~signature ~nonce ()

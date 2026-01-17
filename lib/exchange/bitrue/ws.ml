(** Bitrue WebSocket API *)

open Core
open Async

(* ============================================================ *)
(* Endpoints *)
(* ============================================================ *)

module Endpoint = struct
  let market = "wss://ws.bitrue.com/market/ws"
  let user = "wss://wsapi.bitrue.com"
end

(* ============================================================ *)
(* Stream Types *)
(* ============================================================ *)

module Stream = struct
  type t =
    | Depth of string        (* symbol - market_{symbol}_simple_depth_step0 *)
    | Trade of string        (* symbol - market_{symbol}_trade_ticker *)
    | Kline of { symbol: string; period: string }  (* market_{symbol}_kline_{period} *)
    | Ticker of string       (* symbol - market_{symbol}_ticker *)
  [@@deriving sexp]

  let to_channel = function
    | Depth symbol -> sprintf "market_%s_simple_depth_step0" (String.lowercase symbol)
    | Trade symbol -> sprintf "market_%s_trade_ticker" (String.lowercase symbol)
    | Kline { symbol; period } -> sprintf "market_%s_kline_%s" (String.lowercase symbol) period
    | Ticker symbol -> sprintf "market_%s_ticker" (String.lowercase symbol)

  let to_subscribe_message stream =
    let channel = to_channel stream in
    `Assoc [
      ("event", `String "sub");
      ("params", `Assoc [
        ("channel", `String channel);
        ("cb_id", `String channel);
      ]);
    ]

  let to_unsubscribe_message stream =
    let channel = to_channel stream in
    `Assoc [
      ("event", `String "unsub");
      ("params", `Assoc [
        ("channel", `String channel);
        ("cb_id", `String channel);
      ]);
    ]
end

(* ============================================================ *)
(* Message Types *)
(* ============================================================ *)

module Message = struct
  (** Price level [price, quantity] *)
  type price_level = string * string
  [@@deriving sexp]

  let price_level_of_yojson = function
    | `List [`String price; `String qty] -> Ok (price, qty)
    | _ -> Error "Expected [price, quantity] array"

  let price_level_to_yojson (price, qty) =
    `List [`String price; `String qty]

  let price_level_list_of_yojson json =
    match json with
    | `List levels ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | level :: rest ->
          match price_level_of_yojson level with
          | Error e -> Error e
          | Ok pl -> parse (pl :: acc) rest
      in
      parse [] levels
    | _ -> Error "Expected list of price levels"

  (** Depth update *)
  type depth = {
    channel: string;
    ts: int64;
    tick: depth_tick;
  } [@@deriving yojson { strict = false }, sexp]

  and depth_tick = {
    buys: price_level list;
    asks: price_level list;
  }

  let depth_tick_of_yojson json =
    let open Yojson.Safe.Util in
    try
      let buys_json = member "buys" json in
      let asks_json = member "asks" json in
      match price_level_list_of_yojson buys_json, price_level_list_of_yojson asks_json with
      | Ok buys, Ok asks -> Ok { buys; asks }
      | Error e, _ | _, Error e -> Error e
    with _ -> Error "Failed to parse depth_tick"

  let sexp_of_depth_tick { buys; asks } =
    Sexp.List [
      Sexp.List [Sexp.Atom "buys"; Sexp.List (List.map buys ~f:sexp_of_price_level)];
      Sexp.List [Sexp.Atom "asks"; Sexp.List (List.map asks ~f:sexp_of_price_level)];
    ]

  (** Trade *)
  type trade = {
    id: int64;
    ts: int64;
    price: string;
    vol: string;
    amount: string;
    ds: string;  (* direction: buy/sell *)
  } [@@deriving yojson { strict = false }, sexp]

  type trades = {
    channel: string;
    ts: int64;
    tick: trade;
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker *)
  type ticker_data = {
    amount: string option; [@default None]
    close: string option; [@default None]
    high: string option; [@default None]
    low: string option; [@default None]
    open_: string option; [@default None] [@key "open"]
    rose: string option; [@default None]
    vol: string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type ticker = {
    channel: string;
    ts: int64;
    tick: ticker_data;
  } [@@deriving yojson { strict = false }, sexp]

  (** Kline/Candlestick *)
  type kline_data = {
    id: int64;
    amount: string;
    vol: string;
    count: int;
    open_: string; [@key "open"]
    close: string;
    low: string;
    high: string;
  } [@@deriving yojson { strict = false }, sexp]

  type kline = {
    channel: string;
    ts: int64;
    tick: kline_data;
  } [@@deriving yojson { strict = false }, sexp]

  (** Subscription response *)
  type sub_response = {
    event_rep: string; [@key "event_rep"]
    channel: string option; [@default None]
    cb_id: string option; [@default None]
    status: string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Ping/Pong *)
  type ping = {
    ping: int64;
  } [@@deriving yojson { strict = false }, sexp]

  (** Parsed message type *)
  type t =
    | Depth of depth
    | Trade of trades
    | Ticker of ticker
    | Kline of kline
    | SubResponse of sub_response
    | Ping of int64
    | Pong
    | Unknown of string

  let sexp_of_t = function
    | Depth d -> Sexp.List [Sexp.Atom "Depth"; sexp_of_depth d]
    | Trade t -> Sexp.List [Sexp.Atom "Trade"; sexp_of_trades t]
    | Ticker t -> Sexp.List [Sexp.Atom "Ticker"; sexp_of_ticker t]
    | Kline k -> Sexp.List [Sexp.Atom "Kline"; sexp_of_kline k]
    | SubResponse r -> Sexp.List [Sexp.Atom "SubResponse"; sexp_of_sub_response r]
    | Ping ts -> Sexp.List [Sexp.Atom "Ping"; Sexp.Atom (Int64.to_string ts)]
    | Pong -> Sexp.Atom "Pong"
    | Unknown msg -> Sexp.List [Sexp.Atom "Unknown"; Sexp.Atom msg]
end

(* ============================================================ *)
(* Message Parsing *)
(* ============================================================ *)

let parse_message (msg : string) : Message.t =
  try
    let json = Yojson.Safe.from_string msg in
    (* Check for ping *)
    (match Yojson.Safe.Util.member "ping" json with
     | `Int ts -> Message.Ping (Int64.of_int ts)
     | `Intlit ts -> Message.Ping (Int64.of_string ts)
     | _ ->
       (* Check for pong *)
       match Yojson.Safe.Util.member "pong" json with
       | `Int _ -> Message.Pong
       | `Intlit _ -> Message.Pong
       | _ ->
         (* Check for subscription response *)
         let event_rep = Yojson.Safe.Util.member "event_rep" json
           |> Yojson.Safe.Util.to_string_option
         in
         match event_rep with
         | Some _ ->
           (match Message.sub_response_of_yojson json with
            | Ok resp -> Message.SubResponse resp
            | Error _ -> Message.Unknown msg)
         | None ->
           (* Check channel to determine message type *)
           let channel = Yojson.Safe.Util.member "channel" json
             |> Yojson.Safe.Util.to_string_option
           in
           match channel with
           | Some ch when String.is_substring ch ~substring:"depth" ->
             (match Message.depth_of_yojson json with
              | Ok depth -> Message.Depth depth
              | Error _ -> Message.Unknown msg)
           | Some ch when String.is_substring ch ~substring:"trade_ticker" ->
             (match Message.trades_of_yojson json with
              | Ok trade -> Message.Trade trade
              | Error _ -> Message.Unknown msg)
           | Some ch when String.is_substring ch ~substring:"ticker" ->
             (match Message.ticker_of_yojson json with
              | Ok ticker -> Message.Ticker ticker
              | Error _ -> Message.Unknown msg)
           | Some ch when String.is_substring ch ~substring:"kline" ->
             (match Message.kline_of_yojson json with
              | Ok kline -> Message.Kline kline
              | Error _ -> Message.Unknown msg)
           | _ -> Message.Unknown msg)
  with _ -> Message.Unknown msg

(* NOTE: The old cohttp_async_websocket-based WebSocket client has been removed.
   Use Market_data_curl module instead, which uses websocket_curl and provides:
   - connect: connect to WebSocket
   - subscribe/unsubscribe: manage subscriptions
   - messages: receive parsed messages
   - close: close connection
*)

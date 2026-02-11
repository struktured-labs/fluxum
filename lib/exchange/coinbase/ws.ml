(** Coinbase Advanced Trade WebSocket API *)

open Core
open Async

(* ============================================================ *)
(* Endpoints *)
(* ============================================================ *)

module Endpoint = struct
  let advanced_trade = "wss://advanced-trade-ws.coinbase.com"
  let pro = "wss://ws-feed.pro.coinbase.com"  (* Legacy Coinbase Pro - public data *)
  let exchange = "wss://ws-feed.exchange.coinbase.com"  (* Alternative endpoint *)
end

(* ============================================================ *)
(* Stream Types *)
(* ============================================================ *)

module Stream = struct
  type t =
    | Level2 of string list        (* product_ids *)
    | MarketTrades of string list  (* product_ids *)
    | Ticker of string list        (* product_ids *)
    | TickerBatch of string list   (* product_ids *)
    | Status                       (* All products *)
    | Candles of string list       (* product_ids *)
    | Heartbeats of string list    (* product_ids *)
    | User of string list          (* product_ids - requires authentication *)
  [@@deriving sexp]

  let channel_name = function
    | Level2 _ -> "level2"
    | MarketTrades _ -> "market_trades"
    | Ticker _ -> "ticker"
    | TickerBatch _ -> "ticker_batch"
    | Status -> "status"
    | Candles _ -> "candles"
    | Heartbeats _ -> "heartbeats"
    | User _ -> "user"

  let product_ids = function
    | Level2 ids | MarketTrades ids | Ticker ids
    | TickerBatch ids | Candles ids | Heartbeats ids | User ids -> ids
    | Status -> []

  let to_subscribe_message streams =
    let channel_groups = List.map streams ~f:(fun stream ->
      `Assoc [
        ("name", `String (channel_name stream));
        ("product_ids", `List (List.map (product_ids stream) ~f:(fun id -> `String id)));
      ]
    ) in
    `Assoc [
      ("type", `String "subscribe");
      ("channels", `List channel_groups);
    ]

  let to_subscribe_message_authenticated ~api_key ~signature ~timestamp streams =
    let channel_groups = List.map streams ~f:(fun stream ->
      `Assoc [
        ("name", `String (channel_name stream));
        ("product_ids", `List (List.map (product_ids stream) ~f:(fun id -> `String id)));
      ]
    ) in
    `Assoc [
      ("type", `String "subscribe");
      ("channels", `List channel_groups);
      ("api_key", `String api_key);
      ("signature", `String signature);
      ("timestamp", `String timestamp);
    ]

  let to_unsubscribe_message streams =
    let channel_groups = List.map streams ~f:(fun stream ->
      `Assoc [
        ("name", `String (channel_name stream));
        ("product_ids", `List (List.map (product_ids stream) ~f:(fun id -> `String id)));
      ]
    ) in
    `Assoc [
      ("type", `String "unsubscribe");
      ("channels", `List channel_groups);
    ]
end

(* ============================================================ *)
(* Message Types *)
(* ============================================================ *)

module Message = struct
  (** Level2 price level *)
  type price_level = {
    price_level: string;
    new_quantity: string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Level2 update event *)
  type level2_update = {
    product_id: string;
    updates: price_level list;
    side: string;  (* "bid" or "offer" *)
    event_time: string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Level2 snapshot event *)
  type level2_snapshot = {
    product_id: string;
    updates: price_level list;
    side: string;  (* "bid" or "offer" *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Level2 message *)
  type level2 = {
    channel: string;
    client_id: string;
    timestamp: string;
    sequence_num: int64;
    events: level2_update list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Market trade *)
  type market_trade = {
    trade_id: string;
    product_id: string;
    price: string;
    size: string;
    side: string;
    time: string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Market trades message *)
  type market_trades = {
    channel: string;
    client_id: string;
    timestamp: string;
    sequence_num: int64;
    events: market_trade list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker *)
  type ticker_event = {
    product_id: string;
    price: string option; [@default None]
    volume_24_h: string option; [@default None] [@key "volume_24_h"]
    low_24_h: string option; [@default None] [@key "low_24_h"]
    high_24_h: string option; [@default None] [@key "high_24_h"]
    low_52_w: string option; [@default None] [@key "low_52_w"]
    high_52_w: string option; [@default None] [@key "high_52_w"]
    price_percent_chg_24_h: string option; [@default None] [@key "price_percent_chg_24_h"]
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker message *)
  type ticker = {
    channel: string;
    client_id: string;
    timestamp: string;
    sequence_num: int64;
    events: ticker_event list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Heartbeat *)
  type heartbeat = {
    channel: string;
    client_id: string;
    timestamp: string;
    sequence_num: int64;
    events: Yojson.Safe.t list;
  } [@@deriving yojson { strict = false }]

  let sexp_of_heartbeat { channel; client_id; timestamp; sequence_num; events } =
    Sexp.List [
      Sexp.List [Sexp.Atom "channel"; Sexp.Atom channel];
      Sexp.List [Sexp.Atom "client_id"; Sexp.Atom client_id];
      Sexp.List [Sexp.Atom "timestamp"; Sexp.Atom timestamp];
      Sexp.List [Sexp.Atom "sequence_num"; Sexp.Atom (Int64.to_string sequence_num)];
      Sexp.List [Sexp.Atom "events"; Sexp.Atom (sprintf "%d events" (List.length events))];
    ]

  (** Subscription response *)
  type subscriptions = {
    channel: string;
    product_ids: string list;
  } [@@deriving yojson { strict = false }, sexp]

  type subscribe_response = {
    type_: string; [@key "type"]
    channels: subscriptions list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Order update from user channel - individual order within events array *)
  type order_update = {
    order_id: string;
    client_order_id: string option; [@default None]
    product_id: string;
    side: string;  (* "BUY" or "SELL" *)
    order_type: string;  (* "LIMIT", "MARKET", etc. *)
    status: string;  (* "PENDING", "OPEN", "FILLED", "CANCELLED", "EXPIRED", "FAILED" *)
    time_in_force: string option; [@default None]
    created_time: string option; [@default None]
    completion_percentage: string option; [@default None]
    filled_size: string option; [@default None]
    average_filled_price: string option; [@default None]
    total_fees: string option; [@default None]
    total_value_after_fees: string option; [@default None]
    outstanding_hold_amount: string option; [@default None]
    trigger_status: string option; [@default None]
    order_placement_source: string option; [@default None]
    reject_reason: string option; [@default None]
    settled: bool option; [@default None]
    cancel_message: string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** User channel message - contains order updates *)
  type user_message = {
    channel: string;
    client_id: string;
    timestamp: string;
    sequence_num: int64;
    events: order_update list;
  } [@@deriving yojson { strict = false }, sexp]

  (** Parsed message type *)
  type t =
    | Level2 of level2
    | MarketTrades of market_trades
    | Ticker of ticker
    | Heartbeat of heartbeat
    | User of user_message
    | Subscriptions of subscribe_response
    | Error of string
    | Unknown of string

  let sexp_of_t = function
    | Level2 msg -> Sexp.List [Sexp.Atom "Level2"; sexp_of_level2 msg]
    | MarketTrades msg -> Sexp.List [Sexp.Atom "MarketTrades"; sexp_of_market_trades msg]
    | Ticker msg -> Sexp.List [Sexp.Atom "Ticker"; sexp_of_ticker msg]
    | Heartbeat msg -> Sexp.List [Sexp.Atom "Heartbeat"; sexp_of_heartbeat msg]
    | User msg -> Sexp.List [Sexp.Atom "User"; sexp_of_user_message msg]
    | Subscriptions resp -> Sexp.List [Sexp.Atom "Subscriptions"; sexp_of_subscribe_response resp]
    | Error msg -> Sexp.List [Sexp.Atom "Error"; Sexp.Atom msg]
    | Unknown msg -> Sexp.List [Sexp.Atom "Unknown"; Sexp.Atom msg]
end

(* ============================================================ *)
(* Message Parsing *)
(* ============================================================ *)

let parse_message (msg : string) : Message.t =
  try
    let json = Yojson.Safe.from_string msg in
    let channel =
      Yojson.Safe.Util.member "channel" json
      |> Yojson.Safe.Util.to_string_option
    in
    let type_ =
      Yojson.Safe.Util.member "type" json
      |> Yojson.Safe.Util.to_string_option
    in
    match type_ with
    | Some "subscriptions" ->
      (match Message.subscribe_response_of_yojson json with
       | Ok resp -> Message.Subscriptions resp
       | Error _ -> Message.Unknown msg)
    | Some "error" ->
      let error_msg = Yojson.Safe.Util.member "message" json
        |> Yojson.Safe.Util.to_string_option
        |> Option.value ~default:"Unknown error"
      in
      Message.Error error_msg
    | _ ->
      match channel with
      | Some "l2_data" ->
        (match Message.level2_of_yojson json with
         | Ok level2 -> Message.Level2 level2
         | Error _ -> Message.Unknown msg)
      | Some "market_trades" ->
        (match Message.market_trades_of_yojson json with
         | Ok trades -> Message.MarketTrades trades
         | Error _ -> Message.Unknown msg)
      | Some "ticker" ->
        (match Message.ticker_of_yojson json with
         | Ok ticker -> Message.Ticker ticker
         | Error _ -> Message.Unknown msg)
      | Some "heartbeats" ->
        (match Message.heartbeat_of_yojson json with
         | Ok hb -> Message.Heartbeat hb
         | Error _ -> Message.Unknown msg)
      | Some "user" ->
        (match Message.user_message_of_yojson json with
         | Ok user -> Message.User user
         | Error _ -> Message.Unknown msg)
      | _ -> Message.Unknown msg
  with _ -> Message.Unknown msg

(* NOTE: The old cohttp_async_websocket-based WebSocket client has been removed.
   Use Market_data module instead, which uses websocket_curl and provides:
   - connect: connect to WebSocket
   - subscribe/unsubscribe: manage subscriptions
   - messages: receive parsed messages
   - close: close connection
*)

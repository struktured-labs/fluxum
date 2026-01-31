(** Bybit V5 WebSocket Client

    Implements Bybit's V5 public WebSocket streams for spot trading.

    Supported streams:
    - orderbook.{depth}.{symbol} - Order book updates (snapshots + deltas)
    - publicTrade.{symbol} - Public trades
    - tickers.{symbol} - 24hr ticker updates
    - kline.{interval}.{symbol} - Candlestick/kline updates

    Connection:
    - Production: wss://stream.bybit.com/v5/public/spot
    - Testnet: wss://stream-testnet.bybit.com/v5/public/spot

    Heartbeat: Send {"op":"ping"} every 20 seconds to maintain connection.
*)

open Core
open Async

(* ============================================================ *)
(* Endpoints *)
(* ============================================================ *)

module Endpoint = struct
  let production = "wss://stream.bybit.com/v5/public/spot"
  let testnet = "wss://stream-testnet.bybit.com/v5/public/spot"
end

(* ============================================================ *)
(* Stream Types *)
(* ============================================================ *)

module Stream = struct
  type depth = D1 | D50 | D200 | D500
  [@@deriving sexp]

  let depth_to_int = function
    | D1 -> 1
    | D50 -> 50
    | D200 -> 200
    | D500 -> 500

  type interval =
    | M1 | M3 | M5 | M15 | M30
    | H1 | H2 | H4 | H6 | H12
    | D1 | W1 | Month1
  [@@deriving sexp]

  let interval_to_string = function
    | M1 -> "1" | M3 -> "3" | M5 -> "5" | M15 -> "15" | M30 -> "30"
    | H1 -> "60" | H2 -> "120" | H4 -> "240" | H6 -> "360" | H12 -> "720"
    | D1 -> "D" | W1 -> "W" | Month1 -> "M"

  type t =
    | Orderbook of { depth : depth; symbol : string }
    | PublicTrade of string
    | Tickers of string
    | Kline of { interval : interval; symbol : string }
  [@@deriving sexp]

  (** Convert stream to Bybit subscription topic string *)
  let to_topic = function
    | Orderbook { depth; symbol } ->
      sprintf "orderbook.%d.%s" (depth_to_int depth) symbol
    | PublicTrade symbol ->
      sprintf "publicTrade.%s" symbol
    | Tickers symbol ->
      sprintf "tickers.%s" symbol
    | Kline { interval; symbol } ->
      sprintf "kline.%s.%s" (interval_to_string interval) symbol

  let to_subscribe_message streams =
    let topics = List.map streams ~f:to_topic in
    `Assoc [
      ("op", `String "subscribe");
      ("args", `List (List.map topics ~f:(fun t -> `String t)));
    ]

  let to_unsubscribe_message streams =
    let topics = List.map streams ~f:to_topic in
    `Assoc [
      ("op", `String "unsubscribe");
      ("args", `List (List.map topics ~f:(fun t -> `String t)));
    ]
end

(* ============================================================ *)
(* Message Types *)
(* ============================================================ *)

module Message = struct
  (** Order book level [price, size] *)
  type level = string * string [@@deriving sexp, yojson]

  (** Order book snapshot/delta *)
  type orderbook_data = {
    s : string;  (* symbol *)
    b : level list;  (* bids *)
    a : level list;  (* asks *)
    u : int64;  (* update ID *)
    seq : int64;  (* sequence number *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Public trade *)
  type public_trade = {
    i : string;  (* trade ID *)
    timestamp : int64; [@key "T"]  (* timestamp *)
    p : string;  (* price *)
    v : string;  (* size/volume *)
    side : string; [@key "S"]  (* side: "Buy" or "Sell" *)
    s : string;  (* symbol *)
    is_block_trade : bool; [@key "BT"]  (* is block trade *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker data *)
  type ticker_data = {
    symbol : string;
    lastPrice : string;
    highPrice24h : string;
    lowPrice24h : string;
    prevPrice24h : string;
    volume24h : string;
    turnover24h : string;
    price24hPcnt : string;  (* 24h price change percentage *)
    usdIndexPrice : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Kline/candlestick data *)
  type kline_data = {
    start : int64;  (* start time *)
    close_time : int64; [@key "end"]  (* end time *)
    interval : string;
    open_ : string; [@key "open"]
    close : string;
    high : string;
    low : string;
    volume : string;
    turnover : string;
    confirm : bool;  (* true if kline is closed *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Subscription response *)
  type subscription_response = {
    success : bool;
    ret_msg : string;
    conn_id : string;
    op : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Message wrapper *)
  type 'a wrapper = {
    topic : string;
    ts : int64;  (* server timestamp *)
    type_ : string; [@key "type"]  (* "snapshot" or "delta" *)
    data : 'a;
  } [@@deriving sexp]

  (** Parsed message type *)
  type t =
    | Orderbook of { topic : string; type_ : string; ts : int64; data : orderbook_data }
    | PublicTrade of { topic : string; ts : int64; data : public_trade list }
    | Tickers of { topic : string; ts : int64; data : ticker_data }
    | Kline of { topic : string; ts : int64; data : kline_data list }
    | SubscriptionResponse of subscription_response
    | Pong of { ret_msg : string; conn_id : string; op : string }
    | Error of string
    | Unknown of string
  [@@deriving sexp]
end

(* ============================================================ *)
(* Message Parsing *)
(* ============================================================ *)

let parse_message (msg : string) : Message.t =
  try
    let json = Yojson.Safe.from_string msg in

    (* Check for subscription response (has "success" field) *)
    let success_opt =
      try Some (Yojson.Safe.Util.member "success" json |> Yojson.Safe.Util.to_bool)
      with _ -> None
    in

    (* Check for pong response (has "ret_msg" and "op": "pong") *)
    let op_opt =
      try Some (Yojson.Safe.Util.member "op" json |> Yojson.Safe.Util.to_string)
      with _ -> None
    in

    match success_opt, op_opt with
    | Some _, _ ->
      (* Subscription response *)
      (match Message.subscription_response_of_yojson json with
       | Ok resp -> Message.SubscriptionResponse resp
       | Error _ -> Message.Unknown msg)

    | _, Some "pong" ->
      (* Pong response *)
      let ret_msg =
        try Yojson.Safe.Util.member "ret_msg" json |> Yojson.Safe.Util.to_string
        with _ -> "pong"
      in
      let conn_id =
        try Yojson.Safe.Util.member "conn_id" json |> Yojson.Safe.Util.to_string
        with _ -> ""
      in
      Message.Pong { ret_msg; conn_id; op = "pong" }

    | None, _ ->
      (* Regular data message - has "topic" field *)
      let topic = Yojson.Safe.Util.member "topic" json |> Yojson.Safe.Util.to_string in
      let type_ = Yojson.Safe.Util.member "type" json |> Yojson.Safe.Util.to_string in
      let ts =
        match Yojson.Safe.Util.member "ts" json with
        | `Int i -> Int64.of_int i
        | `Intlit s -> Int64.of_string s
        | _ -> 0L
      in
      let data = Yojson.Safe.Util.member "data" json in

      (* Determine message type based on topic prefix *)
      if String.is_prefix topic ~prefix:"orderbook." then
        (match Message.orderbook_data_of_yojson data with
         | Ok book_data -> Message.Orderbook { topic; type_; ts; data = book_data }
         | Error _ -> Message.Unknown msg)

      else if String.is_prefix topic ~prefix:"publicTrade." then
        (match data with
         | `List trades ->
           let parsed = List.filter_map trades ~f:(fun t ->
             match Message.public_trade_of_yojson t with
             | Ok trade -> Some trade
             | Error _ -> None)
           in
           Message.PublicTrade { topic; ts; data = parsed }
         | _ -> Message.Unknown msg)

      else if String.is_prefix topic ~prefix:"tickers." then
        (match Message.ticker_data_of_yojson data with
         | Ok ticker -> Message.Tickers { topic; ts; data = ticker }
         | Error _ -> Message.Unknown msg)

      else if String.is_prefix topic ~prefix:"kline." then
        (match data with
         | `List klines ->
           let parsed = List.filter_map klines ~f:(fun k ->
             match Message.kline_data_of_yojson k with
             | Ok kline -> Some kline
             | Error _ -> None)
           in
           Message.Kline { topic; ts; data = parsed }
         | _ -> Message.Unknown msg)

      else
        Message.Unknown msg

  with e ->
    Message.Error (sprintf "Parse error: %s" (Exn.to_string e))

(* ============================================================ *)
(* WebSocket Client *)
(* ============================================================ *)

type t = {
  uri : Uri.t;
  ws : Websocket_curl.t;
  streams : Stream.t list ref;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
  mutable active : bool;
}

let connect ?(url = Endpoint.production) ?(streams = []) () : t Deferred.Or_error.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url () in

  match ws_result with
  | Error err ->
    Deferred.Or_error.error_string (Error.to_string_hum err)
  | Ok ws ->
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      ws;
      streams = ref streams;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages *)
    don't_wait_for (
      let rec receive_loop () =
        match t.active with
        | false -> return ()
        | true ->
          let%bind msg_opt = Websocket_curl.receive ws in
          match msg_opt with
          | None ->
            (* Connection closed *)
            t.active <- false;
            Pipe.close t.message_writer;
            return ()
          | Some payload ->
            let%bind () = Pipe.write t.message_writer payload in
            receive_loop ()
      in
      receive_loop ()
    );

    (* Start background task to send periodic pings (every 20 seconds per Bybit spec) *)
    don't_wait_for (
      let rec ping_loop () =
        match t.active with
        | false -> return ()
        | true ->
          let%bind _result =
            try_with (fun () -> Websocket_curl.send ws "{\"op\":\"ping\"}")
          in
          let%bind () = Clock.after (Time_float.Span.of_sec 20.0) in
          ping_loop ()
      in
      ping_loop ()
    );

    (* Subscribe to requested streams *)
    let%bind () =
      match streams with
      | [] -> return ()
      | _ ->
        let msg = Stream.to_subscribe_message streams in
        let msg_str = Yojson.Safe.to_string msg in
        Websocket_curl.send ws msg_str
    in

    Deferred.Or_error.return t

let messages t = t.message_pipe

let subscribe t streams =
  t.streams := List.append !(t.streams) streams;
  match t.active with
  | false -> Deferred.Or_error.error_string "WebSocket connection is closed"
  | true ->
    let msg = Stream.to_subscribe_message streams in
    let msg_str = Yojson.Safe.to_string msg in
    let%bind () = Websocket_curl.send t.ws msg_str in
    Deferred.Or_error.return ()

let unsubscribe t streams =
  t.streams := List.filter !(t.streams) ~f:(fun s ->
    not (List.mem streams s ~equal:Poly.equal));
  match t.active with
  | false -> Deferred.Or_error.error_string "WebSocket connection is closed"
  | true ->
    let msg = Stream.to_unsubscribe_message streams in
    let msg_str = Yojson.Safe.to_string msg in
    let%bind () = Websocket_curl.send t.ws msg_str in
    Deferred.Or_error.return ()

let close t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

let is_active t = t.active

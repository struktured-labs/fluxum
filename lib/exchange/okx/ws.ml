(** OKX V5 WebSocket Client

    Implements OKX's V5 public WebSocket streams for spot and derivatives trading.

    Supported streams:
    - books - Order book updates (snapshot + incremental)
    - trades - Public trades
    - tickers - Ticker updates
    - candle{interval} - Candlestick/kline updates

    Connection:
    - Production Public: wss://ws.okx.com:8443/ws/v5/public
    - Production Private: wss://ws.okx.com:8443/ws/v5/private
    - AWS Public: wss://wsaws.okx.com:8443/ws/v5/public

    Heartbeat: Automatic ping/pong handling (connection timeout after 30s)
*)

open Core
open Async

(* ============================================================ *)
(* Endpoints *)
(* ============================================================ *)

module Endpoint = struct
  let public_production = "wss://ws.okx.com:8443/ws/v5/public"
  let private_production = "wss://ws.okx.com:8443/ws/v5/private"
  let public_aws = "wss://wsaws.okx.com:8443/ws/v5/public"
end

(* ============================================================ *)
(* Stream Types *)
(* ============================================================ *)

module Stream = struct
  type channel =
    | Books      (* Order book - books *)
    | Trades     (* Public trades - trades *)
    | Tickers    (* Ticker data - tickers *)
    | Candle1m | Candle3m | Candle5m | Candle15m | Candle30m
    | Candle1H | Candle2H | Candle4H
    | Candle1D | Candle1W | Candle1M
  [@@deriving sexp]

  let channel_to_string = function
    | Books -> "books"
    | Trades -> "trades"
    | Tickers -> "tickers"
    | Candle1m -> "candle1m"
    | Candle3m -> "candle3m"
    | Candle5m -> "candle5m"
    | Candle15m -> "candle15m"
    | Candle30m -> "candle30m"
    | Candle1H -> "candle1H"
    | Candle2H -> "candle2H"
    | Candle4H -> "candle4H"
    | Candle1D -> "candle1D"
    | Candle1W -> "candle1W"
    | Candle1M -> "candle1M"

  type t =
    { channel : channel
    ; instId : string
    }
  [@@deriving sexp]

  (** Convert stream to OKX subscription arg *)
  let to_arg t =
    `Assoc [
      ("channel", `String (channel_to_string t.channel));
      ("instId", `String t.instId);
    ]

  let to_subscribe_message streams =
    `Assoc [
      ("op", `String "subscribe");
      ("args", `List (List.map streams ~f:to_arg));
    ]

  let to_unsubscribe_message streams =
    `Assoc [
      ("op", `String "unsubscribe");
      ("args", `List (List.map streams ~f:to_arg));
    ]
end

(* ============================================================ *)
(* Message Types *)
(* ============================================================ *)

module Message = struct
  (** Order book level [price, size, liquidated_orders, num_orders] *)
  type level = string * string * string * string [@@deriving sexp]

  let level_of_yojson = function
    | `List [ `String price; `String size; `String liq; `String orders ] ->
      Ok (price, size, liq, orders)
    | json -> Error (sprintf "Invalid level: %s" (Yojson.Safe.to_string json))

  let level_to_yojson (price, size, liq, orders) =
    `List [ `String price; `String size; `String liq; `String orders ]

  (** Order book snapshot/delta *)
  type book_data = {
    asks : level list;
    bids : level list;
    ts : string;
    checksum : int option [@sexp.option];
  } [@@deriving yojson { strict = false }, sexp]

  (** Public trade *)
  type trade_data = {
    instId : string;
    tradeId : string;
    px : string;
    sz : string;
    side : string;
    ts : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Ticker data *)
  type ticker_data = {
    instId : string;
    last : string;
    bidPx : string;
    askPx : string;
    high24h : string;
    low24h : string;
    volCcy24h : string;
    vol24h : string;
    ts : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Candlestick data [timestamp, open, high, low, close, volume, volumeCcy, volumeCcyQuote, confirm] *)
  type candle = string * string * string * string * string * string * string * string * string
  [@@deriving sexp]

  let candle_of_yojson = function
    | `List [ `String ts; `String o; `String h; `String l; `String c; `String vol; `String volCcy; `String volCcyQuote; `String confirm ] ->
      Ok (ts, o, h, l, c, vol, volCcy, volCcyQuote, confirm)
    | json -> Error (sprintf "Invalid candle: %s" (Yojson.Safe.to_string json))

  let candle_to_yojson (ts, o, h, l, c, vol, volCcy, volCcyQuote, confirm) =
    `List [ `String ts; `String o; `String h; `String l; `String c; `String vol; `String volCcy; `String volCcyQuote; `String confirm ]

  (** Subscription response *)
  type subscription_event = {
    event : string;
    arg : string option [@sexp.option];
    connId : string option [@sexp.option];
  } [@@deriving sexp]

  let subscription_event_of_yojson json =
    let open Result.Let_syntax in
    let%bind event = Yojson.Safe.Util.member "event" json |> Yojson.Safe.Util.to_string |> Result.return in
    let arg =
      try Some (Yojson.Safe.Util.member "arg" json |> Yojson.Safe.to_string)
      with _ -> None
    in
    let connId =
      try Some (Yojson.Safe.Util.member "connId" json |> Yojson.Safe.Util.to_string)
      with _ -> None
    in
    Ok { event; arg; connId }

  (** Parsed message type *)
  type t =
    | Books of book_data list
    | Trades of trade_data list
    | Tickers of ticker_data list
    | Candles of candle list
    | SubscriptionEvent of subscription_event
    | Pong
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

    (* Check for event field (subscription/error responses) *)
    let event_opt =
      try Some (Yojson.Safe.Util.member "event" json |> Yojson.Safe.Util.to_string)
      with _ -> None
    in

    match event_opt with
    | Some "subscribe" | Some "unsubscribe" | Some "error" ->
      (* Subscription event *)
      (match Message.subscription_event_of_yojson json with
       | Ok event -> Message.SubscriptionEvent event
       | Error _ -> Message.Unknown msg)

    | Some "pong" ->
      Message.Pong

    | None ->
      (* Regular data message - has "arg" and "data" fields *)
      (match json with
       | `Assoc _ ->
         let arg = Yojson.Safe.Util.member "arg" json in
         let data = Yojson.Safe.Util.member "data" json in
         let channel =
           try Yojson.Safe.Util.member "channel" arg |> Yojson.Safe.Util.to_string
           with _ -> ""
         in

         (* Determine message type based on channel *)
         if String.equal channel "books" then
           (match data with
            | `List items ->
              let parsed = List.filter_map items ~f:(fun item ->
                match Message.book_data_of_yojson item with
                | Ok book -> Some book
                | Error _ -> None)
              in
              Message.Books parsed
            | _ -> Message.Unknown msg)

         else if String.equal channel "trades" then
           (match data with
            | `List items ->
              let parsed = List.filter_map items ~f:(fun item ->
                match Message.trade_data_of_yojson item with
                | Ok trade -> Some trade
                | Error _ -> None)
              in
              Message.Trades parsed
            | _ -> Message.Unknown msg)

         else if String.equal channel "tickers" then
           (match data with
            | `List items ->
              let parsed = List.filter_map items ~f:(fun item ->
                match Message.ticker_data_of_yojson item with
                | Ok ticker -> Some ticker
                | Error _ -> None)
              in
              Message.Tickers parsed
            | _ -> Message.Unknown msg)

         else if String.is_prefix channel ~prefix:"candle" then
           (match data with
            | `List items ->
              let parsed = List.filter_map items ~f:(fun item ->
                match Message.candle_of_yojson item with
                | Ok candle -> Some candle
                | Error _ -> None)
              in
              Message.Candles parsed
            | _ -> Message.Unknown msg)

         else
           Message.Unknown msg

       | _ -> Message.Unknown msg)

    | Some _ -> Message.Unknown msg

  with e ->
    Message.Error (sprintf "Parse error: %s" (Exn.to_string e))

(* ============================================================ *)
(* Authentication *)
(* ============================================================ *)

module Auth = struct
  (** HMAC-SHA256 for WS auth (same algo as Rest.Signature but with Unix epoch timestamp) *)
  let hmac_sha256 ~secret ~message =
    let block_size = 64 in
    let secret_key =
      match String.length secret > block_size with
      | true -> Digestif.SHA256.digest_string secret |> Digestif.SHA256.to_raw_string
      | false -> secret ^ String.make (block_size - String.length secret) '\x00'
    in
    let ipad = String.init block_size ~f:(fun i ->
      Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x36))
    in
    let opad = String.init block_size ~f:(fun i ->
      Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x5c))
    in
    let inner_hash = Digestif.SHA256.digest_string (ipad ^ message) |> Digestif.SHA256.to_raw_string in
    Digestif.SHA256.digest_string (opad ^ inner_hash) |> Digestif.SHA256.to_raw_string

  (** Generate OKX WebSocket login message.
      OKX private WS uses HMAC-SHA256: sign(timestamp + "GET" + "/users/self/verify")
      where timestamp is Unix epoch seconds as a string. *)
  let login_message ~api_key ~api_secret ~passphrase : string =
    let timestamp = sprintf "%d" (Float.to_int (Unix.gettimeofday ())) in
    let message = timestamp ^ "GET" ^ "/users/self/verify" in
    let sign_raw = hmac_sha256 ~secret:api_secret ~message in
    let sign_b64 = Base64.encode_exn sign_raw in
    Yojson.Safe.to_string
      (`Assoc [
        ("op", `String "login");
        ("args", `List [
          `Assoc [
            ("apiKey", `String api_key);
            ("passphrase", `String passphrase);
            ("timestamp", `String timestamp);
            ("sign", `String sign_b64);
          ]
        ]);
      ])
end

(* ============================================================ *)
(* Private Channel Types *)
(* ============================================================ *)

module Private = struct
  (** Order update from private orders channel *)
  type order_data = {
    instId : string;
    ordId : string;
    clOrdId : string;
    px : string;
    sz : string;
    side : string;
    ordType : string;
    state : string;
    fillPx : string;
    fillSz : string;
    avgPx : string;
    fee : string;
    feeCcy : string;
    uTime : string;
    cTime : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Account balance update from private account channel *)
  type account_data = {
    ccy : string;
    availBal : string;
    cashBal : string;
    frozenBal : string;
  } [@@deriving yojson { strict = false }, sexp]

  type message =
    | Orders of order_data list
    | Account of account_data list
    | LoginSuccess
    | LoginError of string
    | Unknown of string
  [@@deriving sexp]

  (** Subscribe to private channels after login *)
  let subscribe_orders ?(inst_type = "SPOT") () : string =
    Yojson.Safe.to_string
      (`Assoc [
        ("op", `String "subscribe");
        ("args", `List [
          `Assoc [
            ("channel", `String "orders");
            ("instType", `String inst_type);
          ]
        ]);
      ])

  let subscribe_account () : string =
    Yojson.Safe.to_string
      (`Assoc [
        ("op", `String "subscribe");
        ("args", `List [
          `Assoc [
            ("channel", `String "account");
          ]
        ]);
      ])

  (** Parse a private channel message *)
  let parse_message (msg : string) : message =
    try
      let json = Yojson.Safe.from_string msg in
      (* Check for login response *)
      let event_opt =
        try Some (Yojson.Safe.Util.member "event" json |> Yojson.Safe.Util.to_string)
        with _ -> None
      in
      match event_opt with
      | Some "login" ->
        let code =
          try Yojson.Safe.Util.member "code" json |> Yojson.Safe.Util.to_string
          with _ -> ""
        in
        (match String.equal code "0" with
         | true -> LoginSuccess
         | false ->
           let err_msg =
             try Yojson.Safe.Util.member "msg" json |> Yojson.Safe.Util.to_string
             with _ -> "unknown error"
           in
           LoginError err_msg)
      | Some "error" ->
        let err_msg =
          try Yojson.Safe.Util.member "msg" json |> Yojson.Safe.Util.to_string
          with _ -> "unknown error"
        in
        LoginError err_msg
      | _ ->
        (* Data message with arg and data fields *)
        let arg = Yojson.Safe.Util.member "arg" json in
        let data = Yojson.Safe.Util.member "data" json in
        let channel =
          try Yojson.Safe.Util.member "channel" arg |> Yojson.Safe.Util.to_string
          with _ -> ""
        in
        match channel with
        | "orders" ->
          (match data with
           | `List items ->
             let orders = List.filter_map items ~f:(fun item ->
               match order_data_of_yojson item with
               | Ok order -> Some order
               | Error _ -> None)
             in
             Orders orders
           | _ -> Unknown msg)
        | "account" ->
          (match data with
           | `List items ->
             let accounts = List.filter_map items ~f:(fun item ->
               (* Account data has nested details *)
               let details =
                 try Yojson.Safe.Util.member "details" item |> function
                   | `List detail_items ->
                     List.filter_map detail_items ~f:(fun d ->
                       match account_data_of_yojson d with
                       | Ok acct -> Some acct
                       | Error _ -> None)
                   | _ -> []
                 with _ -> []
               in
               Some details)
             in
             Account (List.concat accounts)
           | _ -> Unknown msg)
        | _ -> Unknown msg
    with e ->
      Unknown (sprintf "Parse error: %s" (Exn.to_string e))
end

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

let connect ?(url = Endpoint.public_production) ?(streams = []) () : t Deferred.Or_error.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

  (* Connect using websocket_curl *)
  let%bind ws_result = Websocket_curl.connect ~url in

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

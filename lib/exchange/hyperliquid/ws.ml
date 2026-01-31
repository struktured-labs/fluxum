(** Hyperliquid WebSocket API *)

open Core
open Async

(* ============================================================ *)
(* Endpoints *)
(* ============================================================ *)

module Endpoint = struct
  let mainnet = "wss://api.hyperliquid.xyz/ws"
  let testnet = "wss://api.hyperliquid-testnet.xyz/ws"
end

(* ============================================================ *)
(* Stream Types *)
(* ============================================================ *)

module Stream = struct
  type interval =
    | M1 | M3 | M5 | M15 | M30
    | H1 | H2 | H4 | H8 | H12
    | D1 | D3 | W1 | Month1
  [@@deriving sexp]

  let interval_to_string = function
    | M1 -> "1m" | M3 -> "3m" | M5 -> "5m" | M15 -> "15m" | M30 -> "30m"
    | H1 -> "1h" | H2 -> "2h" | H4 -> "4h" | H8 -> "8h" | H12 -> "12h"
    | D1 -> "1d" | D3 -> "3d" | W1 -> "1w" | Month1 -> "1M"

  type t =
    | AllMids
    | L2Book of { coin : string; n_sig_figs : int option; mantissa : int option }
    | Trades of string
    | Candle of { coin : string; interval : interval }
    | Bbo of string
    | OrderUpdates of string     (* user address *)
    | UserFills of string        (* user address *)
    | UserFundings of string     (* user address *)
    | OpenOrders of string       (* user address *)
    | ClearinghouseState of string (* user address *)
    | ActiveAssetCtx of string   (* coin *)
  [@@deriving sexp]

  let to_subscription_json = function
    | AllMids ->
      `Assoc [("type", `String "allMids")]
    | L2Book { coin; n_sig_figs; mantissa } ->
      let base = [("type", `String "l2Book"); ("coin", `String coin)] in
      let with_sig = match n_sig_figs with
        | None -> base
        | Some n -> ("nSigFigs", `Int n) :: base
      in
      let with_mantissa = match mantissa with
        | None -> with_sig
        | Some m -> ("mantissa", `Int m) :: with_sig
      in
      `Assoc with_mantissa
    | Trades coin ->
      `Assoc [("type", `String "trades"); ("coin", `String coin)]
    | Candle { coin; interval } ->
      `Assoc [
        ("type", `String "candle");
        ("coin", `String coin);
        ("interval", `String (interval_to_string interval));
      ]
    | Bbo coin ->
      `Assoc [("type", `String "bbo"); ("coin", `String coin)]
    | OrderUpdates user ->
      `Assoc [("type", `String "orderUpdates"); ("user", `String user)]
    | UserFills user ->
      `Assoc [("type", `String "userFills"); ("user", `String user)]
    | UserFundings user ->
      `Assoc [("type", `String "userFundings"); ("user", `String user)]
    | OpenOrders user ->
      `Assoc [("type", `String "openOrders"); ("user", `String user)]
    | ClearinghouseState user ->
      `Assoc [("type", `String "clearinghouseState"); ("user", `String user)]
    | ActiveAssetCtx coin ->
      `Assoc [("type", `String "activeAssetCtx"); ("coin", `String coin)]

  let to_subscribe_message t =
    `Assoc [
      ("method", `String "subscribe");
      ("subscription", to_subscription_json t);
    ]

  let to_unsubscribe_message t =
    `Assoc [
      ("method", `String "unsubscribe");
      ("subscription", to_subscription_json t);
    ]
end

(* ============================================================ *)
(* Message Types *)
(* ============================================================ *)

module Message = struct
  (** Trade update *)
  type trade = {
    coin : string;
    side : string;
    px : string;
    sz : string;
    time : int64;
    hash : string;
    tid : int64 option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** L2 book level *)
  type book_level = {
    px : string;
    sz : string;
    n : int;
  } [@@deriving yojson { strict = false }, sexp]

  (** L2 book snapshot/update *)
  type l2_book = {
    coin : string;
    levels : book_level list list;  (* [bids, asks] *)
    time : int64;
  } [@@deriving yojson { strict = false }, sexp]

  (** All mids update *)
  type all_mids = (string * string) list
  [@@deriving sexp]

  let all_mids_of_yojson json =
    match json with
    | `Assoc pairs ->
      Ok (List.map pairs ~f:(fun (coin, price) ->
        (coin, Yojson.Safe.Util.to_string price)))
    | _ -> Error "Expected object for allMids"

  (** Best bid/offer *)
  type bbo = {
    coin : string;
    bid : string option; [@default None]
    bidSz : string option; [@default None]
    ask : string option; [@default None]
    askSz : string option; [@default None]
    time : int64;
  } [@@deriving yojson { strict = false }, sexp]

  (** Candle update *)
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

  (** Order update *)
  type order_update = {
    coin : string;
    oid : int64;
    side : string;
    limitPx : string;
    sz : string;
    origSz : string;
    status : string;  (* open, filled, canceled, etc. *)
    statusTimestamp : int64;
  } [@@deriving yojson { strict = false }, sexp]

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
    isSnapshot : bool option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Subscription response *)
  type subscription_response = {
    method_ : string; [@key "method"]
    subscription : Yojson.Safe.t;
  } [@@deriving yojson { strict = false }]

  let sexp_of_subscription_response { method_; subscription } =
    Sexp.List [
      Sexp.List [Sexp.Atom "method_"; Sexp.Atom method_];
      Sexp.List [Sexp.Atom "subscription"; Sexp.Atom (Yojson.Safe.to_string subscription)];
    ]

  (** Channel data wrapper *)
  type 'a channel_data = {
    channel : string;
    data : 'a;
  }

  (** Parsed message type *)
  type t =
    | SubscriptionResponse of subscription_response
    | AllMids of all_mids
    | L2Book of l2_book
    | Trades of trade list
    | Bbo of bbo
    | Candle of candle
    | OrderUpdates of order_update list
    | UserFills of user_fill list
    | Pong
    | Error of string
    | Unknown of string

  let sexp_of_t = function
    | SubscriptionResponse resp -> Sexp.List [Sexp.Atom "SubscriptionResponse"; sexp_of_subscription_response resp]
    | AllMids mids -> Sexp.List [Sexp.Atom "AllMids"; sexp_of_all_mids mids]
    | L2Book book -> Sexp.List [Sexp.Atom "L2Book"; sexp_of_l2_book book]
    | Trades trades -> Sexp.List [Sexp.Atom "Trades"; Sexp.List (List.map trades ~f:sexp_of_trade)]
    | Bbo bbo -> Sexp.List [Sexp.Atom "Bbo"; sexp_of_bbo bbo]
    | Candle c -> Sexp.List [Sexp.Atom "Candle"; sexp_of_candle c]
    | OrderUpdates updates -> Sexp.List [Sexp.Atom "OrderUpdates"; Sexp.List (List.map updates ~f:sexp_of_order_update)]
    | UserFills fills -> Sexp.List [Sexp.Atom "UserFills"; Sexp.List (List.map fills ~f:sexp_of_user_fill)]
    | Pong -> Sexp.Atom "Pong"
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
    match channel with
    | Some "subscriptionResponse" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match Message.subscription_response_of_yojson data with
       | Ok resp -> Message.SubscriptionResponse resp
       | Error _ -> Message.Unknown msg)

    | Some "allMids" ->
      let data = Yojson.Safe.Util.member "data" json in
      let mids = Yojson.Safe.Util.member "mids" data in
      (match Message.all_mids_of_yojson mids with
       | Ok mids -> Message.AllMids mids
       | Error _ -> Message.Unknown msg)

    | Some "l2Book" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match Message.l2_book_of_yojson data with
       | Ok book -> Message.L2Book book
       | Error _ -> Message.Unknown msg)

    | Some "trades" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match data with
       | `List trades ->
         let parsed = List.filter_map trades ~f:(fun t ->
           match Message.trade_of_yojson t with
           | Ok trade -> Some trade
           | Error _ -> None)
         in
         Message.Trades parsed
       | _ -> Message.Unknown msg)

    | Some "bbo" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match Message.bbo_of_yojson data with
       | Ok bbo -> Message.Bbo bbo
       | Error _ -> Message.Unknown msg)

    | Some "candle" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match Message.candle_of_yojson data with
       | Ok candle -> Message.Candle candle
       | Error _ -> Message.Unknown msg)

    | Some "orderUpdates" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match data with
       | `List updates ->
         let parsed = List.filter_map updates ~f:(fun u ->
           match Message.order_update_of_yojson u with
           | Ok update -> Some update
           | Error _ -> None)
         in
         Message.OrderUpdates parsed
       | _ -> Message.Unknown msg)

    | Some "userFills" ->
      let data = Yojson.Safe.Util.member "data" json in
      (match data with
       | `List fills ->
         let parsed = List.filter_map fills ~f:(fun f ->
           match Message.user_fill_of_yojson f with
           | Ok fill -> Some fill
           | Error _ -> None)
         in
         Message.UserFills parsed
       | _ -> Message.Unknown msg)

    | Some "pong" -> Message.Pong

    | Some "error" ->
      let data = Yojson.Safe.Util.member "data" json in
      Message.Error (Yojson.Safe.to_string data)

    | _ -> Message.Unknown msg
  with _ -> Message.Unknown msg

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

let connect ?(url = Endpoint.mainnet) ?(streams = []) () : t Deferred.Or_error.t =
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

    (* Start background task to send periodic pings (every 2.5 seconds) *)
    don't_wait_for (
      let rec ping_loop () =
        match t.active with
        | false -> return ()
        | true ->
          let%bind _result =
            try_with (fun () -> Websocket_curl.send ws "{\"method\":\"ping\"}")
          in
          let%bind () = Clock.after (Time_float.Span.of_sec 2.5) in
          ping_loop ()
      in
      ping_loop ()
    );

    (* Subscribe to requested streams *)
    let%bind () =
      Deferred.List.iter ~how:`Sequential streams ~f:(fun stream ->
        let msg = Stream.to_subscribe_message stream in
        let msg_str = Yojson.Safe.to_string msg in
        Websocket_curl.send ws msg_str
      )
    in

    Deferred.Or_error.return t

let messages t = t.message_pipe

let subscribe t stream =
  t.streams := stream :: !(t.streams);
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = Stream.to_subscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

let unsubscribe t stream =
  t.streams := List.filter !(t.streams) ~f:(fun s -> not (phys_equal s stream));
  match t.active with
  | false -> Deferred.unit
  | true ->
    let msg = Stream.to_unsubscribe_message stream in
    let msg_str = Yojson.Safe.to_string msg in
    Websocket_curl.send t.ws msg_str

let ping t =
  match t.active with
  | false -> Deferred.unit
  | true -> Websocket_curl.send t.ws "{\"method\":\"ping\"}"

let close t =
  t.active <- false;
  Pipe.close t.message_writer;
  Websocket_curl.close t.ws

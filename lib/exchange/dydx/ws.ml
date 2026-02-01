open Core
open Async

(** WebSocket message types *)

type subscription_request = {
  type_ : string [@key "type"];
  channel : string;
  id : string option [@default None];
  batched : bool [@default true];
} [@@deriving sexp, yojson { strict = false }]

type subscribed_response = {
  type_ : string [@key "type"];
  connection_id : string;
  message_id : int;
  channel : string;
  id : string option [@default None];
} [@@deriving sexp, yojson { strict = false }]

type channel_data = {
  type_ : string [@key "type"];
  connection_id : string;
  message_id : int;
  id : string option [@default None];
  channel : string;
  version : string option [@default None];
} [@@deriving sexp, yojson { strict = false }]

(** Orderbook message types *)

type price_level = {
  price : string;
  size : string;
} [@@deriving sexp, yojson { strict = false }]

type orderbook_contents = {
  bids : price_level list [@default []];
  asks : price_level list [@default []];
} [@@deriving sexp, yojson { strict = false }]

(** Trades message types *)

type trade = {
  id : string;
  side : string;
  size : string;
  price : string;
  createdAt : string;
} [@@deriving sexp, yojson { strict = false }]

type trades_contents = {
  trades : trade list;
} [@@deriving sexp, yojson { strict = false }]

(** Generic WebSocket connection *)

let connect ~url =
  match%bind Websocket_curl.connect ~url () with
  | Ok conn -> return conn
  | Error err -> raise_s [%message "dYdX WS connect failed" (err : Error.t)]

let subscribe ~conn ~channel ~id ?(batched=true) () =
  let req = {
    type_ = "subscribe";
    channel;
    id = Some id;
    batched;
  } in
  let json = subscription_request_to_yojson req in
  let msg = Yojson.Safe.to_string json in
  Websocket_curl.send conn msg

let unsubscribe ~conn ~channel ~id () =
  let req = {
    type_ = "unsubscribe";
    channel;
    id = Some id;
    batched = false;
  } in
  let json = subscription_request_to_yojson req in
  let msg = Yojson.Safe.to_string json in
  Websocket_curl.send conn msg

(** Channel-specific subscriptions *)

module Orderbook = struct
  let subscribe ~conn ~market ?(batched=true) () =
    subscribe ~conn ~channel:"v4_orderbook" ~id:market ~batched ()

  let unsubscribe ~conn ~market () =
    unsubscribe ~conn ~channel:"v4_orderbook" ~id:market ()

  let parse_message json =
    try
      match channel_data_of_yojson json with
      | Ok data ->
          let contents_json = Yojson.Safe.Util.member "contents" json in
          (match orderbook_contents_of_yojson contents_json with
           | Ok contents -> Ok (data, contents)
           | Error msg -> Error (`Json_parse_error msg))
      | Error msg -> Error (`Json_parse_error msg)
    with
    | Yojson.Safe.Util.Type_error (msg, _) -> Error (`Json_parse_error msg)
end

module Trades = struct
  let subscribe ~conn ~market () =
    subscribe ~conn ~channel:"v4_trades" ~id:market ~batched:false ()

  let unsubscribe ~conn ~market () =
    unsubscribe ~conn ~channel:"v4_trades" ~id:market ()

  let parse_message json =
    try
      match channel_data_of_yojson json with
      | Ok data ->
          let contents_json = Yojson.Safe.Util.member "contents" json in
          (match trades_contents_of_yojson contents_json with
           | Ok contents -> Ok (data, contents)
           | Error msg -> Error (`Json_parse_error msg))
      | Error msg -> Error (`Json_parse_error msg)
    with
    | Yojson.Safe.Util.Type_error (msg, _) -> Error (`Json_parse_error msg)
end

module Markets = struct
  let subscribe ~conn () =
    subscribe ~conn ~channel:"v4_markets" ~id:"" ~batched:true ()

  let unsubscribe ~conn () =
    unsubscribe ~conn ~channel:"v4_markets" ~id:"" ()
end

module Candles = struct
  let subscribe ~conn ~market ~resolution () =
    let resolution_str = Common.candle_resolution_to_string resolution in
    let id = sprintf "%s/%s" market resolution_str in
    subscribe ~conn ~channel:"v4_candles" ~id ~batched:false ()

  let unsubscribe ~conn ~market ~resolution () =
    let resolution_str = Common.candle_resolution_to_string resolution in
    let id = sprintf "%s/%s" market resolution_str in
    unsubscribe ~conn ~channel:"v4_candles" ~id ()
end

(** Create a pipe that yields parsed messages *)
let create_message_pipe ~conn =
  let r, w = Pipe.create () in

  let rec read_loop () =
    match%bind Websocket_curl.receive conn with
    | Some msg ->
        (try
           let json = Yojson.Safe.from_string msg in
           Pipe.write_without_pushback w json;
           read_loop ()
         with
         | Yojson.Json_error err ->
             Core.printf "JSON parse error: %s\n" err;
             read_loop ())
    | None ->
        Pipe.close w;
        return ()
  in

  don't_wait_for (read_loop ());
  return r

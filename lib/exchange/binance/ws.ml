(** Binance WebSocket Streams *)

open Core
open Async

(** WebSocket endpoints *)
module Endpoint = struct
  let spot_stream = "wss://stream.binance.com:443"
  let spot_stream_alt = "wss://stream.binance.com:9443"
  let data_stream = "wss://data-stream.binance.vision"
end

(** Stream types *)
module Stream = struct
  type t =
    | Trade of string  (* symbol@trade *)
    | AggTrade of string  (* symbol@aggTrade *)
    | Depth of { symbol: string; levels: int option }  (* symbol@depth or symbol@depth5/10/20 *)
    | DepthDiff of string  (* symbol@depth for real-time updates *)
    | Ticker of string  (* symbol@ticker *)
    | MiniTicker of string  (* symbol@miniTicker *)
    | BookTicker of string  (* symbol@bookTicker *)
    | Kline of { symbol: string; interval: string }  (* symbol@kline_1m etc *)
  [@@deriving sexp]

  let to_stream_name = function
    | Trade symbol -> sprintf "%s@trade" (String.lowercase symbol)
    | AggTrade symbol -> sprintf "%s@aggTrade" (String.lowercase symbol)
    | Depth { symbol; levels = Some n } -> sprintf "%s@depth%d" (String.lowercase symbol) n
    | Depth { symbol; levels = None } | DepthDiff symbol ->
        sprintf "%s@depth" (String.lowercase symbol)
    | Ticker symbol -> sprintf "%s@ticker" (String.lowercase symbol)
    | MiniTicker symbol -> sprintf "%s@miniTicker" (String.lowercase symbol)
    | BookTicker symbol -> sprintf "%s@bookTicker" (String.lowercase symbol)
    | Kline { symbol; interval } -> sprintf "%s@kline_%s" (String.lowercase symbol) interval
end

(** WebSocket message types *)
module Message = struct
  type trade = {
    event_type: string; [@key "e"]
    event_time: int64; [@key "E"]
    symbol: string; [@key "s"]
    trade_id: int64; [@key "t"]
    price: string; [@key "p"]
    quantity: string; [@key "q"]
    buyer_order_id: int64; [@key "b"]
    seller_order_id: int64; [@key "a"]
    trade_time: int64; [@key "T"]
    is_buyer_maker: bool; [@key "m"]
  } [@@deriving yojson { strict = false }]

  type depth = {
    last_update_id: int64; [@key "lastUpdateId"]
    bids: (string * string) list;
    asks: (string * string) list;
  } [@@deriving yojson { strict = false }]

  type depth_update = {
    event_type: string; [@key "e"]
    event_time: int64; [@key "E"]
    symbol: string; [@key "s"]
    first_update_id: int64; [@key "U"]
    final_update_id: int64; [@key "u"]
    bids: (string * string) list; [@key "b"]
    asks: (string * string) list; [@key "a"]
  } [@@deriving yojson { strict = false }]

  type ticker = {
    event_type: string; [@key "e"]
    event_time: int64; [@key "E"]
    symbol: string; [@key "s"]
    price_change: string; [@key "p"]
    price_change_percent: string; [@key "P"]
    weighted_avg_price: string; [@key "w"]
    last_price: string; [@key "c"]
    last_qty: string; [@key "Q"]
    bid_price: string; [@key "b"]
    bid_qty: string; [@key "B"]
    ask_price: string; [@key "a"]
    ask_qty: string; [@key "A"]
    open_price: string; [@key "o"]
    high_price: string; [@key "h"]
    low_price: string; [@key "l"]
    volume: string; [@key "v"]
    quote_volume: string; [@key "q"]
  } [@@deriving yojson { strict = false }]

  type book_ticker = {
    update_id: int64; [@key "u"]
    symbol: string; [@key "s"]
    best_bid_price: string; [@key "b"]
    best_bid_qty: string; [@key "B"]
    best_ask_price: string; [@key "a"]
    best_ask_qty: string; [@key "A"]
  } [@@deriving yojson { strict = false }]

  type t =
    | Trade of trade
    | Depth of depth
    | DepthUpdate of depth_update
    | Ticker of ticker
    | BookTicker of book_ticker
    | Unknown of string

  let sexp_of_t = function
    | Trade _ -> Sexp.Atom "Trade"
    | Depth _ -> Sexp.Atom "Depth"
    | DepthUpdate _ -> Sexp.Atom "DepthUpdate"
    | Ticker _ -> Sexp.Atom "Ticker"
    | BookTicker _ -> Sexp.Atom "BookTicker"
    | Unknown msg -> Sexp.List [Sexp.Atom "Unknown"; Sexp.Atom msg]
end

(** WebSocket client *)
type t = {
  uri: Uri.t;
  messages: string Pipe.Reader.t;
}

let parse_message (msg : string) : Message.t =
  try
    let json = Yojson.Safe.from_string msg in
    match Yojson.Safe.Util.member "e" json |> Yojson.Safe.Util.to_string_option with
    | Some "trade" ->
      (match Message.trade_of_yojson json with
       | Ok trade -> Message.Trade trade
       | Error _ -> Message.Unknown msg)
    | Some "depthUpdate" ->
      (match Message.depth_update_of_yojson json with
       | Ok update -> Message.DepthUpdate update
       | Error _ -> Message.Unknown msg)
    | Some "24hrTicker" ->
      (match Message.ticker_of_yojson json with
       | Ok ticker -> Message.Ticker ticker
       | Error _ -> Message.Unknown msg)
    | Some "bookTicker" ->
      (match Message.book_ticker_of_yojson json with
       | Ok book_ticker -> Message.BookTicker book_ticker
       | Error _ -> Message.Unknown msg)
    | _ ->
      (* Could be a depth snapshot *)
      (match Message.depth_of_yojson json with
       | Ok depth -> Message.Depth depth
       | Error _ -> Message.Unknown msg)
  with _ -> Message.Unknown msg

let connect ?(url = Endpoint.spot_stream) ~streams () : t Deferred.Or_error.t =
  let stream_names = List.map streams ~f:Stream.to_stream_name in
  let uri =
    if List.length stream_names = 1 then
      Uri.of_string (sprintf "%s/ws/%s" url (List.hd_exn stream_names))
    else
      Uri.of_string (sprintf "%s/stream?streams=%s" url (String.concat ~sep:"/" stream_names))
  in

  Deferred.Or_error.try_with (fun () ->
    let headers = Cohttp.Header.init () in
    Cohttp_async_websocket.Client.create ~headers uri
    >>= fun conn_result ->
    (match conn_result with
    | Ok (_response, ws) -> return ws
    | Error err ->
      eprintf "Binance WebSocket connection error: %s\n" (Error.to_string_hum err);
      Error.raise err)
    >>= fun ws ->
    let reader, _writer = Websocket.pipes ws in
    return { uri; messages = reader }
  )

let messages t = t.messages

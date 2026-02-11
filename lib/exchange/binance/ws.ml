(** Binance WebSocket Streams *)

open Core
open Async

(** WebSocket endpoints *)
module Endpoint = struct
  let spot_stream = "wss://stream.binance.com:443"
  let spot_stream_alt = "wss://stream.binance.com:9443"
  let data_stream = "wss://data-stream.binance.vision"
  let us_stream = "wss://ws-api.binance.us"
  let us_stream_alt = "wss://stream.binance.us:9443"
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
    | UserData of string  (* User data stream with listen key *)
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
    | UserData listen_key -> listen_key  (* User data stream uses listen key directly *)

  (** User data stream endpoint (different from market data) *)
  let user_data_endpoint = "wss://stream.binance.com:9443/ws"
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

  (** Execution report from user data stream - order updates and fills *)
  type execution_report = {
    event_type: string; [@key "e"]  (* "executionReport" *)
    event_time: int64; [@key "E"]
    symbol: string; [@key "s"]
    client_order_id: string; [@key "c"]
    side: string; [@key "S"]  (* "BUY" or "SELL" *)
    order_type: string; [@key "o"]  (* "LIMIT", "MARKET", etc. *)
    time_in_force: string; [@key "f"]  (* "GTC", "IOC", "FOK", "GTX" *)
    quantity: string; [@key "q"]  (* Original quantity *)
    price: string; [@key "p"]  (* Order price *)
    stop_price: string; [@key "P"]  (* Stop price *)
    iceberg_qty: string; [@key "F"]  (* Iceberg quantity *)
    order_list_id: int64; [@key "g"]  (* OrderListId *)
    orig_client_order_id: string; [@key "C"]  (* Original client order ID *)
    execution_type: string; [@key "x"]  (* NEW, CANCELED, REPLACED, REJECTED, TRADE, EXPIRED *)
    order_status: string; [@key "X"]  (* NEW, PARTIALLY_FILLED, FILLED, CANCELED, etc. *)
    reject_reason: string; [@key "r"]  (* Rejection reason *)
    order_id: int64; [@key "i"]  (* Order ID *)
    last_executed_qty: string; [@key "l"]  (* Last executed quantity *)
    cumulative_filled_qty: string; [@key "z"]  (* Cumulative filled quantity *)
    last_executed_price: string; [@key "L"]  (* Last executed price *)
    commission: string; [@key "n"]  (* Commission amount *)
    commission_asset: string option; [@key "N"] [@default None]  (* Commission asset *)
    trade_time: int64; [@key "T"]  (* Transaction time *)
    trade_id: int64; [@key "t"]  (* Trade ID *)
    is_on_book: bool; [@key "w"]  (* Is the order on the book? *)
    is_maker: bool; [@key "m"]  (* Is this trade the maker side? *)
    order_creation_time: int64; [@key "O"]  (* Order creation time *)
    cumulative_quote_qty: string; [@key "Z"]  (* Cumulative quote asset transacted quantity *)
    last_quote_qty: string; [@key "Y"]  (* Last quote asset transacted quantity *)
    quote_order_qty: string; [@key "Q"]  (* Quote Order Qty *)
  } [@@deriving yojson { strict = false }, sexp]

  (** Balance update within account update *)
  type balance_update = {
    asset: string; [@key "a"]
    free: string; [@key "f"]
    locked: string; [@key "l"]
  } [@@deriving yojson { strict = false }, sexp]

  (** Account update from user data stream - balance changes *)
  type account_update = {
    event_type: string; [@key "e"]  (* "outboundAccountPosition" *)
    event_time: int64; [@key "E"]
    last_update_time: int64; [@key "u"]
    balances: balance_update list; [@key "B"]
  } [@@deriving yojson { strict = false }, sexp]

  (** Balance update event (different from account_update) *)
  type balance_update_event = {
    event_type: string; [@key "e"]  (* "balanceUpdate" *)
    event_time: int64; [@key "E"]
    asset: string; [@key "a"]
    delta: string; [@key "d"]
    clear_time: int64; [@key "T"]
  } [@@deriving yojson { strict = false }, sexp]

  type t =
    | Trade of trade
    | Depth of depth
    | DepthUpdate of depth_update
    | Ticker of ticker
    | BookTicker of book_ticker
    | ExecutionReport of execution_report
    | AccountUpdate of account_update
    | BalanceUpdate of balance_update_event
    | Unknown of string

  let sexp_of_t = function
    | Trade _ -> Sexp.Atom "Trade"
    | Depth _ -> Sexp.Atom "Depth"
    | DepthUpdate _ -> Sexp.Atom "DepthUpdate"
    | Ticker _ -> Sexp.Atom "Ticker"
    | BookTicker _ -> Sexp.Atom "BookTicker"
    | ExecutionReport r -> Sexp.List [Sexp.Atom "ExecutionReport"; sexp_of_execution_report r]
    | AccountUpdate u -> Sexp.List [Sexp.Atom "AccountUpdate"; sexp_of_account_update u]
    | BalanceUpdate b -> Sexp.List [Sexp.Atom "BalanceUpdate"; sexp_of_balance_update_event b]
    | Unknown msg -> Sexp.List [Sexp.Atom "Unknown"; Sexp.Atom msg]
end

(** Parse a WebSocket message into structured Message.t *)
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
    | Some "executionReport" ->
      (match Message.execution_report_of_yojson json with
       | Ok report -> Message.ExecutionReport report
       | Error _ -> Message.Unknown msg)
    | Some "outboundAccountPosition" ->
      (match Message.account_update_of_yojson json with
       | Ok update -> Message.AccountUpdate update
       | Error _ -> Message.Unknown msg)
    | Some "balanceUpdate" ->
      (match Message.balance_update_event_of_yojson json with
       | Ok update -> Message.BalanceUpdate update
       | Error _ -> Message.Unknown msg)
    | _ ->
      (* Could be a depth snapshot *)
      (match Message.depth_of_yojson json with
       | Ok depth -> Message.Depth depth
       | Error _ -> Message.Unknown msg)
  with _ -> Message.Unknown msg

(* NOTE: The old cohttp_async_websocket-based connect function has been removed.
   Use Market_data.connect instead, which uses websocket_curl. *)

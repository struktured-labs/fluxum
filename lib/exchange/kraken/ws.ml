(** Full Websocket API implementation for Kraken *)

(** Kraken WebSocket endpoints *)
module Endpoint = struct
  let private_url = "wss://ws-auth.kraken.com/"
  let public_url = "wss://ws.kraken.com/"

  (* v2 uses the same endpoints - version is determined by message format, not URL *)
  let private_url_v2 = private_url
  let public_url_v2 = public_url
end

(** Error types for WebSocket operations *)
module Error = struct
  type t =
    [ `Json_parse_error of string
    | `Connection_error of string
    | `Authentication_error of string
    | `Channel_parse_error of string
    ]
  [@@deriving sexp, yojson]
end

(** System message types *)
module System = struct
  type status = { 
    status: string; 
    version: string 
  }
  [@@deriving sexp, yojson]

  type message =
    | Heartbeat
    | System_status of status
    [@@deriving sexp, yojson]

  (** Parse a JSON object into a system message *)
  let parse_message (json : Yojson.Safe.t) : (message, Error.t) Result.t =
    match json with
    | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "event" with
      | Some (`String "heartbeat") -> Ok Heartbeat
      | Some (`String "systemStatus") ->
        (match 
          (List.Assoc.find fields ~equal:String.equal "status",
           List.Assoc.find fields ~equal:String.equal "version")
        with
        | Some (`String status), Some (`String version) ->
          Ok (System_status { status; version })
        | _ -> Error (`Channel_parse_error "Missing status or version"))
      | _ -> Error (`Channel_parse_error "Unknown system event"))
    | _ -> Error (`Json_parse_error "Expected object")
end

(** Private channel for order and execution updates *)
module Private = struct
  (** Order description *)
  module Order_descr = struct
    type t =
      { pair : string
      ; type_ : Common.Side.t [@key "type"]
      ; ordertype : Common.Order_type.t
      ; price : string
      ; price2 : string [@default "0"]
      ; leverage : string [@default "none"]
      ; order : string
      ; close : string [@default ""]
      }
    [@@deriving sexp, yojson]
  end

  (** Single order update *)
  module Order_update = struct
    type t =
      { status : Common.Order_status.t
      ; vol : string  (* Volume *)
      ; vol_exec : string  (* Volume executed *)
      ; cost : string  (* Total cost *)
      ; fee : string  (* Total fee *)
      ; avg_price : string  (* Average price *)
      ; stop_price : string [@default "0"]
      ; limit_price : string [@default "0"]
      ; misc : string [@default ""]
      ; oflags : string [@default ""]
      ; descr : Order_descr.t
      ; lastupdated : float option [@default None]
      ; userref : int option [@default None]
      }
    [@@deriving sexp, yojson]
  end

  (** Own orders update *)
  module Own_orders_data = struct
    type t = (string * Order_update.t) list  (* Map of order ID -> order update *)
    [@@deriving sexp]
  end

  (** Single trade execution *)
  module Trade_update = struct
    type t =
      { ordertxid : string  (* Order transaction ID *)
      ; pair : string
      ; time : float
      ; type_ : Common.Side.t [@key "type"]
      ; ordertype : Common.Order_type.t
      ; price : string
      ; cost : string
      ; fee : string
      ; vol : string
      ; margin : string [@default "0"]
      ; misc : string [@default ""]
      ; trade_id : int option [@default None]
      ; maker : bool option [@default None]  (* true if maker, false if taker *)
      }
    [@@deriving sexp, yojson]
  end

  (** Own trades update *)
  module Own_trades_data = struct
    type t = (string * Trade_update.t) list  (* Map of trade ID -> trade *)
    [@@deriving sexp]
  end

  type message =
    | Own_orders of Own_orders_data.t
    | Own_trades of Own_trades_data.t
    | Pong
    [@@deriving sexp]

  (** Parse a JSON object into a private message *)
  let parse_message (json : Yojson.Safe.t) : (message, Error.t) Result.t =
    match json with
    | `List items ->
      (* Private messages: [data, "channelName", {sequence}] *)
      (match items with
      | [data; `String channel; _sequence] ->
        (match channel with
        | "openOrders" | "ownOrders" ->
          (* Parse orders map *)
          (match data with
          | `Assoc order_list ->
            let parse_order (id, order_json) =
              match Order_update.of_yojson order_json with
              | Ok update -> Some (id, update)
              | Error _ -> None
            in
            let orders = List.filter_map order_list ~f:parse_order in
            Ok (Own_orders orders)
          | _ -> Error (`Json_parse_error "Expected object for orders"))
        | "ownTrades" ->
          (* Parse trades map *)
          (match data with
          | `Assoc trade_list ->
            let parse_trade (id, trade_json) =
              match Trade_update.of_yojson trade_json with
              | Ok update -> Some (id, update)
              | Error _ -> None
            in
            let trades = List.filter_map trade_list ~f:parse_trade in
            Ok (Own_trades trades)
          | _ -> Error (`Json_parse_error "Expected object for trades"))
        | _ -> Error (`Channel_parse_error ("Unknown private channel: " ^ channel)))
      | _ -> Error (`Json_parse_error "Expected [data, channelName, sequence]"))
    | `Assoc fields ->
      (* Handle pong messages *)
      (match List.Assoc.find fields ~equal:String.equal "event" with
      | Some (`String "pong") -> Ok Pong
      | _ -> Error (`Channel_parse_error "Unknown private event"))
    | _ -> Error (`Json_parse_error "Expected array or object")

  (** Generate authentication message for own-orders subscription *)
  let auth_message ~signature : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "subscription", `Assoc [
          "name", `String "own-orders";
          "token", `String signature;
        ];
      ])

  (** Generate authentication message for own-trades subscription *)
  let auth_trades_message ~signature : string =
    Yojson.Safe.to_string
      (`Assoc [
        "event", `String "subscribe";
        "subscription", `Assoc [
          "name", `String "ownTrades";
          "token", `String signature;
        ];
      ])

  (** Generate unsubscribe message for own-orders *)
  let unsubscribe_message ~signature : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "unsubscribe";
        "subscription", `Assoc [
          "name", `String "own-orders";
          "token", `String signature;
        ];
      ])
end

(** Public channels for market data *)
module Public = struct
  (** Ticker data *)
  module Ticker_data = struct
    type t =
      { ask : string  (* Best ask price *)
      ; ask_volume : string  (* Ask volume *)
      ; bid : string  (* Best bid price *)
      ; bid_volume : string  (* Bid volume *)
      ; close : string  (* Last trade closed price *)
      ; close_volume : string  (* Last trade closed volume *)
      ; vwap : string  (* Volume weighted average price today *)
      ; volume : string  (* Volume today *)
      ; low : string  (* Low price today *)
      ; high : string  (* High price today *)
      ; open_ : string [@key "open"]  (* Open price today *)
      ; trades : int  (* Number of trades today *)
      }
    [@@deriving sexp, yojson]
  end

  (** Single trade *)
  module Trade_item = struct
    type t =
      { price : string
      ; volume : string
      ; time : float
      ; side : Common.Side.t
      ; order_type : Common.Order_type.t
      ; misc : string
      }
    [@@deriving sexp, yojson]
  end

  (** Spread update *)
  module Spread_data = struct
    type t =
      { bid : string
      ; ask : string
      ; timestamp : float
      ; bid_volume : string
      ; ask_volume : string
      }
    [@@deriving sexp, yojson]
  end

  (** OHLC candlestick *)
  module Ohlc_data = struct
    type t =
      { time : float
      ; etime : float  (* End time *)
      ; open_ : string [@key "open"]
      ; high : string
      ; low : string
      ; close : string
      ; vwap : string
      ; volume : string
      ; count : int  (* Number of trades *)
      }
    [@@deriving sexp, yojson]
  end

  (** Order book price level *)
  module Price_level = struct
    type t =
      { price : string
      ; volume : string
      ; timestamp : float
      }
    [@@deriving sexp, yojson]
  end

  (** Order book update *)
  module Book_data = struct
    type update =
      { bids : Price_level.t list [@default []]
      ; asks : Price_level.t list [@default []]
      ; checksum : int option [@default None]
      }
    [@@deriving sexp, yojson]

    type t =
      { update : update
      ; channel : string
      ; pair : string
      }
    [@@deriving sexp, yojson]
  end

  (** Unified book update for Fluxum adapter *)
  module Book_update = struct
    type t =
      { side : [ `Bid | `Ask ]
      ; levels : (float * float) list  (* price, qty *)
      ; symbol : string
      ; timestamp : Time_float_unix.t
      ; is_snapshot : bool
      }
    [@@deriving sexp]
  end

  type message =
    | Ticker of { data : Ticker_data.t; channel : string; pair : string }
    | Spread of { data : Spread_data.t; channel : string; pair : string }
    | Trade of { trades : Trade_item.t list; channel : string; pair : string }
    | Ohlc of { data : Ohlc_data.t; channel : string; pair : string }
    | Book of Book_data.t
    [@@deriving sexp]

  (** Generate subscription message for ticker feed *)
  let subscribe_ticker pairs : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String "ticker";
        ];
      ])

  (** Generate subscription message for spread feed *)
  let subscribe_spread pairs : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String "spread";
        ];
      ])

  (** Generate subscription message for trade feed *)
  let subscribe_trade pairs : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String "trade";
        ];
      ])

  (** Generate subscription message for OHLC candles *)
  let subscribe_ohlc ?(interval=60) pairs : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String "ohlc";
          "interval", `Int interval;
        ];
      ])

  (** Generate subscription message for order book *)
  let subscribe_book ?(depth=10) pairs : string =
    Yojson.Safe.to_string 
      (`Assoc [
        "event", `String "subscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String "book";
          "depth", `Int depth;
        ];
      ])

  (** Generate unsubscribe message *)
  let unsubscribe channel pairs : string =
    Yojson.Safe.to_string
      (`Assoc [
        "event", `String "unsubscribe";
        "pair", `List (List.map pairs ~f:(fun p -> `String p));
        "subscription", `Assoc [
          "name", `String channel;
        ];
      ])

  (** WebSocket API v2 subscription messages *)
  module V2 = struct
    (** Normalize symbol for v2 API - converts XETHZUSD to ETH/USD format *)
    let normalize_symbol s =
      match String.mem s '/' with
      | true -> s
      | false ->
        (* Remove leading X and convert to base/quote format *)
        let s = match String.is_prefix s ~prefix:"X" && String.length s > 4 with
                | true -> String.drop_prefix s 1
                | false -> s in
        let len = String.length s in
        match len > 3 with
        | true ->
          let base = String.sub s ~pos:0 ~len:(len - 3) in
          let quote = String.sub s ~pos:(len - 3) ~len:3 in
          base ^ "/" ^ quote
        | false -> s

    (** Generate v2 subscription message for ticker feed *)
    let subscribe_ticker ?(event_trigger="trades") ?(snapshot=true) pairs : string =
      let symbols = List.map pairs ~f:normalize_symbol in
      Yojson.Safe.to_string
        (`Assoc [
          "method", `String "subscribe";
          "params", `Assoc [
            "channel", `String "ticker";
            "symbol", `List (List.map symbols ~f:(fun p -> `String p));
            "event_trigger", `String event_trigger;
            "snapshot", `Bool snapshot;
          ];
        ])

    (** Generate v2 subscription message for trade feed *)
    let subscribe_trade ?(snapshot=true) pairs : string =
      let symbols = List.map pairs ~f:normalize_symbol in
      Yojson.Safe.to_string
        (`Assoc [
          "method", `String "subscribe";
          "params", `Assoc [
            "channel", `String "trade";
            "symbol", `List (List.map symbols ~f:(fun p -> `String p));
            "snapshot", `Bool snapshot;
          ];
        ])

    (** Generate v2 subscription message for OHLC candles *)
    let subscribe_ohlc ?(interval=60) ?(snapshot=true) pairs : string =
      let symbols = List.map pairs ~f:normalize_symbol in
      Yojson.Safe.to_string
        (`Assoc [
          "method", `String "subscribe";
          "params", `Assoc [
            "channel", `String "ohlc";
            "symbol", `List (List.map symbols ~f:(fun p -> `String p));
            "interval", `Int interval;
            "snapshot", `Bool snapshot;
          ];
        ])

    (** Generate v2 subscription message for order book *)
    let subscribe_book ?(depth=10) ?(snapshot=true) pairs : string =
      let symbols = List.map pairs ~f:normalize_symbol in
      Yojson.Safe.to_string
        (`Assoc [
          "method", `String "subscribe";
          "params", `Assoc [
            "channel", `String "book";
            "symbol", `List (List.map symbols ~f:(fun p -> `String p));
            "depth", `Int depth;
            "snapshot", `Bool snapshot;
          ];
        ])

    (** Generate v2 unsubscribe message *)
    let unsubscribe channel pairs : string =
      let symbols = List.map pairs ~f:normalize_symbol in
      Yojson.Safe.to_string
        (`Assoc [
          "method", `String "unsubscribe";
          "params", `Assoc [
            "channel", `String channel;
            "symbol", `List (List.map symbols ~f:(fun p -> `String p));
          ];
        ])
  end

  (** Parse book levels from JSON array *)
  let parse_price_levels (json_array : Yojson.Safe.t) : Price_level.t list =
    match json_array with
    | `List items ->
      List.filter_map items ~f:(fun item ->
        match item with
        | `List [`String price; `String volume; `Float timestamp] ->
          Some { Price_level.price; volume; timestamp }
        | `List [`String price; `String volume; `String _timestamp] ->
          (* Sometimes timestamp is a string *)
          Some { Price_level.price; volume; timestamp = 0.0 }
        | _ -> None
      )
    | _ -> []

  (** Parse a JSON array into a public message *)
  let parse_message (json : Yojson.Safe.t) : (message, Error.t) Result.t =
    match json with
    | `List (_channel_id :: data_json :: channel_name :: pair_items) ->
      let channel = match channel_name with `String s -> s | _ -> "unknown" in
      let pair = match pair_items with [`String p] -> p | _ -> "unknown" in

      (* Determine message type from data structure *)
      (match data_json with
      | `Assoc fields ->
        (* Convert fields to Map once for O(log n) lookups instead of O(n) *)
        let fm = List.fold fields ~init:String.Map.empty ~f:(fun acc (k, v) ->
          Map.set acc ~key:k ~data:v) in
        (* Check for book update fields *)
        (match (Map.find fm "a",
                Map.find fm "b",
                Map.find fm "as",
                Map.find fm "bs") with
        | (Some asks_json, _, _, _) | (_, _, Some asks_json, _) ->
          (* Book update or snapshot *)
          let bids_json = Map.find fm "b"
                         |> Option.value ~default:(Map.find fm "bs" |> Option.value ~default:(`List [])) in
          let checksum = Map.find fm "c"
                        |> Option.bind ~f:(function `String s -> Int.of_string_opt s | _ -> None) in
          let asks = parse_price_levels asks_json in
          let bids = parse_price_levels bids_json in
          let update = { Book_data.bids; asks; checksum } in
          Ok (Book { Book_data.update; channel; pair })

        | (None, Some _, None, None) when Map.mem fm "a" ->
          (* Spread message *)
          (match Spread_data.of_yojson data_json with
          | Ok data -> Ok (Spread { data; channel; pair })
          | Error e -> Error (`Json_parse_error ("Spread parse error: " ^ e)))

        | _ ->
          (* Check for ticker fields *)
          match Map.find fm "c" with
          | Some _ ->
            (match Ticker_data.of_yojson data_json with
            | Ok data -> Ok (Ticker { data; channel; pair })
            | Error e -> Error (`Json_parse_error ("Ticker parse error: " ^ e)))
          | None ->
            (* Assume OHLC *)
            match Ohlc_data.of_yojson data_json with
            | Ok data -> Ok (Ohlc { data; channel; pair })
            | Error e -> Error (`Json_parse_error ("OHLC parse error: " ^ e)))

      | `List trade_items ->
        (* Parse as trades *)
        let parse_trade_item item =
          match Trade_item.of_yojson item with
          | Ok trade -> Some trade
          | Error _ -> None
        in
        let trades = List.filter_map trade_items ~f:parse_trade_item in
        Ok (Trade { trades; channel; pair })
      | _ ->
        Error (`Channel_parse_error "Unable to determine message type"))
    | _ -> Error (`Json_parse_error "Expected array message with [channelID, data, channelName, pair]")
end

(** Unified message type *)
type message =
  | System of System.message
  | Public of Public.message
  | Private of Private.message
  | Raw of (Yojson.Safe.t [@sexp.opaque])
  [@@deriving sexp]

(** Parse incoming raw message string *)
let parse_message (msg : string) : (message, Error.t) Result.t =
  try
    let json = Yojson.Safe.from_string msg in
    (* Try system message first *)
    match System.parse_message json with
    | Ok sys_msg -> Ok (System sys_msg)
    | Error _ ->
      (* Try private channel *)
      match Private.parse_message json with
      | Ok priv_msg -> Ok (Private priv_msg)
      | Error _ ->
        (* Try public channel *)
        match Public.parse_message json with
        | Ok pub_msg -> Ok (Public pub_msg)
        | Error _ -> Ok (Raw json)
  with e ->
    Error (`Json_parse_error (Exn.to_string e))

(** WebSocket client connection state *)
module Client = struct
  type t = {
    endpoint : string;
    mutable authenticated : bool;
    mutable active : bool;
    mutable subscriptions : string list;
  }

  (** Create a new public client *)
  let create_public () : t = {
    endpoint = Endpoint.public_url;
    authenticated = false;
    active = true;
    subscriptions = [];
  }

  (** Create a new private client *)
  let create_private () : t = {
    endpoint = Endpoint.private_url;
    authenticated = true;
    active = true;
    subscriptions = [];
  }

  (** Get client endpoint *)
  let endpoint t = t.endpoint

  (** Check if client is authenticated *)
  let is_authenticated t = t.authenticated

  (** Check if client is active *)
  let is_active t = t.active

  (** Add subscription tracking *)
  let add_subscription t sub =
    match t.active with
    | true -> t.subscriptions <- sub :: t.subscriptions
    | false -> ()

  (** Get active subscriptions *)
  let subscriptions t = t.subscriptions

  (** Generate ping message *)
  let ping () : string =
    Yojson.Safe.to_string (`Assoc ["event", `String "ping"])

  (** Generate pong message *)
  let pong () : string =
    Yojson.Safe.to_string (`Assoc ["event", `String "pong"])

  (** Close the connection *)
  let close t : string =
    t.active <- false;
    Yojson.Safe.to_string 
      (`Assoc ["event", `String "closeConnection"])
end

(** Message stream handler *)
type handler = message -> unit Deferred.t

(** Main WebSocket connection *)
type t = {
  client : Client.t;
  mutable handlers : handler list;
  mutable message_count : int;
}

(** Create a WebSocket connection *)
let create ~endpoint : t =
  let client = 
    if String.is_substring endpoint ~substring:"auth"
    then Client.create_private ()
    else Client.create_public ()
  in
  {
    client;
    handlers = [];
    message_count = 0;
  }

(** Register a message handler *)
let on_message ws (handler : handler) : unit =
  ws.handlers <- handler :: ws.handlers

(** Process an incoming message *)
let process_message ws (msg : string) : unit Deferred.t =
  ws.message_count <- ws.message_count + 1;
  match parse_message msg with
  | Ok parsed ->
    Deferred.all_unit (List.map ws.handlers ~f:(fun handler ->
      handler parsed))
  | Error _ ->
    (* Silently handle parse errors *)
    Deferred.unit

(** Get message count *)
let message_count ws = ws.message_count

(** Get client *)
let client ws = ws.client

(** Placeholder for actual connection *)
let connect endpoint : (t, Error.t) Result.t Deferred.t =
  let _ws = create ~endpoint in
  (* This would require async_websocket/cohttp library *)
  Deferred.return 
    (Error (`Connection_error 
      "WebSocket client requires async_websocket library. Use message functions to build client."))

(** Create a message stream with predefined handlers *)
let with_handlers endpoint handlers : (t, Error.t) Result.t Deferred.t =
  let ws = create ~endpoint in
  List.iter handlers ~f:(on_message ws);
  Deferred.return (Ok ws)

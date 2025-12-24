(** Full Websocket API implementation for Kraken *)

(** Kraken WebSocket endpoints *)
module Endpoint = struct
  let private_url = "wss://ws-auth.kraken.com/"
  let public_url = "wss://ws.kraken.com/"
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
  type message = 
    | Own_orders of Yojson.Safe.t
    | Own_trades of Yojson.Safe.t
    | Pong
    [@@deriving sexp]

  (** Parse a JSON object into a private message *)
  let parse_message (json : Yojson.Safe.t) : (message, Error.t) Result.t =
    match json with
    | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "event" with
      | Some (`String "ownOrder") -> Ok (Own_orders json)
      | Some (`String "ownTrades") -> Ok (Own_trades json)
      | Some (`String "pong") -> Ok Pong
      | _ -> Error (`Channel_parse_error "Unknown private event"))
    | _ -> Error (`Json_parse_error "Expected object")

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
  type message =
    | Ticker of Yojson.Safe.t
    | Spread of Yojson.Safe.t
    | Trade of Yojson.Safe.t
    | Ohlc of Yojson.Safe.t
    | Book of Yojson.Safe.t
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

  (** Parse a JSON array into a public message *)
  let parse_message (json : Yojson.Safe.t) : (message, Error.t) Result.t =
    match json with
    | `List items when List.length items >= 3 ->
      (match List.nth items 1 with
      | Some (`Assoc fields) ->
        (match List.Assoc.find fields ~equal:String.equal "c" with
        | Some _ -> Ok (Ticker json)
        | None -> 
          match List.Assoc.find fields ~equal:String.equal "s" with
          | Some _ -> Ok (Spread json)
          | None -> Ok (Book json))
      | Some _ -> Ok (Trade json)
      | None -> Error (`Channel_parse_error "Unexpected message structure"))
    | _ -> Error (`Json_parse_error "Expected array message")
end

(** Unified message type *)
type message =
  | System of System.message
  | Public of Public.message
  | Private of Private.message
  | Raw of Yojson.Safe.t
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
    if t.active then
      t.subscriptions <- sub :: t.subscriptions

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

(** Bitstamp WebSocket API Client

    WebSocket API v2 for real-time market data.

    Channels:
    - live_trades_{pair}
    - live_orders_{pair}
    - order_book_{pair}
    - diff_order_book_{pair}

    @see <https://www.bitstamp.net/websocket/v2/>
*)

open Core
open Async

(** WebSocket channels *)
module Channel = struct
  type t =
    | Live_trades of string      (** Live trades for pair *)
    | Live_orders of string       (** Live order book for pair *)
    | Order_book of string        (** Full order book for pair *)
    | Diff_order_book of string   (** Order book diffs for pair *)
  [@@deriving sexp]

  (** Convert channel to string for subscription *)
  let to_string = function
    | Live_trades pair -> sprintf "live_trades_%s" pair
    | Live_orders pair -> sprintf "live_orders_%s" pair
    | Order_book pair -> sprintf "order_book_%s" pair
    | Diff_order_book pair -> sprintf "diff_order_book_%s" pair

  (** Parse channel from string *)
  let of_string s =
    match String.split s ~on:'_' with
    | ["live"; "trades"; pair] -> Some (Live_trades pair)
    | ["live"; "orders"; pair] -> Some (Live_orders pair)
    | ["order"; "book"; pair] -> Some (Order_book pair)
    | ["diff"; "order"; "book"; pair] -> Some (Diff_order_book pair)
    | _ -> None
end

(** WebSocket message types *)
module Message = struct
  type t =
    | Connected
    | Subscribed of string              (** Channel subscribed *)
    | Unsubscribed of string            (** Channel unsubscribed *)
    | Trade of Types.trade              (** Trade data *)
    | Order_book of Types.order_book    (** Order book snapshot *)
    | Data of string                    (** Generic data message (as JSON string) *)
    | Error of string                   (** Error message *)
    | Unknown of string                 (** Unknown message type *)
  [@@deriving sexp]
end

(** Parse WebSocket message *)
let parse_message (msg : string) : Message.t =
  try
    match Yojson.Safe.from_string msg |> Types.ws_message_of_yojson with
    | Ok ws_msg ->
      (match ws_msg.event with
       | "bts:connection_established" -> Message.Connected
       | "bts:subscription_succeeded" ->
         (match ws_msg.channel with
          | Some ch -> Message.Subscribed ch
          | None -> Message.Unknown "Subscription without channel")
       | "bts:unsubscription_succeeded" ->
         (match ws_msg.channel with
          | Some ch -> Message.Unsubscribed ch
          | None -> Message.Unknown "Unsubscription without channel")
       | "bts:request_reconnect" ->
         Message.Error "Server requested reconnect"
       | "data" | "trade" ->
         (match ws_msg.data with
          | Some data ->
            (* Try to parse as trade *)
            (match Types.trade_of_yojson data with
             | Ok trade -> Message.Trade trade
             | Error _ ->
               (* Try to parse as order book *)
               (match Types.order_book_of_yojson data with
                | Ok book -> Message.Order_book book
                | Error _ -> Message.Data (Yojson.Safe.to_string data)))
          | None -> Message.Unknown "Data event without data")
       | event -> Message.Unknown (sprintf "Unknown event: %s" event))
    | Error msg -> Message.Error (sprintf "JSON parse error: %s" msg)
  with
  | exn -> Message.Error (sprintf "Parse exception: %s" (Exn.to_string exn))

(** Create subscription message *)
let create_subscription_message (channel : Channel.t) : string =
  let channel_str = Channel.to_string channel in
  let subscription = Types.({
    event = "bts:subscribe";
    data = `Assoc [("channel", `String channel_str)];
  }) in
  Yojson.Safe.to_string (Types.subscription_to_yojson subscription)

(** Create unsubscription message *)
let create_unsubscription_message (channel : Channel.t) : string =
  let channel_str = Channel.to_string channel in
  let subscription = Types.({
    event = "bts:unsubscribe";
    data = `Assoc [("channel", `String channel_str)];
  }) in
  Yojson.Safe.to_string (Types.subscription_to_yojson subscription)

(** WebSocket connection state *)
type connection = {
  ws : Websocket_curl.t;
  cfg : Cfg.t;
  channels : Channel.t list ref;
}

(** Connect to Bitstamp WebSocket *)
let connect ~cfg ~channels () : (connection, string) Deferred.Result.t =
  let open Deferred.Result.Let_syntax in

  let%bind ws = Websocket_curl.connect ~url:cfg.Cfg.ws_url
    |> Deferred.Result.map_error ~f:(fun err ->
      sprintf "WebSocket connection failed: %s" (Core.Error.to_string_hum err))
  in

  let conn = { ws; cfg; channels = ref channels } in

  (* Subscribe to initial channels *)
  let%bind () = Deferred.Result.all_unit (
    List.map channels ~f:(fun channel ->
      let sub_msg = create_subscription_message channel in
      Deferred.Or_error.try_with (fun () -> Websocket_curl.send ws sub_msg)
      |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
        sprintf "Subscribe failed: %s" (Core.Error.to_string_hum err))))
  ) in

  return conn

(** Subscribe to channel *)
let subscribe (conn : connection) (channel : Channel.t) : (unit, string) Deferred.Result.t =
  conn.channels := channel :: !(conn.channels);
  let sub_msg = create_subscription_message channel in
  Deferred.Or_error.try_with (fun () -> Websocket_curl.send conn.ws sub_msg)
  |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
    sprintf "Subscribe failed: %s" (Core.Error.to_string_hum err)))

(** Unsubscribe from channel *)
let unsubscribe (conn : connection) (channel : Channel.t) : (unit, string) Deferred.Result.t =
  conn.channels := List.filter !(conn.channels) ~f:(fun ch ->
    not (phys_equal ch channel));
  let unsub_msg = create_unsubscription_message channel in
  Deferred.Or_error.try_with (fun () -> Websocket_curl.send conn.ws unsub_msg)
  |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
    sprintf "Unsubscribe failed: %s" (Core.Error.to_string_hum err)))

(** Receive one message from WebSocket

    @return None if connection closed, Some msg otherwise
    Use in a loop to continuously receive messages *)
let receive (conn : connection) : string option Deferred.t =
  Websocket_curl.receive conn.ws

(** Close WebSocket connection *)
let close (conn : connection) : unit Deferred.t =
  Websocket_curl.close conn.ws

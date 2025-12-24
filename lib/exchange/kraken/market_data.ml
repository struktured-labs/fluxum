(** Market data WebSocket client for Kraken *)

open Core
open Async

(** Represents an active WebSocket subscription *)
type subscription = {
  channel: string;
  pairs: string list;
  interval: int option;
  depth: int option;
}

(** Represents the market data stream *)
type t = {
  uri : Uri.t;
  mutable reader : string Pipe.Reader.t option;
  mutable writer : string Pipe.Writer.t option;
  subscriptions : subscription list ref;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
}

(** Connect to Kraken public WebSocket and subscribe to market data *)
let connect ~(subscriptions : subscription list) ?(url = Ws.Endpoint.public_url_v2) () : (t, Ws.Error.t) Result.t Deferred.t =
  let uri = Uri.of_string url in
  Deferred.Or_error.try_with (fun () ->
    let headers = Cohttp.Header.init () in
    Cohttp_async_websocket.Client.create ~headers uri
    >>= fun conn_result ->
    (match conn_result with
    | Ok (_response, ws) -> return (_response, ws)
    | Error err ->
      (* Try to extract response details from error *)
      let err_str = Error.to_string_hum err in
      eprintf "WebSocket connection error details:\n%s\n" err_str;
      Or_error.ok_exn conn_result |> return)
    >>= fun (_response, ws) ->
    let reader, writer = Websocket.pipes ws in
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      reader = Some reader;
      writer = Some writer;
      subscriptions = ref subscriptions;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
    } in

    (* Start background task to relay messages *)
    don't_wait_for (
      Pipe.iter reader ~f:(fun payload ->
        Pipe.write t.message_writer payload)
      >>| fun () -> Pipe.close t.message_writer
    );

    (* Send subscription messages for each subscription - using v2 API *)
    don't_wait_for (
      Deferred.List.iter ~how:`Sequential subscriptions ~f:(fun sub ->
        let msg = match sub.channel with
          | "ticker" -> Ws.Public.V2.subscribe_ticker sub.pairs
          | "trade" -> Ws.Public.V2.subscribe_trade sub.pairs
          | "ohlc" ->
              let interval = Option.value sub.interval ~default:60 in
              Ws.Public.V2.subscribe_ohlc ~interval sub.pairs
          | "book" ->
              let depth = Option.value sub.depth ~default:10 in
              Ws.Public.V2.subscribe_book ~depth sub.pairs
          | _ -> ""
        in
        if not (String.is_empty msg) then
          Pipe.write writer msg
        else
          Deferred.unit)
    );

    return (Ok t)
  ) 
  >>| (function
    | Ok r -> r
    | Error err -> Error (`Connection_error (Error.to_string_hum err)))

(** Get the message stream from the WebSocket *)
let messages t : string Pipe.Reader.t = t.message_pipe

(** Subscribe to an additional channel *)
let subscribe t ~channel ~pairs : unit Deferred.t =
  let sub = { channel; pairs; interval = None; depth = None } in
  t.subscriptions := sub :: !(t.subscriptions);

  match t.writer with
  | None -> Deferred.unit
  | Some writer ->
    let msg = match channel with
      | "ticker" -> Ws.Public.V2.subscribe_ticker pairs
      | "trade" -> Ws.Public.V2.subscribe_trade pairs
      | "ohlc" -> Ws.Public.V2.subscribe_ohlc ~interval:60 pairs
      | "book" -> Ws.Public.V2.subscribe_book ~depth:10 pairs
      | _ -> ""
    in
    if String.is_empty msg then
      Deferred.unit
    else
      Pipe.write writer msg

(** Unsubscribe from a channel *)
let unsubscribe t ~channel ~pairs : unit Deferred.t =
  t.subscriptions :=
    List.filter !(t.subscriptions) ~f:(fun sub ->
      not (String.equal sub.channel channel && List.equal String.equal sub.pairs pairs));

  match t.writer with
  | None -> Deferred.unit
  | Some writer ->
    let msg = Ws.Public.V2.unsubscribe channel pairs in
    Pipe.write writer msg

(** Close the WebSocket connection *)
let close t : unit Deferred.t =
  match t.writer with
  | None ->
    Pipe.close t.message_writer;
    Deferred.unit
  | Some writer ->
    Pipe.close writer;
    Pipe.close t.message_writer;
    Deferred.unit

(** Get current subscriptions *)
let subscriptions t = !(t.subscriptions)

(** Get connection endpoint *)
let endpoint t = Uri.to_string t.uri


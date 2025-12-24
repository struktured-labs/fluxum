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
let connect ~(subscriptions : subscription list) ?(url = Ws.Endpoint.public_url) () : (t, Ws.Error.t) Result.t Deferred.t =
  let uri = Uri.of_string url in
  Deferred.Or_error.try_with (fun () ->
    let headers =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "User-Agent" "fluxum/kraken-market-data"
      |> fun h -> Cohttp.Header.add h "Origin" "https://www.kraken.com"
    in
    Cohttp_async_websocket.Client.create ~headers uri
    >>= fun conn_result ->
    Or_error.ok_exn conn_result |> return
    >>= fun (_response, ws) ->
    let reader, _writer = Websocket.pipes ws in
    let message_pipe_reader, message_pipe_writer = Pipe.create () in
    
    let t = {
      uri;
      reader = Some reader;
      writer = None;
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
    
    (* For now just return the connected client *)
    (* Actual subscription would require bidirectional write *)
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
  | Some _writer ->
    let msg = match channel with
      | "ticker" -> Ws.Public.subscribe_ticker pairs
      | "spread" -> Ws.Public.subscribe_spread pairs
      | "trade" -> Ws.Public.subscribe_trade pairs
      | "ohlc" -> Ws.Public.subscribe_ohlc ~interval:60 pairs
      | "book" -> Ws.Public.subscribe_book ~depth:10 pairs
      | _ -> ""
    in
    if String.is_empty msg then
      Deferred.unit
    else
      (* For now, just acknowledge - actual writing requires writer pipe *)
      Deferred.unit

(** Unsubscribe from a channel *)
let unsubscribe t ~channel ~pairs : unit Deferred.t =
  t.subscriptions := 
    List.filter !(t.subscriptions) ~f:(fun sub ->
      not (String.equal sub.channel channel && List.equal String.equal sub.pairs pairs));
  
  match t.writer with
  | None -> Deferred.unit
  | Some _writer ->
    let _msg = Ws.Public.unsubscribe channel pairs in
    (* For now, just acknowledge - actual writing requires writer pipe *)
    Deferred.unit

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


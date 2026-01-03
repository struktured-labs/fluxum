(** MEXC WebSocket Streams - Protobuf-based *)

open Core
open Async

(** WebSocket endpoints *)
module Endpoint = struct
  let public_url = "wss://wbs-api.mexc.com/ws"
  let public_url_insecure = "ws://wbs-api.mexc.com/ws"
end

(** Stream types for subscriptions *)
module Stream = struct
  (** Update frequency *)
  type frequency =
    | Ms10   (* 10ms updates *)
    | Ms100  (* 100ms updates *)
  [@@deriving sexp, equal]

  let frequency_to_string = function
    | Ms10 -> "10ms"
    | Ms100 -> "100ms"

  (** Available stream types *)
  type t =
    | AggreDeals of { symbol : string; frequency : frequency }
    | AggreDepths of { symbol : string; frequency : frequency }
    | LimitDepths of { symbol : string; levels : int }
    | BookTicker of string
    | MiniTicker of string
    | Kline of { symbol : string; interval : string }
  [@@deriving sexp]

  (** Convert stream to subscription channel name *)
  let to_channel = function
    | AggreDeals { symbol; frequency } ->
      sprintf "spot@public.aggre.deals.v3.api.pb@%s@%s"
        (frequency_to_string frequency)
        (String.uppercase symbol)
    | AggreDepths { symbol; frequency } ->
      sprintf "spot@public.aggre.depth.v3.api.pb@%s@%s"
        (frequency_to_string frequency)
        (String.uppercase symbol)
    | LimitDepths { symbol; levels } ->
      sprintf "spot@public.limit.depth.v3.api.pb@%s@%d"
        (String.uppercase symbol)
        levels
    | BookTicker symbol ->
      sprintf "spot@public.bookTicker.v3.api@%s"
        (String.uppercase symbol)
    | MiniTicker symbol ->
      sprintf "spot@public.miniTicker.v3.api@%s"
        (String.uppercase symbol)
    | Kline { symbol; interval } ->
      sprintf "spot@public.kline.v3.api@%s@%s"
        interval
        (String.uppercase symbol)
end

(** Protobuf wire types *)
module Protobuf = struct
  type wire_type =
    | Varint      (* 0 *)
    | Fixed64     (* 1 *)
    | LengthDelim (* 2 *)
    | Fixed32     (* 5 *)
  [@@deriving sexp]

  let wire_type_of_int = function
    | 0 -> Some Varint
    | 1 -> Some Fixed64
    | 2 -> Some LengthDelim
    | 5 -> Some Fixed32
    | _ -> None

  (** Decoder state *)
  type decoder = {
    data : string;
    mutable pos : int;
  }

  let create_decoder data = { data; pos = 0 }

  let remaining d = String.length d.data - d.pos

  let read_byte d =
    if d.pos >= String.length d.data then None
    else begin
      let b = Char.to_int d.data.[d.pos] in
      d.pos <- d.pos + 1;
      Some b
    end

  (** Read varint (up to 64 bits) *)
  let read_varint d =
    let rec loop acc shift =
      match read_byte d with
      | None -> None
      | Some b ->
        let b_val = Int64.of_int (b land 0x7f) in
        let value = Int64.(acc lor (shift_left b_val shift)) in
        if b land 0x80 = 0 then Some value
        else if shift >= 63 then None
        else loop value (shift + 7)
    in
    loop 0L 0

  (** Read field tag (field number + wire type) *)
  let read_tag d =
    match read_varint d with
    | None -> None
    | Some tag ->
      let field_number = Int64.(to_int_exn (tag lsr 3)) in
      let wire_type_int = Int64.(to_int_exn (tag land 7L)) in
      match wire_type_of_int wire_type_int with
      | Some wire_type -> Some (field_number, wire_type)
      | None -> None

  (** Read length-delimited bytes *)
  let read_bytes d =
    match read_varint d with
    | None -> None
    | Some len ->
      let len = Int64.to_int_exn len in
      if d.pos + len > String.length d.data then None
      else begin
        let s = String.sub d.data ~pos:d.pos ~len in
        d.pos <- d.pos + len;
        Some s
      end

  (** Read string (length-delimited) *)
  let read_string = read_bytes

  (** Skip a field based on wire type *)
  let skip_field d = function
    | Varint -> ignore (read_varint d)
    | Fixed64 -> d.pos <- d.pos + 8
    | LengthDelim ->
      (match read_varint d with
       | Some len -> d.pos <- d.pos + Int64.to_int_exn len
       | None -> ())
    | Fixed32 -> d.pos <- d.pos + 4
end

(** Message types *)
module Message = struct
  (** Trade deal item *)
  type deal_item = {
    price : string;
    quantity : string;
    trade_type : int;  (* 1 = buy, 2 = sell *)
    time : int64;
  } [@@deriving sexp]

  (** Aggregated deals message *)
  type aggre_deals = {
    deals : deal_item list;
    event_type : string;
  } [@@deriving sexp]

  (** Depth level item *)
  type depth_item = {
    price : string;
    quantity : string;
  } [@@deriving sexp]

  (** Aggregated depth message *)
  type aggre_depth = {
    asks : depth_item list;
    bids : depth_item list;
    event_type : string;
    from_version : string;
    to_version : string;
  } [@@deriving sexp]

  (** Limit depth (snapshot) message *)
  type limit_depth = {
    asks : depth_item list;
    bids : depth_item list;
    event_type : string;
    version : string;
  } [@@deriving sexp]

  (** Book ticker message *)
  type book_ticker = {
    bid_price : string;
    bid_quantity : string;
    ask_price : string;
    ask_quantity : string;
  } [@@deriving sexp]

  (** Wrapper message *)
  type wrapper = {
    channel : string;
    symbol : string option;
    create_time : int64 option;
    send_time : int64 option;
    body : body;
  }
  and body =
    | AggreDeals of aggre_deals
    | AggreDepth of aggre_depth
    | LimitDepth of limit_depth
    | BookTicker of book_ticker
    | Unknown of string
  [@@deriving sexp]

  (** Parsed message *)
  type t =
    | Data of wrapper
    | Pong
    | SubscriptionAck of { id : int; msg : string }
    | Error of string
    | Raw of string
  [@@deriving sexp]
end

(** Protobuf message parsing *)
module Parse = struct
  open Protobuf

  (** Parse deal item from bytes *)
  let parse_deal_item data =
    let d = create_decoder data in
    let price = ref "" in
    let quantity = ref "" in
    let trade_type = ref 0 in
    let time = ref 0L in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_string d with Some s -> price := s | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_string d with Some s -> quantity := s | None -> ());
          loop ()
        | Some (3, Varint) ->
          (match read_varint d with Some v -> trade_type := Int64.to_int_exn v | None -> ());
          loop ()
        | Some (4, Varint) ->
          (match read_varint d with Some v -> time := v | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{ price = !price; quantity = !quantity; trade_type = !trade_type; time = !time }

  (** Parse depth item from bytes *)
  let parse_depth_item data =
    let d = create_decoder data in
    let price = ref "" in
    let quantity = ref "" in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_string d with Some s -> price := s | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_string d with Some s -> quantity := s | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{ price = !price; quantity = !quantity }

  (** Parse aggre deals message *)
  let parse_aggre_deals data =
    let d = create_decoder data in
    let deals = ref [] in
    let event_type = ref "" in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_bytes d with
           | Some item_data ->
             deals := parse_deal_item item_data :: !deals
           | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_string d with Some s -> event_type := s | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{ deals = List.rev !deals; event_type = !event_type }

  (** Parse aggre depth message *)
  let parse_aggre_depth data =
    let d = create_decoder data in
    let asks = ref [] in
    let bids = ref [] in
    let event_type = ref "" in
    let from_version = ref "" in
    let to_version = ref "" in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_bytes d with
           | Some item_data ->
             asks := parse_depth_item item_data :: !asks
           | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_bytes d with
           | Some item_data ->
             bids := parse_depth_item item_data :: !bids
           | None -> ());
          loop ()
        | Some (3, LengthDelim) ->
          (match read_string d with Some s -> event_type := s | None -> ());
          loop ()
        | Some (4, LengthDelim) ->
          (match read_string d with Some s -> from_version := s | None -> ());
          loop ()
        | Some (5, LengthDelim) ->
          (match read_string d with Some s -> to_version := s | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{
      asks = List.rev !asks;
      bids = List.rev !bids;
      event_type = !event_type;
      from_version = !from_version;
      to_version = !to_version;
    }

  (** Parse limit depth message *)
  let parse_limit_depth data =
    let d = create_decoder data in
    let asks = ref [] in
    let bids = ref [] in
    let event_type = ref "" in
    let version = ref "" in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_bytes d with
           | Some item_data ->
             asks := parse_depth_item item_data :: !asks
           | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_bytes d with
           | Some item_data ->
             bids := parse_depth_item item_data :: !bids
           | None -> ());
          loop ()
        | Some (3, LengthDelim) ->
          (match read_string d with Some s -> event_type := s | None -> ());
          loop ()
        | Some (4, LengthDelim) ->
          (match read_string d with Some s -> version := s | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{
      asks = List.rev !asks;
      bids = List.rev !bids;
      event_type = !event_type;
      version = !version;
    }

  (** Parse book ticker message *)
  let parse_book_ticker data =
    let d = create_decoder data in
    let bid_price = ref "" in
    let bid_quantity = ref "" in
    let ask_price = ref "" in
    let ask_quantity = ref "" in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_string d with Some s -> bid_price := s | None -> ());
          loop ()
        | Some (2, LengthDelim) ->
          (match read_string d with Some s -> bid_quantity := s | None -> ());
          loop ()
        | Some (3, LengthDelim) ->
          (match read_string d with Some s -> ask_price := s | None -> ());
          loop ()
        | Some (4, LengthDelim) ->
          (match read_string d with Some s -> ask_quantity := s | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{
      bid_price = !bid_price;
      bid_quantity = !bid_quantity;
      ask_price = !ask_price;
      ask_quantity = !ask_quantity;
    }

  (** Parse wrapper message - field numbers from PushDataV3ApiWrapper.proto *)
  let parse_wrapper data =
    let d = create_decoder data in
    let channel = ref "" in
    let symbol = ref None in
    let create_time = ref None in
    let send_time = ref None in
    let body = ref (Message.Unknown "") in
    let rec loop () =
      if remaining d <= 0 then ()
      else match read_tag d with
        | None -> ()
        | Some (1, LengthDelim) ->
          (match read_string d with Some s -> channel := s | None -> ());
          loop ()
        | Some (3, LengthDelim) ->
          (match read_string d with Some s -> symbol := Some s | None -> ());
          loop ()
        | Some (4, LengthDelim) ->
          (match read_string d with Some s -> symbol := Some s | None -> ());
          loop ()
        | Some (5, Varint) ->
          (match read_varint d with Some v -> create_time := Some v | None -> ());
          loop ()
        | Some (6, Varint) ->
          (match read_varint d with Some v -> send_time := Some v | None -> ());
          loop ()
        (* Body oneof fields - field numbers 301-315 *)
        | Some (313, LengthDelim) ->  (* publicAggreDepths *)
          (match read_bytes d with
           | Some body_data -> body := Message.AggreDepth (parse_aggre_depth body_data)
           | None -> ());
          loop ()
        | Some (314, LengthDelim) ->  (* publicAggreDeals *)
          (match read_bytes d with
           | Some body_data -> body := Message.AggreDeals (parse_aggre_deals body_data)
           | None -> ());
          loop ()
        | Some (303, LengthDelim) ->  (* publicLimitDepths *)
          (match read_bytes d with
           | Some body_data -> body := Message.LimitDepth (parse_limit_depth body_data)
           | None -> ());
          loop ()
        | Some (305, LengthDelim) ->  (* publicBookTicker *)
          (match read_bytes d with
           | Some body_data -> body := Message.BookTicker (parse_book_ticker body_data)
           | None -> ());
          loop ()
        | Some (_, wt) ->
          skip_field d wt;
          loop ()
    in
    loop ();
    Message.{
      channel = !channel;
      symbol = !symbol;
      create_time = !create_time;
      send_time = !send_time;
      body = !body;
    }
end

(** Parse incoming WebSocket message *)
let parse_message (data : string) : Message.t =
  (* Check if it's a JSON message (subscription ack, pong, error) *)
  if String.is_prefix data ~prefix:"{" then begin
    try
      let json = Yojson.Safe.from_string data in
      match Yojson.Safe.Util.member "msg" json with
      | `String msg ->
        let id = Yojson.Safe.Util.member "id" json
                 |> Yojson.Safe.Util.to_int_option
                 |> Option.value ~default:0
        in
        Message.SubscriptionAck { id; msg }
      | `Null ->
        (match Yojson.Safe.Util.member "pong" json with
         | `Null -> Message.Raw data
         | _ -> Message.Pong)
      | _ -> Message.Raw data
    with _ -> Message.Raw data
  end
  else begin
    (* Binary protobuf message *)
    try
      Message.Data (Parse.parse_wrapper data)
    with _ ->
      Message.Error (sprintf "Failed to parse protobuf message of length %d" (String.length data))
  end

(** Subscription request messages (JSON) *)
module Subscribe = struct
  (** Generate subscription message *)
  let subscribe ~id streams : string =
    let channels = List.map streams ~f:Stream.to_channel in
    Yojson.Safe.to_string
      (`Assoc [
         "method", `String "SUBSCRIPTION";
         "params", `List (List.map channels ~f:(fun c -> `String c));
         "id", `Int id;
       ])

  (** Generate unsubscription message *)
  let unsubscribe ~id streams : string =
    let channels = List.map streams ~f:Stream.to_channel in
    Yojson.Safe.to_string
      (`Assoc [
         "method", `String "UNSUBSCRIPTION";
         "params", `List (List.map channels ~f:(fun c -> `String c));
         "id", `Int id;
       ])

  (** Generate ping message *)
  let ping () : string =
    Yojson.Safe.to_string (`Assoc ["method", `String "PING"])
end

(** WebSocket client *)
type t = {
  uri : Uri.t;
  ws : Websocket_curl.t;
  message_pipe : string Pipe.Reader.t;
  message_writer : string Pipe.Writer.t;
  mutable active : bool;
}

(** Connect to MEXC WebSocket using libcurl *)
let connect ?(url = Endpoint.public_url) ~streams () : t Deferred.Or_error.t =
  let open Deferred.Let_syntax in
  let uri = Uri.of_string url in

  (* Connect using websocket_curl (libcurl-based, works with CloudFront) *)
  let%bind ws_result = Websocket_curl.connect ~url in

  match ws_result with
  | Error err ->
    eprintf "MEXC WebSocket connection error: %s\n" (Error.to_string_hum err);
    return (Error err)
  | Ok ws ->
    let message_pipe_reader, message_pipe_writer = Pipe.create () in

    let t = {
      uri;
      ws;
      message_pipe = message_pipe_reader;
      message_writer = message_pipe_writer;
      active = true;
    } in

    (* Start background task to receive messages *)
    don't_wait_for (
      let rec receive_loop () =
        if not t.active then (
          Pipe.close t.message_writer;
          return ()
        ) else
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

    (* Subscribe to streams *)
    let sub_msg = Subscribe.subscribe ~id:1 streams in
    let%bind () = Websocket_curl.send ws sub_msg in

    return (Ok t)

(** Get message pipe *)
let messages t = t.message_pipe

(** Apply depth update to order book *)
let apply_depth_to_book (book : Order_book.Book.t) (depth : Message.aggre_depth) =
  let book =
    List.fold depth.bids ~init:book ~f:(fun acc item ->
      let price = Float.of_string item.price in
      let size = Float.of_string item.quantity in
      Order_book.Book.set acc ~side:`Bid ~price ~size)
  in
  List.fold depth.asks ~init:book ~f:(fun acc item ->
    let price = Float.of_string item.price in
    let size = Float.of_string item.quantity in
    Order_book.Book.set acc ~side:`Ask ~price ~size)

(** Apply limit depth snapshot to order book *)
let apply_limit_depth_to_book (book : Order_book.Book.t) (depth : Message.limit_depth) =
  (* Clear existing book and apply snapshot *)
  let book = Order_book.Book.empty (Order_book.Book.symbol book) in
  let book =
    List.fold depth.bids ~init:book ~f:(fun acc item ->
      let price = Float.of_string item.price in
      let size = Float.of_string item.quantity in
      Order_book.Book.set acc ~side:`Bid ~price ~size)
  in
  List.fold depth.asks ~init:book ~f:(fun acc item ->
    let price = Float.of_string item.price in
    let size = Float.of_string item.quantity in
    Order_book.Book.set acc ~side:`Ask ~price ~size)

(** Convert deal to normalized trade *)
let deal_to_trade ~symbol:sym (deal : Message.deal_item) : Fluxum.Types.Trade.t =
  let side = if deal.trade_type = 1 then Fluxum.Types.Side.Buy else Fluxum.Types.Side.Sell in
  let ts = Time_float_unix.of_span_since_epoch
    (Time_float_unix.Span.of_ms (Int64.to_float deal.time)) in
  { Fluxum.Types.Trade.
    venue = Fluxum.Types.Venue.Mexc
  ; symbol = sym
  ; side
  ; price = Float.of_string deal.price
  ; qty = Float.of_string deal.quantity
  ; fee = None
  ; trade_id = None
  ; ts = Some ts
  }

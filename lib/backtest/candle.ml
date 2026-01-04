(** Unified candle (OHLCV) type for backtesting *)

open Core

type t =
  { symbol    : string
  ; timestamp : Time_ns_unix.t
  ; open_     : float
  ; high      : float
  ; low       : float
  ; close     : float
  ; volume    : float
  }
[@@deriving sexp, compare, equal, fields]

let create ~symbol ~timestamp ~open_ ~high ~low ~close ~volume =
  { symbol; timestamp; open_; high; low; close; volume }

(** Get mid price: (high + low) / 2 *)
let mid t = (t.high +. t.low) /. 2.

(** Get typical price: (high + low + close) / 3 *)
let typical t = (t.high +. t.low +. t.close) /. 3.

(** Get candle body: |close - open| *)
let body t = Float.abs (t.close -. t.open_)

(** Get candle range: high - low *)
let range t = t.high -. t.low

(** Is bullish (green) candle: close > open *)
let is_bullish t = Float.(t.close > t.open_)

(** Is bearish (red) candle: close < open *)
let is_bearish t = Float.(t.close < t.open_)

(** CSV field names for parsing *)
let csv_header = ["timestamp"; "open"; "high"; "low"; "close"; "volume"]

(** Parse from CSV row (assumes standard OHLCV format) *)
let of_csv_row ~symbol row =
  match row with
  | [ts; o; h; l; c; v] ->
    let timestamp =
      (* Try parsing as ISO 8601 first, then as epoch seconds *)
      match Time_ns.of_string ts with
      | ts -> ts
      | exception _ ->
        (* Try as epoch seconds (float) *)
        match Float.of_string ts with
        | epoch ->
          let span = Time_ns.Span.of_sec epoch in
          Time_ns.of_span_since_epoch span
        | exception _ ->
          failwith (sprintf "Cannot parse timestamp: %s" ts)
    in
    { symbol
    ; timestamp
    ; open_  = Float.of_string o
    ; high   = Float.of_string h
    ; low    = Float.of_string l
    ; close  = Float.of_string c
    ; volume = Float.of_string v
    }
  | _ ->
    failwith (sprintf "Invalid CSV row: expected 6 columns, got %d" (List.length row))

(** Convert to CSV row *)
let to_csv_row t =
  [ Time_ns.to_string_utc t.timestamp
  ; Float.to_string t.open_
  ; Float.to_string t.high
  ; Float.to_string t.low
  ; Float.to_string t.close
  ; Float.to_string t.volume
  ]

(** Convert from Yojson *)
let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let symbol = json |> member "symbol" |> to_string in
    let ts_val = json |> member "timestamp" in
    let timestamp =
      match ts_val with
      | `String s -> Time_ns.of_string s
      | `Float f -> Time_ns.of_span_since_epoch (Time_ns.Span.of_sec f)
      | `Int i -> Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Float.of_int i))
      | `Intlit s -> Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Float.of_string s))
      | _ -> failwith "Invalid timestamp format"
    in
    let open_ = json |> member "open" |> to_float in
    let high = json |> member "high" |> to_float in
    let low = json |> member "low" |> to_float in
    let close = json |> member "close" |> to_float in
    let volume = json |> member "volume" |> to_float in
    Ok { symbol; timestamp; open_; high; low; close; volume }
  with
  | e -> Error (Exn.to_string e)

(** Convert to Yojson *)
let to_yojson t =
  `Assoc
    [ ("symbol", `String t.symbol)
    ; ("timestamp", `String (Time_ns.to_string_utc t.timestamp))
    ; ("open", `Float t.open_)
    ; ("high", `Float t.high)
    ; ("low", `Float t.low)
    ; ("close", `Float t.close)
    ; ("volume", `Float t.volume)
    ]

(** Compare by timestamp *)
let compare_by_time a b = Time_ns.compare a.timestamp b.timestamp

(** Sort candles by timestamp ascending *)
let sort_by_time candles = List.sort candles ~compare:compare_by_time

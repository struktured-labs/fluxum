(** Data sources for backtesting - CSV and exchange API loaders *)

open Core
open Async

(** Data source module signature *)
module type SOURCE = sig
  val load_candles
    : symbol:string
    -> start:Time_ns.t
    -> end_:Time_ns.t
    -> interval:Time_ns.Span.t
    -> Candle.t list Deferred.t
end

(** CSV data source *)
module Csv = struct
  (** Load candles from a CSV file
      Expected format: timestamp,open,high,low,close,volume *)
  let load_from_file ~symbol ~path =
    Reader.file_lines path >>| fun lines ->
    (* Skip header if present *)
    let data_lines =
      match lines with
      | [] -> []
      | first :: rest ->
        (* Check if first line looks like a header *)
        match String.is_prefix first ~prefix:"timestamp" ||
              String.is_prefix first ~prefix:"time" ||
              String.is_prefix first ~prefix:"date" with
        | true -> rest
        | false -> lines
    in
    List.filter_map data_lines ~f:(fun line ->
      let line = String.strip line in
      match String.is_empty line with
      | true -> None
      | false ->
        let fields = String.split line ~on:',' in
        try Some (Candle.of_csv_row ~symbol fields)
        with _ -> None)

  (** Load candles from CSV with time filtering *)
  let load_candles ~symbol ~start ~end_ ~interval:_ ~path =
    load_from_file ~symbol ~path >>| fun candles ->
    List.filter candles ~f:(fun c ->
      Time_ns.(c.timestamp >= start && c.timestamp <= end_))
    |> Candle.sort_by_time

  (** Save candles to CSV *)
  let save_to_file ~path candles =
    let lines =
      "timestamp,open,high,low,close,volume" ::
      List.map candles ~f:(fun c -> String.concat ~sep:"," (Candle.to_csv_row c))
    in
    Writer.save_lines path lines
end

(** Generate synthetic candles for testing *)
module Synthetic = struct
  (** Random walk price generator *)
  let random_walk
      ~symbol
      ~start
      ~end_
      ~interval
      ~initial_price
      ~volatility
      ()
    =
    let rec loop current_time current_price acc =
      match Time_ns.(current_time > end_) with
      | true -> List.rev acc
      | false ->
        (* Generate random returns *)
        let rand () = (Random.float 2. -. 1.) *. volatility in
        let open_ = current_price *. (1. +. rand ()) in
        let close = open_ *. (1. +. rand ()) in
        let high = Float.max open_ close *. (1. +. Float.abs (rand ())) in
        let low = Float.min open_ close *. (1. -. Float.abs (rand ())) in
        let volume = Random.float 1000. +. 100. in

        let candle = Candle.create
            ~symbol
            ~timestamp:current_time
            ~open_
            ~high
            ~low
            ~close
            ~volume
        in

        let next_time = Time_ns.add current_time interval in
        loop next_time close (candle :: acc)
    in
    loop start initial_price []

  (** Trending price generator *)
  let trending
      ~symbol
      ~start
      ~end_
      ~interval
      ~initial_price
      ~trend_pct  (* Daily trend percentage, e.g., 0.001 = 0.1% per day *)
      ~volatility
      ()
    =
    let daily_factor = trend_pct /. (Time_ns.Span.to_day Time_ns.Span.day) in
    let interval_factor = daily_factor *. Time_ns.Span.to_day interval in

    let rec loop current_time current_price acc =
      match Time_ns.(current_time > end_) with
      | true -> List.rev acc
      | false ->
        let rand () = (Random.float 2. -. 1.) *. volatility in

        (* Apply trend + noise *)
        let trend_move = current_price *. interval_factor in
        let open_ = current_price +. trend_move +. (current_price *. rand ()) in
        let close = open_ +. trend_move +. (open_ *. rand ()) in
        let high = Float.max open_ close *. (1. +. Float.abs (rand () *. 0.5)) in
        let low = Float.min open_ close *. (1. -. Float.abs (rand () *. 0.5)) in
        let volume = Random.float 1000. +. 100. in

        let candle = Candle.create
            ~symbol
            ~timestamp:current_time
            ~open_
            ~high
            ~low
            ~close
            ~volume
        in

        let next_time = Time_ns.add current_time interval in
        loop next_time close (candle :: acc)
    in
    loop start initial_price []

  (** Mean-reverting price generator *)
  let mean_reverting
      ~symbol
      ~start
      ~end_
      ~interval
      ~mean_price
      ~volatility
      ~reversion_speed  (* How quickly price reverts to mean, 0-1 *)
      ()
    =
    let rec loop current_time current_price acc =
      match Time_ns.(current_time > end_) with
      | true -> List.rev acc
      | false ->
        let rand () = (Random.float 2. -. 1.) *. volatility in

        (* Mean reversion component *)
        let reversion = (mean_price -. current_price) *. reversion_speed in
        let open_ = current_price +. reversion +. (current_price *. rand ()) in
        let close = open_ +. (mean_price -. open_) *. reversion_speed +. (open_ *. rand ()) in
        let high = Float.max open_ close *. (1. +. Float.abs (rand () *. 0.3)) in
        let low = Float.min open_ close *. (1. -. Float.abs (rand () *. 0.3)) in
        let volume = Random.float 1000. +. 100. in

        let candle = Candle.create
            ~symbol
            ~timestamp:current_time
            ~open_
            ~high
            ~low
            ~close
            ~volume
        in

        let next_time = Time_ns.add current_time interval in
        loop next_time close (candle :: acc)
    in
    loop start mean_price []
end

(** Common intervals *)
module Interval = struct
  let minute_1 = Time_ns.Span.minute
  let minute_5 = Time_ns.Span.scale Time_ns.Span.minute 5.
  let minute_15 = Time_ns.Span.scale Time_ns.Span.minute 15.
  let minute_30 = Time_ns.Span.scale Time_ns.Span.minute 30.
  let hour_1 = Time_ns.Span.hour
  let hour_4 = Time_ns.Span.scale Time_ns.Span.hour 4.
  let day_1 = Time_ns.Span.day
  let week_1 = Time_ns.Span.scale Time_ns.Span.day 7.

  let of_string = function
    | "1m" -> minute_1
    | "5m" -> minute_5
    | "15m" -> minute_15
    | "30m" -> minute_30
    | "1h" -> hour_1
    | "4h" -> hour_4
    | "1d" -> day_1
    | "1w" -> week_1
    | s -> Time_ns.Span.of_string s

  let to_string span =
    let minutes = Time_ns.Span.to_min span in
    match Int.of_float minutes with
    | 1 -> "1m"
    | 5 -> "5m"
    | 15 -> "15m"
    | 30 -> "30m"
    | 60 -> "1h"
    | 240 -> "4h"
    | 1440 -> "1d"
    | 10080 -> "1w"
    | _ -> Time_ns.Span.to_string span
end

(** Helper to parse time from CLI *)
let parse_time s =
  try Time_ns.of_string s
  with _ ->
    (* Try parsing as date only *)
    let date = Date.of_string s in
    Time_ns.of_date_ofday ~zone:Time_float.Zone.utc date Time_ns.Ofday.start_of_day

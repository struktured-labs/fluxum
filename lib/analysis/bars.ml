open Core

type t =
  { symbol : string
  ; start_ts : Time_ns_unix.t
  ; end_ts : Time_ns_unix.t
  ; open_ : float
  ; high : float
  ; low : float
  ; close : float
  ; volume : float
  ; notional : float
  ; vwap : float
  ; trade_count : int
  ; buy_volume : float
  ; sell_volume : float
  }
[@@deriving sexp, compare, equal, fields]

(** Mutable accumulator used while folding a trade stream. Symbol/start_ts/open_
    are set at construction in [new_acc] and never reassigned. *)
type acc =
  { symbol : string
  ; start_ts : Time_ns_unix.t
  ; mutable end_ts : Time_ns_unix.t
  ; open_ : float
  ; mutable high : float
  ; mutable low : float
  ; mutable close : float
  ; mutable volume : float
  ; mutable notional : float
  ; mutable trade_count : int
  ; mutable buy_volume : float
  ; mutable sell_volume : float
  }

let new_acc ~symbol ~ts ~price ~qty ~side =
  { symbol
  ; start_ts = ts
  ; end_ts = ts
  ; open_ = price
  ; high = price
  ; low = price
  ; close = price
  ; volume = qty
  ; notional = price *. qty
  ; trade_count = 1
  ; buy_volume =
      (match (side : Fluxum.Types.Side.t) with
       | Buy -> qty
       | Sell -> 0.)
  ; sell_volume =
      (match (side : Fluxum.Types.Side.t) with
       | Buy -> 0.
       | Sell -> qty)
  }

let acc_add acc ~ts ~price ~qty ~side =
  acc.end_ts <- ts;
  acc.high <- Float.max acc.high price;
  acc.low <- Float.min acc.low price;
  acc.close <- price;
  acc.volume <- acc.volume +. qty;
  acc.notional <- acc.notional +. (price *. qty);
  acc.trade_count <- acc.trade_count + 1;
  match (side : Fluxum.Types.Side.t) with
  | Buy -> acc.buy_volume <- acc.buy_volume +. qty
  | Sell -> acc.sell_volume <- acc.sell_volume +. qty

let acc_finalize a : t =
  { symbol = a.symbol
  ; start_ts = a.start_ts
  ; end_ts = a.end_ts
  ; open_ = a.open_
  ; high = a.high
  ; low = a.low
  ; close = a.close
  ; volume = a.volume
  ; notional = a.notional
  ; vwap =
      (match Float.( = ) a.volume 0. with
       | true -> a.close
       | false -> a.notional /. a.volume)
  ; trade_count = a.trade_count
  ; buy_volume = a.buy_volume
  ; sell_volume = a.sell_volume
  }

(** Snap a timestamp down to the start of its [interval] bucket. *)
let bucket_start ts ~interval =
  let span = Time_ns_unix.to_span_since_epoch ts in
  let interval_ns = Time_ns.Span.to_int_ns interval in
  let span_ns = Time_ns.Span.to_int_ns span in
  let bucket_ns = (span_ns / interval_ns) * interval_ns in
    Time_ns_unix.of_span_since_epoch (Time_ns.Span.of_int_ns bucket_ns)

let symbol_to_string (sym : Fluxum.Types.Symbol.t) =
  Sexp.to_string (Fluxum.Types.Symbol.sexp_of_t sym)

let time_bars ~interval ~trades =
  let bars = ref [] in
  let acc_ref : acc option ref = ref None in
  let current_bucket = ref None in
    List.iter trades ~f:(fun (tr : Fluxum.Types.Trade.t) ->
      match tr.ts with
      | None -> ()
      | Some ts_float ->
        let ts =
          Time_ns_unix.of_time_float_round_nearest_microsecond ts_float
        in
        let bucket = bucket_start ts ~interval in
        let symbol = symbol_to_string tr.symbol in
        let price = tr.price in
        let qty = tr.qty in
        let side = tr.side in
          (match !current_bucket with
           | None ->
             acc_ref := Some (new_acc ~symbol ~ts ~price ~qty ~side);
             current_bucket := Some bucket
           | Some b when Time_ns_unix.equal b bucket ->
             (match !acc_ref with
              | Some a -> acc_add a ~ts ~price ~qty ~side
              | None -> ())
           | Some _ ->
             (match !acc_ref with
              | Some a -> bars := acc_finalize a :: !bars
              | None -> ());
             acc_ref := Some (new_acc ~symbol ~ts ~price ~qty ~side);
             current_bucket := Some bucket));
    (match !acc_ref with
     | Some a -> bars := acc_finalize a :: !bars
     | None -> ());
    List.rev !bars

let threshold_bars ~threshold ~measure ~trades =
  let bars = ref [] in
  let acc_ref : acc option ref = ref None in
  let cumulative = ref 0. in
    List.iter trades ~f:(fun (tr : Fluxum.Types.Trade.t) ->
      let ts =
        match tr.ts with
        | Some ts_float ->
          Time_ns_unix.of_time_float_round_nearest_microsecond ts_float
        | None -> Time_ns_unix.epoch
      in
      let symbol = symbol_to_string tr.symbol in
      let price = tr.price in
      let qty = tr.qty in
      let side = tr.side in
      let m = measure ~price ~qty in
        (match !acc_ref with
         | None ->
           acc_ref := Some (new_acc ~symbol ~ts ~price ~qty ~side);
           cumulative := m
         | Some a ->
           acc_add a ~ts ~price ~qty ~side;
           cumulative := !cumulative +. m);
        (match Float.( >= ) !cumulative threshold with
         | true ->
           (match !acc_ref with
            | Some a ->
              bars := acc_finalize a :: !bars;
              acc_ref := None;
              cumulative := 0.
            | None -> ())
         | false -> ()));
    (match !acc_ref with
     | Some a when a.trade_count > 0 -> bars := acc_finalize a :: !bars
     | _ -> ());
    List.rev !bars

let volume_bars ~threshold ~trades =
  threshold_bars ~threshold ~measure:(fun ~price:_ ~qty -> qty) ~trades

let dollar_bars ~threshold ~trades =
  threshold_bars ~threshold ~measure:(fun ~price ~qty -> price *. qty) ~trades

let twap (bars : t list) =
  match bars with
  | [] -> Float.nan
  | _ ->
    let n = List.length bars in
    let sum = List.fold bars ~init:0. ~f:(fun acc (b : t) -> acc +. b.close) in
      sum /. Float.of_int n

let vwap (bars : t list) =
  let total_notional =
    List.fold bars ~init:0. ~f:(fun acc (b : t) -> acc +. b.notional)
  in
  let total_volume =
    List.fold bars ~init:0. ~f:(fun acc (b : t) -> acc +. b.volume)
  in
    match Float.( = ) total_volume 0. with
    | true -> Float.nan
    | false -> total_notional /. total_volume

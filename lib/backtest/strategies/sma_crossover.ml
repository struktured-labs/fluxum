(** SMA Crossover Strategy

    Classic moving average crossover strategy:
    - Buy when fast SMA crosses above slow SMA
    - Sell when fast SMA crosses below slow SMA
*)

open Core

module Config = struct
  type t =
    { fast_period : int  (** Fast SMA period (default: 10) *)
    ; slow_period : int  (** Slow SMA period (default: 30) *)
    ; position_size : float  (** Fraction of equity to trade (0-1) *)
    }
  [@@deriving sexp]

  let default =
    { fast_period = 10
    ; slow_period = 30
    ; position_size = 0.95  (* Use 95% of equity *)
    }

  let create ?(fast_period = 10) ?(slow_period = 30) ?(position_size = 0.95) () =
    { fast_period; slow_period; position_size }
end

type state =
  { config     : Config.t
  ; fast_sma   : float option  (** Current fast SMA value *)
  ; slow_sma   : float option  (** Current slow SMA value *)
  ; prev_fast  : float option  (** Previous fast SMA value *)
  ; prev_slow  : float option  (** Previous slow SMA value *)
  }
[@@deriving sexp]

let name = "sma-crossover"

let initial_state =
  { config = Config.default
  ; fast_sma = None
  ; slow_sma = None
  ; prev_fast = None
  ; prev_slow = None
  }

let with_config config =
  { initial_state with config }

(** Calculate SMA from history *)
let calculate_sma history ~period =
  match List.length history >= period with
  | false -> None
  | true ->
    let prices = List.take history period |> List.map ~f:(fun c -> c.Backtest.Candle.close) in
    let sum = List.fold prices ~init:0. ~f:(+.) in
    Some (sum /. Float.of_int period)

let on_candle state (ctx : Backtest.Strategy_intf.Context.t) =
  let config = state.config in
  let history = ctx.candle :: ctx.history in

  (* Calculate SMAs *)
  let fast_sma = calculate_sma history ~period:config.fast_period in
  let slow_sma = calculate_sma history ~period:config.slow_period in

  (* Detect crossover *)
  let signal =
    match fast_sma, slow_sma, state.prev_fast, state.prev_slow with
    | Some fast, Some slow, Some prev_fast, Some prev_slow ->
      (* Check for crossover *)
      let was_below = Float.(prev_fast < prev_slow) in
      let now_above = Float.(fast > slow) in
      let was_above = Float.(prev_fast > prev_slow) in
      let now_below = Float.(fast < slow) in

      (match was_below && now_above with
       | true ->
         (* Bullish crossover - buy *)
         (match Backtest.Strategy_intf.Position.is_flat ctx.position with
          | true ->
            let price = ctx.candle.close in
            let available = ctx.balance *. config.position_size in
            let qty = available /. price in
            Backtest.Strategy_intf.Signal.buy qty
          | false ->
            match Backtest.Strategy_intf.Position.is_short ctx.position with
            | true -> Backtest.Strategy_intf.Signal.close_short ()
            | false -> Backtest.Strategy_intf.Signal.hold)
       | false ->
         match was_above && now_below with
         | true ->
           (* Bearish crossover - close long *)
           (match Backtest.Strategy_intf.Position.is_long ctx.position with
            | true -> Backtest.Strategy_intf.Signal.close_long ()
            | false -> Backtest.Strategy_intf.Signal.hold)
         | false -> Backtest.Strategy_intf.Signal.hold)

    | _ ->
      (* Not enough data yet *)
      Backtest.Strategy_intf.Signal.hold
  in

  let new_state =
    { state with
      fast_sma
    ; slow_sma
    ; prev_fast = state.fast_sma
    ; prev_slow = state.slow_sma
    }
  in

  (signal, new_state)

(** Create strategy module *)
module Make (C : sig val config : Config.t end) : Backtest.Strategy_intf.S = struct
  let name = "sma-crossover"
  type nonrec state = state
  let initial_state = with_config C.config
  let on_candle = on_candle
end

(** Default strategy module *)
module Default : Backtest.Strategy_intf.S = struct
  let name = "sma-crossover"
  type nonrec state = state
  let initial_state = initial_state
  let on_candle = on_candle
end

(** Register with strategy registry *)
let () =
  Backtest.Strategy_intf.Registry.register
    ~name
    ~description:"SMA crossover: buy when fast crosses above slow, sell on cross below"
    ~create:(fun () -> (module Default : Backtest.Strategy_intf.S))

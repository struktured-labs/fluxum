(** Momentum Strategy

    Buy when price has momentum (rate of change > threshold).
    Sell when momentum fades or reverses.
*)

open Core

module Config = struct
  type t =
    { lookback_period : int     (** Periods to calculate momentum *)
    ; entry_threshold : float   (** Momentum threshold to enter (e.g., 0.02 = 2%) *)
    ; exit_threshold : float    (** Momentum threshold to exit (e.g., -0.01 = -1%) *)
    ; position_size : float     (** Fraction of equity to trade *)
    }
  [@@deriving sexp]

  let default =
    { lookback_period = 14
    ; entry_threshold = 0.02
    ; exit_threshold = -0.01
    ; position_size = 0.95
    }

  let create
      ?(lookback_period = 14)
      ?(entry_threshold = 0.02)
      ?(exit_threshold = -0.01)
      ?(position_size = 0.95)
      ()
    =
    { lookback_period; entry_threshold; exit_threshold; position_size }
end

type state =
  { config : Config.t
  }
[@@deriving sexp]

let name = "momentum"

let initial_state = { config = Config.default }

let with_config config = { config }

(** Calculate rate of change (momentum) *)
let calculate_momentum history ~period =
  match List.length history >= period with
  | false -> None
  | true ->
    let current = (List.hd_exn history).Backtest.Candle.close in
    let past = (List.nth_exn history (period - 1)).Backtest.Candle.close in
    match Float.(past > 0.) with
    | true -> Some ((current -. past) /. past)
    | false -> None

let on_candle state (ctx : Backtest.Strategy_intf.Context.t) =
  let config = state.config in
  let history = ctx.candle :: ctx.history in

  (* Calculate momentum *)
  let momentum = calculate_momentum history ~period:config.lookback_period in

  let signal =
    match momentum with
    | None -> Backtest.Strategy_intf.Signal.hold
    | Some mom ->
      let is_long = Backtest.Strategy_intf.Position.is_long ctx.position in
      let is_flat = Backtest.Strategy_intf.Position.is_flat ctx.position in

      match is_flat && Float.(mom > config.entry_threshold) with
      | true ->
        (* Strong positive momentum - buy *)
        let price = ctx.candle.close in
        let available = ctx.balance *. config.position_size in
        let qty = available /. price in
        Backtest.Strategy_intf.Signal.buy qty
      | false ->
        match is_long && Float.(mom < config.exit_threshold) with
        | true ->
          (* Momentum faded - sell *)
          Backtest.Strategy_intf.Signal.close_long ()
        | false -> Backtest.Strategy_intf.Signal.hold
  in

  (signal, state)

module Default : Backtest.Strategy_intf.S = struct
  let name = name
  type nonrec state = state
  let initial_state = initial_state
  let on_candle = on_candle
end

let () =
  Backtest.Strategy_intf.Registry.register
    ~name
    ~description:"Momentum: buy on positive ROC, exit when momentum fades"
    ~create:(fun () -> (module Default : Backtest.Strategy_intf.S))

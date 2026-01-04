(** Buy and Hold Strategy

    Simple baseline strategy: buy on the first candle and hold forever.
    Useful for comparing other strategies against market performance.
*)

open Core

module Config = struct
  type t =
    { position_size : float  (** Fraction of equity to invest (0-1) *)
    }
  [@@deriving sexp]

  let default = { position_size = 0.95 }
end

type state =
  { config : Config.t
  ; bought : bool
  }
[@@deriving sexp]

let name = "buy-and-hold"

let initial_state =
  { config = Config.default
  ; bought = false
  }

let with_config config =
  { initial_state with config }

let on_candle state (ctx : Backtest.Strategy_intf.Context.t) =
  match state.bought with
  | true ->
    (* Already bought, just hold *)
    (Backtest.Strategy_intf.Signal.hold, state)
  | false ->
    (* First candle - buy *)
    let price = ctx.candle.close in
    let available = ctx.balance *. state.config.position_size in
    let qty = available /. price in
    let signal = Backtest.Strategy_intf.Signal.buy qty in
    (signal, { state with bought = true })

module Default : Backtest.Strategy_intf.S = struct
  let name = name
  type nonrec state = state
  let initial_state = initial_state
  let on_candle = on_candle
end

let () =
  Backtest.Strategy_intf.Registry.register
    ~name
    ~description:"Buy and hold: buy on first candle, hold forever (market baseline)"
    ~create:(fun () -> (module Default : Backtest.Strategy_intf.S))

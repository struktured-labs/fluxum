(** Bundled Trading Strategies

    This module provides access to all built-in trading strategies
    for use with the backtest harness.
*)

module Sma_crossover = Sma_crossover
module Buy_and_hold = Buy_and_hold
module Momentum = Momentum

(* Force initialization of strategy modules to trigger registration *)
let () =
  let _ = Sma_crossover.name in
  let _ = Buy_and_hold.name in
  let _ = Momentum.name in
  ()

(** List all available strategies *)
let list () = Backtest.Strategy_intf.Registry.list ()

(** Find a strategy by name *)
let find name = Backtest.Strategy_intf.Registry.find name

(** Get strategy module by name *)
let get name =
  match find name with
  | Some entry -> Some (entry.create ())
  | None -> None

(** Backtest engine - runs strategies against historical data *)

open Core
open Async

(** Engine configuration *)
module Config = struct
  type t =
    { initial_balance : float
    ; fill_model      : Fill_model.Config.t
    ; history_size    : int  (** Number of historical candles to provide to strategy *)
    }
  [@@deriving sexp]

  let default =
    { initial_balance = 10000.
    ; fill_model = Fill_model.Config.default
    ; history_size = 100
    }

  let create
      ?(initial_balance = 10000.)
      ?(slippage_pct = 0.001)
      ?(commission_pct = 0.001)
      ?(fill_on = `Close)
      ?(history_size = 100)
      ()
    =
    { initial_balance
    ; fill_model = { Fill_model.Config.slippage_pct; commission_pct; fill_on }
    ; history_size
    }
end

(** Internal state during backtest *)
module State = struct
  type t =
    { balance         : float
    ; position        : Strategy_intf.Position.t
    ; equity_curve    : (Time_ns.t * float) list  (** Reversed for efficient append *)
    ; trades          : Backtest_result.Trade_record.t list
    ; pending_entry   : pending_entry option
    ; history         : Candle.t list  (** Most recent first *)
    }

  and pending_entry =
    { entry_time  : Time_ns.t
    ; entry_price : float
    ; qty         : float
    ; side        : [ `Long | `Short ]
    }

  let create ~initial_balance =
    { balance = initial_balance
    ; position = Strategy_intf.Position.empty
    ; equity_curve = []
    ; trades = []
    ; pending_entry = None
    ; history = []
    }

  (** Current equity (balance + unrealized P&L) *)
  let equity t = t.balance +. t.position.unrealized_pnl

  (** Update unrealized P&L based on current price *)
  let update_unrealized_pnl t ~current_price =
    let position = t.position in
    match Float.(position.qty = 0.) with
    | true -> t
    | false ->
      let unrealized_pnl =
        match Float.(position.qty > 0.) with
        | true -> (current_price -. position.avg_price) *. position.qty
        | false -> (position.avg_price -. current_price) *. Float.abs position.qty
      in
      { t with position = { position with unrealized_pnl } }

  (** Record equity point *)
  let record_equity t ~timestamp =
    let point = (timestamp, equity t) in
    { t with equity_curve = point :: t.equity_curve }

  (** Add candle to history *)
  let add_to_history t ~candle ~max_size =
    let history = candle :: t.history in
    let history =
      match List.length history > max_size with
      | true -> List.take history max_size
      | false -> history
    in
    { t with history }
end

(** Process a fill and update state *)
let process_fill state (fill : Fill_model.Fill.t) ~symbol ~config =
  let position = state.State.position in

  match fill.side with
  | `Buy ->
    (* Opening or adding to long / closing short *)
    let new_balance = state.balance -. Fill_model.Fill.total fill in

    let new_position, trade_record =
      match Float.(position.qty < 0.) with
      | true ->
        (* Closing short position *)
        let close_qty = Float.min fill.qty (Float.abs position.qty) in
        let remaining_short = position.qty +. close_qty in

        let trade =
          match state.pending_entry with
          | Some entry ->
            Some (Backtest_result.Trade_record.create
                    ~entry_time:entry.entry_time
                    ~exit_time:fill.timestamp
                    ~symbol
                    ~side:`Short
                    ~entry_price:entry.entry_price
                    ~exit_price:fill.price
                    ~qty:close_qty
                    ~commission:(fill.commission +. (entry.entry_price *. close_qty *. config.Config.fill_model.commission_pct)))
          | None -> None
        in

        let new_pos =
          match Float.(remaining_short = 0.) with
          | true -> Strategy_intf.Position.empty
          | false -> { position with qty = remaining_short }
        in

        let pending =
          match Float.(fill.qty > close_qty) with
          | true ->
            (* Opened a long with the excess *)
            let long_qty = fill.qty -. close_qty in
            Some { State.entry_time = fill.timestamp
                 ; entry_price = fill.price
                 ; qty = long_qty
                 ; side = `Long }
          | false -> None
        in

        let new_pos =
          match pending with
          | Some p -> { Strategy_intf.Position.qty = p.qty; avg_price = p.entry_price; unrealized_pnl = 0. }
          | None -> new_pos
        in

        (new_pos, trade)

      | false ->
        (* Opening/adding to long *)
        let total_qty = position.qty +. fill.qty in
        let total_cost = (position.avg_price *. position.qty) +. (fill.price *. fill.qty) in
        let new_avg = match Float.(total_qty > 0.) with
          | true -> total_cost /. total_qty
          | false -> 0.
        in
        let pending =
          match Float.(position.qty = 0.) with
          | true ->
            Some { State.entry_time = fill.timestamp
                 ; entry_price = fill.price
                 ; qty = fill.qty
                 ; side = `Long }
          | false -> state.pending_entry
        in
        let new_pos = { Strategy_intf.Position.qty = total_qty; avg_price = new_avg; unrealized_pnl = 0. } in
        let _ = pending in  (* Will be used by caller to set pending_entry *)
        (new_pos, None)
    in

    let trades =
      match trade_record with
      | Some t -> t :: state.trades
      | None -> state.trades
    in

    let pending_entry =
      match Float.(new_position.qty > 0.) && Option.is_none state.pending_entry with
      | true ->
        Some { State.entry_time = fill.timestamp
             ; entry_price = fill.price
             ; qty = fill.qty
             ; side = `Long }
      | false ->
        match trade_record with
        | Some _ -> None
        | None -> state.pending_entry
    in

    { state with
      balance = new_balance
    ; position = new_position
    ; trades
    ; pending_entry
    }

  | `Sell ->
    (* Closing long / opening short *)
    let new_balance = state.balance +. (fill.price *. fill.qty) -. fill.commission in

    let new_position, trade_record =
      match Float.(position.qty > 0.) with
      | true ->
        (* Closing long position *)
        let close_qty = Float.min fill.qty position.qty in
        let remaining_long = position.qty -. close_qty in

        let trade =
          match state.pending_entry with
          | Some entry ->
            Some (Backtest_result.Trade_record.create
                    ~entry_time:entry.entry_time
                    ~exit_time:fill.timestamp
                    ~symbol
                    ~side:`Long
                    ~entry_price:entry.entry_price
                    ~exit_price:fill.price
                    ~qty:close_qty
                    ~commission:(fill.commission +. (entry.entry_price *. close_qty *. config.Config.fill_model.commission_pct)))
          | None -> None
        in

        let new_pos =
          match Float.(remaining_long = 0.) with
          | true -> Strategy_intf.Position.empty
          | false -> { position with qty = remaining_long }
        in

        (new_pos, trade)

      | false ->
        (* Opening/adding to short *)
        let total_qty = position.qty -. fill.qty in  (* More negative *)
        let total_cost =
          (position.avg_price *. Float.abs position.qty) +. (fill.price *. fill.qty)
        in
        let new_avg = match Float.(total_qty < 0.) with
          | true -> total_cost /. Float.abs total_qty
          | false -> 0.
        in
        let new_pos = { Strategy_intf.Position.qty = total_qty; avg_price = new_avg; unrealized_pnl = 0. } in
        (new_pos, None)
    in

    let trades =
      match trade_record with
      | Some t -> t :: state.trades
      | None -> state.trades
    in

    let pending_entry =
      match trade_record with
      | Some _ -> None
      | None ->
        match Float.(new_position.qty < 0.) && Option.is_none state.pending_entry with
        | true ->
          Some { State.entry_time = fill.timestamp
               ; entry_price = fill.price
               ; qty = Float.abs fill.qty
               ; side = `Short }
        | false -> state.pending_entry
    in

    { state with
      balance = new_balance
    ; position = new_position
    ; trades
    ; pending_entry
    }

(** Run backtest with a strategy module *)
let run
    (type s)
    (module Strategy : Strategy_intf.S with type state = s)
    ~config
    ~candles
    ()
  =
  match candles with
  | [] -> return (Error "No candles provided")
  | first :: _ ->
    let last = List.last_exn candles in
    let symbol = first.Candle.symbol in

    let initial_state = State.create ~initial_balance:config.Config.initial_balance in
    let strategy_state = ref Strategy.initial_state in

    (* Process each candle *)
    let final_state =
      List.foldi candles ~init:initial_state ~f:(fun idx state candle ->
        (* Update state with new candle *)
        let state = State.add_to_history state ~candle ~max_size:config.history_size in
        let state = State.update_unrealized_pnl state ~current_price:candle.close in

        (* Build context for strategy *)
        let ctx =
          { Strategy_intf.Context.candle
          ; position = state.position
          ; balance = state.balance
          ; equity = State.equity state
          ; candle_index = idx
          ; history = state.history
          }
        in

        (* Get signal from strategy *)
        let signal, new_strategy_state = Strategy.on_candle !strategy_state ctx in
        strategy_state := new_strategy_state;

        (* Process signal *)
        let state =
          match signal with
          | Strategy_intf.Signal.Hold -> state

          | Strategy_intf.Signal.Buy { qty; limit } ->
            let fill_result =
              match limit with
              | None -> Fill_model.fill_market ~config:config.fill_model ~side:`Buy ~qty candle
              | Some l -> Fill_model.fill_limit ~config:config.fill_model ~side:`Buy ~qty ~limit:l candle
            in
            (match fill_result with
             | `Filled fill -> process_fill state fill ~symbol ~config
             | `Pending | `Rejected _ -> state)

          | Strategy_intf.Signal.Sell { qty; limit } ->
            let fill_result =
              match limit with
              | None -> Fill_model.fill_market ~config:config.fill_model ~side:`Sell ~qty candle
              | Some l -> Fill_model.fill_limit ~config:config.fill_model ~side:`Sell ~qty ~limit:l candle
            in
            (match fill_result with
             | `Filled fill -> process_fill state fill ~symbol ~config
             | `Pending | `Rejected _ -> state)

          | Strategy_intf.Signal.Close_long { qty } ->
            let close_qty =
              match qty with
              | Some q -> Float.min q state.position.qty
              | None -> state.position.qty
            in
            (match Float.(close_qty > 0.) with
             | true ->
               let fill_result = Fill_model.fill_market ~config:config.fill_model ~side:`Sell ~qty:close_qty candle in
               (match fill_result with
                | `Filled fill -> process_fill state fill ~symbol ~config
                | `Pending | `Rejected _ -> state)
             | false -> state)

          | Strategy_intf.Signal.Close_short { qty } ->
            let close_qty =
              match qty with
              | Some q -> Float.min q (Float.abs state.position.qty)
              | None -> Float.abs state.position.qty
            in
            (match Float.(close_qty > 0. && state.position.qty < 0.) with
             | true ->
               let fill_result = Fill_model.fill_market ~config:config.fill_model ~side:`Buy ~qty:close_qty candle in
               (match fill_result with
                | `Filled fill -> process_fill state fill ~symbol ~config
                | `Pending | `Rejected _ -> state)
             | false -> state)
        in

        (* Record equity curve point *)
        State.record_equity state ~timestamp:candle.timestamp)
    in

    (* Close any open position at the end *)
    let final_state =
      match Float.(final_state.position.qty <> 0.) with
      | false -> final_state
      | true ->
        let last_candle = last in
        let side = match Float.(final_state.position.qty > 0.) with
          | true -> `Long
          | false -> `Short
        in
        let qty = Float.abs final_state.position.qty in
        let fill_result = Fill_model.simulate_close ~config:config.fill_model ~side ~qty last_candle in
        match fill_result with
        | `Filled fill -> process_fill final_state fill ~symbol ~config
        | _ -> final_state
    in

    let result = Backtest_result.create
        ~strategy_name:Strategy.name
        ~symbol
        ~start_time:first.timestamp
        ~end_time:last.timestamp
        ~initial_balance:config.initial_balance
        ~final_balance:(State.equity final_state)
        ~trades:(List.rev final_state.trades)
        ~equity_curve:(List.rev final_state.equity_curve)
        ~config:{ Backtest_result.slippage_pct = config.fill_model.slippage_pct
                ; commission_pct = config.fill_model.commission_pct
                ; fill_on = config.fill_model.fill_on }
        ()
    in

    return (Ok result)

(** Run backtest from first-class module *)
let run_packed ~strategy ~config ~candles () =
  let (module S : Strategy_intf.S) = strategy in
  run (module S) ~config ~candles ()

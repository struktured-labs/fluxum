(** Performance metrics for backtest results *)

open Core

type t =
  { total_return        : float  (** (final - initial) / initial *)
  ; annualized_return   : float  (** Annualized total return *)
  ; sharpe_ratio        : float  (** (return - risk_free) / std_dev *)
  ; sortino_ratio       : float  (** (return - risk_free) / downside_dev *)
  ; max_drawdown        : float  (** Largest peak-to-trough decline (as positive %) *)
  ; max_drawdown_duration_days : float (** Days from peak to trough *)
  ; win_rate            : float  (** winning_trades / total_trades *)
  ; profit_factor       : float  (** gross_profit / gross_loss *)
  ; avg_trade_pnl       : float  (** Average P&L per trade *)
  ; avg_winner          : float  (** Average winning trade P&L *)
  ; avg_loser           : float  (** Average losing trade P&L (negative) *)
  ; expectancy          : float  (** Expected value per trade *)
  ; total_trades        : int
  ; winning_trades      : int
  ; losing_trades       : int
  }
[@@deriving sexp, compare, equal]

(** Risk-free rate assumption (annual, e.g., 0.05 = 5%) *)
let default_risk_free_rate = 0.0

(** Days per year for annualization *)
let days_per_year = 365.25

(** Calculate standard deviation of returns *)
let std_dev returns =
  match returns with
  | [] -> 0.
  | _ ->
    let n = Float.of_int (List.length returns) in
    let mean = List.fold returns ~init:0. ~f:(+.) /. n in
    let variance =
      List.fold returns ~init:0. ~f:(fun acc r ->
        acc +. Float.square (r -. mean)) /. n
    in
    Float.sqrt variance

(** Calculate downside deviation (only negative returns) *)
let downside_deviation returns ~target =
  let negative_deviations =
    List.filter_map returns ~f:(fun r ->
      match Float.(r < target) with
      | true -> Some (Float.square (r -. target))
      | false -> None)
  in
  match negative_deviations with
  | [] -> 0.
  | deviations ->
    let n = Float.of_int (List.length deviations) in
    Float.sqrt (List.fold deviations ~init:0. ~f:(+.) /. n)

(** Calculate max drawdown from equity curve *)
let calculate_max_drawdown equity_curve =
  match equity_curve with
  | [] -> 0., 0.
  | _ ->
    let peak = ref Float.neg_infinity in
    let max_dd = ref 0. in
    let peak_time = ref Time_ns.epoch in
    let trough_time = ref Time_ns.epoch in
    let max_dd_start = ref Time_ns.epoch in
    let max_dd_end = ref Time_ns.epoch in

    List.iter equity_curve ~f:(fun (ts, value) ->
      match Float.(value > !peak) with
      | true ->
        peak := value;
        peak_time := ts
      | false ->
        let dd = (!peak -. value) /. !peak in
        match Float.(dd > !max_dd) with
        | true ->
          max_dd := dd;
          max_dd_start := !peak_time;
          max_dd_end := ts;
          trough_time := ts
        | false -> ());

    let duration_days =
      let span = Time_ns.diff !max_dd_end !max_dd_start in
      Time_ns.Span.to_day span
    in
    !max_dd, duration_days

(** Calculate daily returns from equity curve *)
let daily_returns equity_curve =
  match equity_curve with
  | [] | [_] -> []
  | first :: rest ->
    let _, returns =
      List.fold rest ~init:(snd first, []) ~f:(fun (prev_value, acc) (_, value) ->
        let ret = (value -. prev_value) /. prev_value in
        (value, ret :: acc))
    in
    List.rev returns

(** Calculate metrics from equity curve and trade P&Ls *)
let calculate
    ~equity_curve
    ~trade_pnls
    ?(risk_free_rate = default_risk_free_rate)
    ()
  =
  (* Basic trade statistics *)
  let total_trades = List.length trade_pnls in
  let winning_pnls = List.filter trade_pnls ~f:(fun pnl -> Float.(pnl > 0.)) in
  let losing_pnls = List.filter trade_pnls ~f:(fun pnl -> Float.(pnl < 0.)) in
  let winning_trades = List.length winning_pnls in
  let losing_trades = List.length losing_pnls in

  (* Returns *)
  let total_return =
    match equity_curve with
    | [] -> 0.
    | [(_, _)] -> 0.
    | first :: _ ->
      let last = List.last_exn equity_curve in
      (snd last -. snd first) /. snd first
  in

  (* Duration in days *)
  let duration_days =
    match equity_curve with
    | [] | [_] -> 1.
    | first :: _ ->
      let last = List.last_exn equity_curve in
      let span = Time_ns.diff (fst last) (fst first) in
      Float.max 1. (Time_ns.Span.to_day span)
  in

  (* Annualized return *)
  let annualized_return =
    let years = duration_days /. days_per_year in
    match Float.(years > 0.) with
    | true -> Float.((1. + total_return) ** (1. / years) - 1.)
    | false -> total_return
  in

  (* Sharpe ratio (annualized) *)
  let returns = daily_returns equity_curve in
  let daily_std = std_dev returns in
  let annual_std = daily_std *. Float.sqrt days_per_year in
  let sharpe_ratio =
    match Float.(annual_std > 0.) with
    | true -> (annualized_return -. risk_free_rate) /. annual_std
    | false -> 0.
  in

  (* Sortino ratio *)
  let daily_downside = downside_deviation returns ~target:0. in
  let annual_downside = daily_downside *. Float.sqrt days_per_year in
  let sortino_ratio =
    match Float.(annual_downside > 0.) with
    | true -> (annualized_return -. risk_free_rate) /. annual_downside
    | false -> 0.
  in

  (* Max drawdown *)
  let max_drawdown, max_drawdown_duration_days = calculate_max_drawdown equity_curve in

  (* Win rate *)
  let win_rate =
    match total_trades > 0 with
    | true -> Float.of_int winning_trades /. Float.of_int total_trades
    | false -> 0.
  in

  (* Profit factor *)
  let gross_profit = List.fold winning_pnls ~init:0. ~f:(+.) in
  let gross_loss = Float.abs (List.fold losing_pnls ~init:0. ~f:(+.)) in
  let profit_factor =
    match Float.(gross_loss > 0.) with
    | true -> gross_profit /. gross_loss
    | false -> match Float.(gross_profit > 0.) with
      | true -> Float.infinity
      | false -> 0.
  in

  (* Average P&L *)
  let avg_trade_pnl =
    match total_trades > 0 with
    | true -> List.fold trade_pnls ~init:0. ~f:(+.) /. Float.of_int total_trades
    | false -> 0.
  in

  let avg_winner =
    match winning_trades > 0 with
    | true -> gross_profit /. Float.of_int winning_trades
    | false -> 0.
  in

  let avg_loser =
    match losing_trades > 0 with
    | true -> List.fold losing_pnls ~init:0. ~f:(+.) /. Float.of_int losing_trades
    | false -> 0.
  in

  (* Expectancy: win_rate * avg_winner + loss_rate * avg_loser *)
  let loss_rate = 1. -. win_rate in
  let expectancy = (win_rate *. avg_winner) +. (loss_rate *. avg_loser) in

  { total_return
  ; annualized_return
  ; sharpe_ratio
  ; sortino_ratio
  ; max_drawdown
  ; max_drawdown_duration_days
  ; win_rate
  ; profit_factor
  ; avg_trade_pnl
  ; avg_winner
  ; avg_loser
  ; expectancy
  ; total_trades
  ; winning_trades
  ; losing_trades
  }

(** Create empty metrics *)
let empty =
  { total_return = 0.
  ; annualized_return = 0.
  ; sharpe_ratio = 0.
  ; sortino_ratio = 0.
  ; max_drawdown = 0.
  ; max_drawdown_duration_days = 0.
  ; win_rate = 0.
  ; profit_factor = 0.
  ; avg_trade_pnl = 0.
  ; avg_winner = 0.
  ; avg_loser = 0.
  ; expectancy = 0.
  ; total_trades = 0
  ; winning_trades = 0
  ; losing_trades = 0
  }

(** Convert to Yojson *)
let to_yojson t =
  `Assoc
    [ ("total_return", `Float t.total_return)
    ; ("annualized_return", `Float t.annualized_return)
    ; ("sharpe_ratio", `Float t.sharpe_ratio)
    ; ("sortino_ratio", `Float t.sortino_ratio)
    ; ("max_drawdown", `Float t.max_drawdown)
    ; ("max_drawdown_duration_days", `Float t.max_drawdown_duration_days)
    ; ("win_rate", `Float t.win_rate)
    ; ("profit_factor", `Float t.profit_factor)
    ; ("avg_trade_pnl", `Float t.avg_trade_pnl)
    ; ("avg_winner", `Float t.avg_winner)
    ; ("avg_loser", `Float t.avg_loser)
    ; ("expectancy", `Float t.expectancy)
    ; ("total_trades", `Int t.total_trades)
    ; ("winning_trades", `Int t.winning_trades)
    ; ("losing_trades", `Int t.losing_trades)
    ]

(** Format metrics as a human-readable string *)
let to_string t =
  sprintf
    {|Performance Metrics:
  Total Return:       %.2f%%
  Annualized Return:  %.2f%%
  Sharpe Ratio:       %.2f
  Sortino Ratio:      %.2f
  Max Drawdown:       %.2f%% (%.1f days)
  Win Rate:           %.1f%% (%d/%d)
  Profit Factor:      %.2f
  Avg Trade P&L:      $%.2f
  Avg Winner:         $%.2f
  Avg Loser:          $%.2f
  Expectancy:         $%.2f|}
    (t.total_return *. 100.)
    (t.annualized_return *. 100.)
    t.sharpe_ratio
    t.sortino_ratio
    (t.max_drawdown *. 100.)
    t.max_drawdown_duration_days
    (t.win_rate *. 100.)
    t.winning_trades
    t.total_trades
    t.profit_factor
    t.avg_trade_pnl
    t.avg_winner
    t.avg_loser
    t.expectancy

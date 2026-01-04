(** Backtest result type *)

open Core

(** A completed trade with P&L *)
module Trade_record = struct
  type t =
    { entry_time    : Time_ns_unix.t
    ; exit_time     : Time_ns_unix.t
    ; symbol        : string
    ; side          : [ `Long | `Short ]
    ; entry_price   : float
    ; exit_price    : float
    ; qty           : float
    ; pnl           : float
    ; pnl_pct       : float
    ; commission    : float
    }
  [@@deriving sexp, compare, equal, fields]

  let create ~entry_time ~exit_time ~symbol ~side ~entry_price ~exit_price ~qty ~commission =
    let gross_pnl =
      match side with
      | `Long -> (exit_price -. entry_price) *. qty
      | `Short -> (entry_price -. exit_price) *. qty
    in
    let pnl = gross_pnl -. commission in
    let pnl_pct = pnl /. (entry_price *. qty) in
    { entry_time; exit_time; symbol; side; entry_price; exit_price; qty; pnl; pnl_pct; commission }

  let to_yojson t =
    `Assoc
      [ ("entry_time", `String (Time_ns.to_string_utc t.entry_time))
      ; ("exit_time", `String (Time_ns.to_string_utc t.exit_time))
      ; ("symbol", `String t.symbol)
      ; ("side", `String (match t.side with `Long -> "long" | `Short -> "short"))
      ; ("entry_price", `Float t.entry_price)
      ; ("exit_price", `Float t.exit_price)
      ; ("qty", `Float t.qty)
      ; ("pnl", `Float t.pnl)
      ; ("pnl_pct", `Float t.pnl_pct)
      ; ("commission", `Float t.commission)
      ]
end

(** Backtest result *)
type t =
  { strategy_name   : string
  ; symbol          : string
  ; start_time      : Time_ns_unix.t
  ; end_time        : Time_ns_unix.t
  ; initial_balance : float
  ; final_balance   : float
  ; metrics         : Metrics.t
  ; trades          : Trade_record.t list
  ; equity_curve    : (Time_ns_unix.t * float) list
  ; config          : config
  }

and config =
  { slippage_pct    : float
  ; commission_pct  : float
  ; fill_on         : [ `Open | `Close | `Midpoint ]
  }
[@@deriving sexp, fields]

let config_to_yojson c =
  `Assoc
    [ ("slippage_pct", `Float c.slippage_pct)
    ; ("commission_pct", `Float c.commission_pct)
    ; ("fill_on", `String (match c.fill_on with
        | `Open -> "open"
        | `Close -> "close"
        | `Midpoint -> "midpoint"))
    ]

let create
    ~strategy_name
    ~symbol
    ~start_time
    ~end_time
    ~initial_balance
    ~final_balance
    ~trades
    ~equity_curve
    ~config
    ()
  =
  let trade_pnls = List.map trades ~f:Trade_record.pnl in
  let metrics = Metrics.calculate ~equity_curve ~trade_pnls () in
  { strategy_name
  ; symbol
  ; start_time
  ; end_time
  ; initial_balance
  ; final_balance
  ; metrics
  ; trades
  ; equity_curve
  ; config
  }

(** Net P&L *)
let net_pnl t = t.final_balance -. t.initial_balance

(** Total return as percentage *)
let total_return_pct t = (t.final_balance -. t.initial_balance) /. t.initial_balance *. 100.

(** Convert to Yojson *)
let to_yojson t =
  `Assoc
    [ ("strategy_name", `String t.strategy_name)
    ; ("symbol", `String t.symbol)
    ; ("start_time", `String (Time_ns.to_string_utc t.start_time))
    ; ("end_time", `String (Time_ns.to_string_utc t.end_time))
    ; ("initial_balance", `Float t.initial_balance)
    ; ("final_balance", `Float t.final_balance)
    ; ("net_pnl", `Float (net_pnl t))
    ; ("total_return_pct", `Float (total_return_pct t))
    ; ("metrics", Metrics.to_yojson t.metrics)
    ; ("trades", `List (List.map t.trades ~f:Trade_record.to_yojson))
    ; ("config", config_to_yojson t.config)
    ]

(** Format as human-readable string *)
let to_string t =
  sprintf
    {|=== Backtest Result ===
Strategy:         %s
Symbol:           %s
Period:           %s to %s
Initial Balance:  $%.2f
Final Balance:    $%.2f
Net P&L:          $%.2f (%.2f%%)

%s

Total Trades: %d|}
    t.strategy_name
    t.symbol
    (Time_ns.to_string_utc t.start_time)
    (Time_ns.to_string_utc t.end_time)
    t.initial_balance
    t.final_balance
    (net_pnl t)
    (total_return_pct t)
    (Metrics.to_string t.metrics)
    (List.length t.trades)

(** Print summary to stdout *)
let print_summary t =
  print_endline (to_string t)

(** Save equity curve to CSV *)
let save_equity_curve_csv t ~path =
  let oc = Out_channel.create path in
  Out_channel.output_string oc "timestamp,equity\n";
  List.iter t.equity_curve ~f:(fun (ts, equity) ->
    Out_channel.fprintf oc "%s,%.2f\n" (Time_ns.to_string_utc ts) equity);
  Out_channel.close oc

(** Save trades to CSV *)
let save_trades_csv t ~path =
  let oc = Out_channel.create path in
  Out_channel.output_string oc "entry_time,exit_time,symbol,side,entry_price,exit_price,qty,pnl,pnl_pct,commission\n";
  List.iter t.trades ~f:(fun trade ->
    Out_channel.fprintf oc "%s,%s,%s,%s,%.8f,%.8f,%.8f,%.2f,%.4f,%.2f\n"
      (Time_ns.to_string_utc trade.entry_time)
      (Time_ns.to_string_utc trade.exit_time)
      trade.symbol
      (match trade.side with `Long -> "long" | `Short -> "short")
      trade.entry_price
      trade.exit_price
      trade.qty
      trade.pnl
      trade.pnl_pct
      trade.commission);
  Out_channel.close oc

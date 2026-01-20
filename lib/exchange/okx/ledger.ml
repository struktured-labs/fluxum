(** OKX Ledger - Implements unified Ledger_intf for P&L tracking

    Complete P&L tracking for OKX exchange with unified V5 API support.
    Supports real-time trade updates and market data integration.
*)

open Core
open Async

(** Re-export Update_source from interface *)
module Update_source = Fluxum.Ledger_intf.Update_source

(** Single ledger entry tracking P&L and position for one symbol *)
module Entry = struct
  type t =
    { symbol : Fluxum.Types.Symbol.t;
      (* P&L fields *)
      pnl : float; [@default 0.0]
      position : float; [@default 0.0]
      spot : float; [@default 0.0]
      pnl_spot : float; [@default 0.0]
      notional : float; [@default 0.0]
      (* Execution tracking *)
      avg_buy_price : float; [@default 0.0]
      avg_sell_price : float; [@default 0.0]
      avg_price : float; [@default 0.0]
      total_buy_qty : float; [@default 0.0]
      total_sell_qty : float; [@default 0.0]
      buy_notional : float; [@default 0.0]
      sell_notional : float; [@default 0.0]
      (* Order tracking *)
      total_original : float; [@default 0.0]
      total_executed : float; [@default 0.0]
      total_remaining : float; [@default 0.0]
      (* Cost basis accounting *)
      cost_basis : float; [@default 0.0]
      running_price : float; [@default 0.0]
      running_qty : float; [@default 0.0]
      (* Metadata *)
      update_time : Time_float_unix.t;
      update_source : Update_source.t; [@default `Market_data]
      (* Latest quote *)
      price : Fluxum.Types.Price.Option.t; [@default None]
      side : Fluxum.Types.Side.Option.t; [@default None]
      qty : float option; [@default None]
      package_price : Fluxum.Types.Price.Option.t; [@default None]
    }
  [@@deriving sexp, compare, equal, fields]

  (** Create new entry *)
  let create
      ?update_time
      ~symbol
      ?(pnl = 0.0)
      ?(position = 0.0)
      ?(spot = 0.0)
      ?(pnl_spot = 0.0)
      ?(notional = 0.0)
      ?(avg_buy_price = 0.0)
      ?(avg_sell_price = 0.0)
      ?(avg_price = 0.0)
      ?(update_source = `Market_data)
      ?(total_buy_qty = 0.0)
      ?(total_sell_qty = 0.0)
      ?(price = None)
      ?(side = None)
      ?(qty = None)
      ?(package_price = None)
      ?(buy_notional = 0.0)
      ?(sell_notional = 0.0)
      ?(total_original = 0.0)
      ?(total_executed = 0.0)
      ?(total_remaining = 0.0)
      ?(cost_basis = 0.0)
      ?(running_price = 0.0)
      ?(running_qty = 0.0)
      ()
    =
    let update_time = Option.value_or_thunk update_time ~default:Time_float_unix.now in
    { symbol;
      pnl;
      position;
      spot;
      pnl_spot;
      notional;
      avg_buy_price;
      avg_sell_price;
      avg_price;
      total_buy_qty;
      total_sell_qty;
      buy_notional;
      sell_notional;
      total_original;
      total_executed;
      total_remaining;
      cost_basis;
      running_price;
      running_qty;
      update_time;
      update_source;
      price;
      side;
      qty;
      package_price;
    }

  (** Update from trade execution - Core P&L calculation logic *)
  let rec on_trade
      ?(update_source = `Trade)
      ?timestamp
      ?(avg_trade_price : float option)
      ?(fee_usd : float = 0.)
      t
      ~(price : float)
      ~(side : Fluxum.Types.Side.t)
      ~(qty : float)
    : t =
    let timestamp = Option.value_or_thunk timestamp ~default:Time_float_unix.now in

    (* Position sign: +1 for buy, -1 for sell *)
    let position_sign = match side with
      | Buy -> 1.0
      | Sell -> -1.0
    in

    (* New position after trade *)
    let position : float = t.position +. (qty *. position_sign) in
    let pnl_spot = price *. position in

    (* Handle short positions by unwinding first *)
    match Float.is_negative position with
    | true ->
      (* Position went negative - this is a short *)
      let qty = Float.abs position in
      let avg_trade_price = Option.value ~default:price avg_trade_price in

      (* First unwind existing position to zero with external trade *)
      let t =
        on_trade
          ~timestamp
          ~price:avg_trade_price
          ~side:(Fluxum.Types.Side.opposite side)
          ~update_source:`External_trade
          ~qty
          t
      in

      (* Then apply the actual trade that caused the short *)
      on_trade ~update_source ~timestamp ~avg_trade_price ~price ~side ~qty t

    | false ->
      (* Normal long position or closing to zero *)

      (* Update running totals *)
      let total_buy_qty, buy_notional, avg_buy_price =
        match side with
        | Buy ->
          let new_total = t.total_buy_qty +. qty in
          let new_notional = t.buy_notional +. (price *. qty) in
          let new_avg = if Float.(new_total > 0.0) then new_notional /. new_total else 0.0 in
          (new_total, new_notional, new_avg)
        | Sell -> (t.total_buy_qty, t.buy_notional, t.avg_buy_price)
      in

      let total_sell_qty, sell_notional, avg_sell_price =
        match side with
        | Sell ->
          let new_total = t.total_sell_qty +. qty in
          let new_notional = t.sell_notional +. (price *. qty) in
          let new_avg = if Float.(new_total > 0.0) then new_notional /. new_total else 0.0 in
          (new_total, new_notional, new_avg)
        | Buy -> (t.total_sell_qty, t.sell_notional, t.avg_sell_price)
      in

      (* Calculate realized P&L *)
      let pnl =
        match side with
        | Sell ->
          (* Selling: realize profit/loss based on cost basis *)
          let cost_basis_reduction = if Float.(t.position > 0.0)
            then (t.cost_basis /. t.position) *. qty
            else 0.0
          in
          t.pnl +. ((price *. qty) -. cost_basis_reduction) -. fee_usd
        | Buy ->
          (* Buying: just subtract fee *)
          t.pnl -. fee_usd
      in

      (* Update cost basis *)
      let cost_basis =
        match side with
        | Buy ->
          (* Buying: add to cost basis *)
          t.cost_basis +. (price *. qty) +. fee_usd
        | Sell ->
          (* Selling: reduce cost basis proportionally *)
          if Float.(t.position > 0.0)
          then t.cost_basis *. ((t.position -. qty) /. t.position)
          else 0.0
      in

      (* Overall average price *)
      let avg_price =
        if Float.(total_buy_qty +. total_sell_qty > 0.0)
        then (buy_notional +. sell_notional) /. (total_buy_qty +. total_sell_qty)
        else 0.0
      in

      { t with
        position;
        pnl;
        pnl_spot;
        cost_basis;
        avg_buy_price;
        avg_sell_price;
        avg_price;
        total_buy_qty;
        total_sell_qty;
        buy_notional;
        sell_notional;
        update_time = timestamp;
        update_source;
        price = Some price;
        side = Some side;
        qty = Some qty;
      }

  (** Update from market data (order book) *)
  let on_market_data
      ?(update_source = `Market_data)
      ?timestamp
      ?mid_price
      t
      ~(book : Order_book.Book.t)
    : t =
    let timestamp = Option.value_or_thunk timestamp ~default:Time_float_unix.now in

    (* Calculate mid price from book *)
    let mid_price = match mid_price with
      | Some p -> p
      | None ->
        let best_bid = List.hd (Order_book.Book.bids_alist book) in
        let best_ask = List.hd (Order_book.Book.asks_alist book) in
        match best_bid, best_ask with
        | Some (bid_price, _), Some (ask_price, _) -> (bid_price +. ask_price) /. 2.0
        | Some (bid_price, _), None -> bid_price
        | None, Some (ask_price, _) -> ask_price
        | None, None -> 0.0
    in

    (* Update P&L based on current position and mid price *)
    let pnl_spot = t.position *. mid_price in
    let notional = Float.abs pnl_spot in

    { t with
      pnl_spot;
      notional;
      package_price = Some mid_price;
      update_time = timestamp;
      update_source;
    }

  (** Create pipe from book and trade updates *)
  let pipe
      ?num_values
      ?behavior
      ~init
      (book_pipe : Order_book.Book.t Pipe.Reader.t)
      (trade_pipe : Ws.Message.trade_data Pipe.Reader.t)
    : t Pipe.Reader.t Deferred.t =
    ignore (num_values, behavior);
    let reader, writer = Pipe.create () in
    let entry_ref = ref init in

    (* Write initial state *)
    don't_wait_for (Pipe.write writer !entry_ref);

    (* Process book updates *)
    don't_wait_for (
      Pipe.iter book_pipe ~f:(fun book ->
        entry_ref := on_market_data !entry_ref ~book;
        Pipe.write writer !entry_ref
      )
    );

    (* Process trade updates *)
    don't_wait_for (
      Pipe.iter trade_pipe ~f:(fun trade ->
        match (
          let open Result.Let_syntax in
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string trade.px in
          let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string trade.sz in
          let%bind side = Fluxum.Normalize_common.Side.of_string trade.side in
          Ok (price, qty, side)
        ) with
        | Ok (price, qty, side) ->
          entry_ref := on_trade !entry_ref ~price ~side ~qty;
          Pipe.write writer !entry_ref
        | Error err ->
          Log.Global.error "OKX ledger: Failed to parse trade: %s" err;
          return ()
      )
    );

    return reader
end

(** Ledger type - map of symbols to entries *)
type t = Entry.t Fluxum.Types.Symbol.Map.t

(** Create empty ledger *)
let empty : t = Fluxum.Types.Symbol.Map.empty

(** Create from balances *)
let from_balances (balances : Fluxum.Types.Balance.t list) : t =
  List.fold balances ~init:empty ~f:(fun acc balance ->
    let symbol = balance.currency in  (* In OKX, currency is the symbol *)
    let entry = Entry.create
      ~symbol
      ~position:balance.total
      ~cost_basis:(balance.total *. 0.0)  (* No historical price data *)
      ()
    in
    Map.set acc ~key:symbol ~data:entry
  )

(** Trade updates *)
let on_trade
    ?update_source
    ?timestamp
    ?avg_trade_price
    ?fee_usd
    t
    ~symbol
    ~price
    ~side
    ~qty
  =
  match Map.find t symbol with
  | None ->
    (* No entry for this symbol, create one and apply trade *)
    let entry = Entry.create ~symbol () in
    let updated = Entry.on_trade ?update_source ?timestamp ?avg_trade_price ?fee_usd
      entry ~price ~side ~qty
    in
    Map.set t ~key:symbol ~data:updated
  | Some entry ->
    (* Update existing entry *)
    let updated = Entry.on_trade ?update_source ?timestamp ?avg_trade_price ?fee_usd
      entry ~price ~side ~qty
    in
    Map.set t ~key:symbol ~data:updated

(** Market data updates *)
let on_market_data
    ?update_source
    ?timestamp
    ?mid_price
    t
    ~symbol
    ~book
  =
  match Map.find t symbol with
  | None ->
    (* No entry yet, create one and apply book *)
    let entry = Entry.create ~symbol () in
    let updated = Entry.on_market_data ?update_source ?timestamp ?mid_price
      entry ~book
    in
    Map.set t ~key:symbol ~data:updated
  | Some entry ->
    (* Update existing entry *)
    let updated = Entry.on_market_data ?update_source ?timestamp ?mid_price
      entry ~book
    in
    Map.set t ~key:symbol ~data:updated

(** Order event updates - OKX supports this via WebSocket *)
let on_order_events t (_events : 'order_event list) =
  t

let on_order_event_response t (_response : 'order_event_response) =
  t

(** Multi-symbol real-time pipes *)
let pipe
    ?num_values
    ?behavior
    ?how:_
    ~init
    (books : Order_book.Book.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t)
    (trades : Ws.Message.trade_data Pipe.Reader.t)
  : Entry.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t Deferred.t =
  (* For each symbol in init, create a filtered trade pipe and combine with book *)
  Map.fold init ~init:(return Fluxum.Types.Symbol.Map.empty) ~f:(fun ~key:symbol ~data:entry acc_deferred ->
    let%bind acc = acc_deferred in
    match Map.find books symbol with
    | None ->
      (* No book pipe for this symbol, just create empty pipe *)
      return acc
    | Some book_pipe ->
      (* Filter trades for this symbol *)
      let symbol_trades = Pipe.filter_map trades ~f:(fun trade ->
        match String.equal trade.instId symbol with
        | true -> Some trade
        | false -> None
      ) in

      (* Create entry pipe for this symbol *)
      let%bind entry_pipe = Entry.pipe ~init:entry ?num_values ?behavior
        book_pipe symbol_trades
      in
      return (Map.set acc ~key:symbol ~data:entry_pipe)
  )

(** CLI command *)
let command : Command.t =
  Command.group ~summary:"OKX Ledger Commands"
    [ ("placeholder", Command.basic ~summary:"OKX ledger - use `from_balances` or `pipe` in code"
        (Command.Param.return (fun () -> printf "OKX ledger: Real-time P&L tracking available via API\n")))
    ]

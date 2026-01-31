(** Uniswap V3 Ledger - P&L tracking for DEX trades

    Follows the Ledger_intf.ENTRY pattern with 24-field entries.
*)

open Core

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
    { symbol; pnl; position; spot; pnl_spot; notional;
      avg_buy_price; avg_sell_price; avg_price;
      total_buy_qty; total_sell_qty; buy_notional; sell_notional;
      total_original; total_executed; total_remaining;
      cost_basis; running_price; running_qty;
      update_time; update_source; price; side; qty; package_price }

  let on_trade ?timestamp t ~price ~side ~qty =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let notional_delta = price *. qty in
    match side with
    | Fluxum.Types.Side.Buy ->
      let new_buy_qty = t.total_buy_qty +. qty in
      let new_buy_notional = t.buy_notional +. notional_delta in
      let new_avg_buy = match Float.(new_buy_qty > 0.) with
        | true -> new_buy_notional /. new_buy_qty
        | false -> 0.0
      in
      let new_position = t.position +. qty in
      let new_cost_basis = t.cost_basis +. notional_delta in
      { t with
        position = new_position;
        total_buy_qty = new_buy_qty;
        buy_notional = new_buy_notional;
        avg_buy_price = new_avg_buy;
        total_executed = t.total_executed +. qty;
        cost_basis = new_cost_basis;
        price = Some price;
        side = Some side;
        qty = Some qty;
        update_time;
        update_source = `Trade;
      }
    | Fluxum.Types.Side.Sell ->
      let new_sell_qty = t.total_sell_qty +. qty in
      let new_sell_notional = t.sell_notional +. notional_delta in
      let new_avg_sell = match Float.(new_sell_qty > 0.) with
        | true -> new_sell_notional /. new_sell_qty
        | false -> 0.0
      in
      let new_position = t.position -. qty in
      let realized = match Float.(t.avg_buy_price > 0.) with
        | true -> (price -. t.avg_buy_price) *. qty
        | false -> 0.0
      in
      { t with
        position = new_position;
        total_sell_qty = new_sell_qty;
        sell_notional = new_sell_notional;
        avg_sell_price = new_avg_sell;
        total_executed = t.total_executed +. qty;
        pnl = t.pnl +. realized;
        price = Some price;
        side = Some side;
        qty = Some qty;
        update_time;
        update_source = `Trade;
      }

  let update_spot ?timestamp t current_price =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let unrealized = match Float.(t.position <> 0. && t.avg_buy_price > 0.) with
      | true -> (current_price -. t.avg_buy_price) *. t.position
      | false -> 0.0
    in
    { t with
      spot = current_price;
      pnl_spot = unrealized;
      notional = t.position *. current_price;
      update_time;
      update_source = `Market_data;
    }

  let update_from_book t _book = t
end

(** Bitrue Ledger - Implements unified Ledger_intf for P&L tracking

    Placeholder P&L tracking for Bitrue exchange.
    Full implementation requires additional REST endpoints (myTrades, etc.).

    TODO: Implement full trade tracking when REST API is complete.
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

  (** Create empty entry for symbol *)
  let empty ~symbol () =
    create ~symbol ()

  (** Update with new market data *)
  let with_market_data t ~price ~qty ~side =
    { t with
      price = Some price;
      qty = Some qty;
      side = Some side;
      update_time = Time_float_unix.now ();
      update_source = `Market_data;
    }

  (** Update PNL based on current price *)
  let recalculate_pnl t ~current_price =
    let pnl = match Float.(t.position <> 0.0 && t.avg_price <> 0.0) with
      | true -> (current_price -. t.avg_price) *. t.position
      | false -> 0.0
    in
    { t with pnl; update_time = Time_float_unix.now () }
end

(** Ledger manages entries for all symbols *)
module Ledger = struct
  type t =
    { entries : Entry.t String.Map.t
    ; mutable state : Fluxum.Session_intf.State.t
    }

  let create () =
    { entries = String.Map.empty
    ; state = Disconnected
    }

  let get_entry t ~symbol =
    Map.find t.entries symbol

  let get_or_create_entry t ~symbol =
    match Map.find t.entries symbol with
    | Some entry -> entry
    | None -> Entry.empty ~symbol ()

  let set_entry t entry =
    { t with entries = Map.set t.entries ~key:entry.Entry.symbol ~data:entry }

  let all_entries t =
    Map.data t.entries

  let state t = t.state

  let set_state t state =
    t.state <- state
end

(** Bitstamp Ledger - P&L tracking for Bitstamp exchange *)

open Core

module Update_source = Fluxum.Ledger_intf.Update_source

module Entry = struct
  type t =
    { symbol : Fluxum.Types.Symbol.t;
      pnl : float; [@default 0.0]
      position : float; [@default 0.0]
      spot : float; [@default 0.0]
      pnl_spot : float; [@default 0.0]
      notional : float; [@default 0.0]
      avg_buy_price : float; [@default 0.0]
      avg_sell_price : float; [@default 0.0]
      avg_price : float; [@default 0.0]
      total_buy_qty : float; [@default 0.0]
      total_sell_qty : float; [@default 0.0]
      buy_notional : float; [@default 0.0]
      sell_notional : float; [@default 0.0]
      total_original : float; [@default 0.0]
      total_executed : float; [@default 0.0]
      total_remaining : float; [@default 0.0]
      cost_basis : float; [@default 0.0]
      running_price : float; [@default 0.0]
      running_qty : float; [@default 0.0]
      update_time : Time_float_unix.t;
      update_source : Update_source.t; [@default `Market_data]
      price : Fluxum.Types.Price.Option.t; [@default None]
      side : Fluxum.Types.Side.Option.t; [@default None]
      qty : float option; [@default None]
      package_price : Fluxum.Types.Price.Option.t; [@default None]
    }
  [@@deriving sexp, compare, equal, fields]

  let create ~symbol () =
    { symbol;
      pnl = 0.0; position = 0.0; spot = 0.0; pnl_spot = 0.0;
      notional = 0.0; avg_buy_price = 0.0; avg_sell_price = 0.0;
      avg_price = 0.0; total_buy_qty = 0.0; total_sell_qty = 0.0;
      buy_notional = 0.0; sell_notional = 0.0;
      total_original = 0.0; total_executed = 0.0; total_remaining = 0.0;
      cost_basis = 0.0; running_price = 0.0; running_qty = 0.0;
      update_time = Time_float_unix.now ();
      update_source = `Market_data;
      price = None; side = None; qty = None; package_price = None;
    }

  let update_spot t spot =
    let pnl = match Float.(t.position <> 0.0 && t.avg_price <> 0.0) with
      | true -> (spot -. t.avg_price) *. t.position
      | false -> 0.0
    in
    let notional = Float.abs t.position *. spot in
    { t with spot; pnl; notional; pnl_spot = spot;
             update_time = Time_float_unix.now ();
             update_source = `Market_data }

  let with_market_data t ~price ~qty ~side =
    { t with
      price = Some price;
      qty = Some qty;
      side = Some side;
      update_time = Time_float_unix.now ();
      update_source = `Market_data;
    }
end

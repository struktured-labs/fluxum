(** Unified Ledger Entry - Exchange-agnostic P&L tracking

    Generalized from Gemini/Kraken implementations with:
    - bin_prot serialization for event sourcing
    - Simplified interface focused on core P&L
    - No exchange-specific dependencies
*)

open Core

(** Time type with proper sexp/bin_io support *)
module Time = struct
  type t = Time_ns.t [@@deriving compare, equal, bin_io]

  let sexp_of_t = Time_ns_unix.sexp_of_t
  let t_of_sexp = Time_ns_unix.t_of_sexp
end

(** Venue type matching bot event venue for serialization *)
module Venue = struct
  module T = struct
    type t =
      | Gemini
      | Kraken
      | Mexc
      | Coinbase
      | Binance
      | Bybit
      | Okx
      | Hyperliquid
      | Bitrue
      | Dydx
      | Jupiter
      | OneInch
      | Other of string
    [@@deriving sexp, compare, equal, bin_io]
  end
  include T
  include Comparator.Make(T)

  let of_fluxum_venue : Fluxum.Types.Venue.t -> t = function
    | Gemini -> Gemini
    | Kraken -> Kraken
    | Mexc -> Mexc
    | Coinbase -> Coinbase
    | Binance -> Binance
    | Bybit -> Bybit
    | Okx -> Okx
    | Hyperliquid -> Hyperliquid
    | Bitrue -> Bitrue
    | Dydx -> Dydx
    | Jupiter -> Jupiter
    | OneInch -> OneInch
    | Gmx -> Other "Gmx"
    | Aave -> Other "Aave"
    | Compound -> Other "Compound"

  let to_string = function
    | Gemini -> "Gemini"
    | Kraken -> "Kraken"
    | Mexc -> "MEXC"
    | Coinbase -> "Coinbase"
    | Binance -> "Binance"
    | Bybit -> "Bybit"
    | Okx -> "OKX"
    | Hyperliquid -> "Hyperliquid"
    | Bitrue -> "Bitrue"
    | Dydx -> "dYdX"
    | Jupiter -> "Jupiter"
    | OneInch -> "1inch"
    | Other s -> s
end

(** Trade side *)
module Side = struct
  type t = Buy | Sell [@@deriving sexp, compare, equal, bin_io]

  let of_fluxum_side : Fluxum.Types.Side.t -> t = function
    | Buy -> Buy
    | Sell -> Sell
end

(** Source of update *)
module Update_source = struct
  type t =
    | Trade           (** Trade execution *)
    | Market_data     (** Price update from market *)
    | Manual          (** Manual adjustment *)
    | Restore         (** Restored from snapshot *)
  [@@deriving sexp, compare, equal, bin_io]
end

(** Single ledger entry for one symbol at one venue *)
type t =
  { (* Identification *)
    symbol : string
  ; venue : Venue.t
    (* Position tracking *)
  ; position : float               (** Current position (signed: +long, -short) *)
  ; avg_cost : float               (** Average cost basis per unit *)
  ; cost_basis : float             (** Total cost basis = position * avg_cost *)
    (* P&L tracking *)
  ; realized_pnl : float           (** Cumulative realized P&L *)
  ; unrealized_pnl : float         (** Current unrealized P&L at mark price *)
  ; total_pnl : float              (** realized + unrealized *)
  ; mark_price : float             (** Current mark price for unrealized calc *)
    (* Volume tracking *)
  ; buy_volume : float             (** Total quantity bought *)
  ; sell_volume : float            (** Total quantity sold *)
  ; buy_notional : float           (** Total notional bought *)
  ; sell_notional : float          (** Total notional sold *)
    (* Fee tracking *)
  ; total_fees : float             (** Cumulative fees paid *)
    (* Trade statistics *)
  ; trade_count : int              (** Number of trades *)
  ; winning_trades : int           (** Trades with positive P&L *)
  ; losing_trades : int            (** Trades with negative P&L *)
  ; largest_win : float            (** Largest winning trade *)
  ; largest_loss : float           (** Largest losing trade *)
    (* Timestamps *)
  ; first_trade : Time.t option (** Time of first trade *)
  ; last_trade : Time.t option  (** Time of most recent trade *)
  ; last_update : Time.t        (** Time of last update (trade or mark) *)
  ; update_source : Update_source.t
  }
[@@deriving sexp, compare, equal, bin_io, fields]

(** Create empty entry for a symbol/venue *)
let empty ~symbol ~venue =
  { symbol
  ; venue
  ; position = 0.
  ; avg_cost = 0.
  ; cost_basis = 0.
  ; realized_pnl = 0.
  ; unrealized_pnl = 0.
  ; total_pnl = 0.
  ; mark_price = 0.
  ; buy_volume = 0.
  ; sell_volume = 0.
  ; buy_notional = 0.
  ; sell_notional = 0.
  ; total_fees = 0.
  ; trade_count = 0
  ; winning_trades = 0
  ; losing_trades = 0
  ; largest_win = 0.
  ; largest_loss = 0.
  ; first_trade = None
  ; last_trade = None
  ; last_update = Time_ns.now ()
  ; update_source = Manual
  }

(** Apply a fill (trade execution) to the entry *)
let apply_fill t ~price ~qty ~side ~fee =
  let now = Time_ns.now () in
  let signed_qty = match side with
    | Side.Buy -> qty
    | Side.Sell -> Float.neg qty
  in
  let trade_notional = Float.abs (price *. qty) in

  (* Update volumes *)
  let buy_volume, sell_volume, buy_notional, sell_notional =
    match side with
    | Buy ->
      (t.buy_volume +. qty, t.sell_volume, t.buy_notional +. trade_notional, t.sell_notional)
    | Sell ->
      (t.buy_volume, t.sell_volume +. qty, t.buy_notional, t.sell_notional +. trade_notional)
  in

  (* Calculate realized P&L if reducing position *)
  let old_position = t.position in
  let new_position = old_position +. signed_qty in
  let is_reducing = Float.(
    (old_position > 0. && signed_qty < 0.) ||
    (old_position < 0. && signed_qty > 0.)
  ) in

  let realized_pnl_delta, trade_pnl =
    match is_reducing with
    | false -> 0., 0.  (* Opening or adding to position *)
    | true ->
      let reduced_qty = Float.min (Float.abs old_position) (Float.abs signed_qty) in
      let pnl_per_unit = match Float.(old_position > 0.) with
        | true -> price -. t.avg_cost   (* Long position, sold *)
        | false -> t.avg_cost -. price  (* Short position, bought to cover *)
      in
      let pnl = pnl_per_unit *. reduced_qty in
      (pnl, pnl)
  in

  (* Update cost basis *)
  let avg_cost, cost_basis =
    match Float.(abs new_position < 0.00000001) with
    | true -> (0., 0.)  (* Position closed *)
    | false ->
      match is_reducing with
      | true ->
        (* Keep existing avg_cost, update cost_basis *)
        (t.avg_cost, t.avg_cost *. new_position)
      | false ->
        (* Averaging in: weighted average cost *)
        let old_value = Float.abs old_position *. t.avg_cost in
        let new_value = Float.abs signed_qty *. price in
        let total_qty = Float.abs old_position +. Float.abs signed_qty in
        let new_avg = (old_value +. new_value) /. total_qty in
        (new_avg, new_avg *. new_position)
  in

  (* Update unrealized P&L *)
  let unrealized_pnl =
    match Float.(abs new_position < 0.00000001) || Float.(t.mark_price = 0.) with
    | true -> 0.
    | false ->
      match Float.(new_position > 0.) with
      | true -> (t.mark_price -. avg_cost) *. new_position
      | false -> (avg_cost -. t.mark_price) *. Float.abs new_position
  in

  let realized_pnl = t.realized_pnl +. realized_pnl_delta in
  let total_pnl = realized_pnl +. unrealized_pnl in

  (* Update trade stats *)
  let winning_trades, losing_trades =
    match Float.(trade_pnl > 0.001), Float.(trade_pnl < -0.001) with
    | true, _ -> (t.winning_trades + 1, t.losing_trades)
    | _, true -> (t.winning_trades, t.losing_trades + 1)
    | _, _ -> (t.winning_trades, t.losing_trades)
  in
  let largest_win = Float.max t.largest_win trade_pnl in
  let largest_loss = Float.min t.largest_loss trade_pnl in

  { t with
    position = new_position
  ; avg_cost
  ; cost_basis
  ; realized_pnl
  ; unrealized_pnl
  ; total_pnl
  ; buy_volume
  ; sell_volume
  ; buy_notional
  ; sell_notional
  ; total_fees = t.total_fees +. fee
  ; trade_count = t.trade_count + 1
  ; winning_trades
  ; losing_trades
  ; largest_win
  ; largest_loss
  ; first_trade = (match t.first_trade with None -> Some now | some -> some)
  ; last_trade = Some now
  ; last_update = now
  ; update_source = Trade
  }

(** Update mark price (for unrealized P&L calculation) *)
let mark_to_market t ~price =
  let now = Time_ns.now () in
  let unrealized_pnl =
    match Float.(abs t.position < 0.00000001) || Float.(price = 0.) with
    | true -> 0.
    | false ->
      match Float.(t.position > 0.) with
      | true -> (price -. t.avg_cost) *. t.position
      | false -> (t.avg_cost -. price) *. Float.abs t.position
  in
  let total_pnl = t.realized_pnl +. unrealized_pnl in
  { t with
    mark_price = price
  ; unrealized_pnl
  ; total_pnl
  ; last_update = now
  ; update_source = Market_data
  }

(** Serialize to binary *)
let serialize t =
  Bin_prot.Writer.to_string bin_writer_t t

(** Deserialize from binary *)
let deserialize data =
  try Ok (Bin_prot.Reader.of_string bin_reader_t data)
  with exn -> Error (Error.of_exn exn)

(** Summary string for display *)
let summary t =
  let pnl_sign = match Float.(t.total_pnl >= 0.) with true -> "+" | false -> "" in
  sprintf "%s@%s: pos=%.8f avg=%.4f pnl=%s%.2f (r:%.2f u:%.2f) trades=%d"
    t.symbol
    (Venue.to_string t.venue)
    t.position
    t.avg_cost
    pnl_sign
    t.total_pnl
    t.realized_pnl
    t.unrealized_pnl
    t.trade_count

(** Detailed report *)
let report t =
  let buf = Buffer.create 512 in
  Buffer.add_string buf (sprintf "\n=== %s @ %s ===\n" t.symbol (Venue.to_string t.venue));
  Buffer.add_string buf (sprintf "Position:     %.8f\n" t.position);
  Buffer.add_string buf (sprintf "Avg Cost:     %.8f\n" t.avg_cost);
  Buffer.add_string buf (sprintf "Cost Basis:   %.8f\n" t.cost_basis);
  Buffer.add_string buf (sprintf "Mark Price:   %.8f\n" t.mark_price);
  Buffer.add_string buf "\n--- P&L ---\n";
  Buffer.add_string buf (sprintf "Realized:     %.8f\n" t.realized_pnl);
  Buffer.add_string buf (sprintf "Unrealized:   %.8f\n" t.unrealized_pnl);
  Buffer.add_string buf (sprintf "Total:        %.8f\n" t.total_pnl);
  Buffer.add_string buf "\n--- Volume ---\n";
  Buffer.add_string buf (sprintf "Buy Vol:      %.8f (%.2f notional)\n" t.buy_volume t.buy_notional);
  Buffer.add_string buf (sprintf "Sell Vol:     %.8f (%.2f notional)\n" t.sell_volume t.sell_notional);
  Buffer.add_string buf (sprintf "Total Fees:   %.8f\n" t.total_fees);
  Buffer.add_string buf "\n--- Trades ---\n";
  Buffer.add_string buf (sprintf "Count:        %d\n" t.trade_count);
  Buffer.add_string buf (sprintf "Winners:      %d\n" t.winning_trades);
  Buffer.add_string buf (sprintf "Losers:       %d\n" t.losing_trades);
  Buffer.add_string buf (sprintf "Win Rate:     %.1f%%\n"
    (match t.trade_count > 0 with
     | true -> 100. *. Float.of_int t.winning_trades /. Float.of_int t.trade_count
     | false -> 0.));
  Buffer.add_string buf (sprintf "Largest Win:  %.8f\n" t.largest_win);
  Buffer.add_string buf (sprintf "Largest Loss: %.8f\n" t.largest_loss);
  Buffer.contents buf

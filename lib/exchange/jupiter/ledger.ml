(** Jupiter Ledger - Implements unified Ledger_intf for P&L tracking *)

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

    let position_sign = match side with
      | Buy -> 1.0
      | Sell -> -1.0
    in

    let position : float = t.position +. (qty *. position_sign) in
    let pnl_spot = price *. position in

    match Float.is_negative position with
    | true ->
      let qty = Float.abs position in
      let avg_trade_price = Option.value ~default:price avg_trade_price in

      let t =
        on_trade
          ~timestamp
          ~price:avg_trade_price
          ~side:(Fluxum.Types.Side.opposite side)
          ~update_source:`External_trade
          ~qty
          t
      in

      on_trade ~update_source ~timestamp ~avg_trade_price ~price ~side ~qty t

    | false ->
      let notional_sign : float = position_sign *. -1.0 in
      let package_price = qty *. price in

      let signed_notional = (notional_sign *. package_price) -. fee_usd in
      let notional = signed_notional +. t.notional in

      let total_buy_qty, total_sell_qty = match side with
        | Buy -> (t.total_buy_qty +. qty), t.total_sell_qty
        | Sell -> t.total_buy_qty, (t.total_sell_qty +. qty)
      in

      let avg_buy_price, avg_sell_price = match side with
        | Buy ->
          let new_avg_buy =
            match Float.(total_buy_qty > 0.) with
            | true -> (t.avg_buy_price *. t.total_buy_qty +. price *. qty) /. total_buy_qty
            | false -> 0.
          in
          (new_avg_buy, t.avg_sell_price)
        | Sell ->
          let new_avg_sell =
            match Float.(total_sell_qty > 0.) with
            | true -> (t.avg_sell_price *. t.total_sell_qty +. price *. qty) /. total_sell_qty
            | false -> 0.
          in
          (t.avg_buy_price, new_avg_sell)
      in

      let avg_price =
        let total_qty = total_buy_qty +. total_sell_qty in
        match Float.(total_qty > 0.) with
        | true -> (avg_buy_price *. total_buy_qty +. avg_sell_price *. total_sell_qty) /. total_qty
        | false -> 0.
      in

      let buy_notional, sell_notional = match side with
        | Buy -> (t.buy_notional +. package_price, t.sell_notional)
        | Sell -> (t.buy_notional, t.sell_notional +. package_price)
      in

      let cost_basis = match side with
        | Buy ->
          t.cost_basis +. package_price +. fee_usd
        | Sell ->
          match Float.(t.running_qty > 0.) with
          | true -> t.cost_basis -. (t.cost_basis *. qty /. t.running_qty)
          | false -> 0.
      in

      let running_qty = match side with
        | Buy -> t.running_qty +. qty
        | Sell -> t.running_qty -. qty
      in

      let running_price =
        match Float.(running_qty > 0.) with true -> cost_basis /. running_qty | false -> 0.
      in

      { symbol = t.symbol;
        spot = price;
        notional;
        pnl_spot;
        position;
        pnl = pnl_spot +. notional;
        update_time = timestamp;
        update_source;
        total_buy_qty;
        total_sell_qty;
        avg_buy_price;
        avg_sell_price;
        avg_price;
        buy_notional;
        sell_notional;
        running_price;
        running_qty;
        cost_basis;
        price = Some price;
        side = Some side;
        qty = Some qty;
        package_price = Some package_price;
        total_original = t.total_original;
        total_executed = t.total_executed;
        total_remaining = t.total_remaining;
      }

  let update_spot ?timestamp t spot =
    let update_time = Option.value_or_thunk timestamp ~default:Time_float_unix.now in
    let pnl_spot = t.position *. spot in
    { t with
      spot;
      pnl_spot;
      pnl = t.notional +. pnl_spot;
      update_time;
      update_source = `Market_data;
      side = None;
      price = None;
      qty = None;
      package_price = None;
    }

  let update_from_book t (_book : 'book) = t

  let pipe
      ~init
      ?num_values
      ?behavior
      (order_book : 'book Pipe.Reader.t)
      (trade_events : 'trade_events Pipe.Reader.t)
    =
    let book_events = Pipe.map order_book ~f:(fun b -> `Book b) in
    let trade_events = Pipe.map trade_events ~f:(fun t -> `Trade t) in

    let combined_pipe = match num_values, behavior with
      | None, None | None, Some `Alternate | Some _, Some `Alternate ->
        Pipe.interleave [book_events; trade_events]
      | _ ->
        Pipe.interleave [book_events; trade_events]
    in

    let reader, writer = Pipe.create () in
    let entry_ref = ref init in

    don't_wait_for (
      Pipe.iter combined_pipe ~f:(fun event ->
        match event with
        | `Book book ->
          entry_ref := update_from_book !entry_ref book;
          Pipe.write writer !entry_ref
        | `Trade _trade ->
          Pipe.write writer !entry_ref
      )
      >>| fun () ->
      Pipe.close writer
    );

    return reader
end

type t = Entry.t Fluxum.Types.Symbol.Map.t [@@deriving sexp, compare, equal]

let from_balances ?notional_currency:_ (_balances : 'balance list) : t =
  Fluxum.Types.Symbol.Map.empty

let from_trades
    ?(init = Fluxum.Types.Symbol.Map.empty)
    ?avg_trade_prices:_
    (_trades : 'trade list)
  : t * Entry.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t =
  (init, Fluxum.Types.Symbol.Map.empty)

let update_from_books t ~books =
  ignore books;
  t

let update_from_book t ~book =
  ignore book;
  t

let update_spots ?timestamp t (spots : float Fluxum.Types.Symbol.Map.t) =
  Map.fold spots ~init:t ~f:(fun ~key:symbol ~data:spot acc ->
    match Map.find acc symbol with
    | None ->
      let entry = Entry.create ~symbol ~spot ?update_time:timestamp () in
      Map.set acc ~key:symbol ~data:entry
    | Some entry ->
      let updated = Entry.update_spot ?timestamp entry spot in
      Map.set acc ~key:symbol ~data:updated
  )

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
    let entry = Entry.create ~symbol () in
    let updated = Entry.on_trade ?update_source ?timestamp ?avg_trade_price ?fee_usd
      entry ~price ~side ~qty
    in
    Map.set t ~key:symbol ~data:updated
  | Some entry ->
    let updated = Entry.on_trade ?update_source ?timestamp ?avg_trade_price ?fee_usd
      entry ~price ~side ~qty
    in
    Map.set t ~key:symbol ~data:updated

let on_order_events t (_events : 'order_event list) = t
let on_order_event_response t (_response : 'order_event_response) = t

let pipe
    ?num_values
    ?behavior
    ?how:_
    ~init
    (books : 'book Pipe.Reader.t Fluxum.Types.Symbol.Map.t)
    (order_events : 'order_events Pipe.Reader.t)
  : Entry.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t Deferred.t =
  Map.fold init ~init:(return Fluxum.Types.Symbol.Map.empty) ~f:(fun ~key:symbol ~data:entry acc_deferred ->
    let%bind acc = acc_deferred in
    match Map.find books symbol with
    | None -> return acc
    | Some book_pipe ->
      let%bind entry_pipe = Entry.pipe ~init:entry ?num_values ?behavior
        book_pipe order_events
      in
      return (Map.set acc ~key:symbol ~data:entry_pipe)
  )

let command : string * Command.t =
  ("ledger", Command.basic ~summary:"Jupiter ledger" (Command.Param.return (fun () -> printf "Jupiter ledger command\n")))

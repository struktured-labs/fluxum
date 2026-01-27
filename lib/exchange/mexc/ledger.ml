(** MEXC Ledger - Implements unified Ledger_intf for P&L tracking

    Complete P&L tracking for MEXC Global exchange with Binance-compatible structure.
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
      (* Normal long position or flat *)
      let notional_sign : float = position_sign *. -1.0 in
      let package_price = qty *. price in

      (* Fees reduce cash regardless of side *)
      let signed_notional = (notional_sign *. package_price) -. fee_usd in
      let notional = signed_notional +. t.notional in

      (* Update buy/sell quantities *)
      let total_buy_qty, total_sell_qty = match side with
        | Buy -> (t.total_buy_qty +. qty), t.total_sell_qty
        | Sell -> t.total_buy_qty, (t.total_sell_qty +. qty)
      in

      (* Update average prices *)
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

      (* Overall average price *)
      let avg_price =
        let total_qty = total_buy_qty +. total_sell_qty in
        match Float.(total_qty > 0.) with
        | true -> (avg_buy_price *. total_buy_qty +. avg_sell_price *. total_sell_qty) /. total_qty
        | false -> 0.
      in

      (* Update buy/sell notionals *)
      let buy_notional, sell_notional = match side with
        | Buy -> (t.buy_notional +. package_price, t.sell_notional)
        | Sell -> (t.buy_notional, t.sell_notional +. package_price)
      in

      (* Cost basis accounting *)
      let cost_basis = match side with
        | Buy ->
          (* Buying adds to cost basis *)
          t.cost_basis +. package_price +. fee_usd
        | Sell ->
          (* Selling reduces cost basis proportionally *)
          match Float.(t.running_qty > 0.) with
          | true -> t.cost_basis -. (t.cost_basis *. qty /. t.running_qty)
          | false -> 0.
      in

      (* Running quantity and price *)
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

  (** Update spot price from market data *)
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

  (** Update from order book (estimate market price for position) *)
  let update_from_book t (book : Order_book.Book.t) =
    (* Extract best bid price as market proxy *)
    let best_bid = Order_book.Book.best_bid book in
    let market_price = Order_book.Price_level.price best_bid in
    update_spot t market_price

  (** Real-time entry pipe combining order book + trade events *)
  let pipe
      ~init
      ?num_values
      ?behavior
      (order_book : Order_book.Book.t Pipe.Reader.t)
      (trade_events : V1.My_trades.trade Pipe.Reader.t)
    =
    (* Combine the two pipes *)
    let book_events = Pipe.map order_book ~f:(fun b -> `Book b) in
    let trade_events = Pipe.map trade_events ~f:(fun t -> `Trade t) in

    (* Merge the pipes based on behavior *)
    let combined_pipe = match num_values, behavior with
      | None, None | None, Some `Alternate | Some _, Some `Alternate ->
        Pipe.interleave [book_events; trade_events]
      | _ ->
        (* For other behaviors, just use interleave for now *)
        Pipe.interleave [book_events; trade_events]
    in

    (* Fold over events to update entry *)
    let reader, writer = Pipe.create () in
    let entry_ref = ref init in

    don't_wait_for (
      Pipe.iter combined_pipe ~f:(fun event ->
        match event with
        | `Book book ->
          entry_ref := update_from_book !entry_ref book;
          Pipe.write writer !entry_ref
        | `Trade trade ->
          (* Parse MEXC trade *)
          (match (
            let open Result.Let_syntax in
            let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string trade.price in
            let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string trade.qty in
            Ok (price, qty)
          ) with
          | Error err ->
            Log.Global.error "MEXC ledger: Failed to parse trade price/qty: %s" err;
            Deferred.unit
          | Ok (price, qty) ->
            let side = match trade.isBuyer with
              | true -> Fluxum.Types.Side.Buy
              | false -> Fluxum.Types.Side.Sell
            in

            (* MEXC doesn't provide fee info in trade data *)
            let fee_usd = 0. in

            (* Parse timestamp if available *)
            let timestamp = Time_float_unix.now () in

            entry_ref := on_trade !entry_ref ~timestamp ~price ~side ~qty ~fee_usd;
            Pipe.write writer !entry_ref)
      )
      >>| fun () ->
      Pipe.close writer
    );

    return reader
end

(** Multi-symbol ledger *)
type t = Entry.t Fluxum.Types.Symbol.Map.t [@@deriving sexp, compare, equal]

(** Bootstrap from account balances *)
let from_balances ?(notional_currency = "USD") (balances : V1.Account.balance list) : t =
  List.fold balances ~init:Fluxum.Types.Symbol.Map.empty ~f:(fun acc account ->
    match Fluxum.Normalize_common.Float_conv.amount_of_string account.free with
    | Error err ->
      Log.Global.error "MEXC ledger: Failed to parse balance for %s: %s"
        account.asset err;
      acc
    | Ok position ->
      (* Only create entries for non-zero positions *)
      match Float.(position > 0.) with
      | false -> acc
      | true ->
        (* Map currency to trading pair with notional currency
           For MEXC, symbols are like "BTC-USD" *)
        let symbol = account.asset ^ "-" ^ notional_currency in
        let entry = Entry.create ~symbol ~position () in
      Map.set acc ~key:symbol ~data:entry
  )

(** Bootstrap from historical trades *)
let from_trades
    ?(init = Fluxum.Types.Symbol.Map.empty)
    ?avg_trade_prices
    (trades : V1.My_trades.trade list)
  : t * Entry.t Pipe.Reader.t Fluxum.Types.Symbol.Map.t =
  (* Sort trades by time field (ascending order) *)
  let sorted_trades =
    List.sort trades ~compare:(fun a b ->
      Int64.compare a.V1.My_trades.time b.V1.My_trades.time)
  in

  (* Fold over trades to build ledger and pipes *)
  let symbol_to_entry_and_pipe = List.fold sorted_trades
    ~init:Fluxum.Types.Symbol.Map.empty
    ~f:(fun acc trade ->
      let symbol = trade.symbol in
      (* Parse price and qty *)
      match (
        let open Result.Let_syntax in
        let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string trade.price in
        let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string trade.qty in
        Ok (price, qty)
      ) with
      | Error err ->
        Log.Global.error "MEXC ledger: Failed to parse trade for %s: %s" symbol err;
        acc
      | Ok (price, qty) ->
        let side = match trade.isBuyer with
          | true -> Fluxum.Types.Side.Buy
          | false -> Fluxum.Types.Side.Sell
        in
        (* MEXC time is unix timestamp in milliseconds *)
        let timestamp =
          Time_float_unix.of_span_since_epoch
            (Time_float.Span.of_ms (Int64.to_float trade.time))
        in

        let fee_usd = 0. in  (* MEXC doesn't provide fee in trade data *)

        let avg_trade_price = match avg_trade_prices with
          | Some prices -> Map.find prices symbol
          | None -> None
        in

        (* Get or create entry with pipe *)
      let entry, reader, writer = match Map.find acc symbol with
        | Some (e, r, w) -> (e, r, w)
        | None ->
          let init_entry = match Map.find init symbol with
            | Some e -> e
            | None -> Entry.create ~symbol ()
          in
          let r, w = Pipe.create () in
          (init_entry, r, w)
      in

      (* Update entry with trade *)
      let updated_entry = Entry.on_trade entry ~timestamp ~price ~side ~qty ~fee_usd ?avg_trade_price in

      (* Write to pipe *)
      Pipe.write_without_pushback writer updated_entry;

      (* Update map *)
      Map.set acc ~key:symbol ~data:(updated_entry, reader, writer)
    )
  in

  (* Separate entries and pipes *)
  let final_entries = Map.map symbol_to_entry_and_pipe ~f:(fun (entry, _, _) -> entry) in
  let pipes = Map.map symbol_to_entry_and_pipe ~f:(fun (_, reader, _) -> reader) in

  (final_entries, pipes)

(** Update from order books *)
let update_from_books t ~(books : Order_book.Books.t) =
  Map.mapi t ~f:(fun ~key:symbol ~data:entry ->
    match Order_book.Books.book books symbol with
    | Some book -> Entry.update_from_book entry book
    | None -> entry
  )

let update_from_book t ~(book : Order_book.Book.t) =
  (* Update single book - need to know which symbol *)
  Map.map t ~f:(fun entry -> Entry.update_from_book entry book)

(** Update spot prices *)
let update_spots ?timestamp t (spots : float Fluxum.Types.Symbol.Map.t) =
  Map.fold spots ~init:t ~f:(fun ~key:symbol ~data:spot acc ->
    match Map.find acc symbol with
    | None ->
      (* No entry for this symbol, create one *)
      let entry = Entry.create ~symbol ~spot ?update_time:timestamp () in
      Map.set acc ~key:symbol ~data:entry
    | Some entry ->
      (* Update existing entry *)
      let updated = Entry.update_spot ?timestamp entry spot in
      Map.set acc ~key:symbol ~data:updated
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

(** Order event updates - Coinbase doesn't support this via Advanced Trade API *)
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
    (trades : V1.My_trades.trade Pipe.Reader.t)
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
        match String.equal trade.symbol symbol with
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
let command : string * Command.t =
  ("ledger", Command.basic ~summary:"Coinbase ledger (P&L tracking - limited API support)"
    (Command.Param.return (fun () -> printf "Coinbase ledger - use `from_balances` in code\n")))

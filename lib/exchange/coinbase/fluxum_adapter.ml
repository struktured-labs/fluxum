(** Coinbase Unified Adapter - Implements Exchange_intf.S

    STATUS: DATA-ONLY VENUE

    This adapter provides read-only market data access:
    - Account balances (authenticated)
    - Order book snapshots
    - Ticker data
    - Recent trades
    - Product/symbol info

    Order operations (place, cancel, status) are NOT implemented.
    For trading, use the Coinbase Advanced Trade API directly or
    contribute order implementation to this adapter.
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

(** Coinbase is a data-only venue - order operations are not supported *)
let data_only_error op =
  `Api_error (sprintf "Coinbase: %s not supported (data-only venue)" op)

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Coinbase
  end

  module Native = struct
    module Order = struct
      type id = string
      type request = unit  (* TODO: implement order placement *)
      type response = unit
      type status = unit
    end

    module Trade = struct
      type t = Rest.Types.trade
    end

    module Balance = struct
      type t = Rest.Account.account
    end

    module Book = struct
      type update = Rest.Types.product_book
      type snapshot = Rest.Types.product_book
    end

    module Ticker = struct
      type t = Rest.Types.ticker
    end

    module Public_trade = struct
      type t = Rest.Types.trade
    end

    module Symbol_info = struct
      type t = Rest.Types.product
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  let place_order _t _req =
    Deferred.return (Error (data_only_error "order placement"))

  let cancel_order _t _order_id =
    Deferred.return (Error (data_only_error "order cancellation"))

  let balances t =
    Rest.accounts t.cfg >>| function
    | Ok resp -> Ok resp.accounts
    | Error e -> Error e

  let get_order_status _t _order_id =
    Deferred.return (Error (data_only_error "order status"))

  let get_open_orders _t ?symbol:_ () =
    Deferred.return (Error (data_only_error "open orders"))

  let get_order_history _t ?symbol:_ ?limit:_ () =
    Deferred.return (Error (data_only_error "order history"))

  let get_my_trades _t ~symbol:_ ?limit:_ () =
    Deferred.return (Error (data_only_error "my trades"))

  let get_symbols t () =
    Rest.products t.cfg >>| function
    | Ok resp -> Ok resp.products
    | Error e -> Error e

  let get_ticker t ~symbol () =
    Rest.best_bid_ask t.cfg ~product_id:symbol >>| function
    | Ok ticker -> Ok ticker
    | Error e -> Error e

  let get_order_book t ~symbol ?limit:_ () =
    Rest.product_book t.cfg ~product_id:symbol >>| function
    | Ok book -> Ok book
    | Error e -> Error e

  let get_recent_trades _t ~symbol:_ ?limit:_ () =
    (* Coinbase doesn't have a direct recent trades endpoint in Advanced Trade API *)
    Deferred.return (Error (`Api_error "Recent trades not available"))

  let cancel_all_orders _t ?symbol:_ () =
    Deferred.return (Error (data_only_error "cancel all orders"))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    (** Coinbase is data-only - order operations not supported *)
    let order_response (_ : Native.Order.response) : (Types.Order.t, string) Result.t =
      Error "Coinbase: order_response not supported (data-only venue)"

    let order_status (_ : Native.Order.status) : (Types.Order_status.t, string) Result.t =
      Error "Coinbase: order_status not supported (data-only venue)"

    let order_from_status (_ : Native.Order.status) : (Types.Order.t, string) Result.t =
      Error "Coinbase: order_from_status not supported (data-only venue)"

    let trade (t : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind side = Fluxum.Normalize_common.Side.of_string t.side in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string t.price in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string t.size in
      Ok ({ venue = Venue.t
         ; symbol = t.product_id
         ; side
         ; price
         ; qty
         ; fee = None
         ; trade_id = Some t.trade_id
         ; ts = None  (* Would need to parse t.time *)
         } : Types.Trade.t)

    let balance (b : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind available = Fluxum.Normalize_common.Float_conv.of_string b.available_balance.value in
      let%bind hold = match b.hold with
        | Some h -> Fluxum.Normalize_common.Float_conv.of_string h.value
        | None -> Ok 0.0
      in
      Ok ({ venue = Venue.t
         ; currency = b.currency
         ; total = available +. hold
         ; available
         ; locked = hold
         } : Types.Balance.t)

    let book_update (book : Native.Book.update) : (Types.Book_update.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind levels =
        List.map book.bids ~f:(fun level ->
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
          let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
          Ok { Types.Book_update.price; qty })
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      Ok ({ venue = Venue.t
         ; symbol = book.product_id
         ; side = Types.Book_update.Side.Bid
         ; levels
         ; ts = None
         ; is_snapshot = true
         } : Types.Book_update.t)

    let symbol_info (s : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      let open Result.Let_syntax in
      let base = match s.base_name with Some n -> n | None -> "" in
      let quote = match s.quote_name with Some n -> n | None -> "" in
      let%bind min_order_size = match s.base_min_size with
        | Some str -> Fluxum.Normalize_common.Float_conv.of_string str
        | None -> Ok 0.0
      in
      let%bind tick_size = match s.base_increment with
        | Some str ->
          let%map f = Fluxum.Normalize_common.Float_conv.of_string str in
          Some f
        | None -> Ok None
      in
      let%bind quote_increment = match s.quote_increment with
        | Some str ->
          let%map f = Fluxum.Normalize_common.Float_conv.of_string str in
          Some f
        | None -> Ok None
      in
      Ok ({ venue = Venue.t
         ; symbol = s.product_id
         ; base_currency = base
         ; quote_currency = quote
         ; status = Option.value s.status ~default:"online"
         ; min_order_size
         ; tick_size
         ; quote_increment
         } : Types.Symbol_info.t)

    let ticker (t : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind bid_price = Fluxum.Normalize_common.Float_conv.price_of_string t.best_bid in
      let%bind ask_price = Fluxum.Normalize_common.Float_conv.price_of_string t.best_ask in
      Ok ({ venue = Venue.t
         ; symbol = ""  (* Not included in ticker response *)
         ; last_price = 0.0  (* Not directly available *)
         ; bid_price
         ; ask_price
         ; high_24h = 0.0
         ; low_24h = 0.0
         ; volume_24h = 0.0
         ; quote_volume = None
         ; price_change = None
         ; price_change_pct = None
         ; ts = None
         } : Types.Ticker.t)

    let order_book (book : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind bids =
        List.map book.bids ~f:(fun level ->
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
          let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
          Ok { Types.Order_book.Price_level.price; volume })
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      let%bind asks =
        List.map book.asks ~f:(fun level ->
          let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string level.price in
          let%bind volume = Fluxum.Normalize_common.Float_conv.qty_of_string level.size in
          Ok { Types.Order_book.Price_level.price; volume })
        |> Fluxum.Normalize_common.Result_util.transpose
      in
      Ok ({ venue = Venue.t
         ; symbol = book.product_id
         ; bids
         ; asks
         ; ts = None
         ; epoch = 0
         } : Types.Order_book.t)

    let public_trade (t : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind price = Fluxum.Normalize_common.Float_conv.price_of_string t.price in
      let%bind qty = Fluxum.Normalize_common.Float_conv.qty_of_string t.size in
      let%bind side = Fluxum.Normalize_common.Side.of_string t.side in
      Ok ({ venue = Venue.t
         ; symbol = t.product_id
         ; price
         ; qty
         ; side = Some side
         ; trade_id = Some t.trade_id
         ; ts = None
         } : Types.Public_trade.t)

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Transport (Failure msg)
      | `Api_error msg ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = "API"; message = msg }
  end
end

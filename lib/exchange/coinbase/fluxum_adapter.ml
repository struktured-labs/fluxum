(** Coinbase Unified Adapter - Implements Exchange_intf.S *)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

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
    Deferred.return (Error (`Api_error "Order placement not yet implemented"))

  let cancel_order _t _order_id =
    Deferred.return (Error (`Api_error "Order cancellation not yet implemented"))

  let balances t =
    Rest.accounts t.cfg >>| function
    | Ok resp -> Ok resp.accounts
    | Error e -> Error e

  let get_order_status _t _order_id =
    Deferred.return (Error (`Api_error "Order status not yet implemented"))

  let get_open_orders _t ?symbol:_ () =
    Deferred.return (Error (`Api_error "Open orders not yet implemented"))

  let get_order_history _t ?symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Order history not yet implemented"))

  let get_my_trades _t ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "My trades not yet implemented"))

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
    Deferred.return (Error (`Api_error "Cancel all not yet implemented"))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (_ : Native.Order.response) : Types.Order.t =
      { venue = Venue.t
      ; id = ""
      ; symbol = ""
      ; side = Types.Side.Buy
      ; kind = Types.Order_kind.Market
      ; qty = 0.0
      ; filled = 0.0
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let order_status (_ : Native.Order.status) : Types.Order_status.t =
      Types.Order_status.New

    let order_from_status (_ : Native.Order.status) : Types.Order.t =
      { venue = Venue.t
      ; id = ""
      ; symbol = ""
      ; side = Types.Side.Buy
      ; kind = Types.Order_kind.Market
      ; qty = 0.0
      ; filled = 0.0
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let trade (t : Native.Trade.t) : Types.Trade.t =
      let side = match t.side with
        | "BUY" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      { venue = Venue.t
      ; symbol = t.product_id
      ; side
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.size
      ; fee = None
      ; trade_id = Some t.trade_id
      ; ts = None  (* Would need to parse t.time *)
      }

    let balance (b : Native.Balance.t) : Types.Balance.t =
      let available = Float.of_string b.available_balance.value in
      let hold = match b.hold with
        | Some h -> Float.of_string h.value
        | None -> 0.0
      in
      { venue = Venue.t
      ; currency = b.currency
      ; total = available +. hold
      ; available
      ; locked = hold
      }

    let book_update (book : Native.Book.update) : Types.Book_update.t =
      let levels =
        List.map book.bids ~f:(fun level ->
          { Types.Book_update.price = Float.of_string level.price
          ; qty = Float.of_string level.size
          })
      in
      { venue = Venue.t
      ; symbol = book.product_id
      ; side = Types.Book_update.Side.Bid
      ; levels
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info (s : Native.Symbol_info.t) : Types.Symbol_info.t =
      let base = match s.base_name with Some n -> n | None -> "" in
      let quote = match s.quote_name with Some n -> n | None -> "" in
      { venue = Venue.t
      ; symbol = s.product_id
      ; base_currency = base
      ; quote_currency = quote
      ; status = Option.value s.status ~default:"online"
      ; min_order_size = (match s.base_min_size with
                          | Some s -> Float.of_string s
                          | None -> 0.0)
      ; tick_size = Option.map s.base_increment ~f:Float.of_string
      ; quote_increment = Option.map s.quote_increment ~f:Float.of_string
      }

    let ticker (t : Native.Ticker.t) : Types.Ticker.t =
      { venue = Venue.t
      ; symbol = ""  (* Not included in ticker response *)
      ; last_price = 0.0  (* Not directly available *)
      ; bid_price = Float.of_string t.best_bid
      ; ask_price = Float.of_string t.best_ask
      ; high_24h = 0.0
      ; low_24h = 0.0
      ; volume_24h = 0.0
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      }

    let order_book (book : Native.Book.snapshot) : Types.Order_book.t =
      let bids = List.map book.bids ~f:(fun level ->
        { Types.Order_book.Price_level.price = Float.of_string level.price
        ; volume = Float.of_string level.size
        })
      in
      let asks = List.map book.asks ~f:(fun level ->
        { Types.Order_book.Price_level.price = Float.of_string level.price
        ; volume = Float.of_string level.size
        })
      in
      { venue = Venue.t
      ; symbol = book.product_id
      ; bids
      ; asks
      ; ts = None
      ; epoch = 0
      }

    let public_trade (t : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = t.product_id
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.size
      ; side = Some (match t.side with
                    | "BUY" -> Types.Side.Buy
                    | _ -> Types.Side.Sell)
      ; trade_id = Some t.trade_id
      ; ts = None
      }

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

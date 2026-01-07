(** Bitrue Unified Adapter - Implements Exchange_intf.S

    STATUS: DATA-ONLY VENUE

    This adapter provides read-only market data access:
    - Account balances (authenticated)
    - Order book snapshots
    - Ticker data
    - Recent trades
    - Exchange info/symbols

    Order operations (place, cancel, status) are NOT implemented.
    For trading, use the Bitrue API directly or contribute order
    implementation to this adapter.
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

(** Bitrue is a data-only venue - order operations are not supported *)
let data_only_error op =
  `Api_error (sprintf "Bitrue: %s not supported (data-only venue)" op)

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Bitrue
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
      type t = Rest.Account.balance
    end

    module Book = struct
      type update = Rest.Types.order_book
      type snapshot = Rest.Types.order_book
    end

    module Ticker = struct
      type t = Rest.Types.ticker_24hr
    end

    module Public_trade = struct
      type t = Rest.Types.trade
    end

    module Symbol_info = struct
      type t = Rest.Types.symbol_info
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
    Rest.account t.cfg >>| function
    | Ok resp -> Ok resp.balances
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
    Rest.exchange_info t.cfg >>| function
    | Ok info -> Ok info.symbols
    | Error e -> Error e

  let get_ticker t ~symbol () =
    Rest.ticker_24hr t.cfg ~symbol >>| function
    | Ok ticker -> Ok ticker
    | Error e -> Error e

  let get_order_book t ~symbol ?limit () =
    let limit = Option.value limit ~default:100 in
    Rest.depth t.cfg ~symbol ~limit () >>| function
    | Ok book -> Ok book
    | Error e -> Error e

  let get_recent_trades t ~symbol ?limit () =
    let limit = Option.value limit ~default:500 in
    Rest.trades t.cfg ~symbol ~limit () >>| function
    | Ok trades -> Ok trades
    | Error e -> Error e

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
      let side = match t.isBuyerMaker with
        | true -> Types.Side.Sell
        | false -> Types.Side.Buy
      in
      { venue = Venue.t
      ; symbol = ""
      ; side
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.qty
      ; fee = None
      ; trade_id = Some (Int64.to_string t.id)
      ; ts = Some (Time_float_unix.of_span_since_epoch
                    (Time_float_unix.Span.of_ms (Int64.to_float t.time)))
      }

    let balance (b : Native.Balance.t) : Types.Balance.t =
      let free = Float.of_string b.free in
      let locked = Float.of_string b.locked in
      { venue = Venue.t
      ; currency = b.asset
      ; total = free +. locked
      ; available = free
      ; locked
      }

    let book_update (book : Native.Book.update) : Types.Book_update.t =
      let levels =
        List.map book.bids ~f:(fun (price, qty) ->
          { Types.Book_update.price = Float.of_string price
          ; qty = Float.of_string qty
          })
      in
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info (s : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = s.symbol
      ; base_currency = s.baseAsset
      ; quote_currency = s.quoteAsset
      ; status = s.status
      ; min_order_size = 0.0
      ; tick_size = None
      ; quote_increment = None
      }

    let ticker (t : Native.Ticker.t) : Types.Ticker.t =
      { venue = Venue.t
      ; symbol = t.symbol
      ; last_price = Float.of_string t.lastPrice
      ; bid_price = (match t.bidPrice with Some p -> Float.of_string p | None -> 0.0)
      ; ask_price = (match t.askPrice with Some p -> Float.of_string p | None -> 0.0)
      ; high_24h = Float.of_string t.highPrice
      ; low_24h = Float.of_string t.lowPrice
      ; volume_24h = Float.of_string t.volume
      ; quote_volume = Some (Float.of_string t.quoteVolume)
      ; price_change = Some (Float.of_string t.priceChange)
      ; price_change_pct = Some (Float.of_string t.priceChangePercent)
      ; ts = Some (Time_float_unix.of_span_since_epoch
                    (Time_float_unix.Span.of_ms (Int64.to_float t.closeTime)))
      }

    let order_book (book : Native.Book.snapshot) : Types.Order_book.t =
      let bids = List.map book.bids ~f:(fun (price, qty) ->
        { Types.Order_book.Price_level.price = Float.of_string price
        ; volume = Float.of_string qty
        })
      in
      let asks = List.map book.asks ~f:(fun (price, qty) ->
        { Types.Order_book.Price_level.price = Float.of_string price
        ; volume = Float.of_string qty
        })
      in
      { venue = Venue.t
      ; symbol = ""
      ; bids
      ; asks
      ; ts = None
      ; epoch = Int64.to_int_trunc book.lastUpdateId
      }

    let public_trade (t : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.qty
      ; side = Some (match t.isBuyerMaker with
                    | true -> Types.Side.Sell
                    | false -> Types.Side.Buy)
      ; trade_id = Some (Int64.to_string t.id)
      ; ts = Some (Time_float_unix.of_span_since_epoch
                    (Time_float_unix.Span.of_ms (Int64.to_float t.time)))
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

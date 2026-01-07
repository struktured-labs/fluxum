open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = V1
module Common = Common

module Adapter = struct
  type t =
     { cfg   : (module Cfg.S)
     ; nonce : Nonce.reader
    ; symbols : string list (* used for public market-data subscription *)
    }

  let create ~cfg ~nonce ?(symbols = []) () =
    { cfg; nonce; symbols }

  module Venue = struct
    let t = Types.Venue.Gemini
  end

  module Native = struct
    module Order = struct
      type id = Common.Int_string.t
      type request = V1.Order.New.request
      type response = V1.Order.Status.response
      type status = V1.Order.Status.response
    end

    module Trade = struct
      (* WebSocket trades come from order events, REST trades from mytrades *)
      type t =
        | Ws of V1.Order_events.Order_event.t
        | Rest of V1.Mytrades.trade
    end

    module Balance = struct
      type t = V1.Balances.balance
    end

    module Book = struct
      type update = V1.Market_data.Update.t
      type snapshot = unit  (* Not implemented for Gemini *)
    end

    module Ticker = struct
      type t = unit  (* Not implemented for Gemini *)
    end

    module Public_trade = struct
      type t = unit  (* Not implemented for Gemini *)
    end

    module Symbol_info = struct
      type t = V1.Symbol_details.T.response
    end

    module Error = struct
      type t = Rest.Error.post
    end
  end

  let place_order (t : t) (req : Native.Order.request) =
    let (module Cfg) = t.cfg in
    V1.Order.New.post (module Cfg) t.nonce req
    >>| function
    | `Ok r -> Ok r
      | (#Rest.Error.post as e) -> Error e

  let cancel_order (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Order.Cancel.By_order_id.post (module Cfg) t.nonce { order_id = id }
    >>| function
    | `Ok _ -> Ok ()
      | (#Rest.Error.post as e) -> Error e

  let balances (t : t) =
    let (module Cfg) = t.cfg in
    V1.Balances.post (module Cfg) t.nonce ()
    >>| function
    | `Ok r -> Ok r
    | (#Rest.Error.post as e) -> Error e

  let get_order_status (t : t) (id : Native.Order.id) =
    let (module Cfg) = t.cfg in
    V1.Order.Status.post (module Cfg) t.nonce { order_id = id }
    >>| function
    | `Ok r -> Ok r
    | (#Rest.Error.post as e) -> Error e

  let get_open_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Orders.post (module Cfg) t.nonce ()
    >>| function
    | `Ok orders -> Ok orders
    | (#Rest.Error.post as e) -> Error e

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    (* Gemini doesn't have a closed orders endpoint *)
    Deferred.return (Error (`Bad_request "Gemini does not support order history endpoint"))

  let get_my_trades (t : t) ~symbol ?limit () =
    let (module Cfg) = t.cfg in
    V1.Mytrades.post (module Cfg) t.nonce
      { symbol = V1.Symbol.of_string symbol
      ; limit_trades = limit
      ; timestamp = None
      }
    >>| function
    | `Ok trades -> Ok (List.map trades ~f:(fun t -> Native.Trade.Rest t))
    | (#Rest.Error.post as e) -> Error e

  let get_symbols (t : t) () =
    let (module Cfg) = t.cfg in
    (* Fetch details for all known symbols *)
    Deferred.List.filter_map V1.Symbol.all ~how:`Parallel ~f:(fun sym ->
      V1.Symbol_details.get (module Cfg) t.nonce ~uri_args:sym ()
      >>| function
      | `Ok r -> Some r
      | #Rest.Error.get -> None)
    >>| fun results -> Ok results

  let get_ticker (_ : t) ~symbol:_ () =
    (* Gemini public ticker endpoint not implemented yet *)
    Deferred.return (Error (`Bad_request "Gemini get_ticker not implemented"))

  let get_order_book (_ : t) ~symbol:_ ?limit:_ () =
    (* Gemini public order book endpoint not implemented yet *)
    Deferred.return (Error (`Bad_request "Gemini get_order_book not implemented"))

  let get_recent_trades (_ : t) ~symbol:_ ?limit:_ () =
    (* Gemini public trades endpoint not implemented yet *)
    Deferred.return (Error (`Bad_request "Gemini get_recent_trades not implemented"))

  let cancel_all_orders (t : t) ?symbol:_ () =
    let (module Cfg) = t.cfg in
    V1.Order.Cancel.All.post (module Cfg) t.nonce ()
    >>| function
    | `Ok { details } -> Ok (List.length details.cancelled_orders)
    | (#Rest.Error.post as e) -> Error e

  module Streams = struct
    let trades (t : t) =
      let (module Cfg) = t.cfg in
      let open V1.Order_events in
      let query =
        [ `Event_type_filter `Fill ]
        |> List.map ~f:sexp_of_query
      in
      client (module Cfg) ~nonce:t.nonce ~query ()
      >>| Pipe.filter_map ~f:(function
        | `Ok (`Order_event ev) -> Some (Native.Trade.Ws ev)
        | `Ok (`Order_events evs) -> Option.map (List.hd evs) ~f:(fun ev -> Native.Trade.Ws ev)
        | `Ok (`Heartbeat _) -> None
        | `Ok (`Subscription_ack _) -> None
        | #Ws.Error.t -> None)
      |> Deferred.map ~f:Fn.id

    let book_updates (t : t) =
      let (module Cfg) = t.cfg in
      let symbol_opt = List.hd t.symbols in
      V1.Market_data.client (module Cfg)
        ?uri_args:(Option.map symbol_opt ~f:V1.Symbol.of_string)
        ()
      >>| Pipe.filter_map ~f:(function
        | `Ok (r : V1.Market_data.response) -> (
            match r.message with
            | `Update u -> Some u
            | `Heartbeat _ -> None)
        | #Ws.Error.t -> None )
      |> Deferred.map ~f:Fn.id
  end

  module Normalize = struct
    let time_of_ts_opt (ts : Common.Timestamp.t option) : Time_float_unix.t option =
      Option.map ts ~f:Fn.id

    let order_kind (type_ : Common.Order_type.t) (price : Common.Decimal_string.t)
        (options : Common.Order_execution_option.t list) : Types.Order_kind.t =
      match type_ with
      | `Exchange_limit ->
        let p = Float.of_string (Common.Decimal_string.to_string price) in
        (match List.exists options ~f:(fun o -> Poly.equal o `Maker_or_cancel) with
         | true -> Types.Order_kind.Post_only_limit p
         | false -> Types.Order_kind.Limit p)
      | _ -> Types.Order_kind.Market

    let side (s : Common.Side.t) : Types.Side.t =
      match s with
      | `Buy -> Types.Side.Buy
      | `Sell -> Types.Side.Sell

    let symbol_to_string (s : Common.Symbol.Enum_or_string.t) : string =
      Common.Symbol.Enum_or_string.to_string s

    let order_response (r : Native.Order.response) : Types.Order.t =
      let qty_orig = Float.of_string (Common.Decimal_string.to_string r.original_amount) in
      let qty_exec = Float.of_string (Common.Decimal_string.to_string r.executed_amount) in
      let qty_rem = Float.of_string (Common.Decimal_string.to_string r.remaining_amount) in
      let reason_to_status = function
        | Some `Invalid_quantity -> Types.Order_status.Rejected "invalid_quantity"
        | Some `Insufficient_funds -> Types.Order_status.Rejected "insufficient_funds"
        | Some `Self_cross_prevented -> Types.Order_status.Rejected "self_cross_prevented"
        | Some `Immediate_or_cancel_would_post -> Types.Order_status.Rejected "ioc_would_post"
        | None -> Types.Order_status.New
      in
      let status : Types.Order_status.t =
        match r.is_cancelled with
        | true -> Types.Order_status.Canceled
        | false ->
          match r.is_live with
          | true ->
            (match Float.(qty_exec = 0.) with
             | true -> Types.Order_status.New
             | false ->
               match Float.(qty_exec > 0.) with
               | true -> Types.Order_status.Partially_filled
               | false ->
                 match Float.(qty_rem = 0.) with
                 | true -> Types.Order_status.Filled
                 | false -> reason_to_status r.reason)
          | false ->
            match Float.(qty_rem = 0.) with
            | true -> Types.Order_status.Filled
            | false -> reason_to_status r.reason
      in
      { Types.Order.venue = Venue.t
      ; id = Common.Int_string.to_string r.order_id
      ; symbol = symbol_to_string r.symbol
      ; side = side r.side
      ; kind = order_kind r.type_ r.price r.options
      ; qty = qty_orig
      ; filled = qty_exec
      ; status
      ; created_at = time_of_ts_opt (Some r.timestamp)
      ; updated_at = time_of_ts_opt (Some r.timestamp)
      }

    let order_status (r : Native.Order.status) : Types.Order_status.t =
      (order_response r).status

    let order_from_status (r : Native.Order.status) : Types.Order.t =
      order_response r

    let trade (t : Native.Trade.t) : Types.Trade.t =
      match t with
      | Ws ev ->
        (match ev.fill with
         | None ->
           { venue = Venue.t
           ; symbol = Common.Symbol.Enum_or_string.to_string ev.symbol
           ; side = side ev.side
           ; price = Option.value_map ev.avg_execution_price ~default:0.0 ~f:(fun d ->
               Float.of_string (Common.Decimal_string.to_string d))
           ; qty = Option.value_map ev.executed_amount ~default:0.0 ~f:(fun d ->
               Float.of_string (Common.Decimal_string.to_string d))
           ; fee = None
           ; trade_id = None
           ; ts = time_of_ts_opt (Some ev.timestamp)
           }
         | Some f ->
           let price = Float.of_string (Common.Decimal_string.to_string f.price) in
           let qty = Float.of_string (Common.Decimal_string.to_string f.amount) in
           let fee = Float.of_string (Common.Decimal_string.to_string f.fee) in
           { venue = Venue.t
           ; symbol = Common.Symbol.Enum_or_string.to_string ev.symbol
           ; side = side ev.side
           ; price
           ; qty
           ; fee = Some fee
           ; trade_id = Some (Common.Int_string.to_string f.trade_id)
           ; ts = time_of_ts_opt (Some ev.timestamp)
           })
      | Rest tr ->
        let price = Float.of_string (Common.Decimal_string.to_string tr.price) in
        let qty = Float.of_string (Common.Decimal_string.to_string tr.amount) in
        let fee = Float.of_string (Common.Decimal_string.to_string tr.fee_amount) in
        { venue = Venue.t
        ; symbol = Common.Symbol.Enum_or_string.to_string tr.symbol
        ; side = side tr.type_
        ; price
        ; qty
        ; fee = Some fee
        ; trade_id = Some (Common.Int_number.to_string tr.tid)
        ; ts = Some tr.timestampms
        }

    let balance (b : Native.Balance.t) : Types.Balance.t =
      let total = Float.of_string (Common.Decimal_string.to_string b.amount) in
      let available = Float.of_string (Common.Decimal_string.to_string b.available) in
      { venue = Venue.t
      ; currency = Common.Currency.Enum_or_string.to_string b.currency
      ; total
      ; available
      ; locked = total -. available
      }

    let book_update (u : Native.Book.update) : Types.Book_update.t =
      let ts = Option.first_some u.timestamp u.timestampms in
      let ts = time_of_ts_opt ts in
      let levels_of_event = function
        | `Change (c : V1.Market_data.Change_event.t) ->
          let price = Float.of_string (Common.Decimal_string.to_string c.price) in
          let qty = Float.of_string (Common.Decimal_string.to_string c.remaining) in
          let side =
            match c.side with
            | `Bid -> Types.Book_update.Side.Bid
            | `Ask -> Types.Book_update.Side.Ask
          in
          Some (side, [{ Types.Book_update.price; qty }])
        | `Trade _ -> None
        | `Auction _ | `Auction_open _ | `Block_trade _ -> None
      in
      (* Gemini batches multiple events; emit the first change as a minimal update *)
      let side_levels =
        Array.to_list u.events |> List.filter_map ~f:levels_of_event |> List.hd
      in
      match side_levels with
      | Some (side, levels) ->
        { venue = Venue.t
        ; symbol = "" (* market data update doesn’t include symbol in Update *)
        ; side
        ; levels
        ; ts
        ; is_snapshot = false
        }
      | None ->
        { venue = Venue.t
        ; symbol = ""
        ; side = Types.Book_update.Side.Bid
        ; levels = []
        ; ts
        ; is_snapshot = false
        }

    let symbol_info (s : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = Common.Symbol.Enum_or_string.to_string s.symbol
      ; base_currency = Common.Currency.Enum_or_string.to_string s.base_currency
      ; quote_currency = Common.Currency.Enum_or_string.to_string s.quote_currency
      ; status = s.status
      ; min_order_size = Float.of_string (Common.Decimal_string.to_string s.min_order_size)
      ; tick_size = Some (Common.Decimal_number.to_float s.tick_size)
      ; quote_increment = Some (Common.Decimal_number.to_float s.quote_increment)
      }

    let ticker (_ : Native.Ticker.t) : Types.Ticker.t =
      (* Not implemented - return empty ticker *)
      { venue = Venue.t
      ; symbol = ""
      ; last_price = 0.
      ; bid_price = 0.
      ; ask_price = 0.
      ; high_24h = 0.
      ; low_24h = 0.
      ; volume_24h = 0.
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      }

    let order_book (_ : Native.Book.snapshot) : Types.Order_book.t =
      (* Not implemented - return empty order book *)
      { venue = Venue.t
      ; symbol = ""
      ; bids = []
      ; asks = []
      ; ts = None
      ; epoch = 0
      }

    let public_trade (_ : Native.Public_trade.t) : Types.Public_trade.t =
      (* Not implemented - return empty trade *)
      { venue = Venue.t
      ; symbol = ""
      ; price = 0.
      ; qty = 0.
      ; side = None
      ; trade_id = None
      ; ts = None
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Bad_request msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "400"; message = msg }
      | `Not_found -> Types.Error.Exchange_specific { venue = Venue.t; code = "404"; message = "not_found" }
      | `Service_unavailable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "503"; message = msg }
      | `Not_acceptable msg -> Types.Error.Exchange_specific { venue = Venue.t; code = "406"; message = msg }
      | `Unauthorized _ -> Types.Error.Auth_failed
      | `Json_parse_error { message; body = _ } -> Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message }
      | `Error { reason; message } -> Types.Error.Exchange_specific { venue = Venue.t; code = reason; message }
    end
end

(* Adapter constructor is defined inside the Adapter module *)

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol ~side ~kind ~qty =
    let open Common in
    let amount = Decimal_string.of_string (Float.to_string qty) in
    let price_str_of p = Decimal_string.of_string (Float.to_string p) in
    let side' = match side with | Types.Side.Buy -> `Buy | Types.Side.Sell -> `Sell in
    let type_ = `Exchange_limit in
    let price, options =
      match kind with
      | Types.Order_kind.Market -> (price_str_of 0.0, [])
      | Types.Order_kind.Limit p -> (price_str_of p, [])
      | Types.Order_kind.Post_only_limit p -> (price_str_of p, [ `Maker_or_cancel ])
    in
    V1.Order.New.
      { client_order_id = Client_order_id.of_string (sprintf "fluxum-%0.0f" (Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_ms))
      ; symbol = V1.Symbol.of_string symbol
      ; amount
      ; price
      ; side = side'
      ; type_
      ; options
      }
end

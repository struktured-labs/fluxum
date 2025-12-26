(** MEXC Unified Adapter - Implements Exchange_intf.S *)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module Adapter : Exchange_intf.S = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : string list
    }

  module Venue = struct
    let t = Types.Venue.Mexc
  end

  module Native = struct
    module Order = struct
      type id = string
      type request = V1.New_order.request
      type response = V1.New_order.response
      type status = V1.Query_order.response
    end

    module Trade = struct
      type t = V1.My_trades.trade
    end

    module Balance = struct
      type t = V1.Account.balance
    end

    module Book = struct
      type update = V1.Depth.response
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  let _create ~cfg ?(symbols = []) () = { cfg; symbols }

  let place_order t (req : Native.Order.request) =
    V1.New_order.request t.cfg req >>| function
    | `Ok resp -> Ok resp
    | #Rest.Error.t as e -> Error e

  let cancel_order t (order_id : Native.Order.id) =
    (* Need symbol for cancel - extract from first configured symbol or fail *)
    match t.symbols with
    | symbol :: _ ->
      V1.Cancel_order.request
        t.cfg
        { symbol; orderId = Some order_id; origClientOrderId = None }
      >>| (function
      | `Ok _ -> Ok ()
      | #Rest.Error.t as e -> Error e)
    | [] ->
      Deferred.return
        (Error (`Api_error Rest.Error.{ code = -1; msg = "No symbol configured" }))

  let balances t =
    V1.Account.request t.cfg () >>| function
    | `Ok resp -> Ok resp.balances
    | #Rest.Error.t as e -> Error e

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (resp : Native.Order.response) : Types.Order.t =
      let side =
        match resp.side with
        | "BUY" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let kind =
        match resp.type_ with
        | "LIMIT" ->
          Types.Order_kind.Limit (Float.of_string resp.price)
        | "MARKET" -> Types.Order_kind.Market
        | _ -> Types.Order_kind.Market
      in
      let status =
        match resp.status with
        | "FILLED" -> Types.Order_status.Filled
        | "PARTIALLY_FILLED" -> Types.Order_status.Partially_filled
        | "CANCELED" -> Types.Order_status.Canceled
        | "REJECTED" -> Types.Order_status.Rejected "Order rejected"
        | _ -> Types.Order_status.New
      in
      { venue = Venue.t
      ; id = resp.orderId
      ; symbol = resp.symbol
      ; side
      ; kind
      ; qty = Float.of_string resp.origQty
      ; filled = Float.of_string resp.executedQty
      ; status
      ; created_at =
          (if Int64.(resp.transactTime > 0L)
          then
            Some
              (Time_float_unix.of_span_since_epoch
                 (Time_float_unix.Span.of_ms (Int64.to_float resp.transactTime)))
          else None)
      ; updated_at = None
      }

    let order_status (status : Native.Order.status) : Types.Order_status.t =
      match status.status with
      | "FILLED" -> Types.Order_status.Filled
      | "PARTIALLY_FILLED" -> Types.Order_status.Partially_filled
      | "CANCELED" -> Types.Order_status.Canceled
      | "REJECTED" -> Types.Order_status.Rejected "Order rejected"
      | _ -> Types.Order_status.New

    let trade (t : Native.Trade.t) : Types.Trade.t =
      let side = if t.isBuyer then Types.Side.Buy else Types.Side.Sell in
      { venue = Venue.t
      ; symbol = t.symbol
      ; side
      ; price = Float.of_string t.price
      ; qty = Float.of_string t.qty
      ; fee = Some (Float.of_string t.commission)
      ; trade_id = Some (Int64.to_string t.id)
      ; ts =
          Some
            (Time_float_unix.of_span_since_epoch
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

    let book_update (depth : Native.Book.update) : Types.Book_update.t =
      (* Return bid side update - caller should handle both sides *)
      let levels =
        List.map depth.bids ~f:(fun (price, qty) ->
          { Types.Book_update.price = Float.of_string price
          ; qty = Float.of_string qty
          })
      in
      { venue = Venue.t
      ; symbol = "" (* Symbol not included in depth response *)
      ; side = Types.Book_update.Side.Bid
      ; levels
      ; ts = None
      ; is_snapshot = true
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Api_error { code; msg } ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Unauthorized _ -> Types.Error.Auth_failed
      | `Too_many_requests _ -> Types.Error.Rate_limited
      | `Json_parse_error { message; _ } ->
        Types.Error.Transport (Failure message)
      | `Bad_request msg -> Types.Error.Transport (Failure msg)
      | `Not_found -> Types.Error.Transport (Failure "Not found")
      | `Service_unavailable msg -> Types.Error.Transport (Failure msg)
      | `Forbidden msg -> Types.Error.Transport (Failure msg)
  end
end

(** Helper to create order request from normalized types *)
let make_order_request
    ~symbol
    ~(side : Types.Side.t)
    ~(kind : Types.Order_kind.t)
    ~(qty : Types.Qty.t)
  : V1.New_order.request =
  let mexc_side : Common.Side.t =
    match side with
    | Types.Side.Buy -> `BUY
    | Types.Side.Sell -> `SELL
  in
  let order_type, price =
    match kind with
    | Types.Order_kind.Market -> (`MARKET : Common.Order_type.t), None
    | Types.Order_kind.Limit p ->
      (`LIMIT : Common.Order_type.t), Some (Float.to_string p)
    | Types.Order_kind.Post_only_limit p ->
      (`LIMIT_MAKER : Common.Order_type.t), Some (Float.to_string p)
  in
  V1.New_order.
    { symbol
    ; side = mexc_side
    ; order_type
    ; quantity = Some (Float.to_string qty)
    ; quoteOrderQty = None
    ; price
    ; newClientOrderId = None
    ; timeInForce =
        (match kind with
        | Types.Order_kind.Market -> None
        | _ -> Some (`GTC : Common.Time_in_force.t))
    }

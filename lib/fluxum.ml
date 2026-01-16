open Core
open Async

module Types = Types
module Exchange_intf = Exchange_intf
module Json = Json
module Cli_args = Cli_args
module Normalize_common = Normalize_common

(* Unified interfaces *)
module Order_book_intf = Order_book_intf
module Ledger_intf = Ledger_intf
module Session_intf = Session_intf

module type BUILDER = sig
  module E : Exchange_intf.S

  val make_order_request
    :  symbol:Types.Symbol.t
    -> side:Types.Side.t
    -> kind:Types.Order_kind.t
    -> qty:Types.Qty.t
    -> E.Native.Order.request
end

(* Suppress unused warnings - these functions will be used by CLI *)
[@@@warning "-32-34"]

module Make (E : Exchange_intf.S) (Builder : BUILDER with module E := E) = struct
  type t = E.t

  module Venue = struct
    let t = E.Venue.t
  end

  module Native = struct
    module Order = struct
      type id = E.Native.Order.id
      type request = E.Native.Order.request
      type response = E.Native.Order.response
      type status = E.Native.Order.status
    end

    module Trade = struct
      type t = E.Native.Trade.t
    end

    module Balance = struct
      type t = E.Native.Balance.t
    end

    module Book = struct
      type update = E.Native.Book.update
    end

    module Symbol_info = struct
      type t = E.Native.Symbol_info.t
    end

    module Error = struct
      type t = E.Native.Error.t
    end
  end

  let map_error (e : E.Native.Error.t) : Types.Error.t = E.Normalize.error e

  (* Helper to map list of Results to Result of list *)
  let result_list_transpose (results : ('a, 'e) Result.t list) : ('a list, 'e) Result.t =
    List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
      match res, acc with
      | Ok v, Ok vs -> Ok (v :: vs)
      | Error e, _ -> Error e
      | _, Error e -> Error e)

  let place_order t req =
    E.place_order t req
    >>| function
    | Ok resp ->
      (match E.Normalize.order_response resp with
       | Ok order -> Ok order
       | Error msg -> Error (Types.Error.Normalization_error msg))
    | Error e -> Error (map_error e)

  let place_order_norm t ~symbol ~side ~kind ~qty =
    let req = Builder.make_order_request ~symbol ~side ~kind ~qty in
    place_order t req

  let cancel_order t id =
    E.cancel_order t id >>| Result.map_error ~f:map_error

  let balances t =
    E.balances t
    >>| function
    | Ok balances ->
      let normalized = List.map balances ~f:E.Normalize.balance in
      (match result_list_transpose normalized with
       | Ok bals -> Ok bals
       | Error msg -> Error (Types.Error.Normalization_error msg))
    | Error e -> Error (map_error e)

  let get_order_status t id =
    E.get_order_status t id
    >>| Result.map ~f:E.Normalize.order_status
    >>| Result.map_error ~f:map_error

  let get_open_orders t ?symbol () =
    E.get_open_orders t ?symbol ()
    >>| function
    | Ok orders ->
      let normalized = List.map orders ~f:E.Normalize.order_from_status in
      (match result_list_transpose normalized with
       | Ok ords -> Ok ords
       | Error msg -> Error (Types.Error.Normalization_error msg))
    | Error e -> Error (map_error e)

  let get_order_history t ?symbol ?limit () =
    E.get_order_history t ?symbol ?limit ()
    >>| function
    | Ok orders ->
      let normalized = List.map orders ~f:E.Normalize.order_from_status in
      (match result_list_transpose normalized with
       | Ok ords -> Ok ords
       | Error msg -> Error (Types.Error.Normalization_error msg))
    | Error e -> Error (map_error e)

  let get_my_trades t ~symbol ?limit () =
    E.get_my_trades t ~symbol ?limit ()
    >>| function
    | Ok trades ->
      let normalized = List.map trades ~f:E.Normalize.trade in
      (match result_list_transpose normalized with
       | Ok trds -> Ok trds
       | Error msg -> Error (Types.Error.Normalization_error msg))
    | Error e -> Error (map_error e)

  let get_symbols t =
    E.get_symbols t ()
    >>| Result.map ~f:(List.map ~f:E.Normalize.symbol_info)
    >>| Result.map_error ~f:map_error

  module Streams = struct
    let trades t =
      E.Streams.trades t >>| Pipe.filter_map ~f:(fun trade ->
        match E.Normalize.trade trade with
        | Ok t -> Some t
        | Error _ -> None)

    let book_updates t =
      E.Streams.book_updates t >>| Pipe.map ~f:E.Normalize.book_update
  end
end

(** Uniswap V3 Virtual Order Book from Concentrated Liquidity Tick Data

    Constructs a virtual order book by polling the subgraph for tick data
    and converting tick ranges into price levels.
*)

open Core
open Async

(** Re-export types from unified interface *)
module Price_level = Exchange_common.Order_book_base.Price_level
module Bid_ask = Fluxum.Order_book_intf.Bid_ask

(** Uniswap V3-specific metadata *)
type metadata = {
  pool_id : string;
  current_tick : int;
  sqrt_price : string;
} [@@deriving sexp]

let default_metadata () = { pool_id = ""; current_tick = 0; sqrt_price = "0" }

(** Instantiate order book base with Uniswap V3-specific types *)
module Book_base = Exchange_common.Order_book_base.Make (struct
  type symbol = Fluxum.Types.Symbol.t
  let sexp_of_symbol = Fluxum.Types.Symbol.sexp_of_t
  let symbol_of_sexp = Fluxum.Types.Symbol.t_of_sexp
  let compare_symbol = Fluxum.Types.Symbol.compare

  type nonrec metadata = metadata
  let sexp_of_metadata = sexp_of_metadata
  let metadata_of_sexp = metadata_of_sexp
  let default_metadata = default_metadata
end)

(** Single symbol order book with Uniswap V3 extensions *)
module Book = struct
  include Book_base.Book

  (** Build order book from pool data and tick list *)
  let of_pool_and_ticks ~(pool : Types.pool) ~(ticks : Types.tick list) =
    let symbol = sprintf "%s/%s" pool.token0.symbol pool.token1.symbol in
    let meta = {
      pool_id = pool.id;
      current_tick = pool.tick;
      sqrt_price = pool.sqrtPrice;
    } in
    let book = create ~symbol in
    (* Ticks below current tick are bids, above are asks *)
    let levels = List.filter_map ticks ~f:(fun tick ->
      let price = tick.Types.price0 in
      match Float.(price > 0.) with
      | false -> None
      | true ->
        (* liquidityNet indicates net liquidity change at this tick *)
        let liq_net = Float.of_string tick.liquidityNet in
        let volume = Float.abs liq_net /. 1e18 in
        match Float.(volume > 0.) with
        | false -> None
        | true ->
          match Int.(tick.tickIdx < pool.tick) with
          | true -> Some (`Bid, price, volume)
          | false -> Some (`Ask, price, volume)
    ) in
    set_many book levels ~metadata:meta

  (** Create live order book pipe by polling subgraph *)
  let pipe ~cfg ~pool_id ?(poll_interval_sec = 10.0) () =
    let reader, writer = Pipe.create () in
    let rec poll_loop () =
      let%bind pool_result = Rest.pool_by_id ~cfg ~pool_id in
      let%bind tick_result = Rest.pool_ticks ~cfg ~pool_id () in
      (match pool_result, tick_result with
       | Ok pool, Ok ticks ->
         let book = of_pool_and_ticks ~pool ~ticks in
         Pipe.write writer (Ok book)
       | Error e, _ ->
         let msg = Sexp.to_string_hum (Rest.sexp_of_error e) in
         Pipe.write writer (Error msg)
       | _, Error e ->
         let msg = Sexp.to_string_hum (Rest.sexp_of_error e) in
         Pipe.write writer (Error msg))
      >>= fun () ->
      after (Time_float_unix.Span.of_sec poll_interval_sec) >>= fun () ->
      poll_loop ()
    in
    don't_wait_for (poll_loop ());
    reader
end

(** Multi-symbol order books *)
module Books = struct
  include Book_base.Books
  type book = Book.t
end

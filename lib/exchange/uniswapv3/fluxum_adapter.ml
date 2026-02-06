(** Uniswap V3 Exchange Adapter

    Implements Exchange_intf.S for Uniswap V3 concentrated liquidity DEX.

    {b Features:}
    - Pool discovery and info from The Graph subgraph
    - Virtual order book from tick data
    - Recent swap history
    - Spot price and quote from concentrated liquidity math
    - ETH balance querying via JSON-RPC
    - On-chain swap execution via SwapRouter02

    {b Authentication:}
    - Public subgraph queries require no auth
    - Swap execution requires wallet private key (UNISWAP_PRIVATE_KEY)
*)

open Core
open Async

(** Alias local types before shadowing with Fluxum types *)
module Uni_types = Types

module Types = Fluxum.Types

module Adapter = struct
  type t =
    { cfg : Cfg.t
    ; pool_ids : string list
    }

  let create ~cfg ?(pool_ids = []) () =
    { cfg; pool_ids }

  module Venue = struct
    let t = Types.Venue.Uniswap_v3
  end

  module Native = struct
    module Order = struct
      type id = string  (* tx hash *)
      type request = {
        pool_id : string;
        token_in : string;
        token_out : string;
        amount_in : float;
        slippage_bps : int;
      }
      type response = {
        tx_hash : string;
        amount_out : float;
      }
      type status = {
        tx_hash : string;
        confirmed : bool;
        pool_id : string;
        amount0 : string;
        amount1 : string;
      }
    end

    module Trade = struct
      type t = Uni_types.swap
    end

    module Balance = struct
      type t = Types.Balance.t
    end

    module Book = struct
      type update = (Order_book.Book.t, string) Result.t
      type snapshot = Order_book.Book.t
    end

    module Ticker = struct
      type t = {
        pool : Uni_types.pool;
        day_data : Rest.pool_day_datum list;
      }
    end

    module Public_trade = struct
      type t = Uni_types.swap
    end

    module Candle = struct
      type t = unit  (* Uniswap V3 doesn't provide candles *)
    end

    module Symbol_info = struct
      type t = Uni_types.pool
    end

    module Error = struct
      type t = Rest.error
    end

    (** Account operations - deposits/withdrawals (stubs)
        Note: Uniswap V3 is an EVM DEX - deposits/withdrawals happen on-chain *)
    module Deposit_address = struct
      type t = unit
    end

    module Deposit = struct
      type t = unit
    end

    module Withdrawal = struct
      type t = unit
    end
  end

  let place_order t (req : Native.Order.request) =
    match t.cfg.private_key_hex, t.cfg.wallet_address with
    | None, _ -> Deferred.return (Error (`GraphQL "Private key not configured"))
    | _, None -> Deferred.return (Error (`GraphQL "Wallet address not configured"))
    | Some _key, Some wallet ->
      (* Get pool info to determine fee tier *)
      let%bind pool_result = Rest.pool_by_id ~cfg:t.cfg ~pool_id:req.pool_id in
      (match pool_result with
       | Error e -> return (Error e)
       | Ok pool ->
         let decimals =
           match String.equal (String.lowercase req.token_in) (String.lowercase pool.token0.id) with
           | true -> pool.token0.decimals
           | false -> pool.token1.decimals
         in
         let amount_in_hex = Ethereum.Abi.uint256_of_float ~decimals req.amount_in in
         (* Calculate minimum output with slippage *)
         let spot_result = Pool_common.Concentrated.price_from_sqrt_price_x96
           ~sqrt_price_x96:pool.sqrtPrice
           ~decimals0:pool.token0.decimals
           ~decimals1:pool.token1.decimals
         in
         let amount_out_min_hex = match spot_result with
           | Error _ -> "0"
           | Ok spot ->
             let is_token0_in = String.equal (String.lowercase req.token_in) (String.lowercase pool.token0.id) in
             let expected_out = match is_token0_in with
               | true -> req.amount_in *. spot
               | false -> req.amount_in /. spot
             in
             let slippage = Float.of_int req.slippage_bps /. 10000.0 in
             let min_out = expected_out *. (1.0 -. slippage) in
             let out_decimals = match is_token0_in with
               | true -> pool.token1.decimals
               | false -> pool.token0.decimals
             in
             Ethereum.Abi.uint256_of_float ~decimals:out_decimals min_out
         in
         let params : Swap_router.exact_input_single_params = {
           token_in = req.token_in;
           token_out = req.token_out;
           fee = pool.feeTier;
           recipient = wallet;
           amount_in = amount_in_hex;
           amount_out_minimum = amount_out_min_hex;
           sqrt_price_limit_x96 = "0";
         } in
         let%bind swap_result = Swap_router.exact_input_single ~cfg:t.cfg ~params in
         (match swap_result with
          | Ok tx_hash ->
            return (Ok { Native.Order.tx_hash; amount_out = 0.0 })
          | Error (`Swap_error msg) -> return (Error (`GraphQL msg))
          | Error (`Rpc msg) -> return (Error (`GraphQL msg))
          | Error (`Network msg) -> return (Error (`Network msg))
          | Error (`Json_parse msg) -> return (Error (`Json_parse msg))))

  let cancel_order _t _id =
    Deferred.return (Error (`GraphQL "DEX swaps are atomic and cannot be cancelled"))

  let balances t =
    match t.cfg.wallet_address with
    | None -> Deferred.return (Ok [])
    | Some owner ->
      (* Query ETH balance *)
      let rpc_url = t.cfg.rpc_url in
      let%bind eth_result = Ethereum.Rpc.eth_get_balance ~rpc_url ~address:owner in
      let eth_balance = match eth_result with
        | Ok hex ->
          let wei = Float.of_string ("0x" ^ (match String.is_prefix hex ~prefix:"0x" with
            | true -> String.drop_prefix hex 2
            | false -> hex))
          in
          [{ Types.Balance.currency = "ETH"
           ; available = wei /. 1e18
           ; locked = 0.0
           ; total = wei /. 1e18
           ; venue = Venue.t }]
        | Error _ -> []
      in
      Deferred.return (Ok eth_balance)

  let get_order_status _t _id =
    Deferred.return (Error (`GraphQL "Order status lookup not yet implemented"))

  let get_open_orders _t ?symbol:_ () =
    (* DEX has no open orders *)
    Deferred.return (Ok [])

  let get_order_history _t ?symbol:_ ?limit:_ () =
    Deferred.return (Ok [])

  let get_my_trades t ~symbol:pool_id ?limit () =
    match t.cfg.wallet_address with
    | None -> Deferred.return (Error (`GraphQL "No wallet address configured"))
    | Some sender ->
      let first = Option.value limit ~default:100 in
      Rest.swaps_for_sender ~cfg:t.cfg ~sender ~first () >>| (function
      | Ok swaps ->
        (* Filter by pool_id if needed *)
        let filtered = List.filter swaps ~f:(fun (_swap : Uni_types.swap) ->
          String.is_prefix pool_id ~prefix:"0x")
        in
        Ok filtered
      | Error e -> Error e)

  let get_symbols t () =
    Rest.pools ~cfg:t.cfg ~first:100 () >>| (function
    | Ok pools -> Ok pools
    | Error e -> Error e)

  let get_ticker t ~symbol:pool_id () =
    let%bind pool_result = Rest.pool_by_id ~cfg:t.cfg ~pool_id in
    match pool_result with
    | Error e -> return (Error e)
    | Ok pool ->
      let%bind day_result = Rest.pool_day_data ~cfg:t.cfg ~pool_id () in
      let day_data = match day_result with
        | Ok data -> data
        | Error _ -> []
      in
      return (Ok { Native.Ticker.pool; day_data })

  let get_order_book t ~symbol:pool_id ?limit:_ () =
    let%bind pool_result = Rest.pool_by_id ~cfg:t.cfg ~pool_id in
    let%bind tick_result = Rest.pool_ticks ~cfg:t.cfg ~pool_id () in
    match pool_result, tick_result with
    | Ok pool, Ok ticks ->
      return (Ok (Order_book.Book.of_pool_and_ticks ~pool ~ticks))
    | Error e, _ -> return (Error e)
    | _, Error e -> return (Error e)

  let get_recent_trades t ~symbol:pool_id ?limit () =
    let first = Option.value limit ~default:100 in
    Rest.recent_swaps ~cfg:t.cfg ~pool_id ~first ()

  let cancel_all_orders _t ?symbol:_ () =
    Deferred.return (Ok 0)

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    Deferred.return (Error (`GraphQL "Uniswap V3 doesn't provide historical candle data"))

  (** {2 Account Operations - Deposits/Withdrawals (Stubs)}
      Note: Uniswap V3 is an EVM DEX - deposits/withdrawals happen on-chain *)

  let get_deposit_address (_ : t) ~currency:_ ?network:_ () =
    Deferred.return (Error (`GraphQL "Uniswap V3 is an EVM DEX - use wallet address for deposits"))

  let withdraw (_ : t) ~currency:_ ~amount:_ ~address:_ ?tag:_ ?network:_ () =
    Deferred.return (Error (`GraphQL "Uniswap V3 is an EVM DEX - use wallet for transfers"))

  let get_deposits (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`GraphQL "Uniswap V3 is an EVM DEX - use blockchain explorer"))

  let get_withdrawals (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`GraphQL "Uniswap V3 is an EVM DEX - use blockchain explorer"))

  module Streams = struct
    let trades _t =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates _t =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (resp : Native.Order.response) : (Types.Order.t, string) Result.t =
      Ok { venue = Venue.t
         ; id = resp.tx_hash
         ; symbol = ""
         ; side = Types.Side.Buy
         ; kind = Types.Order_kind.market
         ; time_in_force = Types.Time_in_force.GTC
         ; qty = 0.0
         ; filled = 0.0
         ; status = Types.Order_status.Filled
         ; created_at = None
         ; updated_at = None
         }

    let order_status (_status : Native.Order.status) : Types.Order_status.t =
      match _status.confirmed with
      | true -> Types.Order_status.Filled
      | false -> Types.Order_status.New

    let order_from_status (status : Native.Order.status) : (Types.Order.t, string) Result.t =
      Ok { venue = Venue.t
         ; id = status.tx_hash
         ; symbol = status.pool_id
         ; side = Types.Side.Buy
         ; kind = Types.Order_kind.market
         ; time_in_force = Types.Time_in_force.GTC
         ; qty = (match Float.of_string_opt status.amount0 with Some f -> Float.abs f | None -> 0.0)
         ; filled = (match Float.of_string_opt status.amount0 with Some f -> Float.abs f | None -> 0.0)
         ; status = (match status.confirmed with true -> Types.Order_status.Filled | false -> Types.Order_status.New)
         ; created_at = None
         ; updated_at = None
         }

    let trade (swap : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      let amount0 = Float.of_string swap.amount0 in
      let amount1 = Float.of_string swap.amount1 in
      let side = match Float.(amount0 < 0.) with
        | true -> Types.Side.Sell
        | false -> Types.Side.Buy
      in
      let price = match Float.(Float.abs amount0 > 0.) with
        | true -> Float.abs (amount1 /. amount0)
        | false -> 0.0
      in
      let ts = match Float.of_string_opt swap.timestamp with
        | Some epoch ->
          Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec epoch))
        | None -> None
      in
      Ok { venue = Venue.t
         ; symbol = ""
         ; side
         ; price
         ; qty = Float.abs amount0
         ; fee = None
         ; trade_id = Some swap.id
         ; ts
         }

    let balance (b : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      Ok b

    let book_update (book_result : Native.Book.update) : Types.Book_update.t =
      match book_result with
      | Error _ ->
        { venue = Venue.t; symbol = ""; side = Types.Book_update.Side.Bid;
          levels = []; ts = None; is_snapshot = false }
      | Ok book ->
        let bids = Order_book.Book.best_n_bids book ~n:20 () in
        let bid_levels = List.map bids ~f:(fun (level : Order_book.Price_level.t) ->
          { Types.Book_update.price = level.price; qty = level.volume }) in
        { venue = Venue.t
        ; symbol = Order_book.Book.symbol book
        ; side = Types.Book_update.Side.Bid
        ; levels = bid_levels
        ; ts = None
        ; is_snapshot = true
        }

    let symbol_info (pool : Native.Symbol_info.t) : (Types.Symbol_info.t, string) Result.t =
      Ok { venue = Venue.t
         ; symbol = sprintf "%s/%s" pool.token0.symbol pool.token1.symbol
         ; base_currency = pool.token0.symbol
         ; quote_currency = pool.token1.symbol
         ; status = "trading"
         ; min_order_size = 0.0
         ; tick_size = None
         ; quote_increment = None
         }

    let ticker (native : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      let pool = native.pool in
      let decimals0 = pool.token0.decimals in
      let decimals1 = pool.token1.decimals in
      match Pool_common.Concentrated.price_from_sqrt_price_x96
              ~sqrt_price_x96:pool.sqrtPrice ~decimals0 ~decimals1 with
      | Error msg -> Error msg
      | Ok spot ->
        let high, low = match native.day_data with
          | [] -> spot, spot
          | data ->
            List.fold data ~init:(Float.neg_infinity, Float.infinity) ~f:(fun (h, l) d ->
              (Float.max h d.high, Float.min l d.low))
        in
        let volume = match native.day_data with
          | [] -> 0.0
          | d :: _ -> d.volumeUSD
        in
        Ok { venue = Venue.t
           ; symbol = sprintf "%s/%s" pool.token0.symbol pool.token1.symbol
           ; last_price = spot
           ; bid_price = spot
           ; ask_price = spot
           ; high_24h = high
           ; low_24h = low
           ; volume_24h = volume
           ; quote_volume = None
           ; price_change = None
           ; price_change_pct = None
           ; ts = None
           }

    let order_book (book : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let bids = Order_book.Book.best_n_bids book ~n:50 ()
        |> List.map ~f:(fun (level : Order_book.Price_level.t) ->
          { Types.Order_book.Price_level.price = level.price; volume = level.volume })
      in
      let asks = Order_book.Book.best_n_asks book ~n:50 ()
        |> List.map ~f:(fun (level : Order_book.Price_level.t) ->
          { Types.Order_book.Price_level.price = level.price; volume = level.volume })
      in
      Ok { venue = Venue.t
         ; symbol = Order_book.Book.symbol book
         ; bids
         ; asks
         ; ts = None
         ; epoch = Order_book.Book.epoch book
         }

    let public_trade (swap : Native.Public_trade.t) : (Types.Public_trade.t, string) Result.t =
      let amount0 = Float.of_string swap.amount0 in
      let amount1 = Float.of_string swap.amount1 in
      let side = match Float.(amount0 < 0.) with
        | true -> Some Types.Side.Sell
        | false -> Some Types.Side.Buy
      in
      let price = match Float.(Float.abs amount0 > 0.) with
        | true -> Float.abs (amount1 /. amount0)
        | false -> 0.0
      in
      let ts = match Float.of_string_opt swap.timestamp with
        | Some epoch ->
          Some (Time_float_unix.of_span_since_epoch (Time_float_unix.Span.of_sec epoch))
        | None -> None
      in
      Ok { venue = Venue.t
         ; symbol = ""
         ; price
         ; qty = Float.abs amount0
         ; side
         ; trade_id = Some swap.id
         ; ts
         }

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "Uniswap V3 doesn't provide candle data"

    (** Account operations normalization (stubs) *)
    let deposit_address (_ : Native.Deposit_address.t) : (Types.Deposit_address.t, string) Result.t =
      Error "Uniswap V3 is an EVM DEX - use wallet address"

    let deposit (_ : Native.Deposit.t) : (Types.Deposit.t, string) Result.t =
      Error "Uniswap V3 is an EVM DEX - use blockchain explorer"

    let withdrawal (_ : Native.Withdrawal.t) : (Types.Withdrawal.t, string) Result.t =
      Error "Uniswap V3 is an EVM DEX - use blockchain explorer"

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Transport (Failure msg)
      | `Network msg ->
        Types.Error.Transport (Failure msg)
      | `GraphQL msg ->
        Types.Error.Exchange_specific
          { venue = Venue.t; code = "graphql"; message = msg }
  end
end

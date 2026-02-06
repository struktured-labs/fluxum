(** 1inch EVM DEX Aggregator Adapter

    Partial implementation of Exchange_intf.S for 1inch DEX aggregator.

    {b Status:} MARKET DATA ONLY

    {b Features:}
    - ✅ REST market data (ticker via quote API)
    - ✅ Synthetic order book from quotes
    - ✅ Token list for supported chains
    - ❌ Trading operations (requires wallet signing)

    {b Architecture:}
    - DEX aggregator across 15+ EVM chains
    - Routes through 400+ liquidity sources (Uniswap, Curve, Balancer, etc.)
    - No persistent order book - quotes computed on-demand
    - Pathfinder algorithm for optimal routing
    - Smart contract execution on target chain

    {b Supported Chains:}
    - Ethereum, BSC, Polygon, Arbitrum, Optimism
    - Avalanche, Fantom, Gnosis, Base, zkSync Era
    - And more (check API for full list)

    {b Authentication:}
    - Public endpoints: API key recommended but optional
    - Trading: Requires wallet private key (not implemented)
    - Rate limits higher with API key

    {b Rate Limits:}
    - Free tier: 1 request/second
    - With API key: 10 requests/second
    - Enterprise: Custom limits

    {b Symbol Format:}
    - EVM token addresses: ["0xEeeee..."] (ETH), ["0xA0b86..."] (USDC)
    - Trading pairs represented as (src_token, dst_token)
    - Chain ID required for multi-chain support

    {b Known Limitations:}
    - ❌ No REST trading (requires blockchain integration)
    - ❌ No WebSocket support (REST only)
    - ❌ No order book depth (only top of book from quotes)
    - Quotes expire in 30 seconds
    - Gas estimation required for transaction execution
    - Price impact varies significantly with liquidity
    - Different token standards (ERC-20, native ETH) require special handling

    {b Trading Implementation Plan:}
    - Phase 1: Market data ✅ (complete)
    - Phase 2: EVM wallet integration (pending)
    - Phase 3: Transaction building with gas estimation (pending)
    - Phase 4: Swap execution and monitoring (pending)

    @see <https://docs.1inch.io/docs/aggregation-protocol/api/> 1inch Aggregation API Documentation
    @see <https://docs.1inch.io/docs/aggregation-protocol/introduction> 1inch Protocol Overview
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = Rest

(* Suppress unused warnings *)
[@@@warning "-32-69"]

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : (string * string) list  (* (base_token, quote_token) pairs *)
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.OneInch
  end

  module Native = struct
    module Order = struct
      type id = string  (* Transaction hash *)
      type request = unit
      type response = Rest.Types.swap
      type status = unit
    end

    module Trade = struct
      type t = unit  (* Trades are on-chain *)
    end

    module Balance = struct
      type t = unit  (* Balances are on EVM, use RPC *)
    end

    module Book = struct
      type update = unit
      type snapshot = Rest.Types.quote * Rest.Types.quote
    end

    module Ticker = struct
      type t = Rest.Types.quote
    end

    module Public_trade = struct
      type t = unit
    end

    module Candle = struct
      type t = unit  (* 1inch doesn't provide candles *)
    end

    module Symbol_info = struct
      type t = Rest.Types.token_info
    end

    module Error = struct
      type t = Rest.Error.t
    end

    (** Account operations - deposits/withdrawals (stubs)
        Note: 1inch is an EVM DEX aggregator - deposits/withdrawals happen on-chain *)
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

  (* Trading operations - require wallet *)
  let place_order (_ : t) (_ : Native.Order.request) =
    Deferred.return (Error (`Api_error "1inch swaps require wallet signing"))

  let cancel_order (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "1inch trades are atomic swaps, no cancellation"))

  let balances (_ : t) =
    Deferred.return (Error (`Api_error "Balances require EVM RPC, not 1inch API"))

  let get_order_status (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "Use EVM RPC to track transaction status"))

  let get_open_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "1inch is a DEX aggregator, no persistent orders"))

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Order history is on EVM blockchain"))

  let get_my_trades (_ : t) ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Trade history is on EVM blockchain"))

  let cancel_all_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "1inch trades are atomic swaps"))

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "1inch doesn't provide historical candle data"))

  (** {2 Account Operations - Deposits/Withdrawals (Stubs)}
      Note: 1inch is an EVM DEX aggregator - deposits/withdrawals happen on-chain *)

  let get_deposit_address (_ : t) ~currency:_ ?network:_ () =
    Deferred.return (Error (`Api_error "1inch is an EVM DEX - use wallet address for deposits"))

  let withdraw (_ : t) ~currency:_ ~amount:_ ~address:_ ?tag:_ ?network:_ () =
    Deferred.return (Error (`Api_error "1inch is an EVM DEX - use wallet for transfers"))

  let get_deposits (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "1inch is an EVM DEX - use blockchain explorer"))

  let get_withdrawals (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "1inch is an EVM DEX - use blockchain explorer"))

  (* Public market data *)
  let get_symbols (t : t) () =
    Rest.tokens t.cfg >>| function
    | Error _ as err -> err
    | Ok resp -> Ok (List.map resp.tokens ~f:snd)

  let get_ticker (t : t) ~symbol () =
    (* Parse symbol as "BASE-QUOTE" using token addresses *)
    let (base_token, quote_token) = match String.lsplit2 symbol ~on:'-' with
      | Some ("ETH", "USDC") -> (Rest.Tokens.eth, Rest.Tokens.usdc)
      | Some ("ETH", "USDT") -> (Rest.Tokens.eth, Rest.Tokens.usdt)
      | Some ("WBTC", "USDC") -> (Rest.Tokens.wbtc, Rest.Tokens.usdc)
      | Some ("ETH", "DAI") -> (Rest.Tokens.eth, Rest.Tokens.dai)
      | _ ->
        (* Assume symbol is "baseAddr-quoteAddr" *)
        match String.lsplit2 symbol ~on:'-' with
        | Some (base, quote) -> (base, quote)
        | None -> (symbol, Rest.Tokens.usdc)
    in
    (* Get quote for 1 ETH worth *)
    let amount = "1000000000000000000" in  (* 1 ETH in wei *)
    Rest.quote t.cfg ~src:base_token ~dst:quote_token ~amount ()

  let get_order_book (t : t) ~symbol ?limit:_ () =
    let (base_token, quote_token) = match String.lsplit2 symbol ~on:'-' with
      | Some ("ETH", "USDC") -> (Rest.Tokens.eth, Rest.Tokens.usdc)
      | Some ("ETH", "USDT") -> (Rest.Tokens.eth, Rest.Tokens.usdt)
      | Some ("WBTC", "USDC") -> (Rest.Tokens.wbtc, Rest.Tokens.usdc)
      | _ ->
        match String.lsplit2 symbol ~on:'-' with
        | Some (base, quote) -> (base, quote)
        | None -> (symbol, Rest.Tokens.usdc)
    in
    let amount = "1000000000000000000" in
    Rest.order_book t.cfg ~base_token ~quote_token ~amount

  let get_recent_trades (_ : t) ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Recent trades are on EVM blockchain"))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (_ : Native.Order.response) : (Types.Order.t, string) Result.t =
      Error "1inch is a DEX aggregator - use swap/quote APIs instead of order tracking"

    let order_status (_ : Native.Order.status) : (Types.Order_status.t, string) Result.t =
      Error "1inch is a DEX aggregator - use swap/quote APIs instead of order tracking"

    let order_from_status (_ : Native.Order.status) : (Types.Order.t, string) Result.t =
      Error "1inch is a DEX aggregator - use swap/quote APIs instead of order tracking"

    let trade (_ : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      Error "1inch is a DEX aggregator - use swap/quote APIs instead of trade tracking"

    let balance (_ : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      Error "1inch is a DEX aggregator - balances are on-chain, use wallet APIs"

    let book_update (() : Native.Book.update) : Types.Book_update.t =
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels = []
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info (info : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = info.symbol
      ; base_currency = info.symbol
      ; quote_currency = "USD"
      ; status = "active"
      ; min_order_size = 0.
      ; tick_size = None
      ; quote_increment = None
      }

    let ticker (quote : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      (* Calculate price from amounts and decimals *)
      let open Result.Let_syntax in
      let%bind to_amount = Fluxum.Normalize_common.Float_conv.qty_of_string quote.toAmount in
      let from_decimals = Float.of_int quote.fromToken.decimals in
      let to_decimals = Float.of_int quote.toToken.decimals in
      let from_amount = Float.(10. ** from_decimals) in  (* 1 unit of from token *)
      let price = to_amount /. Float.(10. ** to_decimals) /. (from_amount /. Float.(10. ** from_decimals)) in
      let symbol = sprintf "%s-%s" quote.fromToken.symbol quote.toToken.symbol in
      Ok ({ venue = Venue.t
      ; symbol
      ; last_price = price
      ; bid_price = price
      ; ask_price = price
      ; high_24h = 0.
      ; low_24h = 0.
      ; volume_24h = 0.
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      } : Types.Ticker.t)

    let order_book ((sell_quote, buy_quote) : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind sell_to_amt = Fluxum.Normalize_common.Float_conv.qty_of_string sell_quote.toAmount in
      let%bind buy_to_amt = Fluxum.Normalize_common.Float_conv.qty_of_string buy_quote.toAmount in
      let from_dec = Float.of_int sell_quote.fromToken.decimals in
      let to_dec = Float.of_int sell_quote.toToken.decimals in
      let sell_price = sell_to_amt /. Float.(10. ** to_dec) in
      let buy_dec = Float.of_int buy_quote.toToken.decimals in
      let buy_price = Float.(10. ** buy_dec) /. buy_to_amt in
      let _ = from_dec in  (* Suppress unused warning *)
      let bid = { Types.Order_book.Price_level.
        price = buy_price;
        volume = 1.0;
      } in
      let ask = { Types.Order_book.Price_level.
        price = sell_price;
        volume = 1.0;
      } in
      Ok ({ venue = Venue.t
      ; symbol = ""
      ; bids = [bid]
      ; asks = [ask]
      ; ts = None
      ; epoch = 0
      } : Types.Order_book.t)

    let public_trade (() : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; price = 0.
      ; qty = 0.
      ; side = None
      ; trade_id = None
      ; ts = None
      }

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "1inch doesn't provide candle data"

    (** Account operations normalization (stubs) *)
    let deposit_address (_ : Native.Deposit_address.t) : (Types.Deposit_address.t, string) Result.t =
      Error "1inch is an EVM DEX - use wallet address"

    let deposit (_ : Native.Deposit.t) : (Types.Deposit.t, string) Result.t =
      Error "1inch is an EVM DEX - use blockchain explorer"

    let withdrawal (_ : Native.Withdrawal.t) : (Types.Withdrawal.t, string) Result.t =
      Error "1inch is an EVM DEX - use blockchain explorer"

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message = msg }
      | `Api_error msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "api"; message = msg }
      | `Unauthorized ->
        Types.Error.Auth_failed
  end
end

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol:_ ~side:_ ~kind:_ ~qty:_ =
    ()
end

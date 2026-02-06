(** Exchange Adapter Interface

    This module defines the contract that every exchange adapter must implement
    to be compatible with the Fluxum unified trading system.

    {b Key Design:}
    - Dual interface: Native (exchange-specific) + Normalize (unified)
    - All operations return Deferred.Result.t for async + error handling
    - Polymorphic over exchange-specific types (Native module)
    - WebSocket streams via Pipe.Reader.t

    {b Implementation Guide:}
    To add a new exchange, implement this signature in your adapter module:
    {[
      module MyExchange_adapter : Exchange_intf.S = struct
        type t = { ... }  (* Your connection state *)
        module Venue = struct let t = Types.Venue.MyExchange end
        module Native = struct ... end
        (* ... implement all functions ... *)
      end
    ]}

    Then use Fluxum.Make functor for automatic normalization:
    {[
      module E = MyExchange_adapter
      module F = Fluxum.Make(E)(E.Builder)
      (* F.place_order returns normalized Types.Order.t *)
    ]}

    {b Reference Implementations:}
    - Gemini: lib/exchange/gemini/fluxum_adapter.ml (complete, with session)
    - Kraken: lib/exchange/kraken/fluxum_adapter.ml (complete, with v2 WebSocket)
    - MEXC: lib/exchange/mexc/fluxum_adapter.ml (complete, beta status)
    - Hyperliquid: lib/exchange/hyperliquid/fluxum_adapter.ml (market data only)

    @see <https://github.com/struktured/fluxum/blob/main/CLAUDE.md> for architecture overview
*)

open Async

(** {1 Exchange Adapter Signature} *)

module type S = sig
  (** {2 Connection State} *)

  type t
  (** Opaque exchange connection state

      Contains authentication credentials, API client, rate limiters, etc.
      Implementation-specific - not exposed to users.
  *)

  (** {2 Exchange Identity} *)

  module Venue : sig
    val t : Types.Venue.t
    (** Which exchange this adapter implements

        Used for:
        - Logging and error messages
        - Symbol format conversions
        - Exchange-specific behavior switches

        Example: Gemini adapter returns [Types.Venue.Gemini]
    *)
  end

  (** {2 Native Types} *)

  module Native : sig
    (** Exchange-specific types (not normalized)

        These types mirror the exchange's actual API responses.
        Use Normalize module to convert to unified Types.

        {b Why separate Native types?}
        - Preserves all exchange-specific fields (for advanced users)
        - Allows zero-copy access to raw responses
        - Enables exchange-specific optimizations
    *)

    module Order : sig
      type id
      (** Exchange's internal order ID format

          Examples:
          - Gemini: string like "106817811"
          - Kraken: string like "OQCLML-BW3P3-BUCMWZ"
          - Binance: int64
      *)

      type request
      (** Order placement request structure

          Use exchange's order builder to construct valid requests:
          - Gemini: [Gemini.Order.build ~symbol ~side ~qty ~type_ ()]
          - Kraken: [Kraken.Order.build ~pair ~side ~volume ~ordertype ()]

          Typically includes: symbol, side, quantity, order type, price (for limits)
      *)

      type response
      (** Raw order response from exchange API

          Returned by [place_order].
          Use [Normalize.order_response] to convert to [Types.Order.t].

          Contains: order_id, status, filled_qty, remaining_qty, timestamps
      *)

      type status
      (** Order status query response

          Returned by [get_order_status] and [get_open_orders].
          Use [Normalize.order_from_status] to convert to [Types.Order.t].

          More detailed than [response] - includes execution history, fees, etc.
      *)
    end

    module Trade : sig
      type t
      (** User's trade execution (fill)

          Returned by [get_my_trades] and [Streams.trades].
          Use [Normalize.trade] to convert to [Types.Trade.t].

          Contains: trade_id, order_id, price, qty, fee, timestamp
      *)
    end

    module Balance : sig
      type t
      (** Account balance for a single currency

          Returned by [balances].
          Use [Normalize.balance] to convert to [Types.Balance.t].

          Contains: currency, total, available, locked
      *)
    end

    module Book : sig
      type update
      (** Incremental order book update

          Streamed by [Streams.book_updates].
          Use [Normalize.book_update] to convert to [Types.Book_update.t].

          Contains: side, price, size, timestamp
          Size = 0 means remove level.
      *)

      type snapshot
      (** Order book depth snapshot

          Returned by [get_order_book].
          Use [Normalize.order_book] to convert to [Types.Order_book.t].

          Contains: bids list, asks list, timestamp
      *)
    end

    module Symbol_info : sig
      type t
      (** Trading pair metadata

          Returned by [get_symbols].
          Use [Normalize.symbol_info] to convert to [Types.Symbol_info.t].

          Contains: symbol, base currency, quote currency, min qty, tick size
      *)
    end

    module Ticker : sig
      type t
      (** 24hr ticker statistics

          Returned by [get_ticker].
          Use [Normalize.ticker] to convert to [Types.Ticker.t].

          Contains: symbol, bid, ask, last, volume, high, low, change%
      *)
    end

    module Public_trade : sig
      type t
      (** Public market trade (not user-specific)

          Returned by [get_recent_trades].
          Use [Normalize.public_trade] to convert to [Types.Public_trade.t].

          Contains: price, qty, side (buyer/seller), timestamp
      *)
    end

    module Candle : sig
      type t
      (** OHLCV candle (historical price data)

          Returned by [get_candles].
          Use [Normalize.candle] to convert to [Types.Candle.t].

          Contains: open_time, open, high, low, close, volume, quote_volume
      *)
    end

    module Error : sig
      type t
      (** Exchange-specific error representation

          Use [Normalize.error] to convert to [Types.Error.t].

          Examples:
          - Gemini: polymorphic variant [`Invalid_signature | `Insufficient_balance | ...]
          - Kraken: polymorphic variant [`Unknown_asset_pair | `Invalid_volume | ...]
      *)
    end

    (** {3 Account Operations - Deposits/Withdrawals} *)

    module Deposit_address : sig
      type t
      (** Exchange-specific deposit address representation

          Use [Normalize.deposit_address] to convert to [Types.Deposit_address.t].

          Contains: currency, address, memo/tag (if applicable), network
      *)
    end

    module Deposit : sig
      type t
      (** Exchange-specific deposit record

          Use [Normalize.deposit] to convert to [Types.Deposit.t].

          Contains: id, currency, amount, status, tx_id, timestamps
      *)
    end

    module Withdrawal : sig
      type t
      (** Exchange-specific withdrawal record

          Use [Normalize.withdrawal] to convert to [Types.Withdrawal.t].

          Contains: id, currency, amount, fee, status, address, tx_id, timestamps
      *)
    end
  end

  (** {2 Trading Operations} *)

  val place_order :
    t ->
    Native.Order.request ->
    (Native.Order.response, Native.Error.t) Deferred.Result.t
  (** Place a new order on the exchange

      @param t Exchange adapter instance
      @param request Order details (use exchange's order builder)
      @return Native order response (use Normalize.order_response to convert)

      {b Errors:}
      - Insufficient balance
      - Invalid order parameters (price/qty outside exchange limits)
      - Rate limit exceeded
      - Exchange connectivity issues
      - Authentication failures

      {b Example:}
      {[
        let open Deferred.Result.Let_syntax in
        let request = Gemini.Order.build
          ~symbol:"btcusd"
          ~side:`Buy
          ~qty:0.1
          ~type_:`Limit
          ~price:50000.0
          ()
        in
        let%bind response = Adapter.place_order adapter request in
        match Adapter.Normalize.order_response response with
        | Ok order -> printf "Placed order: %s\n" order.order_id
        | Error msg -> failwith msg
      ]}

      {b Note:} Some exchanges (Hyperliquid, dYdX) do not support REST trading.
      Use blockchain signing or WebSocket only. Check exchange documentation.
  *)

  val cancel_order :
    t ->
    Native.Order.id ->
    (unit, Native.Error.t) Deferred.Result.t
  (** Cancel an existing order

      @param t Exchange adapter instance
      @param order_id Exchange-specific order ID (from order response)
      @return unit on success

      {b Errors:}
      - Order not found (already filled or canceled)
      - Order not cancelable (immediate-or-cancel)
      - Rate limit exceeded

      {b Note:} Some exchanges return updated order status instead of unit.
      Check exchange-specific adapter documentation.
  *)

  (** {2 Account Operations} *)

  val balances :
    t ->
    (Native.Balance.t list, Native.Error.t) Deferred.Result.t
  (** Get all account balances

      @return List of balances for all currencies
      @return May include zero balances (filter with [Balance.total > 0.0])

      {b Example:}
      {[
        let%bind native_balances = Adapter.balances adapter in
        let balances = List.map native_balances
          ~f:Adapter.Normalize.balance
        in
        List.iter balances ~f:(fun b ->
          if Float.(b.total > 0.0) then
            printf "%s: %.8f\n" b.currency b.total
        )
      ]}
  *)

  val get_order_status :
    t ->
    Native.Order.id ->
    (Native.Order.status, Native.Error.t) Deferred.Result.t
  (** Get status of a specific order by ID

      @return Detailed order status including execution history

      Use this to:
      - Check if order filled
      - Get average fill price
      - See fees paid
      - Track partial fills
  *)

  val get_open_orders :
    t ->
    ?symbol:Types.Symbol.t ->
    unit ->
    (Native.Order.status list, Native.Error.t) Deferred.Result.t
  (** Get all open orders, optionally filtered by symbol

      @param symbol Optional symbol filter (omit to get all symbols)
      @return List of open orders sorted by newest first (exchange-dependent)

      {b Rate Limit:} This is usually a high-weight endpoint.
      Use [Streams.book_updates] for real-time tracking instead.
  *)

  val get_order_history :
    t ->
    ?symbol:Types.Symbol.t ->
    ?limit:int ->
    unit ->
    (Native.Order.status list, Native.Error.t) Deferred.Result.t
  (** Get closed/historical orders

      @param symbol Optional symbol filter
      @param limit Maximum number of orders to return (default: exchange-specific, typically 100-500)
      @return List of closed orders (filled, canceled, rejected)

      {b Note:} Some exchanges have limited history (7-90 days).
      Use [get_my_trades] for longer-term records.
  *)

  val get_my_trades :
    t ->
    symbol:Types.Symbol.t ->
    ?limit:int ->
    unit ->
    (Native.Trade.t list, Native.Error.t) Deferred.Result.t
  (** Get user's trade history for a symbol

      @param symbol Required - trades are per-symbol
      @param limit Maximum trades to return (default: exchange-specific)
      @return List of trade executions (one order can have multiple trades)

      Use for:
      - P&L calculations (see Ledger_intf for automated tracking)
      - Tax reporting
      - Execution quality analysis
  *)

  (** {2 Market Data Operations} *)

  val get_symbols :
    t ->
    unit ->
    (Native.Symbol_info.t list, Native.Error.t) Deferred.Result.t
  (** Get available trading symbols/pairs

      @return List of all tradable symbols with metadata

      Use to:
      - Discover available markets
      - Get min/max order sizes
      - Get price precision (tick size)
  *)

  val get_ticker :
    t ->
    symbol:Types.Symbol.t ->
    unit ->
    (Native.Ticker.t, Native.Error.t) Deferred.Result.t
  (** Get 24hr ticker statistics for a symbol

      @return Current bid/ask, last price, volume, high/low, change%

      {b Rate Limit:} Use [Streams.book_updates] for real-time updates
      instead of polling this endpoint.
  *)

  val get_order_book :
    t ->
    symbol:Types.Symbol.t ->
    ?limit:int ->
    unit ->
    (Native.Book.snapshot, Native.Error.t) Deferred.Result.t
  (** Get order book depth snapshot

      @param limit Number of price levels per side (default: 100, max: exchange-specific)
      @return Full snapshot of bids and asks

      {b Use cases:}
      - Bootstrap order book before WebSocket streaming
      - Calculate market impact for large orders
      - Find liquidity zones

      {b Note:} For real-time tracking, use [Streams.book_updates] instead.
  *)

  val get_recent_trades :
    t ->
    symbol:Types.Symbol.t ->
    ?limit:int ->
    unit ->
    (Native.Public_trade.t list, Native.Error.t) Deferred.Result.t
  (** Get recent public trades for a symbol

      @param limit Number of trades to return (default: exchange-specific, typically 100-1000)
      @return List of recent market trades (buyer/seller agnostic)

      Use for:
      - Price discovery
      - Volume analysis
      - Trade flow visualization
  *)

  val get_candles :
    t ->
    symbol:Types.Symbol.t ->
    timeframe:Types.Timeframe.t ->
    ?since:Time_float_unix.t ->
    ?until:Time_float_unix.t ->
    ?limit:int ->
    unit ->
    (Native.Candle.t list, Native.Error.t) Deferred.Result.t
  (** Get historical OHLCV candles for a symbol

      @param symbol Trading pair
      @param timeframe Candle interval (1m, 5m, 1h, 1d, etc.)
      @param since Start time (oldest candle to fetch)
      @param until End time (newest candle to fetch)
      @param limit Maximum candles to return (default: exchange-specific, typically 500-1000)
      @return List of candles sorted by time ascending (oldest first)

      Use for:
      - Backtesting strategies
      - Technical analysis
      - Chart rendering

      {b Note:} Available timeframes vary by exchange. Check exchange docs.
      Most support: 1m, 5m, 15m, 1h, 4h, 1d.
  *)

  (** {2 Batch Operations} *)

  val cancel_all_orders :
    t ->
    ?symbol:Types.Symbol.t ->
    unit ->
    (int, Native.Error.t) Deferred.Result.t
  (** Cancel all open orders, optionally filtered by symbol

      @param symbol If provided, only cancel orders for this symbol
      @return Number of orders canceled

      {b Warning:} Destructive operation. Use with caution.
      Some exchanges may have rate limit penalties for bulk cancellations.
  *)

  (** {2 Account Operations - Deposits/Withdrawals} *)

  val get_deposit_address :
    t ->
    currency:string ->
    ?network:string ->
    unit ->
    (Native.Deposit_address.t, Native.Error.t) Deferred.Result.t
  (** Get a deposit address for a currency

      @param currency Currency code (e.g., "BTC", "ETH")
      @param network Optional network (e.g., "ETH", "TRC20" for USDT)
      @return Deposit address with optional memo/tag

      {b Note:} Some exchanges generate new addresses each call,
      others return the same address. Check exchange documentation.

      {b Warning:} Always verify the address matches the currency.
      Sending to wrong address may result in lost funds.
  *)

  val withdraw :
    t ->
    currency:string ->
    amount:float ->
    address:string ->
    ?tag:string ->
    ?network:string ->
    unit ->
    (Native.Withdrawal.t, Native.Error.t) Deferred.Result.t
  (** Initiate a withdrawal

      @param currency Currency code (e.g., "BTC", "ETH")
      @param amount Amount to withdraw
      @param address Destination blockchain address
      @param tag Optional memo/tag for XRP, XLM, etc.
      @param network Optional network for multi-network tokens (USDT, etc.)
      @return Withdrawal record with status

      {b Warning:} Double-check the address before calling.
      Withdrawals to incorrect addresses cannot be reversed.

      {b Rate Limits:} Some exchanges limit withdrawal frequency.

      {b Security:} Ensure withdrawal addresses are whitelisted if required.
  *)

  val get_deposits :
    t ->
    ?currency:string ->
    ?limit:int ->
    unit ->
    (Native.Deposit.t list, Native.Error.t) Deferred.Result.t
  (** Get deposit history

      @param currency Optional filter by currency
      @param limit Maximum number of deposits to return
      @return List of deposit records

      Use for:
      - Tracking incoming deposits
      - Reconciliation
      - Verifying deposit confirmations
  *)

  val get_withdrawals :
    t ->
    ?currency:string ->
    ?limit:int ->
    unit ->
    (Native.Withdrawal.t list, Native.Error.t) Deferred.Result.t
  (** Get withdrawal history

      @param currency Optional filter by currency
      @param limit Maximum number of withdrawals to return
      @return List of withdrawal records

      Use for:
      - Tracking outgoing withdrawals
      - Reconciliation
      - Verifying withdrawal status
  *)

  (** {2 Real-Time Streams} *)

  module Streams : sig
    val trades : t -> Native.Trade.t Pipe.Reader.t Deferred.t
    (** Stream of user's trade executions

        Emits a trade event every time an order fills (full or partial).

        {b Lifecycle:}
        - Pipe never closes (auto-reconnects on WebSocket disconnect)
        - To stop: close the returned pipe with [Pipe.close_read]

        {b Example:}
        {[
          let%bind trade_pipe = Streams.trades adapter in
          Pipe.iter_without_pushback trade_pipe ~f:(fun native_trade ->
            match Normalize.trade native_trade with
            | Ok trade ->
              printf "Fill: %s %.8f @ %.2f\n"
                trade.symbol trade.qty trade.price
            | Error msg ->
              Log.error "Failed to normalize trade: %s" msg
          )
        ]}
    *)

    val book_updates : t -> Native.Book.update Pipe.Reader.t Deferred.t
    (** Stream of order book updates

        Emits incremental updates (price level changes).
        Size = 0 means remove level.

        {b Bootstrap:}
        1. Get snapshot via [get_order_book]
        2. Subscribe to this stream
        3. Apply incremental updates to snapshot

        {b Example:}
        {[
          let%bind book = get_order_book adapter ~symbol:"btcusd" () in
          let%bind update_pipe = Streams.book_updates adapter in
          let book_ref = ref book in
          Pipe.iter_without_pushback update_pipe ~f:(fun update ->
            book_ref := apply_update !book_ref update;
            printf "Bid: %.2f  Ask: %.2f\n"
              (best_bid !book_ref).price
              (best_ask !book_ref).price
          )
        ]}

        {b Note:} Use Order_book_intf.S implementations (Gemini.Order_book, Kraken.Order_book)
        for automatic update application and management.
    *)
  end

  (** {2 Normalization} *)

  module Normalize : sig
    (** Convert exchange-specific types to unified Types

        {b Why normalization?}
        - Write exchange-agnostic trading logic
        - Use consolidated views (multi-exchange order books, balances)
        - Simplified testing and comparison

        {b Fallible Operations:}
        Most normalize functions return [(Types.t, string) Result.t]
        because exchange APIs can return malformed data:
        - Missing required fields
        - Invalid numeric strings
        - Unrecognized enum values
        - Null values in non-optional fields

        {b Error Handling:}
        {[
          match Normalize.ticker native_ticker with
          | Ok ticker -> process_ticker ticker
          | Error msg ->
            Log.error "Normalization failed: %s" msg;
            (* Don't crash - handle gracefully *)
        ]}

        See Phase 1 changes (git log) for migration from infallible to fallible.
    *)

    val order_response : Native.Order.response -> (Types.Order.t, string) Result.t
    (** Normalize order placement response

        Converts exchange's order response to unified [Types.Order.t].

        {b Errors:}
        - Missing order_id
        - Invalid price/qty strings
        - Unrecognized order status
        - Malformed timestamps

        {b Example:}
        {[
          let%bind response = place_order adapter request in
          match Normalize.order_response response with
          | Ok order -> printf "Order placed: %s\n" order.order_id
          | Error msg -> failwithf "Failed to normalize: %s" msg ()
        ]}
    *)

    val order_status : Native.Order.status -> Types.Order_status.t
    (** Normalize order status enum

        This is NOT fallible - order status is always normalizable.
        Unknown statuses map to a reasonable default (typically New).

        Use [order_from_status] for full order conversion.
    *)

    val order_from_status : Native.Order.status -> (Types.Order.t, string) Result.t
    (** Convert order status query to full order

        Like [order_response] but for status queries.
        Status queries include more fields (fees, avg price, etc).
    *)

    val trade : Native.Trade.t -> (Types.Trade.t, string) Result.t
    (** Normalize trade execution

        {b Errors:}
        - Invalid price/qty
        - Missing trade_id or order_id
        - Invalid side
    *)

    val balance : Native.Balance.t -> (Types.Balance.t, string) Result.t
    (** Normalize account balance

        {b Errors:}
        - Invalid amounts (non-numeric, negative, NaN)
        - Missing currency
        - Total != available + locked (consistency check)
    *)

    val book_update : Native.Book.update -> Types.Book_update.t
    (** Normalize order book update

        NOT fallible - book updates are simple (side, price, size).
        Invalid updates are logged but not rejected.
    *)

    val symbol_info : Native.Symbol_info.t -> Types.Symbol_info.t
    (** Normalize trading pair metadata

        NOT fallible - uses exchange defaults for missing fields.
    *)

    val ticker : Native.Ticker.t -> (Types.Ticker.t, string) Result.t
    (** Normalize ticker statistics

        {b Errors:}
        - Null bid/ask/last price
        - Invalid numeric strings
        - Missing timestamp
    *)

    val order_book : Native.Book.snapshot -> (Types.Order_book.t, string) Result.t
    (** Normalize order book snapshot

        {b Errors:}
        - Missing bids or asks lists
        - Invalid price levels
        - Negative prices or quantities
    *)

    val public_trade : Native.Public_trade.t -> (Types.Public_trade.t, string) Result.t
    (** Normalize public trade

        {b Errors:}
        - Invalid price/qty
        - Missing timestamp
        - Invalid side
    *)

    val candle : Native.Candle.t -> (Types.Candle.t, string) Result.t
    (** Normalize OHLCV candle

        {b Errors:}
        - Invalid price values (open, high, low, close)
        - Invalid volume
        - Missing timestamp
        - high < low (data integrity check)
    *)

    val error : Native.Error.t -> Types.Error.t
    (** Normalize exchange-specific error to unified error type

        This is the ONLY normalize function that is NOT fallible.
        Error conversion should never fail (worst case: Unknown error).

        Maps exchange-specific error variants to [Types.Error.t]:
        - Insufficient_balance
        - Invalid_order
        - Rate_limit_exceeded
        - Authentication_failed
        - etc.
    *)

    (** {3 Account Operations Normalization} *)

    val deposit_address : Native.Deposit_address.t -> (Types.Deposit_address.t, string) Result.t
    (** Normalize deposit address

        {b Errors:}
        - Missing address
        - Invalid currency
    *)

    val deposit : Native.Deposit.t -> (Types.Deposit.t, string) Result.t
    (** Normalize deposit record

        {b Errors:}
        - Invalid amount
        - Unknown status
        - Missing required fields
    *)

    val withdrawal : Native.Withdrawal.t -> (Types.Withdrawal.t, string) Result.t
    (** Normalize withdrawal record

        {b Errors:}
        - Invalid amount or fee
        - Unknown status
        - Missing required fields
    *)
  end
end

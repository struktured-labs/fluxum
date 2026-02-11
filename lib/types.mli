(** Normalized Types for Multi-Exchange Trading

    This module defines the unified type system used across all exchange adapters.
    All exchanges normalize their native types to these common representations.

    {b Key Design Principles:}
    - Exchange-agnostic: Works with any CEX or DEX
    - Type-safe: Leverages OCaml's type system
    - Extensible: Easy to add new exchanges or fields
    - Portable: Code using these types works across all supported venues

    {b Symbol Formats:}
    Exchanges use different symbol formats. Adapters handle conversion:
    - Gemini: "btcusd" (lowercase, no separator)
    - Kraken: "XBTUSD" (uppercase, XBT for Bitcoin)
    - Binance: "BTCUSDT" (uppercase, no separator)
    - Use exchange-specific adapters for normalization

    {b Precision:}
    - Prices: Float with exchange-dependent precision (2-8 decimals)
    - Quantities: Float with symbol-dependent precision
    - No automatic rounding - use exchange-specific rules
*)

open Core

(** {1 Exchange Venues} *)

module Venue : sig
  (** Supported exchange venues (CEX and DEX) *)
  type t =
    | Gemini        (** Gemini - US-based CEX *)
    | Kraken        (** Kraken - Global CEX *)
    | Mexc          (** MEXC - Asian CEX *)
    | Coinbase      (** Coinbase - US-based CEX *)
    | Binance       (** Binance - Global CEX *)
    | Bybit         (** Bybit - Global CEX (derivatives leader) *)
    | Okx           (** OKX - Global CEX (derivatives and spot) *)
    | Hyperliquid   (** Hyperliquid - L1 derivatives DEX *)
    | Bitrue        (** Bitrue - Global CEX *)
    | Bitstamp      (** Bitstamp - European CEX *)
    | Dydx          (** dYdX v4 - DeFi perpetuals *)
    | Jupiter       (** Jupiter - Solana DEX aggregator *)
    | OneInch       (** 1inch - EVM DEX aggregator *)
    | Gmx           (** GMX - DeFi perpetuals (planned) *)
    | Aave          (** Aave - DeFi lending (planned) *)
    | Compound      (** Compound - DeFi lending (planned) *)
    | Uniswap_v3    (** Uniswap V3 - Concentrated liquidity DEX *)
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  (** Human-readable exchange name *)

  val is_defi : t -> bool
  (** @return true for decentralized exchanges (dYdX, Jupiter, 1inch, GMX, Aave, Compound, Uniswap V3) *)
end

(** {1 Trading Pair Identifiers} *)

module Symbol : sig
  (** Trading pair symbol

      Format varies by exchange - use adapter functions for conversion:
      - Gemini: "btcusd"
      - Kraken: "XBTUSD"
      - Binance: "BTCUSDT"
  *)
  type t = string [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Currency : sig
  (** Currency/asset identifier (e.g., "BTC", "ETH", "USD") *)
  type t = string [@@deriving sexp, compare, equal]
end

(** {1 Numeric Types} *)

module Price : sig
  (** Price in quote currency

      Precision: 2-8 decimal places (exchange-dependent)
      Validation: None - caller must ensure positive values
      Rounding: None - use exchange-specific precision rules
  *)
  type t = float [@@deriving sexp, compare, equal]

  module Option : sig
    type t = float option [@@deriving sexp, compare, equal]
  end

  val of_string : string -> t
  val of_string_opt : string -> t option
  val to_string : t -> string
end

module Qty : sig
  (** Quantity in base currency

      Precision: Varies by exchange and symbol
      Minimum: Typically > 0, but 0 allowed for order cancellations
  *)
  type t = float [@@deriving sexp, compare, equal]

  val of_string : string -> t
  val of_string_opt : string -> t option
  val to_string : t -> string
end

(** {1 Order Types} *)

module Side : sig
  (** Order side - Buy or Sell *)
  type t = Buy | Sell [@@deriving sexp, compare, equal]

  val opposite : t -> t
  (** @return Buy → Sell, Sell → Buy *)

  val to_string : t -> string
  (** @return "buy" or "sell" (lowercase) *)

  val of_string_opt : string -> t option
  (** Parse from string, handles multiple formats:
      - "buy", "Buy", "BUY" → Some Buy
      - "sell", "Sell", "SELL" → Some Sell
      - Other → None
  *)

  module Option : sig
    type side = t [@@deriving sexp, compare, equal]
    type t = side option [@@deriving sexp, compare, equal]
  end
end

module Time_in_force : sig
  (** Order duration/execution policy *)
  type t =
    | GTC           (** Good till canceled (default) *)
    | IOC           (** Immediate or cancel - fill available, cancel rest *)
    | FOK           (** Fill or kill - complete fill or cancel *)
    | GTD of Time_float_unix.t  (** Good till date *)
  [@@deriving sexp, compare]

  val to_string : t -> string
  val of_string_opt : string -> t option
end

module Order_kind : sig
  (** Basic order types without trigger conditions *)
  type basic =
    | Market
    | Limit of Price.t
    | Post_only of Price.t  (** Maker-only limit order *)
  [@@deriving sexp, compare]

  (** Conditional/stop order types with trigger prices *)
  type conditional =
    | Stop_market of Price.t         (** Market order triggered at stop price *)
    | Stop_limit of { stop : Price.t; limit : Price.t }
    | Take_profit_market of Price.t
    | Take_profit_limit of { trigger : Price.t; limit : Price.t }
    | Trailing_stop of { callback_rate : float }  (** Trailing stop by % *)
  [@@deriving sexp, compare]

  type t =
    | Basic of basic
    | Conditional of conditional
  [@@deriving sexp, compare]

  (** Convenience constructors *)
  val market : t
  val limit : Price.t -> t
  val post_only : Price.t -> t
  val stop_market : Price.t -> t
  val stop_limit : stop:Price.t -> limit:Price.t -> t
  val take_profit_market : Price.t -> t
  val take_profit_limit : trigger:Price.t -> limit:Price.t -> t
  val trailing_stop : callback_rate:float -> t

  (** Extract limit price if present *)
  val limit_price : t -> Price.t option

  (** Extract trigger/stop price if present *)
  val trigger_price : t -> Price.t option

  (** Check if this is a conditional/stop order *)
  val is_conditional : t -> bool
end

module Order_status : sig
  (** Order lifecycle status *)
  type t =
    | New                  (** Order accepted but not yet filled *)
    | Partially_filled     (** Partial execution *)
    | Filled               (** Fully executed *)
    | Canceled             (** Canceled by user or exchange *)
    | Rejected of string   (** Exchange rejected - reason in string *)
  [@@deriving sexp, compare]
end

module Order : sig
  (** Normalized order representation

      Combines order placement details with execution state.
      All exchanges normalize to this format via Exchange_intf.Normalize.
  *)

  type id = string [@@deriving sexp, compare, equal]
  (** Exchange-specific order ID (typically int64 or UUID as string) *)

  type t =
    { venue         : Venue.t
    ; id            : id
    ; symbol        : Symbol.t
    ; side          : Side.t
    ; kind          : Order_kind.t
    ; time_in_force : Time_in_force.t             (** Order duration policy *)
    ; qty           : Qty.t                       (** Original order quantity *)
    ; filled        : Qty.t                       (** Quantity filled so far *)
    ; status        : Order_status.t
    ; created_at    : Time_float_unix.t option    (** Order creation time *)
    ; updated_at    : Time_float_unix.t option    (** Last update time *)
    }
  [@@deriving sexp, fields]
end

(** {1 Trade Execution} *)

module Trade : sig
  (** User's filled trade (not public market trades)

      Represents a completed fill of an order.
      One order may result in multiple trades.
  *)
  type t =
    { venue    : Venue.t
    ; symbol   : Symbol.t
    ; side     : Side.t
    ; price    : Price.t            (** Execution price *)
    ; qty      : Qty.t              (** Quantity filled *)
    ; fee      : Qty.t option       (** Trading fee (in quote currency) *)
    ; trade_id : string option      (** Exchange's trade ID *)
    ; ts       : Time_float_unix.t option  (** Execution timestamp *)
    }
  [@@deriving sexp, fields]
end

(** {1 Account Balance} *)

module Balance : sig
  (** Account balance for a single currency

      Tracks total, available, and locked amounts.
  *)
  type t =
    { venue     : Venue.t
    ; currency  : Currency.t
    ; total     : Qty.t         (** Total balance (available + locked) *)
    ; available : Qty.t         (** Available for trading *)
    ; locked    : Qty.t         (** Locked in open orders *)
    }
  [@@deriving sexp, fields]
end

(** {1 Market Data} *)

module Symbol_info : sig
  (** Trading pair metadata and rules *)
  type t =
    { venue          : Venue.t
    ; symbol         : Symbol.t
    ; base_currency  : Currency.t       (** Asset being traded (e.g., BTC) *)
    ; quote_currency : Currency.t       (** Pricing currency (e.g., USD) *)
    ; status         : string           (** "trading", "halt", "auction", etc. *)
    ; min_order_size : Qty.t            (** Minimum order quantity *)
    ; tick_size      : Price.t option   (** Price increment *)
    ; quote_increment : Price.t option  (** Quote currency increment *)
    }
  [@@deriving sexp, fields]

  val create :
    venue:Venue.t ->
    symbol:Symbol.t ->
    base_currency:Currency.t ->
    quote_currency:Currency.t ->
    status:string ->
    min_order_size:Qty.t ->
    ?tick_size:Price.t ->
    ?quote_increment:Price.t ->
    unit -> t
end

module Book_update : sig
  (** Incremental order book update (for WebSocket streams) *)

  module Side : sig
    type t = Bid | Ask [@@deriving sexp, compare, equal]
  end

  type level =
    { price : Price.t
    ; qty   : Qty.t  (** 0 means level removed *)
    }
  [@@deriving sexp, fields]

  type t =
    { venue       : Venue.t
    ; symbol      : Symbol.t
    ; side        : Side.t
    ; levels      : level list       (** Price levels updated *)
    ; ts          : Time_float_unix.t option
    ; is_snapshot : bool             (** true = full book, false = incremental *)
    }
  [@@deriving sexp, fields]
end

module Order_book : sig
  (** Normalized order book snapshot

      Provides a point-in-time view of the order book.
      Used by DeFi adapters and exchange-agnostic code.
  *)

  module Price_level : sig
    (** Single price level in the order book *)
    type t =
      { price  : Price.t
      ; volume : Qty.t
      }
    [@@deriving sexp, compare, equal, fields]

    val create : price:Price.t -> volume:Qty.t -> t
    val empty : t
  end

  type t =
    { venue  : Venue.t
    ; symbol : Symbol.t
    ; bids   : Price_level.t list  (** Sorted descending by price *)
    ; asks   : Price_level.t list  (** Sorted ascending by price *)
    ; ts     : Time_float_unix.t option
    ; epoch  : int                 (** Update sequence number *)
    }
  [@@deriving sexp, fields]

  val create : venue:Venue.t -> symbol:Symbol.t -> t
  (** Create empty order book *)

  val best_bid : t -> Price_level.t
  (** Get best bid (highest buy price). Returns empty if no bids. *)

  val best_ask : t -> Price_level.t
  (** Get best ask (lowest sell price). Returns empty if no asks. *)

  val mid_price : t -> float
  (** Calculate mid price: (best_bid + best_ask) / 2.
      Returns 0 if book is empty. *)

  val spread : t -> float
  (** Calculate spread: best_ask - best_bid.
      Returns 0 if book is empty. *)

  val best_n_bids : t -> n:int -> Price_level.t list
  (** Get top N bids (highest prices first) *)

  val best_n_asks : t -> n:int -> Price_level.t list
  (** Get top N asks (lowest prices first) *)

  val total_volume_n : t -> side:[`Bid | `Ask] -> n:int -> float
  (** Calculate total volume in top N levels of a side *)
end

module Ticker : sig
  (** 24-hour ticker statistics for a trading pair *)
  type t =
    { venue            : Venue.t
    ; symbol           : Symbol.t
    ; last_price       : Price.t
    ; bid_price        : Price.t     (** Current best bid *)
    ; ask_price        : Price.t     (** Current best ask *)
    ; high_24h         : Price.t
    ; low_24h          : Price.t
    ; volume_24h       : Qty.t       (** Base asset volume *)
    ; quote_volume     : Qty.t option (** Quote asset volume *)
    ; price_change     : Price.t option (** 24h price change (absolute) *)
    ; price_change_pct : float option (** 24h price change (percentage) *)
    ; ts               : Time_float_unix.t option
    }
  [@@deriving sexp, fields]

  val create :
    venue:Venue.t ->
    symbol:Symbol.t ->
    last_price:Price.t ->
    bid_price:Price.t ->
    ask_price:Price.t ->
    high_24h:Price.t ->
    low_24h:Price.t ->
    volume_24h:Qty.t ->
    ?quote_volume:Qty.t ->
    ?price_change:Price.t ->
    ?price_change_pct:float ->
    ?ts:Time_float_unix.t ->
    unit -> t
end

module Public_trade : sig
  (** Public market trade (ticker tape)

      Not user-specific - represents a trade that occurred on the exchange.
      Used for market analysis and tape reading.
  *)
  type t =
    { venue    : Venue.t
    ; symbol   : Symbol.t
    ; price    : Price.t
    ; qty      : Qty.t
    ; side     : Side.t option  (** May not be available on all exchanges *)
    ; trade_id : string option
    ; ts       : Time_float_unix.t option
    }
  [@@deriving sexp, fields]

  val create :
    venue:Venue.t ->
    symbol:Symbol.t ->
    price:Price.t ->
    qty:Qty.t ->
    ?side:Side.t ->
    ?trade_id:string ->
    ?ts:Time_float_unix.t ->
    unit -> t
end

(** {1 Historical Market Data} *)

module Timeframe : sig
  (** Candle timeframe (interval) for OHLCV data *)
  type t =
    | M1    (** 1 minute *)
    | M3    (** 3 minutes *)
    | M5    (** 5 minutes *)
    | M15   (** 15 minutes *)
    | M30   (** 30 minutes *)
    | H1    (** 1 hour *)
    | H2    (** 2 hours *)
    | H4    (** 4 hours *)
    | H6    (** 6 hours *)
    | H8    (** 8 hours *)
    | H12   (** 12 hours *)
    | D1    (** 1 day *)
    | D3    (** 3 days *)
    | W1    (** 1 week *)
    | MO1   (** 1 month *)
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  (** Convert to common string format: "1m", "5m", "1h", "1d", etc. *)

  val of_string : string -> t
  (** Parse timeframe string. Raises on invalid input. *)

  val of_string_opt : string -> t option
  (** Parse timeframe string safely *)

  val to_seconds : t -> int
  (** Convert timeframe to duration in seconds *)
end

module Candle : sig
  (** OHLCV (Open-High-Low-Close-Volume) candle data

      Historical price data aggregated over a timeframe.
      Used for backtesting, technical analysis, and charting.
  *)
  type t =
    { venue        : Venue.t
    ; symbol       : Symbol.t
    ; timeframe    : Timeframe.t
    ; open_time    : Time_float_unix.t  (** Candle open timestamp *)
    ; open_        : Price.t
    ; high         : Price.t
    ; low          : Price.t
    ; close        : Price.t
    ; volume       : Qty.t              (** Base asset volume *)
    ; quote_volume : Qty.t option       (** Quote asset volume (if available) *)
    ; trades       : int option         (** Number of trades in candle *)
    ; closed       : bool               (** Whether candle is finalized *)
    }
  [@@deriving sexp, fields]

  val create :
    venue:Venue.t ->
    symbol:Symbol.t ->
    timeframe:Timeframe.t ->
    open_time:Time_float_unix.t ->
    open_:Price.t ->
    high:Price.t ->
    low:Price.t ->
    close:Price.t ->
    volume:Qty.t ->
    ?quote_volume:Qty.t ->
    ?trades:int ->
    ?closed:bool ->
    unit -> t

  val mid : t -> float
  (** Mid price: (high + low) / 2 *)

  val typical : t -> float
  (** Typical price: (high + low + close) / 3 *)

  val body : t -> float
  (** Candle body size: |close - open| *)

  val range : t -> float
  (** Candle range: high - low *)

  val is_bullish : t -> bool
  (** Green candle: close > open *)

  val is_bearish : t -> bool
  (** Red candle: close < open *)

  val close_time : t -> Time_float_unix.t
  (** Calculate close time based on open_time + timeframe *)

  val compare_by_time : t -> t -> int
  (** Compare candles by open_time *)
end

(** {1 Account Operations - Deposits/Withdrawals} *)

module Transfer_status : sig
  (** Status of a deposit or withdrawal transfer *)
  type t =
    | Pending      (** Transfer initiated but not processed *)
    | Processing   (** Transfer is being processed *)
    | Completed    (** Transfer completed successfully *)
    | Failed       (** Transfer failed *)
    | Cancelled    (** Transfer was cancelled *)
  [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string_opt : string -> t option
  val of_string : string -> t
end

module Deposit_address : sig
  (** Deposit address for receiving funds

      Contains the address to send funds to, with optional memo/tag
      for networks that require them (XRP, XLM, etc.).
  *)
  type t =
    { venue : Venue.t
    ; currency : string           (** Currency code (e.g., "BTC", "ETH") *)
    ; address : string            (** Blockchain address *)
    ; tag : string option         (** Memo/tag for XRP, XLM, etc. *)
    ; network : string option     (** Network identifier (ETH, TRC20, etc.) *)
    }
  [@@deriving sexp, compare, equal, fields]

  val create :
    venue:Venue.t ->
    currency:string ->
    address:string ->
    ?tag:string ->
    ?network:string ->
    unit -> t
end

module Deposit : sig
  (** Deposit record

      Represents an incoming deposit to the exchange account.
  *)
  type t =
    { venue : Venue.t
    ; id : string                           (** Exchange's deposit ID *)
    ; currency : string                     (** Currency code *)
    ; amount : float                        (** Deposit amount *)
    ; status : Transfer_status.t            (** Current status *)
    ; address : string option               (** Receiving address *)
    ; tx_id : string option                 (** Blockchain transaction ID *)
    ; created_at : Time_float_unix.t option (** When deposit was detected *)
    ; updated_at : Time_float_unix.t option (** Last status update *)
    }
  [@@deriving sexp, compare, equal, fields]

  val create :
    venue:Venue.t ->
    id:string ->
    currency:string ->
    amount:float ->
    status:Transfer_status.t ->
    ?address:string ->
    ?tx_id:string ->
    ?created_at:Time_float_unix.t ->
    ?updated_at:Time_float_unix.t ->
    unit -> t
end

module Withdrawal : sig
  (** Withdrawal record

      Represents an outgoing withdrawal from the exchange account.
  *)
  type t =
    { venue : Venue.t
    ; id : string                           (** Exchange's withdrawal ID *)
    ; currency : string                     (** Currency code *)
    ; amount : float                        (** Withdrawal amount *)
    ; fee : float option                    (** Withdrawal fee charged *)
    ; status : Transfer_status.t            (** Current status *)
    ; address : string                      (** Destination address *)
    ; tag : string option                   (** Memo/tag for XRP, XLM, etc. *)
    ; tx_id : string option                 (** Blockchain transaction ID *)
    ; created_at : Time_float_unix.t option (** When withdrawal was initiated *)
    ; updated_at : Time_float_unix.t option (** Last status update *)
    }
  [@@deriving sexp, compare, equal, fields]

  val create :
    venue:Venue.t ->
    id:string ->
    currency:string ->
    amount:float ->
    ?fee:float ->
    status:Transfer_status.t ->
    address:string ->
    ?tag:string ->
    ?tx_id:string ->
    ?created_at:Time_float_unix.t ->
    ?updated_at:Time_float_unix.t ->
    unit -> t
end

(** {1 Prediction Markets} *)

module Prediction_outcome : sig
  (** Binary outcome for prediction market contracts *)
  type t = Yes | No [@@deriving sexp, compare, equal]

  val to_string : t -> string
  val of_string_opt : string -> t option
  val of_string : string -> t
end

module Prediction_contract : sig
  (** A single contract within a prediction market event *)
  type t =
    { instrument_symbol : string  (** Tradeable symbol (e.g., GEMI-BTC100K-YES) *)
    ; label : string
    ; ticker : string
    ; last_price : Price.t option  (** Last trade price (0.00-1.00) *)
    ; best_bid : Price.t option
    ; best_ask : Price.t option
    ; total_shares : Qty.t
    ; status : string
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_event : sig
  (** A prediction market event with its contracts *)
  type t =
    { venue : Venue.t
    ; id : string
    ; title : string
    ; description : string
    ; category : string
    ; ticker : string              (** Event ticker (e.g., BTC100K) *)
    ; status : string
    ; volume : Qty.t
    ; liquidity : Qty.t
    ; contracts : Prediction_contract.t list
    ; is_live : bool
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_order : sig
  (** A prediction market order *)
  type t =
    { venue : Venue.t
    ; id : string
    ; symbol : string              (** Instrument symbol *)
    ; side : Side.t
    ; outcome : Prediction_outcome.t
    ; qty : Qty.t                  (** Number of contracts *)
    ; filled : Qty.t
    ; remaining : Qty.t
    ; price : Price.t              (** Probability price (0.00-1.00) *)
    ; avg_execution_price : Price.t option
    ; status : string
    ; event_ticker : string option
    ; created_at : Time_float_unix.t option
    ; updated_at : Time_float_unix.t option
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_position : sig
  (** A prediction market position *)
  type t =
    { venue : Venue.t
    ; symbol : string              (** Instrument symbol *)
    ; outcome : Prediction_outcome.t
    ; qty : Qty.t
    ; avg_price : Price.t          (** Average entry price (0.00-1.00) *)
    ; event_ticker : string option
    ; contract_name : string option
    }
  [@@deriving sexp, compare, equal, fields]
end

(** {1 Error Handling} *)

module Error : sig
  (** Unified error type across all exchanges *)
  type t =
    | Transport of exn [@sexp.opaque]
      (** Network/transport errors (connection failures, timeouts) *)
    | Rate_limited
      (** Exchange rate limit exceeded *)
    | Auth_failed
      (** API authentication failed (invalid key, signature, etc.) *)
    | Normalization_error of string
      (** Failed to convert exchange type to normalized type *)
    | Exchange_specific of
        { venue   : Venue.t
        ; code    : string   (** Exchange error code *)
        ; message : string   (** Exchange error message *)
        }
      (** Exchange-specific error (see exchange documentation) *)
  [@@deriving sexp_of]
end

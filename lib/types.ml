open Core

module Venue = struct
  type t =
    | Gemini
    | Kraken
    | Mexc
    | Coinbase
    | Binance
    | Bybit
    | Okx
    | Hyperliquid
    | Bitrue
    | Bitstamp  (** Bitstamp - European exchange *)
    | Dydx      (** dYdX v4 - DeFi perpetuals *)
    | Jupiter   (** Jupiter - Solana DEX aggregator *)
    | OneInch   (** 1inch - EVM DEX aggregator *)
    | Gmx       (** GMX - DeFi perpetuals (planned) *)
    | Aave      (** Aave - DeFi lending (planned) *)
    | Compound  (** Compound - DeFi lending (planned) *)
    | Uniswap_v3 (** Uniswap V3 - Concentrated liquidity DEX *)
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Gemini -> "Gemini"
    | Kraken -> "Kraken"
    | Mexc -> "MEXC"
    | Coinbase -> "Coinbase"
    | Binance -> "Binance"
    | Bybit -> "Bybit"
    | Okx -> "OKX"
    | Hyperliquid -> "Hyperliquid"
    | Bitrue -> "Bitrue"
    | Bitstamp -> "Bitstamp"
    | Dydx -> "dYdX"
    | Jupiter -> "Jupiter"
    | OneInch -> "1inch"
    | Gmx -> "GMX"
    | Aave -> "Aave"
    | Compound -> "Compound"
    | Uniswap_v3 -> "Uniswap V3"

  let is_defi = function
    | Dydx | Jupiter | OneInch | Gmx | Aave | Compound | Uniswap_v3 -> true
    | Gemini | Kraken | Mexc | Coinbase | Binance | Bybit | Okx | Hyperliquid | Bitrue | Bitstamp -> false
end

module Symbol = struct
  type t = string [@@deriving sexp, compare, equal]

  include Comparable.Make(struct
    type t = string [@@deriving sexp, compare]
  end)

  let of_string s = s
  let to_string s = s
end

module Currency = struct
  type t = string [@@deriving sexp, compare, equal]
end

module Price = struct
  type t = float [@@deriving sexp, compare, equal]

  module Option = struct
    type t = float option [@@deriving sexp, compare, equal]
  end

  let of_string = Float.of_string
  let of_string_opt = Float.of_string_opt
  let to_string = Float.to_string
end

module Qty = struct
  type t = float [@@deriving sexp, compare, equal]

  let of_string = Float.of_string
  let of_string_opt = Float.of_string_opt
  let to_string = Float.to_string
end

module Side = struct
  type t = Buy | Sell [@@deriving sexp, compare, equal]

  let opposite = function
    | Buy -> Sell
    | Sell -> Buy

  let to_string = function
    | Buy -> "buy"
    | Sell -> "sell"

  let of_string_opt = function
    | "buy" | "Buy" | "BUY" -> Some Buy
    | "sell" | "Sell" | "SELL" -> Some Sell
    | _ -> None

  module Option = struct
    type side = t [@@deriving sexp, compare, equal]
    type t = side option [@@deriving sexp, compare, equal]
  end
end

module Time_in_force = struct
  type t =
    | GTC           (** Good till canceled (default) *)
    | IOC           (** Immediate or cancel - fill what's available, cancel rest *)
    | FOK           (** Fill or kill - fill entire order or cancel completely *)
    | GTD of Time_float_unix.t  (** Good till date - cancel after specified time *)
  [@@deriving sexp, compare]

  let to_string = function
    | GTC -> "GTC"
    | IOC -> "IOC"
    | FOK -> "FOK"
    | GTD _ -> "GTD"

  let of_string_opt = function
    | "GTC" | "gtc" -> Some GTC
    | "IOC" | "ioc" -> Some IOC
    | "FOK" | "fok" -> Some FOK
    | _ -> None
end

module Order_kind = struct
  (** Basic order type without trigger conditions *)
  type basic =
    | Market
    | Limit of Price.t
    | Post_only of Price.t  (** Maker-only limit order *)
  [@@deriving sexp, compare]

  (** Conditional/stop order types with trigger prices *)
  type conditional =
    | Stop_market of Price.t         (** Trigger market order at stop price *)
    | Stop_limit of { stop : Price.t; limit : Price.t }  (** Trigger limit at stop *)
    | Take_profit_market of Price.t  (** Take profit market order *)
    | Take_profit_limit of { trigger : Price.t; limit : Price.t }
    | Trailing_stop of { callback_rate : float }  (** Trailing stop by percentage *)
  [@@deriving sexp, compare]

  type t =
    | Basic of basic
    | Conditional of conditional
  [@@deriving sexp, compare]

  (** Convenience constructors *)
  let market = Basic Market
  let limit price = Basic (Limit price)
  let post_only price = Basic (Post_only price)
  let stop_market stop = Conditional (Stop_market stop)
  let stop_limit ~stop ~limit = Conditional (Stop_limit { stop; limit })
  let take_profit_market trigger = Conditional (Take_profit_market trigger)
  let take_profit_limit ~trigger ~limit = Conditional (Take_profit_limit { trigger; limit })
  let trailing_stop ~callback_rate = Conditional (Trailing_stop { callback_rate })

  (** For backward compatibility - extract limit price if present *)
  let limit_price = function
    | Basic (Limit p) | Basic (Post_only p) -> Some p
    | Conditional (Stop_limit { limit; _ }) -> Some limit
    | Conditional (Take_profit_limit { limit; _ }) -> Some limit
    | _ -> None

  (** Extract trigger/stop price if present *)
  let trigger_price = function
    | Conditional (Stop_market p) -> Some p
    | Conditional (Stop_limit { stop; _ }) -> Some stop
    | Conditional (Take_profit_market p) -> Some p
    | Conditional (Take_profit_limit { trigger; _ }) -> Some trigger
    | _ -> None

  (** Check if this is a conditional/stop order *)
  let is_conditional = function
    | Conditional _ -> true
    | Basic _ -> false
end

module Order_status = struct
  type t =
    | New
    | Partially_filled
    | Filled
    | Canceled
    | Rejected of string
  [@@deriving sexp, compare]
end

module Order = struct
  type id = string [@@deriving sexp, compare, equal]

  type t =
    { venue         : Venue.t
    ; id            : id
    ; symbol        : Symbol.t
    ; side          : Side.t
    ; kind          : Order_kind.t
    ; time_in_force : Time_in_force.t  (** Order duration policy *)
    ; qty           : Qty.t
    ; filled        : Qty.t
    ; status        : Order_status.t
    ; created_at    : Time_float_unix.t option
    ; updated_at    : Time_float_unix.t option
    }
  [@@deriving sexp, fields]
end

module Trade = struct
  type t =
    { venue    : Venue.t
    ; symbol   : Symbol.t
    ; side     : Side.t
    ; price    : Price.t
    ; qty      : Qty.t
    ; fee      : Qty.t option
    ; trade_id : string option
    ; ts       : Time_float_unix.t option
    }
  [@@deriving sexp, fields]
end

module Balance = struct
  type t =
    { venue    : Venue.t
    ; currency : Currency.t
    ; total    : Qty.t
    ; available : Qty.t
    ; locked   : Qty.t
    }
  [@@deriving sexp, fields]
end

module Symbol_info = struct
  type t =
    { venue : Venue.t
    ; symbol : Symbol.t
    ; base_currency : Currency.t
    ; quote_currency : Currency.t
    ; status : string  (** e.g., "trading", "halt", "auction" *)
    ; min_order_size : Qty.t
    ; tick_size : Price.t option
    ; quote_increment : Price.t option
    }
  [@@deriving sexp, fields]

  let create ~venue ~symbol ~base_currency ~quote_currency ~status ~min_order_size
      ?tick_size ?quote_increment () =
    { venue; symbol; base_currency; quote_currency; status; min_order_size;
      tick_size; quote_increment }
end

module Book_update = struct
  module Side = struct
    type t = Bid | Ask [@@deriving sexp, compare, equal]
  end

  type level =
    { price : Price.t
    ; qty   : Qty.t
    }
  [@@deriving sexp, fields]

  type t =
    { venue      : Venue.t
    ; symbol     : Symbol.t
    ; side       : Side.t
    ; levels     : level list
    ; ts         : Time_float_unix.t option
    ; is_snapshot : bool
    }
  [@@deriving sexp, fields]
end

(** Order book types for DeFi and exchange-agnostic order books *)
module Order_book = struct
  (** A single price level in the order book *)
  module Price_level = struct
    type t =
      { price  : Price.t
      ; volume : Qty.t
      }
    [@@deriving sexp, compare, equal, fields]

    let create ~price ~volume = { price; volume }

    let empty = { price = 0.; volume = 0. }
  end

  (** Order book snapshot with bids and asks *)
  type t =
    { venue      : Venue.t
    ; symbol     : Symbol.t
    ; bids       : Price_level.t list  (** Sorted descending by price *)
    ; asks       : Price_level.t list  (** Sorted ascending by price *)
    ; ts         : Time_float_unix.t option
    ; epoch      : int  (** Update sequence number *)
    }
  [@@deriving sexp, fields]

  let create ~venue ~symbol =
    { venue
    ; symbol
    ; bids = []
    ; asks = []
    ; ts = None
    ; epoch = 0
    }

  (** Get best bid (highest buy price) *)
  let best_bid t =
    match t.bids with
    | [] -> Price_level.empty
    | hd :: _ -> hd

  (** Get best ask (lowest sell price) *)
  let best_ask t =
    match t.asks with
    | [] -> Price_level.empty
    | hd :: _ -> hd

  (** Calculate mid price: (best_bid + best_ask) / 2 *)
  let mid_price t =
    let bid = best_bid t in
    let ask = best_ask t in
    match Float.(bid.price = 0. || ask.price = 0.) with
    | true -> 0.
    | false -> (bid.price +. ask.price) /. 2.

  (** Calculate spread: best_ask - best_bid *)
  let spread t =
    let bid = best_bid t in
    let ask = best_ask t in
    match Float.(bid.price = 0. || ask.price = 0.) with
    | true -> 0.
    | false -> ask.price -. bid.price

  (** Get top N bids *)
  let best_n_bids t ~n =
    List.take t.bids n

  (** Get top N asks *)
  let best_n_asks t ~n =
    List.take t.asks n

  (** Calculate total volume in top N levels of a side *)
  let total_volume_n t ~side ~n =
    let levels = match side with
      | `Bid -> best_n_bids t ~n
      | `Ask -> best_n_asks t ~n
    in
    List.fold levels ~init:0. ~f:(fun acc level -> acc +. level.volume)
end

(** 24-hour ticker statistics for a trading pair *)
module Ticker = struct
  type t =
    { venue          : Venue.t
    ; symbol         : Symbol.t
    ; last_price     : Price.t
    ; bid_price      : Price.t
    ; ask_price      : Price.t
    ; high_24h       : Price.t
    ; low_24h        : Price.t
    ; volume_24h     : Qty.t          (** Base asset volume *)
    ; quote_volume   : Qty.t option   (** Quote asset volume *)
    ; price_change   : Price.t option (** 24h price change *)
    ; price_change_pct : float option (** 24h price change percent *)
    ; ts             : Time_float_unix.t option
    }
  [@@deriving sexp, fields]

  let create ~venue ~symbol ~last_price ~bid_price ~ask_price
      ~high_24h ~low_24h ~volume_24h ?quote_volume ?price_change
      ?price_change_pct ?ts () =
    { venue; symbol; last_price; bid_price; ask_price; high_24h; low_24h;
      volume_24h; quote_volume; price_change; price_change_pct; ts }
end

(** Public trade (market trade, not user-specific) *)
module Public_trade = struct
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

  let create ~venue ~symbol ~price ~qty ?side ?trade_id ?ts () =
    { venue; symbol; price; qty; side; trade_id; ts }
end

(** Candle timeframe (interval) for OHLCV data *)
module Timeframe = struct
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

  let to_string = function
    | M1  -> "1m"
    | M3  -> "3m"
    | M5  -> "5m"
    | M15 -> "15m"
    | M30 -> "30m"
    | H1  -> "1h"
    | H2  -> "2h"
    | H4  -> "4h"
    | H6  -> "6h"
    | H8  -> "8h"
    | H12 -> "12h"
    | D1  -> "1d"
    | D3  -> "3d"
    | W1  -> "1w"
    | MO1 -> "1M"

  let of_string_opt = function
    | "1m" | "1min" -> Some M1
    | "3m" | "3min" -> Some M3
    | "5m" | "5min" -> Some M5
    | "15m" | "15min" -> Some M15
    | "30m" | "30min" -> Some M30
    | "1h" | "60m" | "1H" -> Some H1
    | "2h" | "2H" -> Some H2
    | "4h" | "4H" -> Some H4
    | "6h" | "6H" -> Some H6
    | "8h" | "8H" -> Some H8
    | "12h" | "12H" -> Some H12
    | "1d" | "1D" | "D" -> Some D1
    | "3d" | "3D" -> Some D3
    | "1w" | "1W" | "W" -> Some W1
    | "1M" | "M" -> Some MO1
    | _ -> None

  let of_string s =
    match of_string_opt s with
    | Some t -> t
    | None -> failwith (sprintf "Unknown timeframe: %s" s)

  (** Convert timeframe to seconds for calculations *)
  let to_seconds = function
    | M1  -> 60
    | M3  -> 180
    | M5  -> 300
    | M15 -> 900
    | M30 -> 1800
    | H1  -> 3600
    | H2  -> 7200
    | H4  -> 14400
    | H6  -> 21600
    | H8  -> 28800
    | H12 -> 43200
    | D1  -> 86400
    | D3  -> 259200
    | W1  -> 604800
    | MO1 -> 2592000  (* ~30 days *)
end

(** OHLCV candle data *)
module Candle = struct
  type t =
    { venue     : Venue.t
    ; symbol    : Symbol.t
    ; timeframe : Timeframe.t
    ; open_time : Time_float_unix.t  (** Candle open timestamp *)
    ; open_     : Price.t
    ; high      : Price.t
    ; low       : Price.t
    ; close     : Price.t
    ; volume    : Qty.t              (** Base asset volume *)
    ; quote_volume : Qty.t option    (** Quote asset volume (if available) *)
    ; trades    : int option         (** Number of trades in candle *)
    ; closed    : bool               (** Whether candle is finalized *)
    }
  [@@deriving sexp, fields]

  let create ~venue ~symbol ~timeframe ~open_time ~open_ ~high ~low ~close
      ~volume ?quote_volume ?trades ?(closed = true) () =
    { venue; symbol; timeframe; open_time; open_; high; low; close;
      volume; quote_volume; trades; closed }

  (** Get mid price: (high + low) / 2 *)
  let mid t = (t.high +. t.low) /. 2.

  (** Get typical price: (high + low + close) / 3 *)
  let typical t = (t.high +. t.low +. t.close) /. 3.

  (** Get candle body size: |close - open| *)
  let body t = Float.abs (t.close -. t.open_)

  (** Get candle range: high - low *)
  let range t = t.high -. t.low

  (** Is bullish (green) candle: close > open *)
  let is_bullish t = Float.(t.close > t.open_)

  (** Is bearish (red) candle: close < open *)
  let is_bearish t = Float.(t.close < t.open_)

  (** Calculate close time based on timeframe *)
  let close_time t =
    let span = Time_float_unix.Span.of_int_sec (Timeframe.to_seconds t.timeframe) in
    Time_float_unix.add t.open_time span

  (** Compare candles by open time *)
  let compare_by_time a b = Time_float_unix.compare a.open_time b.open_time
end

(** Transfer/withdrawal status for deposits and withdrawals *)
module Transfer_status = struct
  type t =
    | Pending
    | Processing
    | Completed
    | Failed
    | Cancelled
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Pending -> "pending"
    | Processing -> "processing"
    | Completed -> "completed"
    | Failed -> "failed"
    | Cancelled -> "cancelled"

  let of_string_opt = function
    | "pending" | "Pending" | "PENDING" -> Some Pending
    | "processing" | "Processing" | "PROCESSING" -> Some Processing
    | "completed" | "Completed" | "COMPLETED" | "complete" | "Complete" | "COMPLETE" -> Some Completed
    | "failed" | "Failed" | "FAILED" -> Some Failed
    | "cancelled" | "Cancelled" | "CANCELLED" | "canceled" | "Canceled" | "CANCELED" -> Some Cancelled
    | _ -> None

  let of_string s =
    match of_string_opt s with
    | Some t -> t
    | None -> failwith (sprintf "Unknown transfer status: %s" s)
end

(** Deposit address for receiving funds *)
module Deposit_address = struct
  type t =
    { venue : Venue.t
    ; currency : string
    ; address : string
    ; tag : string option        (** Memo/tag for XRP, XLM, etc. *)
    ; network : string option    (** ETH, TRC20, etc. *)
    }
  [@@deriving sexp, compare, equal, fields]

  let create ~venue ~currency ~address ?tag ?network () =
    { venue; currency; address; tag; network }
end

(** Deposit record *)
module Deposit = struct
  type t =
    { venue : Venue.t
    ; id : string
    ; currency : string
    ; amount : float
    ; status : Transfer_status.t
    ; address : string option
    ; tx_id : string option
    ; created_at : Time_float_unix.t option
    ; updated_at : Time_float_unix.t option
    }
  [@@deriving sexp, compare, equal, fields]

  let create ~venue ~id ~currency ~amount ~status ?address ?tx_id ?created_at ?updated_at () =
    { venue; id; currency; amount; status; address; tx_id; created_at; updated_at }
end

(** Withdrawal record *)
module Withdrawal = struct
  type t =
    { venue : Venue.t
    ; id : string
    ; currency : string
    ; amount : float
    ; fee : float option
    ; status : Transfer_status.t
    ; address : string
    ; tag : string option
    ; tx_id : string option
    ; created_at : Time_float_unix.t option
    ; updated_at : Time_float_unix.t option
    }
  [@@deriving sexp, compare, equal, fields]

  let create ~venue ~id ~currency ~amount ?fee ~status ~address ?tag ?tx_id ?created_at ?updated_at () =
    { venue; id; currency; amount; fee; status; address; tag; tx_id; created_at; updated_at }
end

(** {1 Prediction Markets} *)

module Prediction_outcome = struct
  type t = Yes | No [@@deriving sexp, compare, equal]

  let to_string = function
    | Yes -> "yes"
    | No -> "no"

  let of_string_opt = function
    | "yes" | "Yes" | "YES" -> Some Yes
    | "no" | "No" | "NO" -> Some No
    | _ -> None

  let of_string s =
    match of_string_opt s with
    | Some t -> t
    | None -> failwith (sprintf "Unknown prediction outcome: %s" s)
end

module Prediction_contract = struct
  type t =
    { instrument_symbol : string
    ; label : string
    ; ticker : string
    ; last_price : Price.t option
    ; best_bid : Price.t option
    ; best_ask : Price.t option
    ; total_shares : Qty.t
    ; status : string
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_event = struct
  type t =
    { venue : Venue.t
    ; id : string
    ; title : string
    ; description : string
    ; category : string
    ; ticker : string
    ; status : string
    ; volume : Qty.t
    ; liquidity : Qty.t
    ; contracts : Prediction_contract.t list
    ; is_live : bool
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_order = struct
  type t =
    { venue : Venue.t
    ; id : string
    ; symbol : string
    ; side : Side.t
    ; outcome : Prediction_outcome.t
    ; qty : Qty.t
    ; filled : Qty.t
    ; remaining : Qty.t
    ; price : Price.t
    ; avg_execution_price : Price.t option
    ; status : string
    ; event_ticker : string option
    ; created_at : Time_float_unix.t option
    ; updated_at : Time_float_unix.t option
    }
  [@@deriving sexp, compare, equal, fields]
end

module Prediction_position = struct
  type t =
    { venue : Venue.t
    ; symbol : string
    ; outcome : Prediction_outcome.t
    ; qty : Qty.t
    ; avg_price : Price.t
    ; event_ticker : string option
    ; contract_name : string option
    }
  [@@deriving sexp, compare, equal, fields]
end

module Error = struct
  type t =
    | Transport of exn [@sexp.opaque]
    | Rate_limited
    | Auth_failed
    | Normalization_error of string
    | Exchange_specific of
        { venue   : Venue.t
        ; code    : string
        ; message : string
        }
  [@@deriving sexp_of]
end

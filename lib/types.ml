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
    | Dydx      (** dYdX v4 - DeFi perpetuals *)
    | Jupiter   (** Jupiter - Solana DEX aggregator *)
    | OneInch   (** 1inch - EVM DEX aggregator *)
    | Gmx       (** GMX - DeFi perpetuals (planned) *)
    | Aave      (** Aave - DeFi lending (planned) *)
    | Compound  (** Compound - DeFi lending (planned) *)
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
    | Dydx -> "dYdX"
    | Jupiter -> "Jupiter"
    | OneInch -> "1inch"
    | Gmx -> "GMX"
    | Aave -> "Aave"
    | Compound -> "Compound"

  let is_defi = function
    | Dydx | Jupiter | OneInch | Gmx | Aave | Compound -> true
    | Gemini | Kraken | Mexc | Coinbase | Binance | Bybit | Okx | Hyperliquid | Bitrue -> false
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
  let to_string = Float.to_string
end

module Qty = struct
  type t = float [@@deriving sexp, compare, equal]
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

module Order_kind = struct
  type t =
    | Market
    | Limit of Price.t
    | Post_only_limit of Price.t
  [@@deriving sexp, compare]
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
    { venue      : Venue.t
    ; id         : id
    ; symbol     : Symbol.t
    ; side       : Side.t
    ; kind       : Order_kind.t
    ; qty        : Qty.t
    ; filled     : Qty.t
    ; status     : Order_status.t
    ; created_at : Time_float_unix.t option
    ; updated_at : Time_float_unix.t option
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

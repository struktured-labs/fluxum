open Core

module Venue = struct
  type t =
    | Gemini
    | Kraken
    | Mexc
    | Coinbase
  [@@deriving sexp, compare, equal]
end

module Symbol = struct
  type t = string [@@deriving sexp, compare, equal]
end

module Currency = struct
  type t = string [@@deriving sexp, compare, equal]
end

module Price = struct
  type t = float [@@deriving sexp, compare, equal]
end

module Qty = struct
  type t = float [@@deriving sexp, compare, equal]
end

module Side = struct
  type t = Buy | Sell [@@deriving sexp, compare, equal]
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

module Error = struct
  type t =
    | Transport of exn [@sexp.opaque]
    | Rate_limited
    | Auth_failed
    | Exchange_specific of
        { venue   : Venue.t
        ; code    : string
        ; message : string
        }
  [@@deriving sexp_of]
end

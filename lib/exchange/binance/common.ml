(** Common types for Binance exchange *)

(** Order side - BUY or SELL (Binance uses uppercase) *)
module Side = struct
  module T = struct
    type t = [ `BUY | `SELL ] [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `BUY -> "BUY"
      | `SELL -> "SELL"

    let of_string_opt = function
      | "BUY" | "buy" -> Some `BUY
      | "SELL" | "sell" -> Some `SELL
      | _ -> None
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Order type - LIMIT, MARKET, STOP_LOSS, etc. *)
module Order_type = struct
  module T = struct
    type t =
      [ `LIMIT
      | `MARKET
      | `STOP_LOSS
      | `STOP_LOSS_LIMIT
      | `TAKE_PROFIT
      | `TAKE_PROFIT_LIMIT
      | `LIMIT_MAKER
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `LIMIT -> "LIMIT"
      | `MARKET -> "MARKET"
      | `STOP_LOSS -> "STOP_LOSS"
      | `STOP_LOSS_LIMIT -> "STOP_LOSS_LIMIT"
      | `TAKE_PROFIT -> "TAKE_PROFIT"
      | `TAKE_PROFIT_LIMIT -> "TAKE_PROFIT_LIMIT"
      | `LIMIT_MAKER -> "LIMIT_MAKER"

    let of_string_opt = function
      | "LIMIT" | "limit" -> Some `LIMIT
      | "MARKET" | "market" -> Some `MARKET
      | "STOP_LOSS" | "stop_loss" -> Some `STOP_LOSS
      | "STOP_LOSS_LIMIT" | "stop_loss_limit" -> Some `STOP_LOSS_LIMIT
      | "TAKE_PROFIT" | "take_profit" -> Some `TAKE_PROFIT
      | "TAKE_PROFIT_LIMIT" | "take_profit_limit" -> Some `TAKE_PROFIT_LIMIT
      | "LIMIT_MAKER" | "limit_maker" -> Some `LIMIT_MAKER
      | _ -> None
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Time in force - how long an order remains active *)
module Time_in_force = struct
  module T = struct
    type t =
      [ `GTC  (** Good till canceled *)
      | `IOC  (** Immediate or cancel *)
      | `FOK  (** Fill or kill *)
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `GTC -> "GTC"
      | `IOC -> "IOC"
      | `FOK -> "FOK"

    let of_string_opt = function
      | "GTC" | "gtc" -> Some `GTC
      | "IOC" | "ioc" -> Some `IOC
      | "FOK" | "fok" -> Some `FOK
      | _ -> None
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Order status *)
module Order_status = struct
  module T = struct
    type t =
      [ `NEW
      | `PARTIALLY_FILLED
      | `FILLED
      | `CANCELED
      | `PENDING_CANCEL
      | `REJECTED
      | `EXPIRED
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `NEW -> "NEW"
      | `PARTIALLY_FILLED -> "PARTIALLY_FILLED"
      | `FILLED -> "FILLED"
      | `CANCELED -> "CANCELED"
      | `PENDING_CANCEL -> "PENDING_CANCEL"
      | `REJECTED -> "REJECTED"
      | `EXPIRED -> "EXPIRED"

    let of_string_opt = function
      | "NEW" | "new" -> Some `NEW
      | "PARTIALLY_FILLED" | "partially_filled" -> Some `PARTIALLY_FILLED
      | "FILLED" | "filled" -> Some `FILLED
      | "CANCELED" | "canceled" -> Some `CANCELED
      | "PENDING_CANCEL" | "pending_cancel" -> Some `PENDING_CANCEL
      | "REJECTED" | "rejected" -> Some `REJECTED
      | "EXPIRED" | "expired" -> Some `EXPIRED
      | _ -> None
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

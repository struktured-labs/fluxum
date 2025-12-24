(** Common types for Kraken exchange *)

(** Order side - buy or sell *)
module Side = struct
  module T = struct
    type t =
      [ `Buy
      | `Sell
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Buy -> "buy"
      | `Sell -> "sell"
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Order type - market, limit, stop-loss, etc. *)
module Order_type = struct
  module T = struct
    type t =
      [ `Market
      | `Limit
      | `Stop_loss
      | `Take_profit
      | `Stop_loss_limit
      | `Take_profit_limit
      | `Settle_position
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Market -> "market"
      | `Limit -> "limit"
      | `Stop_loss -> "stop-loss"
      | `Take_profit -> "take-profit"
      | `Stop_loss_limit -> "stop-loss-limit"
      | `Take_profit_limit -> "take-profit-limit"
      | `Settle_position -> "settle-position"
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Order status *)
module Order_status = struct
  module T = struct
    type t =
      [ `Pending
      | `Open
      | `Closed
      | `Canceled
      | `Expired
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Pending -> "pending"
      | `Open -> "open"
      | `Closed -> "closed"
      | `Canceled -> "canceled"
      | `Expired -> "expired"
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

(** Time in force *)
module Time_in_force = struct
  module T = struct
    type t =
      [ `GTC  (** Good till canceled *)
      | `IOC  (** Immediate or cancel *)
      | `GTD  (** Good till date *)
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `GTC -> "GTC"
      | `IOC -> "IOC"
      | `GTD -> "GTD"
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

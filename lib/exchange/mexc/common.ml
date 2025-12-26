(** Common types for MEXC exchange *)

(** Order side - BUY or SELL (MEXC uses uppercase) *)
module Side = struct
  module T = struct
    type t =
      [ `BUY
      | `SELL
      ]
    [@@deriving sexp, enumerate, equal, compare]

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

(** Order type - LIMIT, MARKET, etc. *)
module Order_type = struct
  module T = struct
    type t =
      [ `LIMIT
      | `MARKET
      | `LIMIT_MAKER
      | `IMMEDIATE_OR_CANCEL
      | `FILL_OR_KILL
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `LIMIT -> "LIMIT"
      | `MARKET -> "MARKET"
      | `LIMIT_MAKER -> "LIMIT_MAKER"
      | `IMMEDIATE_OR_CANCEL -> "IMMEDIATE_OR_CANCEL"
      | `FILL_OR_KILL -> "FILL_OR_KILL"

    let of_string_opt = function
      | "LIMIT" -> Some `LIMIT
      | "MARKET" -> Some `MARKET
      | "LIMIT_MAKER" -> Some `LIMIT_MAKER
      | "IMMEDIATE_OR_CANCEL" | "IOC" -> Some `IMMEDIATE_OR_CANCEL
      | "FILL_OR_KILL" | "FOK" -> Some `FILL_OR_KILL
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
      | `FILLED
      | `PARTIALLY_FILLED
      | `CANCELED
      | `PARTIALLY_CANCELED
      | `PENDING_CANCEL
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `NEW -> "NEW"
      | `FILLED -> "FILLED"
      | `PARTIALLY_FILLED -> "PARTIALLY_FILLED"
      | `CANCELED -> "CANCELED"
      | `PARTIALLY_CANCELED -> "PARTIALLY_CANCELED"
      | `PENDING_CANCEL -> "PENDING_CANCEL"

    let of_string_opt = function
      | "NEW" -> Some `NEW
      | "FILLED" -> Some `FILLED
      | "PARTIALLY_FILLED" -> Some `PARTIALLY_FILLED
      | "CANCELED" -> Some `CANCELED
      | "PARTIALLY_CANCELED" -> Some `PARTIALLY_CANCELED
      | "PENDING_CANCEL" -> Some `PENDING_CANCEL
      | _ -> None
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
      | `FOK  (** Fill or kill *)
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `GTC -> "GTC"
      | `IOC -> "IOC"
      | `FOK -> "FOK"

    let of_string_opt = function
      | "GTC" -> Some `GTC
      | "IOC" -> Some `IOC
      | "FOK" -> Some `FOK
      | _ -> None
  end

  include T
  include (Fluxum.Json.Make (T) : Fluxum.Json.S with type t := t)
end

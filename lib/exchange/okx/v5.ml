(** OKX V5 API Endpoints

    Complete implementation of OKX's V5 unified API.

    Supports:
    - Public market data (tickers, order books, recent trades, instruments)
    - Authenticated trading (order placement, cancellation, status)
    - Account management (balances, trade history)

    All endpoints use the instType parameter to specify product type:
    - "SPOT" - Spot trading
    - "FUTURES" - Delivery futures
    - "SWAP" - Perpetual swaps
    - "OPTION" - Options trading
*)

open Core

module Cfg = Cfg
module Rest = Rest

(* Common types *)

module InstType = struct
  type t =
    [ `Spot
    | `Futures
    | `Swap
    | `Option
    ]
  [@@deriving sexp, enumerate, equal, compare]

  let to_string = function
    | `Spot -> "SPOT"
    | `Futures -> "FUTURES"
    | `Swap -> "SWAP"
    | `Option -> "OPTION"

  let of_string_opt = function
    | "SPOT" -> Some `Spot
    | "FUTURES" -> Some `Futures
    | "SWAP" -> Some `Swap
    | "OPTION" -> Some `Option
    | _ -> None

  let of_yojson = function
    | `String s -> (
      match of_string_opt s with
      | Some c -> Ok c
      | None -> Error (sprintf "Invalid instType: %s" s))
    | json -> Error (sprintf "Expected string, got: %s" (Yojson.Safe.to_string json))

  let to_yojson t = `String (to_string t)
end

module Side = struct
  type t =
    [ `Buy
    | `Sell
    ]
  [@@deriving sexp, enumerate, equal, compare]

  let to_string = function
    | `Buy -> "buy"
    | `Sell -> "sell"

  let of_string_opt = function
    | "buy" -> Some `Buy
    | "sell" -> Some `Sell
    | _ -> None

  let of_yojson = function
    | `String s -> (
      match of_string_opt s with
      | Some c -> Ok c
      | None -> Error (sprintf "Invalid side: %s" s))
    | json -> Error (sprintf "Expected string, got: %s" (Yojson.Safe.to_string json))

  let to_yojson t = `String (to_string t)
end

module TdMode = struct
  type t =
    [ `Cash       (* Non-margin mode (spot only) *)
    | `Cross      (* Cross margin *)
    | `Isolated   (* Isolated margin *)
    ]
  [@@deriving sexp, enumerate, equal, compare]

  let to_string = function
    | `Cash -> "cash"
    | `Cross -> "cross"
    | `Isolated -> "isolated"

  let of_string_opt = function
    | "cash" -> Some `Cash
    | "cross" -> Some `Cross
    | "isolated" -> Some `Isolated
    | _ -> None

  let of_yojson = function
    | `String s -> (
      match of_string_opt s with
      | Some c -> Ok c
      | None -> Error (sprintf "Invalid tdMode: %s" s))
    | json -> Error (sprintf "Expected string, got: %s" (Yojson.Safe.to_string json))

  let to_yojson t = `String (to_string t)
end

(* Public Market Data Endpoints *)

module Market_tickers = struct
  module T = struct
    let name = "market-tickers"
    let endpoint = "market/tickers"
    let http_method = `GET
    let requires_auth = false

    type request =
      { instType : InstType.t
      ; uly : string option [@sexp.option]  (* Underlying asset *)
      ; instFamily : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instType; uly; instFamily } =
      let base = [ ("instType", InstType.to_string instType) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "uly" uly
      |> add_opt "instFamily" instFamily

    type ticker =
      { instId : string
      ; last : string
      ; bidPx : string
      ; askPx : string
      ; high24h : string
      ; low24h : string
      ; volCcy24h : string  (* Quote currency volume *)
      ; vol24h : string      (* Base currency volume *)
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { data : ticker list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Instruments = struct
  module T = struct
    let name = "instruments"
    let endpoint = "public/instruments"
    let http_method = `GET
    let requires_auth = false

    type request =
      { instType : InstType.t
      ; instId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instType; instId } =
      let base = [ ("instType", InstType.to_string instType) ] in
      match instId with
      | Some id -> base @ [ ("instId", id) ]
      | None -> base

    type instrument =
      { instId : string
      ; baseCcy : string
      ; quoteCcy : string
      ; state : string
      ; minSz : string
      ; tickSz : string
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { data : instrument list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Orderbook = struct
  module T = struct
    let name = "orderbook"
    let endpoint = "market/books"
    let http_method = `GET
    let requires_auth = false

    type request =
      { instId : string
      ; sz : int option [@sexp.option]  (* Depth (max 400) *)
      }
    [@@deriving sexp]

    let request_to_params { instId; sz } =
      let base = [ ("instId", instId) ] in
      match sz with
      | Some n -> base @ [ ("sz", Int.to_string n) ]
      | None -> base

    type level = string * string * string * string  (* price, size, liquidated orders, num orders *)
    [@@deriving sexp]

    let level_of_yojson = function
      | `List [ `String price; `String size; `String liq; `String orders ] ->
        Ok (price, size, liq, orders)
      | json -> Error (sprintf "Invalid level: %s" (Yojson.Safe.to_string json))

    type book_data =
      { asks : level list
      ; bids : level list
      ; ts : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { data : book_data list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Recent_trades = struct
  module T = struct
    let name = "recent-trades"
    let endpoint = "market/trades"
    let http_method = `GET
    let requires_auth = false

    type request =
      { instId : string
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instId; limit } =
      let base = [ ("instId", instId) ] in
      match limit with
      | Some n -> base @ [ ("limit", Int.to_string n) ]
      | None -> base

    type trade =
      { tradeId : string
      ; instId : string
      ; px : string
      ; sz : string
      ; side : string
      ; ts : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { data : trade list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* Authenticated Account Endpoints *)

module Account_balance = struct
  module T = struct
    let name = "account-balance"
    let endpoint = "account/balance"
    let http_method = `GET
    let requires_auth = true

    type request =
      { ccy : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { ccy } =
      match ccy with
      | Some c -> [ ("ccy", c) ]
      | None -> []

    type balance_detail =
      { ccy : string
      ; availBal : string
      ; cashBal : string
      ; frozenBal : string
      }
    [@@deriving sexp, of_yojson]

    type account_data =
      { totalEq : string
      ; details : balance_detail list
      }
    [@@deriving sexp, of_yojson]

    type response =
      { data : account_data list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* Authenticated Trading Endpoints *)

module Place_order = struct
  module T = struct
    let name = "place-order"
    let endpoint = "trade/order"
    let http_method = `POST
    let requires_auth = true

    type request =
      { instId : string
      ; tdMode : TdMode.t
      ; side : Side.t
      ; ordType : string  (* "market" or "limit" *)
      ; sz : string
      ; px : string option [@sexp.option]
      ; clOrdId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instId; tdMode; side; ordType; sz; px; clOrdId } =
      let base =
        [ ("instId", instId)
        ; ("tdMode", TdMode.to_string tdMode)
        ; ("side", Side.to_string side)
        ; ("ordType", ordType)
        ; ("sz", sz)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "px" px
      |> add_opt "clOrdId" clOrdId

    type order_data =
      { ordId : string
      ; clOrdId : string
      ; sCode : string
      ; sMsg : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { data : order_data list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Cancel_order = struct
  module T = struct
    let name = "cancel-order"
    let endpoint = "trade/cancel-order"
    let http_method = `POST
    let requires_auth = true

    type request =
      { instId : string
      ; ordId : string option [@sexp.option]
      ; clOrdId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instId; ordId; clOrdId } =
      let base = [ ("instId", instId) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "ordId" ordId
      |> add_opt "clOrdId" clOrdId

    type cancel_data =
      { ordId : string
      ; clOrdId : string
      ; sCode : string
      ; sMsg : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { data : cancel_data list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Order_details = struct
  module T = struct
    let name = "order-details"
    let endpoint = "trade/order"
    let http_method = `GET
    let requires_auth = true

    type request =
      { instId : string
      ; ordId : string option [@sexp.option]
      ; clOrdId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instId; ordId; clOrdId } =
      let base = [ ("instId", instId) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "ordId" ordId
      |> add_opt "clOrdId" clOrdId

    type order =
      { ordId : string
      ; clOrdId : string
      ; instId : string
      ; px : string
      ; sz : string
      ; side : string
      ; state : string
      ; accFillSz : string
      ; avgPx : string
      ; ordType : string
      ; cTime : string
      ; uTime : string
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { data : order list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Orders_history = struct
  module T = struct
    let name = "orders-history"
    let endpoint = "trade/orders-history"
    let http_method = `GET
    let requires_auth = true

    type request =
      { instType : InstType.t
      ; instId : string option [@sexp.option]
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { instType; instId; limit } =
      let base = [ ("instType", InstType.to_string instType) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "instId" instId
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)

    type order =
      { ordId : string
      ; clOrdId : string
      ; instId : string
      ; px : string
      ; sz : string
      ; side : string
      ; state : string
      ; accFillSz : string
      ; avgPx : string
      ; ordType : string
      ; cTime : string
      ; uTime : string
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { data : order list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

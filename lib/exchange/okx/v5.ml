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

(* ============================================================ *)
(* Account Operations - Deposits/Withdrawals *)
(* ============================================================ *)

(** Get deposit address - GET /api/v5/asset/deposit-address *)
module Deposit_address = struct
  module T = struct
    let name = "deposit-address"
    let endpoint = "asset/deposit-address"
    let http_method = `GET
    let requires_auth = true

    type request =
      { ccy : string  (** Currency, e.g., BTC, ETH *)
      }
    [@@deriving sexp]

    let request_to_params { ccy } = [ ("ccy", ccy) ]

    type address_info =
      { addr : string         (** Deposit address *)
      ; tag : string          (** Deposit tag/memo (for XRP, XLM, etc.) *)
      ; memo : string         (** Alias for tag in some chains *)
      ; pmtId : string        (** Payment ID (for XMR) *)
      ; ccy : string          (** Currency *)
      ; chain : string        (** Chain name, e.g., BTC-Bitcoin, ETH-ERC20 *)
      ; ctAddr : string       (** Contract address (for tokens) *)
      ; selected : bool       (** Whether this is the default address *)
      }
    [@@deriving sexp]

    let address_info_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        let get_bool key = match get key with Some (`Bool b) -> b | _ -> false in
        Result.Ok
          { addr = get_str "addr"
          ; tag = get_str "tag"
          ; memo = get_str "memo"
          ; pmtId = get_str "pmtId"
          ; ccy = get_str "ccy"
          ; chain = get_str "chain"
          ; ctAddr = get_str "ctAddr"
          ; selected = get_bool "selected"
          }
      | _ -> Result.Error "Expected address info object"

    type response =
      { data : address_info list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Get deposit history - GET /api/v5/asset/deposit-history *)
module Deposit_history = struct
  module T = struct
    let name = "deposit-history"
    let endpoint = "asset/deposit-history"
    let http_method = `GET
    let requires_auth = true

    type request =
      { ccy : string option [@sexp.option]    (** Currency filter *)
      ; depId : string option [@sexp.option]  (** Deposit ID filter *)
      ; txId : string option [@sexp.option]   (** Transaction ID filter *)
      ; state : string option [@sexp.option]  (** State filter: 0=waiting, 1=credited, 2=successful, etc. *)
      ; after : string option [@sexp.option]  (** Pagination: return records before this depId *)
      ; before : string option [@sexp.option] (** Pagination: return records after this depId *)
      ; limit : int option [@sexp.option]     (** Number of results (max 100) *)
      }
    [@@deriving sexp]

    let request_to_params { ccy; depId; txId; state; after; before; limit } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "ccy" ccy
      |> add_opt "depId" depId
      |> add_opt "txId" txId
      |> add_opt "state" state
      |> add_opt "after" after
      |> add_opt "before" before
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)

    (** OKX deposit states:
        0 = waiting for confirmation
        1 = deposit credited (waiting for settlement)
        2 = deposit successful
        8 = pending due to temporary deposit suspension
        11 = match the address
        12 = account or deposit is frozen *)
    type deposit_record =
      { ccy : string      (** Currency *)
      ; chain : string    (** Chain name *)
      ; amt : string      (** Deposit amount *)
      ; from : string     (** Sender address *)
      ; to_ : string [@key "to"]  (** Receiving address *)
      ; txId : string     (** Transaction hash *)
      ; ts : string       (** Deposit time (Unix ms) *)
      ; state : string    (** Status code *)
      ; depId : string    (** Deposit ID *)
      }
    [@@deriving sexp]

    let deposit_record_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        Result.Ok
          { ccy = get_str "ccy"
          ; chain = get_str "chain"
          ; amt = get_str "amt"
          ; from = get_str "from"
          ; to_ = get_str "to"
          ; txId = get_str "txId"
          ; ts = get_str "ts"
          ; state = get_str "state"
          ; depId = get_str "depId"
          }
      | _ -> Result.Error "Expected deposit record object"

    type response =
      { data : deposit_record list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Withdraw - POST /api/v5/asset/withdrawal *)
module Withdraw = struct
  module T = struct
    let name = "withdraw"
    let endpoint = "asset/withdrawal"
    let http_method = `POST
    let requires_auth = true

    type request =
      { ccy : string          (** Currency *)
      ; amt : string          (** Withdrawal amount *)
      ; dest : string         (** Destination: "3" = internal, "4" = on-chain *)
      ; toAddr : string       (** Destination address or OKX login *)
      ; fee : string          (** Network fee *)
      ; chain : string option [@sexp.option]  (** Chain name (required for on-chain) *)
      }
    [@@deriving sexp]

    let request_to_params { ccy; amt; dest; toAddr; fee; chain } =
      let base =
        [ ("ccy", ccy)
        ; ("amt", amt)
        ; ("dest", dest)
        ; ("toAddr", toAddr)
        ; ("fee", fee)
        ]
      in
      match chain with
      | Some c -> ("chain", c) :: base
      | None -> base

    type withdraw_response =
      { wdId : string     (** Withdrawal ID *)
      ; ccy : string      (** Currency *)
      ; chain : string    (** Chain name *)
      ; amt : string      (** Amount *)
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { data : withdraw_response list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Get withdrawal history - GET /api/v5/asset/withdrawal-history *)
module Withdrawal_history = struct
  module T = struct
    let name = "withdrawal-history"
    let endpoint = "asset/withdrawal-history"
    let http_method = `GET
    let requires_auth = true

    type request =
      { ccy : string option [@sexp.option]    (** Currency filter *)
      ; wdId : string option [@sexp.option]   (** Withdrawal ID filter *)
      ; txId : string option [@sexp.option]   (** Transaction ID filter *)
      ; state : string option [@sexp.option]  (** State filter *)
      ; after : string option [@sexp.option]  (** Pagination *)
      ; before : string option [@sexp.option]
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { ccy; wdId; txId; state; after; before; limit } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "ccy" ccy
      |> add_opt "wdId" wdId
      |> add_opt "txId" txId
      |> add_opt "state" state
      |> add_opt "after" after
      |> add_opt "before" before
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)

    (** OKX withdrawal states:
        -3 = canceling
        -2 = cancelled
        -1 = failed
        0 = pending
        1 = sending
        2 = sent
        3 = awaiting email verification
        4 = awaiting manual verification
        5 = awaiting identity verification *)
    type withdrawal_record =
      { ccy : string      (** Currency *)
      ; chain : string    (** Chain name *)
      ; amt : string      (** Withdrawal amount *)
      ; ts : string       (** Withdrawal request time (Unix ms) *)
      ; from : string     (** Sender address (may be empty) *)
      ; to_ : string [@key "to"]  (** Receiving address *)
      ; txId : string     (** Transaction hash *)
      ; fee : string      (** Withdrawal fee *)
      ; state : string    (** Status code *)
      ; wdId : string     (** Withdrawal ID *)
      }
    [@@deriving sexp]

    let withdrawal_record_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        Result.Ok
          { ccy = get_str "ccy"
          ; chain = get_str "chain"
          ; amt = get_str "amt"
          ; ts = get_str "ts"
          ; from = get_str "from"
          ; to_ = get_str "to"
          ; txId = get_str "txId"
          ; fee = get_str "fee"
          ; state = get_str "state"
          ; wdId = get_str "wdId"
          }
      | _ -> Result.Error "Expected withdrawal record object"

    type response =
      { data : withdrawal_record list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

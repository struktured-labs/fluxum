(** Bybit V5 API Endpoints

    Complete implementation of Bybit's V5 unified API.

    Supports:
    - Public market data (tickers, order books, recent trades, instruments)
    - Authenticated trading (order placement, cancellation, status)
    - Account management (balances, trade history)

    All endpoints use the category parameter to specify product type:
    - "spot" - Spot trading
    - "linear" - USDT/USDC perpetuals and futures
    - "inverse" - Inverse perpetuals and futures
    - "option" - Options trading
*)

open Core

module Cfg = Cfg
module Rest = Rest

(* Common types *)

module Category = struct
  type t =
    [ `Spot
    | `Linear
    | `Inverse
    | `Option
    ]
  [@@deriving sexp, enumerate, equal, compare]

  let to_string = function
    | `Spot -> "spot"
    | `Linear -> "linear"
    | `Inverse -> "inverse"
    | `Option -> "option"

  let of_string_opt = function
    | "spot" -> Some `Spot
    | "linear" -> Some `Linear
    | "inverse" -> Some `Inverse
    | "option" -> Some `Option
    | _ -> None

  let of_yojson = function
    | `String s -> (
      match of_string_opt s with
      | Some c -> Ok c
      | None -> Error (sprintf "Invalid category: %s" s))
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
    | `Buy -> "Buy"
    | `Sell -> "Sell"

  let of_string_opt = function
    | "Buy" -> Some `Buy
    | "Sell" -> Some `Sell
    | _ -> None

  let of_yojson = function
    | `String s -> (
      match of_string_opt s with
      | Some c -> Ok c
      | None -> Error (sprintf "Invalid side: %s" s))
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
      { category : Category.t
      ; symbol : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol } =
      let base = [ ("category", Category.to_string category) ] in
      match symbol with
      | Some s -> base @ [ ("symbol", s) ]
      | None -> base

    type ticker =
      { symbol : string
      ; lastPrice : string
      ; bid1Price : string
      ; ask1Price : string
      ; volume24h : string
      ; turnover24h : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { category : string
      ; list : ticker list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Instruments_info = struct
  module T = struct
    let name = "instruments-info"
    let endpoint = "market/instruments-info"
    let http_method = `GET
    let requires_auth = false

    type request =
      { category : Category.t
      ; symbol : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol } =
      let base = [ ("category", Category.to_string category) ] in
      match symbol with
      | Some s -> base @ [ ("symbol", s) ]
      | None -> base

    type instrument =
      { symbol : string
      ; baseCoin : string
      ; quoteCoin : string
      ; status : string
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { category : string
      ; list : instrument list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Orderbook = struct
  module T = struct
    let name = "orderbook"
    let endpoint = "market/orderbook"
    let http_method = `GET
    let requires_auth = false

    type request =
      { category : Category.t
      ; symbol : string
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; limit } =
      let base =
        [ ("category", Category.to_string category)
        ; ("symbol", symbol)
        ]
      in
      match limit with
      | Some n -> base @ [ ("limit", Int.to_string n) ]
      | None -> base

    type level = string * string [@@deriving sexp]

    let level_of_yojson = function
      | `List [ `String price; `String qty ] -> Ok (price, qty)
      | json -> Error (sprintf "Invalid level: %s" (Yojson.Safe.to_string json))

    type response =
      { s : string  (* symbol *)
      ; b : level list  (* bids *)
      ; a : level list  (* asks *)
      ; ts : int64  (* timestamp *)
      ; u : int64  (* update ID *)
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Recent_trade = struct
  module T = struct
    let name = "recent-trade"
    let endpoint = "market/recent-trade"
    let http_method = `GET
    let requires_auth = false

    type request =
      { category : Category.t
      ; symbol : string
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; limit } =
      let base =
        [ ("category", Category.to_string category)
        ; ("symbol", symbol)
        ]
      in
      match limit with
      | Some n -> base @ [ ("limit", Int.to_string n) ]
      | None -> base

    type trade =
      { execId : string
      ; symbol : string
      ; price : string
      ; size : string
      ; side : string
      ; time : string
      ; isBlockTrade : bool
      }
    [@@deriving sexp, of_yojson]

    type response =
      { category : string
      ; list : trade list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* Authenticated Account Endpoints *)

module Wallet_balance = struct
  module T = struct
    let name = "wallet-balance"
    let endpoint = "account/wallet-balance"
    let http_method = `GET
    let requires_auth = true

    type request =
      { accountType : string  (* "UNIFIED" or "CONTRACT" or "SPOT" *)
      ; coin : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { accountType; coin } =
      let base = [ ("accountType", accountType) ] in
      match coin with
      | Some c -> base @ [ ("coin", c) ]
      | None -> base

    type coin_info =
      { coin : string
      ; equity : string
      ; walletBalance : string
      ; availableToWithdraw : string
      ; locked : string
      }
    [@@deriving sexp, of_yojson]

    type account =
      { accountType : string
      ; totalEquity : string
      ; totalWalletBalance : string
      ; coin : coin_info list
      }
    [@@deriving sexp, of_yojson]

    type response =
      { list : account list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* Authenticated Trading Endpoints *)

module Create_order = struct
  module T = struct
    let name = "create-order"
    let endpoint = "order/create"
    let http_method = `POST
    let requires_auth = true

    type request =
      { category : Category.t
      ; symbol : string
      ; side : Side.t
      ; orderType : string  (* "Market" or "Limit" *)
      ; qty : string
      ; price : string option [@sexp.option]
      ; timeInForce : string option [@sexp.option]
      ; orderLinkId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; side; orderType; qty; price; timeInForce; orderLinkId } =
      let base =
        [ ("category", Category.to_string category)
        ; ("symbol", symbol)
        ; ("side", Side.to_string side)
        ; ("orderType", orderType)
        ; ("qty", qty)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "price" price
      |> add_opt "timeInForce" timeInForce
      |> add_opt "orderLinkId" orderLinkId

    type response =
      { orderId : string
      ; orderLinkId : string
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Cancel_order = struct
  module T = struct
    let name = "cancel-order"
    let endpoint = "order/cancel"
    let http_method = `POST
    let requires_auth = true

    type request =
      { category : Category.t
      ; symbol : string
      ; orderId : string option [@sexp.option]
      ; orderLinkId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; orderId; orderLinkId } =
      let base =
        [ ("category", Category.to_string category)
        ; ("symbol", symbol)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" orderId
      |> add_opt "orderLinkId" orderLinkId

    type response =
      { orderId : string
      ; orderLinkId : string
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Order_realtime = struct
  module T = struct
    let name = "order-realtime"
    let endpoint = "order/realtime"
    let http_method = `GET
    let requires_auth = true

    type request =
      { category : Category.t
      ; symbol : string option [@sexp.option]
      ; orderId : string option [@sexp.option]
      ; orderLinkId : string option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; orderId; orderLinkId } =
      let base = [ ("category", Category.to_string category) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "symbol" symbol
      |> add_opt "orderId" orderId
      |> add_opt "orderLinkId" orderLinkId

    type order =
      { orderId : string
      ; orderLinkId : string
      ; symbol : string
      ; price : string
      ; qty : string
      ; side : string
      ; orderStatus : string
      ; cumExecQty : string
      ; cumExecValue : string
      ; avgPrice : string
      ; orderType : string
      ; createdTime : string
      ; updatedTime : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { category : string
      ; list : order list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

module Execution_list = struct
  module T = struct
    let name = "execution-list"
    let endpoint = "execution/list"
    let http_method = `GET
    let requires_auth = true

    type request =
      { category : Category.t
      ; symbol : string option [@sexp.option]
      ; orderId : string option [@sexp.option]
      ; limit : int option [@sexp.option]
      }
    [@@deriving sexp]

    let request_to_params { category; symbol; orderId; limit } =
      let base = [ ("category", Category.to_string category) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "symbol" symbol
      |> add_opt "orderId" orderId
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)

    type execution =
      { execId : string
      ; orderId : string
      ; orderLinkId : string
      ; symbol : string
      ; price : string
      ; execQty : string
      ; execValue : string
      ; side : string
      ; execTime : string
      ; isMaker : bool
      ; execFee : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { category : string
      ; list : execution list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

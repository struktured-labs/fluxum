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

(* ============================================================ *)
(* Account Operations - Deposits/Withdrawals (V5 Asset API) *)
(* ============================================================ *)

(** Get deposit address - GET /v5/asset/deposit/query-address *)
module Deposit_address = struct
  module T = struct
    let name = "deposit-address"
    let endpoint = "asset/deposit/query-address"
    let http_method = `GET
    let requires_auth = true

    type request =
      { coin : string                           (** Coin name, e.g., BTC, ETH *)
      ; chainType : string option [@sexp.option] (** Chain type, e.g., ETH, TRX *)
      }
    [@@deriving sexp]

    let request_to_params { coin; chainType } =
      let base = [ ("coin", coin) ] in
      match chainType with
      | Some c -> ("chainType", c) :: base
      | None -> base

    type chain_info =
      { chainType : string         (** Chain type *)
      ; addressDeposit : string    (** Deposit address *)
      ; tagDeposit : string        (** Deposit tag/memo *)
      }
    [@@deriving sexp]

    let chain_info_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        Result.Ok
          { chainType = get_str "chainType"
          ; addressDeposit = get_str "addressDeposit"
          ; tagDeposit = get_str "tagDeposit"
          }
      | _ -> Result.Error "Expected chain info object"

    type response =
      { coin : string
      ; chains : chain_info list
      }
    [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let coin = match get "coin" with Some (`String s) -> s | _ -> "" in
        let chains = match get "chains" with
          | Some (`List lst) ->
            List.filter_map lst ~f:(fun json ->
              match chain_info_of_yojson json with
              | Result.Ok c -> Some c
              | Result.Error _ -> None)
          | _ -> []
        in
        Result.Ok { coin; chains }
      | _ -> Result.Error "Expected deposit address response object"
  end

  include T
  include Rest.Make (T)
end

(** Get deposit records - GET /v5/asset/deposit/query-record *)
module Deposit_record = struct
  module T = struct
    let name = "deposit-record"
    let endpoint = "asset/deposit/query-record"
    let http_method = `GET
    let requires_auth = true

    type request =
      { coin : string option [@sexp.option]        (** Coin filter *)
      ; startTime : int64 option [@sexp.option]    (** Start time (Unix ms) *)
      ; endTime : int64 option [@sexp.option]      (** End time (Unix ms) *)
      ; limit : int option [@sexp.option]          (** Limit (max 50) *)
      ; cursor : string option [@sexp.option]      (** Pagination cursor *)
      }
    [@@deriving sexp]

    let request_to_params { coin; startTime; endTime; limit; cursor } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "coin" coin
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "cursor" cursor

    (** Bybit deposit status:
        0 = unknown
        1 = toBeConfirmed
        2 = processing
        3 = success
        4 = depositFailed *)
    type deposit =
      { coin : string           (** Coin name *)
      ; chain : string          (** Chain type *)
      ; amount : string         (** Deposit amount *)
      ; txID : string           (** Transaction hash *)
      ; status : int            (** Deposit status code *)
      ; toAddress : string      (** Deposit address *)
      ; tag : string            (** Tag/memo *)
      ; depositFee : string     (** Deposit fee *)
      ; successAt : string      (** Success time (Unix ms) *)
      ; confirmations : string  (** Number of confirmations *)
      ; txIndex : string        (** Transaction index (for deposit ID) *)
      ; blockHash : string      (** Block hash *)
      }
    [@@deriving sexp]

    let deposit_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        let get_int key = match get key with Some (`Int i) -> i | _ -> 0 in
        Result.Ok
          { coin = get_str "coin"
          ; chain = get_str "chain"
          ; amount = get_str "amount"
          ; txID = get_str "txID"
          ; status = get_int "status"
          ; toAddress = get_str "toAddress"
          ; tag = get_str "tag"
          ; depositFee = get_str "depositFee"
          ; successAt = get_str "successAt"
          ; confirmations = get_str "confirmations"
          ; txIndex = get_str "txIndex"
          ; blockHash = get_str "blockHash"
          }
      | _ -> Result.Error "Expected deposit record object"

    type response =
      { rows : deposit list
      ; nextPageCursor : string
      }
    [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let rows = match get "rows" with
          | Some (`List lst) ->
            List.filter_map lst ~f:(fun json ->
              match deposit_of_yojson json with
              | Result.Ok d -> Some d
              | Result.Error _ -> None)
          | _ -> []
        in
        let cursor = match get "nextPageCursor" with Some (`String s) -> s | _ -> "" in
        Result.Ok { rows; nextPageCursor = cursor }
      | _ -> Result.Error "Expected deposit records response"
  end

  include T
  include Rest.Make (T)
end

(** Create withdrawal - POST /v5/asset/withdraw/create *)
module Withdraw_create = struct
  module T = struct
    let name = "withdraw-create"
    let endpoint = "asset/withdraw/create"
    let http_method = `POST
    let requires_auth = true

    type request =
      { coin : string                             (** Coin name *)
      ; chain : string                            (** Chain type *)
      ; address : string                          (** Withdrawal address *)
      ; tag : string option [@sexp.option]        (** Tag/memo for some chains *)
      ; amount : string                           (** Withdrawal amount *)
      ; timestamp : int64                         (** Current timestamp (Unix ms) *)
      ; forceChain : int option [@sexp.option]    (** Force chain: 0=false, 1=true *)
      ; accountType : string option [@sexp.option] (** FUND or UNIFIED *)
      }
    [@@deriving sexp]

    let request_to_params { coin; chain; address; tag; amount; timestamp; forceChain; accountType } =
      let base =
        [ ("coin", coin)
        ; ("chain", chain)
        ; ("address", address)
        ; ("amount", amount)
        ; ("timestamp", Int64.to_string timestamp)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "tag" tag
      |> add_opt "forceChain" (Option.map forceChain ~f:Int.to_string)
      |> add_opt "accountType" accountType

    type response =
      { id : string  (** Withdrawal ID *)
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Get withdrawal records - GET /v5/asset/withdraw/query-record *)
module Withdrawal_record = struct
  module T = struct
    let name = "withdrawal-record"
    let endpoint = "asset/withdraw/query-record"
    let http_method = `GET
    let requires_auth = true

    type request =
      { withdrawID : string option [@sexp.option]   (** Withdrawal ID filter *)
      ; coin : string option [@sexp.option]         (** Coin filter *)
      ; withdrawType : int option [@sexp.option]    (** 0=on-chain, 1=off-chain, 2=all *)
      ; startTime : int64 option [@sexp.option]     (** Start time (Unix ms) *)
      ; endTime : int64 option [@sexp.option]       (** End time (Unix ms) *)
      ; limit : int option [@sexp.option]           (** Limit (max 50) *)
      ; cursor : string option [@sexp.option]       (** Pagination cursor *)
      }
    [@@deriving sexp]

    let request_to_params { withdrawID; coin; withdrawType; startTime; endTime; limit; cursor } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "withdrawID" withdrawID
      |> add_opt "coin" coin
      |> add_opt "withdrawType" (Option.map withdrawType ~f:Int.to_string)
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "cursor" cursor

    (** Bybit withdrawal status:
        SecurityCheck, Pending, success, CancelByUser, Reject, Fail, BlockchainConfirmed *)
    type withdrawal =
      { withdrawId : string     (** Withdrawal ID *)
      ; txID : string           (** Transaction hash *)
      ; coin : string           (** Coin name *)
      ; chain : string          (** Chain type *)
      ; amount : string         (** Withdrawal amount *)
      ; withdrawFee : string    (** Withdrawal fee *)
      ; status : string         (** Status string *)
      ; toAddress : string      (** Destination address *)
      ; tag : string            (** Tag/memo *)
      ; createTime : string     (** Create time (Unix ms) *)
      ; updateTime : string     (** Update time (Unix ms) *)
      }
    [@@deriving sexp]

    let withdrawal_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        Result.Ok
          { withdrawId = get_str "withdrawId"
          ; txID = get_str "txID"
          ; coin = get_str "coin"
          ; chain = get_str "chain"
          ; amount = get_str "amount"
          ; withdrawFee = get_str "withdrawFee"
          ; status = get_str "status"
          ; toAddress = get_str "toAddress"
          ; tag = get_str "tag"
          ; createTime = get_str "createTime"
          ; updateTime = get_str "updateTime"
          }
      | _ -> Result.Error "Expected withdrawal record object"

    type response =
      { rows : withdrawal list
      ; nextPageCursor : string
      }
    [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let rows = match get "rows" with
          | Some (`List lst) ->
            List.filter_map lst ~f:(fun json ->
              match withdrawal_of_yojson json with
              | Result.Ok w -> Some w
              | Result.Error _ -> None)
          | _ -> []
        in
        let cursor = match get "nextPageCursor" with Some (`String s) -> s | _ -> "" in
        Result.Ok { rows; nextPageCursor = cursor }
      | _ -> Result.Error "Expected withdrawal records response"
  end

  include T
  include Rest.Make (T)
end

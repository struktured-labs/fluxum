(** MEXC REST API V3 Endpoints *)

open Core
open Async

module Cfg = Cfg
module Rest = Rest

(* ============================================================ *)
(* Public Endpoints *)
(* ============================================================ *)

(** Server time - GET /api/v3/time *)
module Server_time = struct
  module T = struct
    let name = "server-time"
    let endpoint = "time"
    let http_method = `GET
    let requires_auth = false

    type request = unit [@@deriving sexp]

    let request_to_params () = []

    type response = { serverTime : int64 } [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(** Exchange info - GET /api/v3/exchangeInfo *)
module Exchange_info = struct
  module T = struct
    let name = "exchange-info"
    let endpoint = "exchangeInfo"
    let http_method = `GET
    let requires_auth = false

    type request = { symbol : string option [@default None] } [@@deriving sexp]

    let request_to_params { symbol } =
      match symbol with
      | Some s -> [ ("symbol", s) ]
      | None -> []

    type symbol_info =
      { symbol : string
      ; status : string
      ; baseAsset : string
      ; quoteAsset : string
      ; baseAssetPrecision : int [@default 8]
      ; quotePrecision : int [@default 8]
      ; quoteAssetPrecision : int [@default 8]
      ; baseCommissionPrecision : int [@default 8]
      ; quoteCommissionPrecision : int [@default 8]
      ; isSpotTradingAllowed : bool [@default true]
      ; isMarginTradingAllowed : bool [@default false]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response =
      { timezone : string
      ; serverTime : int64
      ; symbols : symbol_info list
      }
    [@@deriving sexp, of_yojson { strict = false }]
  end

  include T
  include Rest.Make (T)
end

(** Order book depth - GET /api/v3/depth *)
module Depth = struct
  module T = struct
    let name = "depth"
    let endpoint = "depth"
    let http_method = `GET
    let requires_auth = false

    type request =
      { symbol : string
      ; limit : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; limit } =
      let base = [ ("symbol", symbol) ] in
      match limit with
      | Some n -> base @ [ ("limit", Int.to_string n) ]
      | None -> base

    type level = string * string [@@deriving sexp]

    let level_of_yojson = function
      | `List [ `String price; `String qty ] -> Ok (price, qty)
      | json -> Error (sprintf "Invalid level: %s" (Yojson.Safe.to_string json))

    type response =
      { lastUpdateId : int64 [@default 0L]
      ; bids : level list
      ; asks : level list
      ; timestamp : int64 [@default 0L]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** 24hr ticker - GET /api/v3/ticker/24hr *)
module Ticker_24hr = struct
  module T = struct
    let name = "ticker-24hr"
    let endpoint = "ticker/24hr"
    let http_method = `GET
    let requires_auth = false

    type request = { symbol : string option [@default None] } [@@deriving sexp]

    let request_to_params { symbol } =
      match symbol with
      | Some s -> [ ("symbol", s) ]
      | None -> []

    type ticker =
      { symbol : string
      ; priceChange : string
      ; priceChangePercent : string
      ; prevClosePrice : string [@default "0"]
      ; lastPrice : string
      ; bidPrice : string [@default "0"]
      ; bidQty : string [@default "0"]
      ; askPrice : string [@default "0"]
      ; askQty : string [@default "0"]
      ; openPrice : string
      ; highPrice : string
      ; lowPrice : string
      ; volume : string
      ; quoteVolume : string
      ; openTime : int64
      ; closeTime : int64
      ; count : int option [@default None]
      }
    [@@deriving sexp, of_yojson]

    type response = ticker [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Recent trades - GET /api/v3/trades *)
module Recent_trades = struct
  module T = struct
    let name = "recent-trades"
    let endpoint = "trades"
    let http_method = `GET
    let requires_auth = false

    type request =
      { symbol : string
      ; limit : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; limit } =
      let base = [ ("symbol", symbol) ] in
      match limit with
      | Some n -> base @ [ ("limit", Int.to_string n) ]
      | None -> base

    type trade =
      { id : int64 option [@default None]
      ; price : string
      ; qty : string
      ; quoteQty : string
      ; time : int64
      ; isBuyerMaker : bool
      ; isBestMatch : bool [@default true]
      ; tradeType : string [@default ""]
      }
    [@@deriving sexp, of_yojson]

    type response = trade list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* ============================================================ *)
(* Authenticated Endpoints *)
(* ============================================================ *)

(** Account information - GET /api/v3/account *)
module Account = struct
  module T = struct
    let name = "account"
    let endpoint = "account"
    let http_method = `GET
    let requires_auth = true

    type request = unit [@@deriving sexp]

    let request_to_params () = []

    type balance =
      { asset : string
      ; free : string
      ; locked : string
      }
    [@@deriving sexp, of_yojson]

    type response =
      { makerCommission : int [@default 0]
      ; takerCommission : int [@default 0]
      ; buyerCommission : int [@default 0]
      ; sellerCommission : int [@default 0]
      ; canTrade : bool [@default true]
      ; canWithdraw : bool [@default true]
      ; canDeposit : bool [@default true]
      ; accountType : string [@default "SPOT"]
      ; balances : balance list
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(** Place new order - POST /api/v3/order *)
module New_order = struct
  module T = struct
    let name = "new-order"
    let endpoint = "order"
    let http_method = `POST
    let requires_auth = true

    type request =
      { symbol : string
      ; side : Common.Side.t
      ; order_type : Common.Order_type.t
      ; quantity : string option [@default None]
      ; quoteOrderQty : string option [@default None]
      ; price : string option [@default None]
      ; newClientOrderId : string option [@default None]
      ; timeInForce : Common.Time_in_force.t option [@default None]
      }
    [@@deriving sexp]

    let request_to_params
        { symbol; side; order_type; quantity; quoteOrderQty; price
        ; newClientOrderId; timeInForce } =
      let base =
        [ ("symbol", symbol)
        ; ("side", Common.Side.to_string side)
        ; ("type", Common.Order_type.to_string order_type)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "quantity" quantity
      |> add_opt "quoteOrderQty" quoteOrderQty
      |> add_opt "price" price
      |> add_opt "newClientOrderId" newClientOrderId
      |> add_opt "timeInForce"
           (Option.map timeInForce ~f:Common.Time_in_force.to_string)

    type response =
      { symbol : string
      ; orderId : string
      ; orderListId : int64 [@default (-1L)]
      ; price : string [@default "0"]
      ; origQty : string
      ; executedQty : string [@default "0"]
      ; cummulativeQuoteQty : string [@default "0"]
      ; status : string
      ; timeInForce : string [@default "GTC"]
      ; type_ : string [@key "type"]
      ; side : string
      ; transactTime : int64 [@default 0L]
      }
    [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and side =
          enum_flag
            ~field_name:"side"
            ~type_name:"SIDE"
            ~of_string_opt:Common.Side.of_string_opt
            ~all:Common.Side.all
            ~to_string:Common.Side.to_string
            ~doc:"Order side (BUY or SELL)"
        and order_type =
          enum_flag
            ~field_name:"type"
            ~type_name:"ORDER_TYPE"
            ~of_string_opt:Common.Order_type.of_string_opt
            ~all:Common.Order_type.all
            ~to_string:Common.Order_type.to_string
            ~doc:"Order type (LIMIT, MARKET, etc.)"
        and quantity =
          string_flag_option ~field_name:"quantity" ~doc:"Order quantity"
        and quoteOrderQty =
          string_flag_option
            ~field_name:"quoteOrderQty"
            ~doc:"Quote order quantity (for MARKET orders)"
        and price =
          string_flag_option ~field_name:"price" ~doc:"Limit price"
        and newClientOrderId =
          string_flag_option
            ~field_name:"newClientOrderId"
            ~doc:"Custom client order ID"
        and timeInForce =
          enum_flag_option
            ~field_name:"timeInForce"
            ~type_name:"TIME_IN_FORCE"
            ~of_string_opt:Common.Time_in_force.of_string_opt
            ~all:Common.Time_in_force.all
            ~to_string:Common.Time_in_force.to_string
            ~doc:"Time in force (GTC, IOC, FOK)"
        in
        { symbol
        ; side
        ; order_type
        ; quantity
        ; quoteOrderQty
        ; price
        ; newClientOrderId
        ; timeInForce
        }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Cancel order - DELETE /api/v3/order *)
module Cancel_order = struct
  module T = struct
    let name = "cancel-order"
    let endpoint = "order"
    let http_method = `DELETE
    let requires_auth = true

    type request =
      { symbol : string
      ; orderId : string option [@default None]
      ; origClientOrderId : string option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; origClientOrderId } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" orderId
      |> add_opt "origClientOrderId" origClientOrderId

    type response =
      { symbol : string
      ; orderId : string
      ; origClientOrderId : string [@default ""]
      ; price : string [@default "0"]
      ; origQty : string
      ; executedQty : string [@default "0"]
      ; cummulativeQuoteQty : string [@default "0"]
      ; status : string
      ; timeInForce : string [@default "GTC"]
      ; type_ : string [@key "type"]
      ; side : string
      }
    [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and orderId =
          string_flag_option ~field_name:"orderId" ~doc:"Order ID to cancel"
        and origClientOrderId =
          string_flag_option
            ~field_name:"origClientOrderId"
            ~doc:"Client order ID to cancel"
        in
        { symbol; orderId; origClientOrderId }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Query order - GET /api/v3/order *)
module Query_order = struct
  module T = struct
    let name = "query-order"
    let endpoint = "order"
    let http_method = `GET
    let requires_auth = true

    type request =
      { symbol : string
      ; orderId : string option [@default None]
      ; origClientOrderId : string option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; origClientOrderId } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" orderId
      |> add_opt "origClientOrderId" origClientOrderId

    type response =
      { symbol : string
      ; orderId : string
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string [@default ""]
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string [@default "0"]
      ; status : string
      ; timeInForce : string [@default "GTC"]
      ; type_ : string [@key "type"]
      ; side : string
      ; stopPrice : string [@default "0"]
      ; time : int64
      ; updateTime : int64
      ; isWorking : bool [@default true]
      ; origQuoteOrderQty : string [@default "0"]
      }
    [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and orderId =
          string_flag_option ~field_name:"orderId" ~doc:"Order ID to query"
        and origClientOrderId =
          string_flag_option
            ~field_name:"origClientOrderId"
            ~doc:"Client order ID to query"
        in
        { symbol; orderId; origClientOrderId }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Open orders - GET /api/v3/openOrders *)
module Open_orders = struct
  module T = struct
    let name = "open-orders"
    let endpoint = "openOrders"
    let http_method = `GET
    let requires_auth = true

    type request = { symbol : string option [@default None] } [@@deriving sexp]

    let request_to_params { symbol } =
      match symbol with
      | Some s -> [ ("symbol", s) ]
      | None -> []

    type order =
      { symbol : string
      ; orderId : string
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string [@default ""]
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string [@default "0"]
      ; status : string
      ; timeInForce : string [@default "GTC"]
      ; type_ : string [@key "type"]
      ; side : string
      ; stopPrice : string [@default "0"]
      ; time : int64
      ; updateTime : int64
      ; isWorking : bool [@default true]
      ; origQuoteOrderQty : string [@default "0"]
      }
    [@@deriving sexp, of_yojson]

    type response = order list [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag_option ~field_name:"symbol" ~doc:"Trading pair (optional)"
        in
        { symbol }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** All orders - GET /api/v3/allOrders *)
module All_orders = struct
  module T = struct
    let name = "all-orders"
    let endpoint = "allOrders"
    let http_method = `GET
    let requires_auth = true

    type request =
      { symbol : string
      ; orderId : string option [@default None]
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; limit : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; startTime; endTime; limit } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key f = function
        | None -> Fun.id
        | Some v -> List.cons (key, f v)
      in
      base
      |> add_opt "orderId" Fn.id orderId
      |> add_opt "startTime" Int64.to_string startTime
      |> add_opt "endTime" Int64.to_string endTime
      |> add_opt "limit" Int.to_string limit

    type response = Open_orders.T.order list [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and orderId =
          string_flag_option ~field_name:"orderId" ~doc:"Start from order ID"
        and startTime =
          int64_flag_option ~field_name:"startTime" ~doc:"Start time (ms)"
        and endTime =
          int64_flag_option ~field_name:"endTime" ~doc:"End time (ms)"
        and limit =
          int_flag_option ~field_name:"limit" ~doc:"Max results (default 500)"
        in
        { symbol; orderId; startTime; endTime; limit }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** My trades - GET /api/v3/myTrades *)
module My_trades = struct
  module T = struct
    let name = "my-trades"
    let endpoint = "myTrades"
    let http_method = `GET
    let requires_auth = true

    type request =
      { symbol : string
      ; orderId : string option [@default None]
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; fromId : int64 option [@default None]
      ; limit : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; startTime; endTime; fromId; limit } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key f = function
        | None -> Fun.id
        | Some v -> List.cons (key, f v)
      in
      base
      |> add_opt "orderId" Fn.id orderId
      |> add_opt "startTime" Int64.to_string startTime
      |> add_opt "endTime" Int64.to_string endTime
      |> add_opt "fromId" Int64.to_string fromId
      |> add_opt "limit" Int.to_string limit

    type trade =
      { symbol : string
      ; id : int64
      ; orderId : string
      ; price : string
      ; qty : string
      ; quoteQty : string
      ; commission : string
      ; commissionAsset : string
      ; time : int64
      ; isBuyer : bool
      ; isMaker : bool
      ; isBestMatch : bool [@default true]
      }
    [@@deriving sexp, of_yojson]

    type response = trade list [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and orderId =
          string_flag_option ~field_name:"orderId" ~doc:"Filter by order ID"
        and startTime =
          int64_flag_option ~field_name:"startTime" ~doc:"Start time (ms)"
        and endTime =
          int64_flag_option ~field_name:"endTime" ~doc:"End time (ms)"
        and fromId =
          int64_flag_option ~field_name:"fromId" ~doc:"Start from trade ID"
        and limit =
          int_flag_option ~field_name:"limit" ~doc:"Max results (default 500)"
        in
        { symbol; orderId; startTime; endTime; fromId; limit }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Cancel all open orders - DELETE /api/v3/openOrders *)
module Cancel_all_orders = struct
  module T = struct
    let name = "cancel-all-orders"
    let endpoint = "openOrders"
    let http_method = `DELETE
    let requires_auth = true

    type request = { symbol : string } [@@deriving sexp]

    let request_to_params { symbol } = [ ("symbol", symbol) ]

    type response = Cancel_order.T.response list [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol =
          string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        in
        { symbol }]
  end

  include Rest.Make_with_params (T) (Params)
end

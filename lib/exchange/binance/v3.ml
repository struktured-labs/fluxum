(** Binance API v3 Endpoints *)

open Core
open Async

(* ===== PUBLIC ENDPOINTS ===== *)

(** Server time - GET /api/v3/time (no auth required) *)
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

(** Exchange information - GET /api/v3/exchangeInfo *)
module Exchange_info = struct
  module T = struct
    let name = "exchange-info"
    let endpoint = "exchangeInfo"
    let http_method = `GET
    let requires_auth = false

    type request = { symbol : string option [@default None] } [@@deriving sexp]

    let request_to_params { symbol } =
      match symbol with
      | None -> []
      | Some s -> [ ("symbol", s) ]

    (** Symbol filter from Binance API *)
    type symbol_filter =
      { filterType : string
      ; minPrice : string option [@default None]
      ; maxPrice : string option [@default None]
      ; tickSize : string option [@default None]
      ; minQty : string option [@default None]
      ; maxQty : string option [@default None]
      ; stepSize : string option [@default None]
      ; minNotional : string option [@default None]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type symbol_info =
      { symbol : string
      ; status : string
      ; baseAsset : string
      ; quoteAsset : string
      ; filters : symbol_filter list [@default []]
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

(** Order book depth snapshot - GET /api/v3/depth *)
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
      | None -> base
      | Some l -> base @ [ ("limit", Int.to_string l) ]

    type response =
      { lastUpdateId : int64
      ; bids : (string * string) list
      ; asks : (string * string) list
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

    type request = { symbol : string } [@@deriving sexp]

    let request_to_params { symbol } = [ ("symbol", symbol) ]

    type response =
      { symbol : string
      ; priceChange : string
      ; priceChangePercent : string
      ; weightedAvgPrice : string
      ; prevClosePrice : string
      ; lastPrice : string
      ; lastQty : string
      ; bidPrice : string
      ; bidQty : string
      ; askPrice : string
      ; askQty : string
      ; openPrice : string
      ; highPrice : string
      ; lowPrice : string
      ; volume : string
      ; quoteVolume : string
      ; openTime : int64
      ; closeTime : int64
      ; firstId : int64
      ; lastId : int64
      ; count : int64
      }
    [@@deriving sexp, of_yojson]
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
      | None -> base
      | Some l -> base @ [ ("limit", Int.to_string l) ]

    type trade =
      { id : int64
      ; price : string
      ; qty : string
      ; quoteQty : string
      ; time : int64
      ; isBuyerMaker : bool
      ; isBestMatch : bool
      }
    [@@deriving sexp, of_yojson]

    type response = trade list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(* ===== ACCOUNT ENDPOINTS ===== *)

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
      ; updateTime : int64 [@default 0L]
      ; accountType : string [@default "SPOT"]
      ; balances : balance list
      ; permissions : string list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(* ===== TRADING ENDPOINTS ===== *)

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
      ; order_type : Common.Order_type.t [@key "type"]
      ; timeInForce : Common.Time_in_force.t option [@default None]
      ; quantity : string option [@default None]
      ; quoteOrderQty : string option [@default None]
      ; price : string option [@default None]
      ; newClientOrderId : string option [@default None]
      ; stopPrice : string option [@default None]
      ; icebergQty : string option [@default None]
      ; newOrderRespType : string option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params
        { symbol; side; order_type; timeInForce; quantity; quoteOrderQty
        ; price; newClientOrderId; stopPrice; icebergQty; newOrderRespType
        ; recvWindow } =
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
      |> add_opt "timeInForce" (Option.map timeInForce ~f:Common.Time_in_force.to_string)
      |> add_opt "quantity" quantity
      |> add_opt "quoteOrderQty" quoteOrderQty
      |> add_opt "price" price
      |> add_opt "newClientOrderId" newClientOrderId
      |> add_opt "stopPrice" stopPrice
      |> add_opt "icebergQty" icebergQty
      |> add_opt "newOrderRespType" newOrderRespType
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type fill =
      { price : string
      ; qty : string
      ; commission : string
      ; commissionAsset : string
      ; tradeId : int64 [@default 0L]
      }
    [@@deriving sexp, of_yojson]

    type response =
      { symbol : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; transactTime : int64
      ; price : string [@default "0"]
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string [@default "GTC"]
      ; type_ : string [@key "type"]
      ; side : string
      ; fills : fill list [@default []]
      }
    [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let symbol = string_flag ~field_name:"symbol" ~doc:"Trading pair (e.g., BTCUSDT)"
        and side = enum_flag
          ~field_name:"side"
          ~type_name:"SIDE"
          ~of_string_opt:Common.Side.of_string_opt
          ~all:Common.Side.all
          ~to_string:Common.Side.to_string
          ~doc:"Order side (BUY or SELL)"
        and order_type = enum_flag
          ~field_name:"type"
          ~type_name:"ORDER_TYPE"
          ~of_string_opt:Common.Order_type.of_string_opt
          ~all:Common.Order_type.all
          ~to_string:Common.Order_type.to_string
          ~doc:"Order type (LIMIT, MARKET, etc.)"
        and timeInForce = enum_flag_option
          ~field_name:"timeInForce"
          ~type_name:"TIME_IN_FORCE"
          ~of_string_opt:Common.Time_in_force.of_string_opt
          ~all:Common.Time_in_force.all
          ~to_string:Common.Time_in_force.to_string
          ~doc:"Time in force (GTC, IOC, FOK)"
        and quantity = string_flag_option ~field_name:"quantity" ~doc:"Order quantity"
        and quoteOrderQty = string_flag_option
          ~field_name:"quoteOrderQty"
          ~doc:"Quote order quantity (for MARKET orders)"
        and price = string_flag_option ~field_name:"price" ~doc:"Limit price"
        and newClientOrderId = string_flag_option
          ~field_name:"newClientOrderId"
          ~doc:"Custom client order ID"
        and stopPrice = string_flag_option ~field_name:"stopPrice" ~doc:"Stop price"
        and icebergQty = string_flag_option ~field_name:"icebergQty" ~doc:"Iceberg quantity"
        and newOrderRespType = string_flag_option
          ~field_name:"newOrderRespType"
          ~doc:"Response type (ACK, RESULT, FULL)"
        and recvWindow = int_flag_option ~field_name:"recvWindow" ~doc:"Receive window (ms)"
        in
        { symbol; side; order_type; timeInForce; quantity; quoteOrderQty
        ; price; newClientOrderId; stopPrice; icebergQty; newOrderRespType
        ; recvWindow
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
      ; orderId : int64 option [@default None]
      ; origClientOrderId : string option [@default None]
      ; newClientOrderId : string option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; origClientOrderId; newClientOrderId; recvWindow } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" (Option.map orderId ~f:Int64.to_string)
      |> add_opt "origClientOrderId" origClientOrderId
      |> add_opt "newClientOrderId" newClientOrderId
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type response =
      { symbol : string
      ; origClientOrderId : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string
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
        let symbol = string_flag ~field_name:"symbol" ~doc:"Trading pair"
        and orderId = int64_flag_option ~field_name:"orderId" ~doc:"Order ID"
        and origClientOrderId = string_flag_option
          ~field_name:"origClientOrderId"
          ~doc:"Original client order ID"
        and newClientOrderId = string_flag_option
          ~field_name:"newClientOrderId"
          ~doc:"New client order ID"
        and recvWindow = int_flag_option ~field_name:"recvWindow" ~doc:"Receive window (ms)"
        in
        { symbol; orderId; origClientOrderId; newClientOrderId; recvWindow }]
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
      ; orderId : int64 option [@default None]
      ; origClientOrderId : string option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; origClientOrderId; recvWindow } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" (Option.map orderId ~f:Int64.to_string)
      |> add_opt "origClientOrderId" origClientOrderId
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type response =
      { symbol : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string
      ; type_ : string [@key "type"]
      ; side : string
      ; stopPrice : string [@default "0"]
      ; icebergQty : string [@default "0"]
      ; time : int64
      ; updateTime : int64
      ; isWorking : bool
      ; origQuoteOrderQty : string [@default "0"]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Open orders - GET /api/v3/openOrders *)
module Open_orders = struct
  module T = struct
    let name = "open-orders"
    let endpoint = "openOrders"
    let http_method = `GET
    let requires_auth = true

    type request =
      { symbol : string option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; recvWindow } =
      let base = [] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "symbol" symbol
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type order =
      { symbol : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string
      ; type_ : string [@key "type"]
      ; side : string
      ; stopPrice : string [@default "0"]
      ; icebergQty : string [@default "0"]
      ; time : int64
      ; updateTime : int64
      ; isWorking : bool
      ; origQuoteOrderQty : string [@default "0"]
      }
    [@@deriving sexp, of_yojson]

    type response = order list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** All orders (history) - GET /api/v3/allOrders *)
module All_orders = struct
  module T = struct
    let name = "all-orders"
    let endpoint = "allOrders"
    let http_method = `GET
    let requires_auth = true

    type request =
      { symbol : string
      ; orderId : int64 option [@default None]
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; limit : int option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; startTime; endTime; limit; recvWindow } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" (Option.map orderId ~f:Int64.to_string)
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type order =
      { symbol : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string
      ; type_ : string [@key "type"]
      ; side : string
      ; stopPrice : string [@default "0"]
      ; icebergQty : string [@default "0"]
      ; time : int64
      ; updateTime : int64
      ; isWorking : bool
      ; origQuoteOrderQty : string [@default "0"]
      }
    [@@deriving sexp, of_yojson]

    type response = order list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
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
      ; orderId : int64 option [@default None]
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; fromId : int64 option [@default None]
      ; limit : int option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; orderId; startTime; endTime; fromId; limit; recvWindow } =
      let base = [ ("symbol", symbol) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "orderId" (Option.map orderId ~f:Int64.to_string)
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "fromId" (Option.map fromId ~f:Int64.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type trade =
      { symbol : string
      ; id : int64
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; price : string
      ; qty : string
      ; quoteQty : string
      ; commission : string
      ; commissionAsset : string
      ; time : int64
      ; isBuyer : bool
      ; isMaker : bool
      ; isBestMatch : bool
      }
    [@@deriving sexp, of_yojson]

    type response = trade list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** Cancel all open orders - DELETE /api/v3/openOrders *)
module Cancel_all_orders = struct
  module T = struct
    let name = "cancel-all-orders"
    let endpoint = "openOrders"
    let http_method = `DELETE
    let requires_auth = true

    type request =
      { symbol : string
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { symbol; recvWindow } =
      let base = [ ("symbol", symbol) ] in
      match recvWindow with
      | None -> base
      | Some rw -> base @ [ ("recvWindow", Int.to_string rw) ]

    type order =
      { symbol : string
      ; origClientOrderId : string
      ; orderId : int64
      ; orderListId : int64 [@default (-1L)]
      ; clientOrderId : string
      ; price : string
      ; origQty : string
      ; executedQty : string
      ; cummulativeQuoteQty : string
      ; status : string
      ; timeInForce : string
      ; type_ : string [@key "type"]
      ; side : string
      }
    [@@deriving sexp, of_yojson]

    type response = order list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make (T)
end

(** User Data Stream - Manage listen keys for WebSocket user data streams
    POST /api/v3/userDataStream - Create listen key
    PUT /api/v3/userDataStream - Keepalive (extend validity)
    DELETE /api/v3/userDataStream - Close listen key

    Listen keys are valid for 60 minutes. Call keepalive every 30 minutes.
    Use with wss://stream.binance.com:9443/ws/<listenKey> *)
module User_data_stream = struct
  module Create = struct
    module T = struct
      let name = "user-data-stream-create"
      let endpoint = "userDataStream"
      let http_method = `POST
      let requires_auth = true

      type request = unit [@@deriving sexp]
      let request_to_params () = []

      type response = { listenKey : string } [@@deriving sexp, of_yojson]
    end

    include T
    include Rest.Make_no_arg (T)
  end

  module Keepalive = struct
    module T = struct
      let name = "user-data-stream-keepalive"
      let endpoint = "userDataStream"
      let http_method = `PUT
      let requires_auth = true

      type request = { listenKey : string } [@@deriving sexp]

      let request_to_params { listenKey } = [ ("listenKey", listenKey) ]

      type response = unit [@@deriving sexp]

      let response_of_yojson _ = Ok ()
    end

    include T
    include Rest.Make (T)
  end

  module Close = struct
    module T = struct
      let name = "user-data-stream-close"
      let endpoint = "userDataStream"
      let http_method = `DELETE
      let requires_auth = true

      type request = { listenKey : string } [@@deriving sexp]

      let request_to_params { listenKey } = [ ("listenKey", listenKey) ]

      type response = unit [@@deriving sexp]

      let response_of_yojson _ = Ok ()
    end

    include T
    include Rest.Make (T)
  end
end

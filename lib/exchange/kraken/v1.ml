open Core
open Async

module Cfg = Cfg
module Rest = Rest
module Signature = Signature

(* Common types *)

module Balance = struct
  (** Kraken balance response is a map of currency -> balance *)
  type t = (string * string) list [@@deriving sexp]

  let of_yojson = function
    | `Assoc pairs ->
      Result.Ok (List.map pairs ~f:(fun (k, v) ->
        match v with
        | `String s -> (k, s)
        | _ -> (k, Yojson.Safe.to_string v)))
    | _ -> Result.Error "Expected object for balance"
end

(** Get account balances *)
module Balances = struct
  module T = struct
    let name = "balances"
    let endpoint = "Balance"

    type request = unit [@@deriving sexp]

    let request_to_params () = []

    type response = Balance.t [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(** Open orders *)
module Open_orders = struct
  (** Single order description *)
  module Order_descr = struct
    type t =
      { pair : string
      ; type_ : Common.Side.t [@key "type"]
      ; ordertype : Common.Order_type.t
      ; price : string
      ; price2 : string
      ; leverage : string
      ; order : string
      ; close : string
      }
    [@@deriving sexp, of_yojson]
  end

  (** Single order *)
  module Order = struct
    type t =
      { refid : string option [@default None]
      ; userref : int [@default 0]
      ; status : string
      ; reason : string option [@default None]
      ; opentm : float
      ; closetm : float [@default 0.0]
      ; starttm : float [@default 0.0]
      ; expiretm : float [@default 0.0]
      ; descr : Order_descr.t
      ; vol : string
      ; vol_exec : string
      ; cost : string
      ; fee : string
      ; price : string
      ; stopprice : string [@default "0"]
      ; limitprice : string [@default "0"]
      ; misc : string
      ; oflags : string
      }
    [@@deriving sexp, of_yojson]
  end

  (** Response is a map of order ID -> order *)
  module Orders_map = struct
    type t =
      { open_ : (string * Order.t) list [@key "open"]
      }
    [@@deriving sexp]

    let of_yojson = function
      | `Assoc [ ("open", `Assoc orders) ] ->
        let open_orders =
          List.map orders ~f:(fun (id, json) ->
            match Order.of_yojson json with
            | Result.Ok order -> (id, order)
            | Result.Error _ -> failwith "Failed to parse order")
        in
        Result.Ok { open_ = open_orders }
      | _ -> Result.Error "Expected {\"open\": {...}}"
  end

  module T = struct
    let name = "open-orders"
    let endpoint = "OpenOrders"

    type request = { trades : bool [@default false] } [@@deriving sexp]

    let request_to_params { trades } =
      [ ("trades", match trades with true -> "true" | false -> "false") ]

    type response = Orders_map.t [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let trades = bool_flag ~field_name:"trades" ~default:false
          ~doc:"Include trade info in output"
        in
        { trades }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Add a new order *)
module Add_order = struct
  module T = struct
    let name = "add-order"
    let endpoint = "AddOrder"

    type request =
      { pair : string
      ; type_ : Common.Side.t
      ; ordertype : Common.Order_type.t
      ; volume : float
      ; price : string option [@default None]
      ; price2 : string option [@default None]
      ; leverage : string option [@default None]
      ; oflags : string option [@default None]
      ; starttm : string option [@default None]
      ; expiretm : string option [@default None]
      ; timeinforce : string option [@default None]
      }
    [@@deriving sexp]

    let request_to_params
        { pair; type_; ordertype; volume; price; price2; leverage
        ; oflags; starttm; expiretm; timeinforce } =
      let base =
        [ ("pair", pair)
        ; ("type", Common.Side.to_string type_)
        ; ("ordertype", Common.Order_type.to_string ordertype)
        ; ("volume", Float.to_string volume)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "price" price
      |> add_opt "price2" price2
      |> add_opt "leverage" leverage
      |> add_opt "oflags" oflags
      |> add_opt "starttm" starttm
      |> add_opt "expiretm" expiretm
      |> add_opt "timeinforce" timeinforce

    (** Order description in response *)
    type order_descr_result =
      { order : string [@default ""]
      ; close : string [@default ""]
      }
    [@@deriving sexp, of_yojson]

    (** Response contains order description and transaction IDs *)
    type order_result =
      { descr : order_descr_result [@default { order = ""; close = "" }]
      ; txid : string list [@default []]
      }
    [@@deriving sexp, of_yojson]

    type response = order_result [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let pair = string_flag ~field_name:"pair" ~doc:"Trading pair (e.g., XETHZUSD)"
        and type_ =
          enum_flag ~field_name:"type_" ~type_name:"SIDE"
            ~of_string_opt:Common.Side.of_string_opt
            ~all:Common.Side.all
            ~to_string:Common.Side.to_string
            ~doc:"Order side (buy or sell)"
        and ordertype =
          enum_flag ~field_name:"ordertype" ~type_name:"ORDER_TYPE"
            ~of_string_opt:Common.Order_type.of_string_opt
            ~all:Common.Order_type.all
            ~to_string:Common.Order_type.to_string
            ~doc:"Order type (market, limit, etc.)"
        and volume = float_flag ~field_name:"volume" ~doc:"Order volume"
        and price = string_flag_option ~field_name:"price" ~doc:"Limit price (for limit orders)"
        and price2 = string_flag_option ~field_name:"price2" ~doc:"Secondary price (for stop orders)"
        and leverage = string_flag_option ~field_name:"leverage" ~doc:"Leverage amount"
        and oflags = string_flag_option ~field_name:"oflags" ~doc:"Order flags (comma-separated)"
        and starttm = string_flag_option ~field_name:"starttm" ~doc:"Start time"
        and expiretm = string_flag_option ~field_name:"expiretm" ~doc:"Expiration time"
        and timeinforce = string_flag_option ~field_name:"timeinforce" ~doc:"Time in force"
        in
        { pair; type_; ordertype; volume; price; price2; leverage; oflags; starttm; expiretm; timeinforce }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Cancel an order *)
module Cancel_order = struct
  module T = struct
    let name = "cancel-order"
    let endpoint = "CancelOrder"

    type request = { txid : string } [@@deriving sexp]

    let request_to_params { txid } = [ ("txid", txid) ]

    (** Response contains count of orders cancelled *)
    type cancel_result =
      { count : int
      ; pending : bool option [@default None]
      }
    [@@deriving sexp, of_yojson]

    type response = cancel_result [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let txid = string_flag ~field_name:"txid" ~doc:"Transaction ID of order to cancel"
        in
        { txid }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Query orders by transaction ID *)
module Query_orders = struct
  module T = struct
    let name = "query-orders"
    let endpoint = "QueryOrders"

    type request =
      { txids : string list
      ; trades : bool [@default false]
      }
    [@@deriving sexp]

    let request_to_params { txids; trades } =
      [ ("txid", String.concat ~sep:"," txids)
      ; ("trades", match trades with true -> "true" | false -> "false")
      ]

    (** Response is a map of order ID -> order (reuse Order from Open_orders) *)
    type orders_map = (string * Open_orders.Order.t) list [@@deriving sexp]

    let orders_map_of_yojson = function
      | `Assoc pairs ->
        Result.Ok
          (List.map pairs ~f:(fun (id, json) ->
            match Open_orders.Order.of_yojson json with
            | Result.Ok order -> (id, order)
            | Result.Error e -> failwith e))
      | _ -> Result.Error "Expected object"

    type response = orders_map [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let txids = string_list_flag ~field_name:"txids"
          ~doc:"Transaction IDs to query (can specify multiple times)"
        and trades = bool_flag ~field_name:"trades" ~default:false
          ~doc:"Include trade info in output"
        in
        { txids; trades }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Get closed orders *)
module Closed_orders = struct
  module T = struct
    let name = "closed-orders"
    let endpoint = "ClosedOrders"

    type request =
      { trades : bool [@default false]
      ; userref : int option [@default None]
      ; start : int option [@default None]
      ; end_ : int option [@default None]
      ; ofs : int option [@default None]
      ; closetime : string option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { trades; userref; start; end_; ofs; closetime } =
      let base = [ ("trades", match trades with true -> "true" | false -> "false") ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, Int.to_string v)
      in
      let add_opt_str key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "userref" userref
      |> add_opt "start" start
      |> add_opt "end" end_
      |> add_opt "ofs" ofs
      |> add_opt_str "closetime" closetime

    (** Response has closed orders and count *)
    type closed_result =
      { closed : (string * Open_orders.Order.t) list
      ; count : int
      }
    [@@deriving sexp]

    let closed_result_of_yojson = function
      | `Assoc pairs ->
        let closed =
          List.find_map pairs ~f:(function
            | ("closed", `Assoc orders) ->
              Some
                (List.map orders ~f:(fun (id, json) ->
                  match Open_orders.Order.of_yojson json with
                  | Result.Ok order -> (id, order)
                  | Result.Error e -> failwith e))
            | _ -> None)
          |> Option.value ~default:[]
        in
        let count =
          List.find_map pairs ~f:(function
            | ("count", `Int n) -> Some n
            | _ -> None)
          |> Option.value ~default:0
        in
        Result.Ok { closed; count }
      | _ -> Result.Error "Expected object"

    type response = closed_result [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let trades = bool_flag ~field_name:"trades" ~default:false
          ~doc:"Include trade info in output"
        and userref = int_flag_option ~field_name:"userref"
          ~doc:"Filter by user reference ID"
        and start = int_flag_option ~field_name:"start"
          ~doc:"Starting timestamp or order ID"
        and end_ = int_flag_option ~field_name:"end_"
          ~doc:"Ending timestamp or order ID"
        and ofs = int_flag_option ~field_name:"ofs"
          ~doc:"Result offset"
        and closetime = string_flag_option ~field_name:"closetime"
          ~doc:"Which time to use for filtering (open, close, both)"
        in
        { trades; userref; start; end_; ofs; closetime }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Get trade history - TradesHistory *)
module Trades_history = struct
  module Trade = struct
    type t =
      { ordertxid : string
      ; pair : string
      ; time : float
      ; type_ : string [@key "type"]
      ; ordertype : string
      ; price : string
      ; cost : string
      ; fee : string
      ; vol : string
      ; margin : string
      ; misc : string
      }
    [@@deriving sexp]

    let of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        let get_float key = match get key with Some (`Float f) -> f | _ -> 0.0 in
        Result.Ok
          { ordertxid = get_str "ordertxid"
          ; pair = get_str "pair"
          ; time = get_float "time"
          ; type_ = get_str "type"
          ; ordertype = get_str "ordertype"
          ; price = get_str "price"
          ; cost = get_str "cost"
          ; fee = get_str "fee"
          ; vol = get_str "vol"
          ; margin = get_str "margin"
          ; misc = get_str "misc"
          }
      | _ -> Result.Error "Expected trade object"
  end

  module T = struct
    let name = "trades-history"
    let endpoint = "TradesHistory"

    type request =
      { type_ : string option [@default None]
      ; trades : bool [@default false]
      ; start : int option [@default None]
      ; end_ : int option [@default None]
      ; ofs : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { type_; trades; start; end_; ofs } =
      let base = [ ("trades", match trades with true -> "true" | false -> "false") ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, Int.to_string v)
      in
      let add_opt_str key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt_str "type" type_
      |> add_opt "start" start
      |> add_opt "end" end_
      |> add_opt "ofs" ofs

    type trades_result =
      { trades : (string * Trade.t) list
      ; count : int
      }
    [@@deriving sexp]

    let trades_result_of_yojson = function
      | `Assoc pairs ->
        let trades =
          List.find_map pairs ~f:(function
            | ("trades", `Assoc trade_list) ->
              Some
                (List.filter_map trade_list ~f:(fun (id, json) ->
                  match Trade.of_yojson json with
                  | Result.Ok trade -> Some (id, trade)
                  | Result.Error _ -> None))
            | _ -> None)
          |> Option.value ~default:[]
        in
        let count =
          List.find_map pairs ~f:(function
            | ("count", `Int n) -> Some n
            | _ -> None)
          |> Option.value ~default:0
        in
        Result.Ok { trades; count }
      | _ -> Result.Error "Expected trades object"

    type response = trades_result [@@deriving sexp, of_yojson]
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let type_ = string_flag_option ~field_name:"type"
          ~doc:"Trade type filter (all, any, closed, no position)"
        and trades = bool_flag ~field_name:"trades" ~default:false
          ~doc:"Include related trade IDs"
        and start = int_flag_option ~field_name:"start"
          ~doc:"Starting timestamp"
        and end_ = int_flag_option ~field_name:"end"
          ~doc:"Ending timestamp"
        and ofs = int_flag_option ~field_name:"ofs"
          ~doc:"Result offset"
        in
        { type_; trades; start; end_; ofs }]
  end

  include Rest.Make_with_params (T) (Params)
end

(** Get tradeable asset pairs - AssetPairs *)
module Asset_pairs = struct
  module Pair_info = struct
    type t =
      { altname : string
      ; wsname : string option [@default None]
      ; base : string
      ; quote : string
      ; pair_decimals : int
      ; lot_decimals : int
      ; lot_multiplier : int
      ; ordermin : string option [@default None]
      ; status : string [@default "online"]
      }
    [@@deriving sexp]

    let of_yojson = function
      | `Assoc pairs ->
        let get key = List.Assoc.find pairs key ~equal:String.equal in
        let get_str key = match get key with Some (`String s) -> s | _ -> "" in
        let get_str_opt key = match get key with Some (`String s) -> Some s | _ -> None in
        let get_int key = match get key with Some (`Int i) -> i | _ -> 0 in
        Result.Ok
          { altname = get_str "altname"
          ; wsname = get_str_opt "wsname"
          ; base = get_str "base"
          ; quote = get_str "quote"
          ; pair_decimals = get_int "pair_decimals"
          ; lot_decimals = get_int "lot_decimals"
          ; lot_multiplier = get_int "lot_multiplier"
          ; ordermin = get_str_opt "ordermin"
          ; status = (match get_str_opt "status" with Some s -> s | None -> "online")
          }
      | _ -> Result.Error "Expected pair info object"
  end

  module T = struct
    let name = "asset-pairs"
    let endpoint = "AssetPairs"

    type request =
      { pair : string option [@default None]
      ; info : string option [@default None]  (* "info", "leverage", "fees", "margin" *)
      }
    [@@deriving sexp]

    let request_to_params { pair; info } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "pair" pair
      |> add_opt "info" info

    type response = (string * Pair_info.t) list [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let parsed =
          List.filter_map pairs ~f:(fun (name, json) ->
            match Pair_info.of_yojson json with
            | Result.Ok info -> Some (name, info)
            | Result.Error _ -> None)
        in
        Result.Ok parsed
      | _ -> Result.Error "Expected asset pairs object"
  end

  include T

  module Params = struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let pair = string_flag_option ~field_name:"pair"
          ~doc:"Specific pair to query (comma-separated for multiple)"
        and info = string_flag_option ~field_name:"info"
          ~doc:"Info level (info, leverage, fees, margin)"
        in
        { pair; info }]
  end

  include Rest.Make_with_params (T) (Params)
end

(* ============================================================ *)
(* Public Market Data Endpoints *)
(* ============================================================ *)

(** Get ticker information - GET /0/public/Ticker *)
module Ticker = struct
  module Ticker_data = struct
    (** Ticker data for a trading pair - arrays of [price, whole_lot_volume, lot_volume] *)
    type t =
      { a : string list  (** Ask [price, whole_lot_volume, lot_volume] *)
      ; b : string list  (** Bid [price, whole_lot_volume, lot_volume] *)
      ; c : string list  (** Last trade closed [price, lot_volume] *)
      ; v : string list  (** Volume [today, last 24 hours] *)
      ; p : string list  (** Volume weighted average price [today, last 24 hours] *)
      ; t : int list     (** Number of trades [today, last 24 hours] *)
      ; l : string list  (** Low [today, last 24 hours] *)
      ; h : string list  (** High [today, last 24 hours] *)
      ; o : string       (** Today's opening price *)
      }
    [@@deriving sexp]

    let of_yojson = function
      | `Assoc pairs ->
        let get_list key =
          List.Assoc.find pairs key ~equal:String.equal
          |> Option.bind ~f:(function
            | `List lst -> Some (List.filter_map lst ~f:(function `String s -> Some s | _ -> None))
            | _ -> None)
          |> Option.value ~default:[]
        in
        let get_int_list key =
          List.Assoc.find pairs key ~equal:String.equal
          |> Option.bind ~f:(function
            | `List lst -> Some (List.filter_map lst ~f:(function `Int i -> Some i | _ -> None))
            | _ -> None)
          |> Option.value ~default:[]
        in
        let get_str key =
          List.Assoc.find pairs key ~equal:String.equal
          |> Option.bind ~f:(function `String s -> Some s | _ -> None)
          |> Option.value ~default:""
        in
        Result.Ok
          { a = get_list "a"
          ; b = get_list "b"
          ; c = get_list "c"
          ; v = get_list "v"
          ; p = get_list "p"
          ; t = get_int_list "t"
          ; l = get_list "l"
          ; h = get_list "h"
          ; o = get_str "o"
          }
      | _ -> Result.Error "Expected ticker data object"
  end

  module T = struct
    let name = "ticker"
    let endpoint = "Ticker"

    type request = { pair : string } [@@deriving sexp]

    let request_to_params { pair } = [ ("pair", pair) ]

    type response = (string * Ticker_data.t) list [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let parsed =
          List.filter_map pairs ~f:(fun (name, json) ->
            match Ticker_data.of_yojson json with
            | Result.Ok data -> Some (name, data)
            | Result.Error _ -> None)
        in
        Result.Ok parsed
      | _ -> Result.Error "Expected ticker object"
  end

  include T
  include Rest.Make_public_with_params (T) (struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let pair = string_flag ~field_name:"pair" ~doc:"Trading pair (e.g., XETHZUSD)"
        in
        { pair }]
  end)
end

(** Get order book depth - GET /0/public/Depth *)
module Depth = struct
  module T = struct
    let name = "depth"
    let endpoint = "Depth"

    type request =
      { pair : string
      ; count : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { pair; count } =
      let base = [ ("pair", pair) ] in
      match count with
      | Some n -> base @ [ ("count", Int.to_string n) ]
      | None -> base

    (** Level is [price, volume, timestamp] *)
    type level = string * string * int [@@deriving sexp]

    let level_of_yojson = function
      | `List [ `String price; `String vol; `Int ts ] -> Ok (price, vol, ts)
      | json -> Error (sprintf "Invalid level: %s" (Yojson.Safe.to_string json))

    type depth_data =
      { asks : level list
      ; bids : level list
      }
    [@@deriving sexp]

    let depth_data_of_yojson = function
      | `Assoc pairs ->
        let get_levels key =
          List.Assoc.find pairs key ~equal:String.equal
          |> Option.bind ~f:(function
            | `List lst ->
              Some (List.filter_map lst ~f:(fun json ->
                match level_of_yojson json with
                | Ok l -> Some l
                | Error _ -> None))
            | _ -> None)
          |> Option.value ~default:[]
        in
        Result.Ok { asks = get_levels "asks"; bids = get_levels "bids" }
      | _ -> Result.Error "Expected depth data object"

    type response = (string * depth_data) list [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let parsed =
          List.filter_map pairs ~f:(fun (name, json) ->
            match depth_data_of_yojson json with
            | Result.Ok data -> Some (name, data)
            | Result.Error _ -> None)
        in
        Result.Ok parsed
      | _ -> Result.Error "Expected depth object"
  end

  include T
  include Rest.Make_public_with_params (T) (struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let pair = string_flag ~field_name:"pair" ~doc:"Trading pair (e.g., XETHZUSD)"
        and count = int_flag_option ~field_name:"count" ~doc:"Number of levels (max 500)"
        in
        { pair; count }]
  end)
end

(** Get recent trades - GET /0/public/Trades *)
module Recent_trades = struct
  module T = struct
    let name = "recent-trades"
    let endpoint = "Trades"

    type request =
      { pair : string
      ; since : string option [@default None]
      ; count : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { pair; since; count } =
      let base = [ ("pair", pair) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "since" since
      |> add_opt "count" (Option.map count ~f:Int.to_string)

    (** Trade is [price, volume, time, side, type, misc, trade_id] *)
    type trade =
      { price : string
      ; volume : string
      ; time : float
      ; side : string      (** "b" for buy, "s" for sell *)
      ; order_type : string  (** "m" for market, "l" for limit *)
      ; misc : string
      ; trade_id : int
      }
    [@@deriving sexp]

    let trade_of_yojson = function
      | `List [ `String price; `String volume; `Float time; `String side;
                `String order_type; `String misc; `Int trade_id ] ->
        Ok { price; volume; time; side; order_type; misc; trade_id }
      | `List [ `String price; `String volume; `Float time; `String side;
                `String order_type; `String misc ] ->
        (* Older format without trade_id *)
        Ok { price; volume; time; side; order_type; misc; trade_id = 0 }
      | json -> Error (sprintf "Invalid trade: %s" (Yojson.Safe.to_string json))

    type trades_result =
      { trades : trade list
      ; last : string
      }
    [@@deriving sexp]

    let trades_result_of_yojson pair_name = function
      | `Assoc pairs ->
        let trades =
          List.Assoc.find pairs pair_name ~equal:String.equal
          |> Option.bind ~f:(function
            | `List lst ->
              Some (List.filter_map lst ~f:(fun json ->
                match trade_of_yojson json with
                | Ok t -> Some t
                | Error _ -> None))
            | _ -> None)
          |> Option.value ~default:[]
        in
        let last =
          List.Assoc.find pairs "last" ~equal:String.equal
          |> Option.bind ~f:(function `String s -> Some s | _ -> None)
          |> Option.value ~default:""
        in
        Result.Ok { trades; last }
      | _ -> Result.Error "Expected trades result object"

    type response = (string * trade list) list * string [@@deriving sexp]

    let response_of_yojson = function
      | `Assoc pairs ->
        let last =
          List.Assoc.find pairs "last" ~equal:String.equal
          |> Option.bind ~f:(function `String s -> Some s | _ -> None)
          |> Option.value ~default:""
        in
        let trades =
          List.filter_map pairs ~f:(fun (name, json) ->
            if String.equal name "last" then None
            else
              match json with
              | `List lst ->
                let parsed = List.filter_map lst ~f:(fun j ->
                  match trade_of_yojson j with
                  | Ok t -> Some t
                  | Error _ -> None)
                in
                Some (name, parsed)
              | _ -> None)
        in
        Result.Ok (trades, last)
      | _ -> Result.Error "Expected trades object"
  end

  include T
  include Rest.Make_public_with_params (T) (struct
    let params =
      let open Command.Let_syntax in
      let open Fluxum.Cli_args in
      [%map_open
        let pair = string_flag ~field_name:"pair" ~doc:"Trading pair (e.g., XETHZUSD)"
        and since = string_flag_option ~field_name:"since" ~doc:"Return trades since timestamp"
        and count = int_flag_option ~field_name:"count" ~doc:"Number of trades to return"
        in
        { pair; since; count }]
  end)
end

(** Cancel all orders - POST /0/private/CancelAll *)
module Cancel_all = struct
  module T = struct
    let name = "cancel-all"
    let endpoint = "CancelAll"

    type request = unit [@@deriving sexp]

    let request_to_params () = []

    type response =
      { count : int
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(** Backwards compatibility wrappers (non-typed versions) *)

(** Get account balances - returns raw JSON *)
let balances (module Cfg : Cfg.S) () =
  Balances.post (module Cfg) () >>| function
  | `Ok balance ->
    let json =
      `Assoc (List.map balance ~f:(fun (k, v) -> (k, `String v)))
    in
    `Ok json
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

(** Get open orders *)
let open_orders ?(trades = false) (module Cfg : Cfg.S) () =
  Open_orders.post (module Cfg) { trades } >>| function
  | `Ok { open_ } ->
    let orders_json =
      `Assoc
        (List.map open_ ~f:(fun (id, _order) ->
          (* For backwards compat, return simplified version *)
          (id, `String "order")))
    in
    `Ok (`Assoc [ ("open", orders_json) ])
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

(** Add order *)
let add_order ?price ?price2 ?leverage ?oflags ?(starttm = "0") ?(expiretm = "0")
    ?timeinforce (module Cfg : Cfg.S) ~pair ~type_ ~ordertype ~volume () =
  let starttm = match String.equal starttm "0" with true -> None | false -> Some starttm in
  let expiretm = match String.equal expiretm "0" with true -> None | false -> Some expiretm in
  (* Parse string type_ and ordertype to enums *)
  let type_enum = Common.Side.of_string type_ in
  let ordertype_enum = Common.Order_type.of_string ordertype in
  Add_order.post (module Cfg)
    { pair
    ; type_ = type_enum
    ; ordertype = ordertype_enum
    ; volume
    ; price
    ; price2
    ; leverage
    ; oflags
    ; starttm
    ; expiretm
    ; timeinforce
    }
  >>| function
  | `Ok result ->
    `Ok
      (`Assoc
        [ ("txid", `List (List.map result.txid ~f:(fun s -> `String s)))
        ; ("descr", `Assoc [ ("order", `String result.descr.order)
                           ; ("close", `String result.descr.close) ])
        ])
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

(** Cancel order *)
let cancel_order (module Cfg : Cfg.S) ~txid () =
  Cancel_order.post (module Cfg) { txid } >>| function
  | `Ok result ->
    `Ok (`Assoc [ ("count", `Int result.count) ])
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

(** Cancel multiple orders *)
let cancel_orders (module Cfg : Cfg.S) ~txids () =
  (* Kraken CancelOrder accepts comma-separated txids *)
  let txid = String.concat ~sep:"," txids in
  cancel_order (module Cfg) ~txid ()

(** Query orders *)
let query_orders ?(trades = false) (module Cfg : Cfg.S) ~txids () =
  Query_orders.post (module Cfg) { txids; trades } >>| function
  | `Ok orders ->
    let orders_json =
      `Assoc
        (List.map orders ~f:(fun (id, _order) ->
          (id, `String "order")))
    in
    `Ok orders_json
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

(** Get closed orders *)
let closed_orders ?(trades = false) ?(userref = "") (module Cfg : Cfg.S) () =
  let userref =
    match String.is_empty userref with
    | true -> None
    | false -> Some (Int.of_string userref)
  in
  Closed_orders.post (module Cfg)
    { trades; userref; start = None; end_ = None; ofs = None; closetime = None }
  >>| function
  | `Ok { closed; count } ->
    let closed_json =
      `Assoc
        (List.map closed ~f:(fun (id, _order) ->
          (id, `String "order")))
    in
    `Ok (`Assoc [ ("closed", closed_json); ("count", `Int count) ])
  | #Rest.Error.post as e -> (e :> [ `Ok of Yojson.Safe.t | Rest.Error.post ])

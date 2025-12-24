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
  include Rest.Make (T)
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
  include Rest.Make (T)
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
  include Rest.Make (T)
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
  include Rest.Make (T)
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
  include Rest.Make (T)
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

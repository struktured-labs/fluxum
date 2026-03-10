(** Kalshi Prediction Markets API

    CFTC-regulated event contracts traded on Kalshi.
    API base: /trade-api/v2/
    Auth: RSA-PSS signature (KALSHI-ACCESS-KEY/TIMESTAMP/SIGNATURE headers)
    Price range: 1-99 cents ($0.01-$0.99), YES + NO = $1.00
*)

(** Base path for all Kalshi API endpoints *)
let base_path = "/trade-api/v2"

(** {1 Domain Types} *)

module Market_status = struct
  type t =
    [ `Active
    | `Closed
    | `Determined
    | `Settled
    | `Finalized
    | `Initialized
    | `Inactive
    | `Disputed
    | `Amended ]
  [@@deriving sexp, equal, compare]

  let of_string = function
    | "active" -> Some `Active
    | "closed" -> Some `Closed
    | "determined" -> Some `Determined
    | "settled" -> Some `Settled
    | "finalized" -> Some `Finalized
    | "initialized" -> Some `Initialized
    | "inactive" -> Some `Inactive
    | "disputed" -> Some `Disputed
    | "amended" -> Some `Amended
    | _ -> None

  let to_string = function
    | `Active -> "active"
    | `Closed -> "closed"
    | `Determined -> "determined"
    | `Settled -> "settled"
    | `Finalized -> "finalized"
    | `Initialized -> "initialized"
    | `Inactive -> "inactive"
    | `Disputed -> "disputed"
    | `Amended -> "amended"
end

module Side = struct
  type t =
    [ `Yes
    | `No ]
  [@@deriving sexp, equal, compare]

  let of_string = function
    | "yes" -> Some `Yes
    | "no" -> Some `No
    | _ -> None

  let to_string = function
    | `Yes -> "yes"
    | `No -> "no"
end

module Action = struct
  type t =
    [ `Buy
    | `Sell ]
  [@@deriving sexp, equal, compare]

  let of_string = function
    | "buy" -> Some `Buy
    | "sell" -> Some `Sell
    | _ -> None

  let to_string = function
    | `Buy -> "buy"
    | `Sell -> "sell"
end

(** {1 Response Types} *)

module Market = struct
  type t =
    { ticker: string
    ; event_ticker: string
    ; market_type: string
    ; title: string
    ; subtitle: string
    ; status: string
    ; yes_bid: string
    ; yes_ask: string
    ; no_bid: string
    ; no_ask: string
    ; last_price: string
    ; volume: string
    ; volume_24h: string
    ; open_interest: string
    ; result: string
    ; close_time: string option
    ; open_time: string option
    ; category: string }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key =
        List.Assoc.find assoc ~equal:String.equal key
      in
      let find_str key =
        match find key with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let find_str_d key ~default =
        Option.value (find_str key) ~default
      in
      Ok
        { ticker= find_str_d "ticker" ~default:""
        ; event_ticker= find_str_d "event_ticker" ~default:""
        ; market_type= find_str_d "market_type" ~default:"binary"
        ; title= find_str_d "title" ~default:""
        ; subtitle= find_str_d "subtitle" ~default:""
        ; status= find_str_d "status" ~default:""
        ; yes_bid= find_str_d "yes_bid_dollars" ~default:"0"
        ; yes_ask= find_str_d "yes_ask_dollars" ~default:"0"
        ; no_bid= find_str_d "no_bid_dollars" ~default:"0"
        ; no_ask= find_str_d "no_ask_dollars" ~default:"0"
        ; last_price= find_str_d "last_price_dollars" ~default:"0"
        ; volume= find_str_d "volume_fp" ~default:"0"
        ; volume_24h= find_str_d "volume_24h_fp" ~default:"0"
        ; open_interest= find_str_d "open_interest_fp" ~default:"0"
        ; result= find_str_d "result" ~default:""
        ; close_time= find_str "close_time"
        ; open_time= find_str "open_time"
        ; category= find_str_d "category" ~default:"" }
    | _ -> Error "expected JSON object for Market"
end

module Event = struct
  type t =
    { event_ticker: string
    ; series_ticker: string
    ; title: string
    ; subtitle: string
    ; category: string
    ; mutually_exclusive: bool
    ; markets: Market.t list }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key =
        List.Assoc.find assoc ~equal:String.equal key
      in
      let find_str key =
        match find key with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let find_str_d key ~default =
        Option.value (find_str key) ~default
      in
      let markets =
        match find "markets" with
        | Some (`List items) ->
          List.filter_map items ~f:(fun item ->
            match Market.of_yojson item with
            | Ok m -> Some m
            | Error e ->
              Log.Global.debug "skipping market parse error: %s" e;
              None)
        | _ -> []
      in
      let mutually_exclusive =
        match find "mutually_exclusive" with
        | Some (`Bool b) -> b
        | _ -> false
      in
      Ok
        { event_ticker= find_str_d "event_ticker" ~default:""
        ; series_ticker= find_str_d "series_ticker" ~default:""
        ; title= find_str_d "title" ~default:""
        ; subtitle= find_str_d "sub_title" ~default:""
        ; category= find_str_d "category" ~default:""
        ; mutually_exclusive
        ; markets }
    | _ -> Error "expected JSON object for Event"
end

module Trade = struct
  type t =
    { trade_id: string
    ; ticker: string
    ; count: string
    ; yes_price: string
    ; no_price: string
    ; taker_side: string
    ; created_time: string }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key =
        List.Assoc.find assoc ~equal:String.equal key
      in
      let find_str key =
        match find key with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let find_str_d key ~default =
        Option.value (find_str key) ~default
      in
      Ok
        { trade_id= find_str_d "trade_id" ~default:""
        ; ticker= find_str_d "ticker" ~default:""
        ; count= find_str_d "count_fp" ~default:"0"
        ; yes_price= find_str_d "yes_price_dollars" ~default:"0"
        ; no_price= find_str_d "no_price_dollars" ~default:"0"
        ; taker_side= find_str_d "taker_side" ~default:""
        ; created_time= find_str_d "created_time" ~default:"" }
    | _ -> Error "expected JSON object for Trade"
end

module Orderbook = struct
  type t =
    { yes: (string * string) list
    ; no: (string * string) list }
  [@@deriving sexp]

  let parse_levels items =
    List.filter_map items ~f:(function
      | `List [`String price; `String qty] -> Some (price, qty)
      | `List [price_json; qty_json] ->
        let to_s = function
          | `String s -> Some s
          | `Float f -> Some (Float.to_string f)
          | `Int i -> Some (Int.to_string i)
          | _ -> None
        in
        Option.both (to_s price_json) (to_s qty_json)
      | _ -> None)

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let ob =
        List.Assoc.find assoc ~equal:String.equal "orderbook_fp"
      in
      (match ob with
       | Some (`Assoc ob_assoc) ->
         let yes =
           match List.Assoc.find ob_assoc ~equal:String.equal "yes_dollars" with
           | Some (`List items) -> parse_levels items
           | _ -> []
         in
         let no =
           match List.Assoc.find ob_assoc ~equal:String.equal "no_dollars" with
           | Some (`List items) -> parse_levels items
           | _ -> []
         in
         Ok {yes; no}
       | _ -> Ok {yes= []; no= []})
    | _ -> Error "expected JSON object for Orderbook"
end

module Order = struct
  type t =
    { order_id: string
    ; ticker: string
    ; action: string
    ; side: string
    ; order_type: string
    ; status: string
    ; yes_price: int
    ; no_price: int
    ; quantity: int
    ; remaining_count: int
    ; filled_count: int
    ; created_time: string option
    ; updated_time: string option
    ; client_order_id: string }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key =
        List.Assoc.find assoc ~equal:String.equal key
      in
      let find_str key =
        match find key with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let find_str_d key ~default =
        Option.value (find_str key) ~default
      in
      let find_int key =
        match find key with
        | Some (`Int i) -> i
        | Some (`String s) -> (try Int.of_string s with _ -> 0)
        | _ -> 0
      in
      Ok
        { order_id= find_str_d "order_id" ~default:""
        ; ticker= find_str_d "ticker" ~default:""
        ; action= find_str_d "action" ~default:""
        ; side= find_str_d "side" ~default:"yes"
        ; order_type= find_str_d "type" ~default:"limit"
        ; status= find_str_d "status" ~default:""
        ; yes_price= find_int "yes_price"
        ; no_price= find_int "no_price"
        ; quantity= find_int "count"
        ; remaining_count= find_int "remaining_count"
        ; filled_count= find_int "filled_count"
        ; created_time= find_str "created_time"
        ; updated_time= find_str "updated_time"
        ; client_order_id= find_str_d "client_order_id" ~default:"" }
    | _ -> Error "expected JSON object for Order"
end

module Position = struct
  type t =
    { ticker: string
    ; market_exposure: int
    ; position: int
    ; resting_orders_count: int
    ; total_traded: int
    ; realized_pnl: int
    ; fees_paid: int }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key =
        List.Assoc.find assoc ~equal:String.equal key
      in
      let find_int key =
        match find key with
        | Some (`Int i) -> i
        | Some (`String s) -> (try Int.of_string s with _ -> 0)
        | _ -> 0
      in
      let find_str_d key ~default =
        match find key with
        | Some (`String s) -> s
        | _ -> default
      in
      Ok
        { ticker= find_str_d "ticker" ~default:""
        ; market_exposure= find_int "market_exposure"
        ; position= find_int "position"
        ; resting_orders_count= find_int "resting_orders_count"
        ; total_traded= find_int "total_traded"
        ; realized_pnl= find_int "realized_pnl"
        ; fees_paid= find_int "fees_paid" }
    | _ -> Error "expected JSON object for Position"
end

module Balance = struct
  type t =
    { balance: int
    ; portfolio_value: int }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find_int key =
        match List.Assoc.find assoc ~equal:String.equal key with
        | Some (`Int i) -> i
        | _ -> 0
      in
      Ok
        { balance= find_int "balance"
        ; portfolio_value= find_int "portfolio_value" }
    | _ -> Error "expected JSON object for Balance"
end

(** {1 HTTP Helpers} *)

module Error = struct
  type t =
    [ `Http of int * string
    | `Json_parse_error of string
    | `Api_error of string
    | `Not_found
    | `Unauthorized
    | `Rate_limited ]
  [@@deriving sexp]
end

let http_get ~host:_ uri =
  Cohttp_async.Client.get uri
  >>= fun (response, body) ->
  let status = Cohttp.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  Cohttp_async.Body.to_string body
  >>| fun body_str ->
  match code with
  | 200 ->
    (match Yojson.Safe.from_string body_str with
     | json -> Ok json
     | exception Yojson.Json_error msg ->
       Error (`Json_parse_error (sprintf "JSON parse error: %s" msg)))
  | 401 -> Error `Unauthorized
  | 404 -> Error `Not_found
  | 429 -> Error `Rate_limited
  | _ -> Error (`Http (code, body_str))

let http_get_public ~host path ?query () =
  let uri = Uri.make ~scheme:"https" ~host ~path ?query () in
  http_get ~host uri

let http_get_authed ~(cfg : (module Cfg.S)) path ?query () =
  let module Cfg = (val cfg : Cfg.S) in
  let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path ?query () in
  let headers = Auth.headers cfg ~http_method:"GET" ~path in
  Cohttp_async.Client.call ~headers `GET uri
  >>= fun (response, body) ->
  let status = Cohttp.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  Cohttp_async.Body.to_string body
  >>| fun body_str ->
  match code with
  | 200 ->
    (match Yojson.Safe.from_string body_str with
     | json -> Ok json
     | exception Yojson.Json_error msg ->
       Error (`Json_parse_error (sprintf "JSON parse error: %s" msg)))
  | 401 -> Error `Unauthorized
  | 404 -> Error `Not_found
  | 429 -> Error `Rate_limited
  | _ -> Error (`Http (code, body_str))

module List_markets = struct
  type response = Market.t list [@@deriving sexp]

  let get ~host ?status ?event_ticker ?series_ticker:_ ?limit ?cursor () =
    let query =
      List.filter_opt
        [ Option.map status ~f:(fun s -> ("status", [Market_status.to_string s]))
        ; Option.map event_ticker ~f:(fun e -> ("event_ticker", [e]))
        ; Option.map limit ~f:(fun n -> ("limit", [Int.to_string n]))
        ; Option.map cursor ~f:(fun c -> ("cursor", [c])) ]
    in
    let path = sprintf "%s/markets" base_path in
    http_get_public ~host path ~query ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "markets" with
         | Some (`List items) ->
           Ok (List.filter_map items ~f:(fun item ->
             match Market.of_yojson item with
             | Ok m -> Some m
             | Error e ->
               Log.Global.debug "skipping market parse error: %s" e;
               None))
         | _ -> Ok [])
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "markets"
    , Command.async
        ~summary:"List Kalshi prediction markets"
        [%map_open
          let env = Cfg.cfg_param_public
          and _status =
            flag "-status" (optional string)
              ~doc:"STRING filter by status"
          and _event_ticker =
            flag "-event" (optional string) ~doc:"STRING filter by event ticker"
          and _limit = flag "-limit" (optional int) ~doc:"INT max markets to return"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ()
            >>= function
            | Ok markets ->
              List.iter markets ~f:(fun m ->
                printf "%s [%s] %s\n" m.ticker m.status m.title);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "list markets failed" e Error.sexp_of_t] )
end

let http_post_authed ~(cfg : (module Cfg.S)) path ~body () =
  let module Cfg = (val cfg : Cfg.S) in
  let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path () in
  let headers = Auth.headers cfg ~http_method:"POST" ~path in
  let body_str = Yojson.Safe.to_string body in
  let cohttp_body = Cohttp_async.Body.of_string body_str in
  Cohttp_async.Client.post ~headers ~body:cohttp_body uri
  >>= fun (response, resp_body) ->
  let status = Cohttp.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  Cohttp_async.Body.to_string resp_body
  >>| fun resp_str ->
  match code with
  | 200 | 201 ->
    (match Yojson.Safe.from_string resp_str with
     | json -> Ok json
     | exception Yojson.Json_error msg ->
       Error (`Json_parse_error (sprintf "JSON parse error: %s" msg)))
  | 401 -> Error `Unauthorized
  | 404 -> Error `Not_found
  | 429 -> Error `Rate_limited
  | _ -> Error (`Http (code, resp_str))

let http_delete_authed ~(cfg : (module Cfg.S)) path () =
  let module Cfg = (val cfg : Cfg.S) in
  let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path () in
  let headers = Auth.headers cfg ~http_method:"DELETE" ~path in
  Cohttp_async.Client.call ~headers `DELETE uri
  >>= fun (response, body) ->
  let status = Cohttp.Response.status response in
  let code = Cohttp.Code.code_of_status status in
  Cohttp_async.Body.to_string body
  >>| fun body_str ->
  match code with
  | 200 | 204 -> Ok `Deleted
  | 401 -> Error `Unauthorized
  | 404 -> Error `Not_found
  | 429 -> Error `Rate_limited
  | _ -> Error (`Http (code, body_str))

module Place_order = struct
  type request =
    { ticker: string
    ; action: string
    ; side: string
    ; count: int
    ; order_type: string
    ; yes_price: int option
    ; no_price: int option
    ; client_order_id: string option }
  [@@deriving sexp]

  let request_to_yojson r =
    let fields =
      [ ("ticker", `String r.ticker)
      ; ("action", `String r.action)
      ; ("side", `String r.side)
      ; ("count", `Int r.count)
      ; ("type", `String r.order_type) ]
      @ List.filter_opt
          [ Option.map r.yes_price ~f:(fun p -> ("yes_price", `Int p))
          ; Option.map r.no_price ~f:(fun p -> ("no_price", `Int p))
          ; Option.map r.client_order_id ~f:(fun id -> ("client_order_id", `String id)) ]
    in
    `Assoc fields

  let post ~cfg request =
    let path = sprintf "%s/portfolio/orders" base_path in
    let body = request_to_yojson request in
    http_post_authed ~cfg path ~body ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "order" with
         | Some order_json -> Order.of_yojson order_json |> Result.map_error ~f:(fun e -> `Json_parse_error e)
         | None -> Order.of_yojson json |> Result.map_error ~f:(fun e -> `Json_parse_error e))
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "order"
    , Command.async
        ~summary:"Place a Kalshi prediction market order"
        [%map_open
          let config = Cfg.param
          and ticker = flag "-ticker" (required string) ~doc:"STRING market ticker"
          and action = flag "-action" (required string) ~doc:"STRING buy or sell"
          and side = flag "-side" (required string) ~doc:"STRING yes or no"
          and count = flag "-count" (required int) ~doc:"INT number of contracts"
          and yes_price = flag "-yes-price" (optional int) ~doc:"INT yes price in cents (1-99)"
          and order_type = flag "-type" (optional_with_default "limit" string) ~doc:"STRING order type"
          and client_order_id = flag "-client-order-id" (optional string) ~doc:"STRING optional client order ID"
          in
          fun () ->
            let cfg = Cfg.or_default config in
            let request = { ticker; action; side; count; order_type; yes_price; no_price= None; client_order_id } in
            post ~cfg request
            >>= function
            | Ok order ->
              printf "Order %s placed: %s %s %s  qty=%d  [%s]\n"
                order.order_id order.action order.side order.ticker order.quantity order.status;
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "place order failed" e Error.sexp_of_t] )
end

module Cancel_order = struct
  let delete ~cfg ~order_id () =
    let path = sprintf "%s/portfolio/orders/%s" base_path order_id in
    http_delete_authed ~cfg path ()

  let command =
    let open Command.Let_syntax in
    ( "cancel"
    , Command.async
        ~summary:"Cancel a Kalshi order"
        [%map_open
          let config = Cfg.param
          and order_id = flag "-order-id" (required string) ~doc:"STRING order ID to cancel"
          in
          fun () ->
            let cfg = Cfg.or_default config in
            delete ~cfg ~order_id ()
            >>= function
            | Ok `Deleted ->
              printf "Order %s cancelled\n" order_id;
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "cancel order failed" e Error.sexp_of_t] )
end

module Get_orders = struct
  let get ~cfg ?status ?ticker ?limit () =
    let query =
      List.filter_opt
        [ Option.map status ~f:(fun s -> ("status", [s]))
        ; Option.map ticker ~f:(fun t -> ("ticker", [t]))
        ; Option.map limit ~f:(fun n -> ("limit", [Int.to_string n])) ]
    in
    let path = sprintf "%s/portfolio/orders" base_path in
    http_get_authed ~cfg path ~query ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "orders" with
         | Some (`List items) ->
           Ok (List.filter_map items ~f:(fun item ->
             match Order.of_yojson item with
             | Ok o -> Some o
             | Error e ->
               Log.Global.debug "skipping order parse error: %s" e;
               None))
         | _ -> Ok [])
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "orders"
    , Command.async
        ~summary:"List Kalshi orders"
        [%map_open
          let config = Cfg.param
          and status = flag "-status" (optional string) ~doc:"STRING filter by status"
          and ticker = flag "-ticker" (optional string) ~doc:"STRING filter by ticker"
          and limit = flag "-limit" (optional int) ~doc:"INT max orders to return"
          in
          fun () ->
            let cfg = Cfg.or_default config in
            get ~cfg ?status ?ticker ?limit ()
            >>= function
            | Ok orders ->
              List.iter orders ~f:(fun o ->
                printf "%s  %s %s %s [%s]\n" o.order_id o.action o.side o.ticker o.status);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get orders failed" e Error.sexp_of_t] )
end

module Get_positions = struct
  let get ~cfg ?ticker ?limit () =
    let query =
      List.filter_opt
        [ Option.map ticker ~f:(fun t -> ("ticker", [t]))
        ; Option.map limit ~f:(fun n -> ("limit", [Int.to_string n])) ]
    in
    let path = sprintf "%s/portfolio/positions" base_path in
    http_get_authed ~cfg path ~query ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "market_positions" with
         | Some (`List items) ->
           Ok (List.filter_map items ~f:(fun item ->
             match Position.of_yojson item with
             | Ok p -> Some p
             | Error e ->
               Log.Global.debug "skipping position parse error: %s" e;
               None))
         | _ -> Ok [])
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "positions"
    , Command.async
        ~summary:"List Kalshi positions"
        [%map_open
          let config = Cfg.param
          and ticker = flag "-ticker" (optional string) ~doc:"STRING filter by ticker"
          in
          fun () ->
            let cfg = Cfg.or_default config in
            get ~cfg ?ticker ()
            >>= function
            | Ok positions ->
              List.iter positions ~f:(fun p ->
                printf "%s  pos=%d  exposure=%d\n" p.ticker p.position p.market_exposure);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get positions failed" e Error.sexp_of_t] )
end

module Get_balance = struct
  let get ~cfg () =
    let path = sprintf "%s/portfolio/balance" base_path in
    http_get_authed ~cfg path ()
    >>| Result.bind ~f:(fun json ->
      Balance.of_yojson json |> Result.map_error ~f:(fun e -> `Json_parse_error e))

  let command =
    let open Command.Let_syntax in
    ( "balance"
    , Command.async
        ~summary:"Get Kalshi account balance"
        [%map_open
          let config = Cfg.param in
          fun () ->
            let cfg = Cfg.or_default config in
            get ~cfg ()
            >>= function
            | Ok b ->
              printf "Balance: $%.2f  Portfolio: $%.2f\n"
                (Float.of_int b.balance /. 100.0)
                (Float.of_int b.portfolio_value /. 100.0);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get balance failed" e Error.sexp_of_t] )
end

module Get_market = struct
  let get ~host ~ticker () =
    let path = sprintf "%s/markets/%s" base_path ticker in
    http_get_public ~host path ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "market" with
         | Some market_json -> Market.of_yojson market_json |> Result.map_error ~f:(fun e -> `Json_parse_error e)
         | None -> Market.of_yojson json |> Result.map_error ~f:(fun e -> `Json_parse_error e))
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "market"
    , Command.async
        ~summary:"Get Kalshi market details"
        [%map_open
          let env = Cfg.cfg_param_public
          and ticker = flag "-ticker" (required string) ~doc:"STRING market ticker"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ~ticker ()
            >>= function
            | Ok m ->
              printf "%s [%s] %s  yes=%s/%s last=%s\n" m.ticker m.status m.title m.yes_bid m.yes_ask m.last_price;
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get market failed" e Error.sexp_of_t] )
end

module Get_event = struct
  let get ~host ~event_ticker () =
    let path = sprintf "%s/events/%s" base_path event_ticker in
    http_get_public ~host path ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "event" with
         | Some event_json -> Event.of_yojson event_json |> Result.map_error ~f:(fun e -> `Json_parse_error e)
         | None -> Event.of_yojson json |> Result.map_error ~f:(fun e -> `Json_parse_error e))
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "event"
    , Command.async
        ~summary:"Get Kalshi event details"
        [%map_open
          let env = Cfg.cfg_param_public
          and event_ticker = flag "-ticker" (required string) ~doc:"STRING event ticker"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ~event_ticker ()
            >>= function
            | Ok event ->
              printf "%s [%s] %s\n" event.event_ticker event.category event.title;
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get event failed" e Error.sexp_of_t] )
end

module List_events = struct
  let get ~host ?status ?series_ticker ?limit ?cursor () =
    let query =
      List.filter_opt
        [ Option.map status ~f:(fun s -> ("status", [s]))
        ; Option.map series_ticker ~f:(fun s -> ("series_ticker", [s]))
        ; Option.map limit ~f:(fun n -> ("limit", [Int.to_string n]))
        ; Option.map cursor ~f:(fun c -> ("cursor", [c])) ]
    in
    let path = sprintf "%s/events" base_path in
    http_get_public ~host path ~query ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "events" with
         | Some (`List items) ->
           Ok (List.filter_map items ~f:(fun item ->
             match Event.of_yojson item with
             | Ok e -> Some e
             | Error err ->
               Log.Global.debug "skipping event parse error: %s" err;
               None))
         | _ -> Ok [])
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "events"
    , Command.async
        ~summary:"List Kalshi events"
        [%map_open
          let env = Cfg.cfg_param_public
          and status = flag "-status" (optional string) ~doc:"STRING filter by status"
          and series_ticker = flag "-series" (optional string) ~doc:"STRING filter by series"
          and limit = flag "-limit" (optional int) ~doc:"INT max events"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ?status ?series_ticker ?limit ()
            >>= function
            | Ok events ->
              List.iter events ~f:(fun e ->
                printf "%s [%s] %s\n" e.event_ticker e.category e.title);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "list events failed" e Error.sexp_of_t] )
end

module Get_trades = struct
  let get ~host ~ticker ?limit ?cursor () =
    let query =
      List.filter_opt
        [ Some ("ticker", [ticker])
        ; Option.map limit ~f:(fun n -> ("limit", [Int.to_string n]))
        ; Option.map cursor ~f:(fun c -> ("cursor", [c])) ]
    in
    let path = sprintf "%s/markets/trades" base_path in
    http_get_public ~host path ~query ()
    >>| Result.bind ~f:(fun json ->
      match json with
      | `Assoc assoc ->
        (match List.Assoc.find assoc ~equal:String.equal "trades" with
         | Some (`List items) ->
           Ok (List.filter_map items ~f:(fun item ->
             match Trade.of_yojson item with
             | Ok t -> Some t
             | Error e ->
               Log.Global.debug "skipping trade parse error: %s" e;
               None))
         | _ -> Ok [])
      | _ -> Error (`Json_parse_error "expected JSON object"))

  let command =
    let open Command.Let_syntax in
    ( "trades"
    , Command.async
        ~summary:"Get recent Kalshi trades"
        [%map_open
          let env = Cfg.cfg_param_public
          and ticker = flag "-ticker" (required string) ~doc:"STRING market ticker"
          and limit = flag "-limit" (optional int) ~doc:"INT max trades"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ~ticker ?limit ()
            >>= function
            | Ok trades ->
              List.iter trades ~f:(fun t ->
                printf "%s %s yes=%s no=%s qty=%s\n" t.created_time t.ticker t.yes_price t.no_price t.count);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "get trades failed" e Error.sexp_of_t] )
end

module Get_orderbook = struct
  let get ~host ~ticker ?depth () =
    let path = sprintf "%s/markets/%s/orderbook" base_path ticker in
    let query =
      Option.value_map depth ~default:[] ~f:(fun n -> [("depth", [Int.to_string n])])
    in
    http_get_public ~host path ~query ()
    >>| Result.bind ~f:(fun json ->
      Orderbook.of_yojson json |> Result.map_error ~f:(fun e -> `Json_parse_error e))

  let command =
    let open Command.Let_syntax in
    ( "orderbook"
    , Command.async
        ~summary:"Get Kalshi market orderbook"
        [%map_open
          let env = Cfg.cfg_param_public
          and ticker = flag "-ticker" (required string) ~doc:"STRING market ticker"
          and depth = flag "-depth" (optional int) ~doc:"INT max levels"
          in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ~ticker ?depth ()
            >>= function
            | Ok ob ->
              printf "=== %s Orderbook ===\n" ticker;
              printf "--- YES ---\n";
              List.iter ob.yes ~f:(fun (p, q) -> printf "  %s  qty=%s\n" p q);
              printf "--- NO ---\n";
              List.iter ob.no ~f:(fun (p, q) -> printf "  %s  qty=%s\n" p q);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "orderbook failed" e Error.sexp_of_t] )
end

module Exchange_status = struct
  let get ~host () =
    let path = sprintf "%s/exchange/status" base_path in
    http_get_public ~host path ()

  let command =
    let open Command.Let_syntax in
    ( "status"
    , Command.async
        ~summary:"Check Kalshi exchange status"
        [%map_open
          let env = Cfg.cfg_param_public in
          fun () ->
            let host = Cfg.host ~env in
            get ~host ()
            >>= function
            | Ok json ->
              printf "%s\n" (Yojson.Safe.pretty_to_string json);
              Deferred.unit
            | Error e ->
              failwiths ~here:[%here] "status failed" e Error.sexp_of_t] )
end

let command : string * Command.t =
  ( "prediction-markets"
  , Command.group
      ~summary:"Kalshi Prediction Markets Commands"
      [ List_markets.command
      ; Get_market.command
      ; Get_event.command
      ; List_events.command
      ; Get_trades.command
      ; Get_orderbook.command
      ; Place_order.command
      ; Cancel_order.command
      ; Get_orders.command
      ; Get_positions.command
      ; Get_balance.command
      ; Exchange_status.command ] )

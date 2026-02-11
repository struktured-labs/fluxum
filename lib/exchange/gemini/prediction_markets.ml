(** Gemini Prediction Markets API

    CFTC-regulated binary event contracts (YES/NO outcomes) on real-world events.
    API base: /v1/prediction-markets/
    Auth: Same HMAC-SHA384 as spot (X-GEMINI-* headers)
    Symbol format: GEMI-{EVENT_TICKER}-{SUFFIX} (e.g., GEMI-BTC100K-YES)
*)

open Common

(** Base path for all prediction market endpoints *)
let base_path = [ "v1"; "prediction-markets" ]

(** {1 Domain Types} *)

module Event_status = struct
  module T = struct
    type t =
      [ `Active
      | `Closed
      | `Under_review
      | `Settled
      | `Invalid
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Active -> "active"
      | `Closed -> "closed"
      | `Under_review -> "under_review"
      | `Settled -> "settled"
      | `Invalid -> "invalid"
  end

  include T
  include (Json.Make (T) : Json.S with type t := t)
end

module Event_type = struct
  module T = struct
    type t =
      [ `Binary
      | `Categorical
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Binary -> "binary"
      | `Categorical -> "categorical"
  end

  include T
  include (Json.Make (T) : Json.S with type t := t)
end

module Outcome = struct
  module T = struct
    type t =
      [ `Yes
      | `No
      ]
    [@@deriving sexp, enumerate, equal, compare]

    let to_string = function
      | `Yes -> "yes"
      | `No -> "no"
  end

  include T
  include (Json.Make (T) : Json.S with type t := t)
end

module Price_history_entry = struct
  type t =
    { timestamp : string
    ; price : Decimal_string.t
    }
  [@@deriving sexp, of_yojson]
end

module Contract_prices = struct
  type side_prices =
    { yes : Decimal_string.t option [@default None]
    ; no : Decimal_string.t option [@default None]
    }
  [@@deriving sexp, of_yojson]

  type t =
    { buy : side_prices option [@default None]
    ; sell : side_prices option [@default None]
    ; best_bid : Decimal_string.t option [@default None] [@key "bestBid"]
    ; best_ask : Decimal_string.t option [@default None] [@key "bestAsk"]
    ; last_trade_price : Decimal_string.t option [@default None] [@key "lastTradePrice"]
    }
  [@@deriving sexp, of_yojson]
end

module Contract = struct
  type t =
    { id : string
    ; label : string
    ; abbreviated_name : string option [@default None] [@key "abbreviatedName"]
    ; ticker : string
    ; instrument_symbol : string [@key "instrumentSymbol"]
    ; prices : Contract_prices.t option [@default None]
    ; total_shares : Decimal_string.t [@key "totalShares"] [@default "0"]
    ; status : string
    ; color : string [@default ""]
    ; image_url : string option [@default None] [@key "imageUrl"]
    ; created_at : string option [@default None] [@key "createdAt"]
    ; expiry_date : string option [@default None] [@key "expiryDate"]
    ; effective_date : string option [@default None] [@key "effectiveDate"]
    ; resolved_at : string option [@default None] [@key "resolvedAt"]
    ; resolution_side : string option [@default None] [@key "resolutionSide"]
    ; terms_and_conditions_url : string option [@default None] [@key "termsAndConditionsUrl"]
    ; market_state : string option [@default None] [@key "marketState"]
    ; sort_order : int option [@default None] [@key "sortOrder"]
    }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    (* Remove 'description' field before parsing since it can be a rich text object *)
    let json' =
      match json with
      | `Assoc assoc ->
        `Assoc (List.Assoc.remove assoc ~equal:String.equal "description")
      | other -> other
    in
    match json' with
    | `Assoc assoc ->
      let find key = List.Assoc.find assoc ~equal:String.equal key in
      let find_str key = match find key with Some (`String s) -> Some s | _ -> None in
      let find_str_d key ~default = Option.value (find_str key) ~default in
      let find_int key = match find key with Some (`Int i) -> Some i | _ -> None in
      Result.Ok
        { id = find_str_d "id" ~default:""
        ; label = find_str_d "label" ~default:""
        ; abbreviated_name = find_str "abbreviatedName"
        ; ticker = find_str_d "ticker" ~default:""
        ; instrument_symbol = find_str_d "instrumentSymbol" ~default:""
        ; prices = (match find "prices" with
            | Some p -> (match Contract_prices.of_yojson p with Ok v -> Some v | Error _ -> None)
            | None -> None)
        ; total_shares = find_str_d "totalShares" ~default:"0"
        ; status = find_str_d "status" ~default:""
        ; color = find_str_d "color" ~default:""
        ; image_url = find_str "imageUrl"
        ; created_at = find_str "createdAt"
        ; expiry_date = find_str "expiryDate"
        ; effective_date = find_str "effectiveDate"
        ; resolved_at = find_str "resolvedAt"
        ; resolution_side = find_str "resolutionSide"
        ; terms_and_conditions_url = find_str "termsAndConditionsUrl"
        ; market_state = find_str "marketState"
        ; sort_order = find_int "sortOrder"
        }
    | _ -> Error "expected JSON object for Contract"
end

module Event = struct
  type t =
    { id : string
    ; title : string
    ; slug : string
    ; description : string
    ; image_url : string option
    ; type_ : Event_type.t
    ; category : string
    ; series : string option
    ; ticker : string
    ; status : Event_status.t
    ; created_at : string option
    ; effective_date : string option
    ; expiry_date : string option
    ; resolved_at : string option
    ; volume : Decimal_string.t
    ; liquidity : Decimal_string.t
    ; tags : string list
    ; contracts : Contract.t list
    ; is_live : bool
    }
  [@@deriving sexp]

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc assoc ->
      let find key = List.Assoc.find assoc ~equal:String.equal key in
      let find_str key = match find key with Some (`String s) -> Some s | _ -> None in
      let find_str_d key ~default = Option.value (find_str key) ~default in
      let status_str = find_str_d "status" ~default:"active" in
      let status =
        match Event_status.of_string_opt status_str with
        | Some s -> s
        | None -> `Active
      in
      let type_str = find_str_d "type" ~default:"binary" in
      let type_ =
        match Event_type.of_string_opt type_str with
        | Some t -> t
        | None -> `Binary
      in
      let tags = match find "tags" with
        | Some (`List items) ->
          List.filter_map items ~f:(function `String s -> Some s | _ -> None)
        | _ -> []
      in
      let contracts = match find "contracts" with
        | Some (`List items) ->
          List.filter_map items ~f:(fun item ->
            match Contract.of_yojson item with Ok c -> Some c | Error _ -> None)
        | _ -> []
      in
      let is_live = match find "isLive" with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let volume = match find "volume" with
        | Some (`String s) -> s
        | Some (`Int i) -> Int.to_string i
        | Some `Null | None -> "0"
        | _ -> "0"
      in
      let liquidity = match find "liquidity" with
        | Some (`String s) -> s
        | Some (`Int i) -> Int.to_string i
        | Some `Null | None -> "0"
        | _ -> "0"
      in
      Result.Ok
        { id = find_str_d "id" ~default:""
        ; title = find_str_d "title" ~default:""
        ; slug = find_str_d "slug" ~default:""
        ; description = find_str_d "description" ~default:""
        ; image_url = find_str "imageUrl"
        ; type_
        ; category = find_str_d "category" ~default:""
        ; series = find_str "series"
        ; ticker = find_str_d "ticker" ~default:""
        ; status
        ; created_at = find_str "createdAt"
        ; effective_date = find_str "effectiveDate"
        ; expiry_date = find_str "expiryDate"
        ; resolved_at = find_str "resolvedAt"
        ; volume
        ; liquidity
        ; tags
        ; contracts
        ; is_live
        }
    | _ -> Error "expected JSON object for Event"
end

module Contract_metadata = struct
  type t =
    { contract_id : string option [@default None] [@key "contractId"]
    ; contract_name : string option [@default None] [@key "contractName"]
    ; contract_ticker : string option [@default None] [@key "contractTicker"]
    ; event_ticker : string option [@default None] [@key "eventTicker"]
    ; event_name : string option [@default None] [@key "eventName"]
    ; category : string option [@default None]
    ; contract_status : string option [@default None] [@key "contractStatus"]
    ; image_url : string option [@default None] [@key "imageUrl"]
    ; expiry_date : string option [@default None] [@key "expiryDate"]
    ; resolved_at : string option [@default None] [@key "resolvedAt"]
    ; description : string option [@default None]
    }
  [@@deriving sexp, of_yojson, to_yojson]
end

(** {1 Public GET Endpoints} *)

(** Make a Cfg.S that only needs the host, no API keys (for public endpoints) *)
let public_config_of_env env =
  let host = Cfg.host ~env in
  let module M = struct
    let version = "v1"
    let api_host = host
    let api_key = ""
    let api_secret = ""
  end in
  (module M : Cfg.S)

let cfg_param_public =
  Command.Param.(
    flag "-env" (optional_with_default "production" string)
      ~doc:"STRING environment (production or sandbox, default: production)")

(** Shared HTTP GET helper following rest.ml error patterns *)
let http_get uri =
  Cohttp_async.Client.get uri >>= fun (response, body) ->
  match Cohttp.Response.status response with
  | `OK ->
    Cohttp_async.Body.to_string body >>| fun s ->
    Log.Global.debug "prediction markets response:\n %s" s;
    `Ok (Yojson.Safe.from_string s)
  | `Not_found -> return `Not_found
  | `Not_acceptable ->
    Cohttp_async.Body.to_string body >>| fun b -> `Not_acceptable b
  | `Bad_request ->
    Cohttp_async.Body.to_string body >>| fun b -> `Bad_request b
  | `Service_unavailable ->
    Cohttp_async.Body.to_string body >>| fun b -> `Service_unavailable b
  | `Unauthorized ->
    Cohttp_async.Body.to_string body >>| fun b -> `Unauthorized b
  | `Bad_gateway ->
    Cohttp_async.Body.to_string body >>| fun b -> `Bad_gateway b
  | `Gateway_timeout ->
    Cohttp_async.Body.to_string body >>| fun b -> `Gateway_timeout b
  | (code : Cohttp.Code.status_code) ->
    Cohttp_async.Body.to_string body >>| fun b ->
    let msg = sprintf "HTTP %s (body=%S)"
      (Cohttp.Code.string_of_status code) b in
    `Service_unavailable msg

let parse_json_response json of_yojson =
  match of_yojson json with
  | Result.Ok x -> `Ok x
  | Result.Error e ->
    `Json_parse_error Rest.Error.{ message = e; body = Yojson.Safe.to_string json }

module List_events = struct
  type response = Event.t list [@@deriving sexp]

  let parse_event_list items =
    let results = List.filter_map items ~f:(fun item ->
      match Event.of_yojson item with
      | Ok e -> Some e
      | Error e ->
        Log.Global.debug "skipping event parse error: %s" e;
        None)
    in
    Ok results

  let response_of_yojson json =
    match json with
    | `Assoc assoc -> (
      match List.Assoc.find assoc ~equal:String.equal "data" with
      | Some (`List items) -> parse_event_list items
      | _ -> Error "expected 'data' field in events response")
    | `List items -> parse_event_list items
    | _ -> Error "expected JSON object or array"

  let get (module Cfg : Cfg.S) ?status ?category ?search ?limit ?offset () =
    let query = List.concat
      [ (match status with
         | None -> []
         | Some statuses ->
           [ ("status[]", List.map statuses ~f:Event_status.to_string) ])
      ; (match category with
         | None -> []
         | Some categories -> [ ("category[]", categories) ])
      ; (match search with
         | None -> []
         | Some s -> [ ("search", [ s ]) ])
      ; (match limit with
         | None -> []
         | Some n -> [ ("limit", [ Int.to_string n ]) ])
      ; (match offset with
         | None -> []
         | Some n -> [ ("offset", [ Int.to_string n ]) ])
      ]
    in
    let path = String.concat ~sep:"/" (base_path @ [ "events" ]) in
    let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path ~query () in
    http_get uri >>| function
    | `Ok json -> parse_json_response json response_of_yojson
    | #Rest.Error.get as e -> e

  let command =
    let open Command.Let_syntax in
    ( "events",
      Command.async
        ~summary:"List prediction market events"
        [%map_open
          let env = cfg_param_public
          and status =
            flag "-status" (optional string)
              ~doc:"STRING filter by status (active, closed, settled, etc.)"
          and category =
            flag "-category" (optional string)
              ~doc:"STRING filter by category (crypto, economics, sports, etc.)"
          and search =
            flag "-search" (optional string)
              ~doc:"STRING search events by text"
          and limit =
            flag "-limit" (optional int) ~doc:"INT max events to return"
          and offset =
            flag "-offset" (optional int) ~doc:"INT pagination offset"
          in
          fun () ->
            let config = public_config_of_env env in
            let status =
              Option.map status ~f:(fun s ->
                String.split s ~on:','
                |> List.filter_map ~f:Event_status.of_string_opt)
            in
            let category = Option.map category ~f:(fun s ->
              String.split s ~on:',') in
            get config ?status ?category ?search ?limit ?offset ()
            >>= function
            | `Ok events ->
              List.iter events ~f:(fun e ->
                printf "%s [%s] %s (%s) - %d contracts\n"
                  e.ticker
                  (Event_status.to_string e.status)
                  e.title
                  e.category
                  (List.length e.contracts);
                List.iter e.contracts ~f:(fun c ->
                  let price_str = match c.prices with
                    | Some p -> (match p.last_trade_price with
                      | Some ltp -> ltp
                      | None -> match p.best_ask with
                        | Some ba -> ba
                        | None -> "?")
                    | None -> "?"
                  in
                  printf "  %s  price=%s  shares=%s\n"
                    c.instrument_symbol price_str c.total_shares));
              Deferred.unit
            | #Rest.Error.get as err ->
              failwiths ~here:[%here] "list events failed"
                err Rest.Error.sexp_of_get] )
end

module Get_event = struct
  type response = Event.t [@@deriving sexp]

  let get (module Cfg : Cfg.S) ~event_ticker () =
    let path = String.concat ~sep:"/"
      (base_path @ [ "events"; event_ticker ]) in
    let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path () in
    http_get uri >>| function
    | `Ok json -> parse_json_response json Event.of_yojson
    | #Rest.Error.get as e -> e

  let command =
    let open Command.Let_syntax in
    ( "event",
      Command.async
        ~summary:"Get prediction market event details"
        [%map_open
          let env = cfg_param_public
          and event_ticker =
            flag "-ticker" (required string)
              ~doc:"STRING event ticker (e.g., BTC100K)"
          in
          fun () ->
            let config = public_config_of_env env in
            get config ~event_ticker () >>= function
            | `Ok event ->
              printf "%s\n" (Sexp.to_string_hum (Event.sexp_of_t event));
              Deferred.unit
            | #Rest.Error.get as err ->
              failwiths ~here:[%here] "get event failed"
                err Rest.Error.sexp_of_get] )
end

module List_categories = struct
  type response = string list [@@deriving sexp]

  let response_of_yojson json =
    match json with
    | `Assoc assoc -> (
      match List.Assoc.find assoc ~equal:String.equal "categories" with
      | Some (`List items) ->
        Ok (List.filter_map items ~f:(function
          | `String s -> Some s
          | _ -> None))
      | _ -> Error "expected 'categories' field in response")
    | `List items ->
      Ok (List.filter_map items ~f:(function
        | `String s -> Some s
        | _ -> None))
    | _ -> Error "expected JSON object or array"

  let get (module Cfg : Cfg.S) () =
    let path = String.concat ~sep:"/"
      (base_path @ [ "categories" ]) in
    let uri = Uri.make ~scheme:"https" ~host:Cfg.api_host ~path () in
    http_get uri >>| function
    | `Ok json -> parse_json_response json response_of_yojson
    | #Rest.Error.get as e -> e

  let command =
    let open Command.Let_syntax in
    ( "categories",
      Command.async
        ~summary:"List prediction market categories"
        [%map_open
          let env = cfg_param_public in
          fun () ->
            let config = public_config_of_env env in
            get config () >>= function
            | `Ok categories ->
              List.iter categories ~f:(fun c -> printf "%s\n" c);
              Deferred.unit
            | #Rest.Error.get as err ->
              failwiths ~here:[%here] "list categories failed"
                err Rest.Error.sexp_of_get] )
end

(** {1 Authenticated POST Endpoints} *)

module Place_order = struct
  module T = struct
    let name = "order"
    let path = base_path @ [ "order" ]

    type request =
      { symbol : string
      ; order_type : string [@key "orderType"]
      ; side : Side.t
      ; quantity : Decimal_string.t
      ; price : Decimal_string.t
      ; outcome : Outcome.t
      ; time_in_force : string [@key "timeInForce"]
      }
    [@@deriving sexp, to_yojson]

    type response =
      { order_id : Int_number.t [@key "orderId"]
      ; status : string
      ; symbol : string
      ; side : Side.t
      ; outcome : Outcome.t
      ; order_type : string [@key "orderType"]
      ; quantity : Decimal_string.t
      ; filled_quantity : Decimal_string.t [@key "filledQuantity"] [@default "0"]
      ; remaining_quantity : Decimal_string.t [@key "remainingQuantity"] [@default "0"]
      ; price : Decimal_string.t
      ; avg_execution_price : Decimal_string.t option [@default None] [@key "avgExecutionPrice"]
      ; created_at : string option [@default None] [@key "createdAt"]
      ; updated_at : string option [@default None] [@key "updatedAt"]
      ; cancelled_at : string option [@default None] [@key "cancelledAt"]
      ; contract_metadata : Contract_metadata.t option [@default None] [@key "contractMetadata"]
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Post (T)

  let command =
    let open Command.Let_syntax in
    ( "order",
      Command.async
        ~summary:"Place a prediction market order"
        [%map_open
          let config = Cfg.param
          and symbol =
            flag "-symbol" (required string)
              ~doc:"STRING instrument symbol (e.g., GEMI-BTC100K-YES)"
          and side =
            flag "-side" (required string)
              ~doc:"STRING order side (buy or sell)"
          and quantity =
            flag "-quantity" (required string)
              ~doc:"STRING number of contracts"
          and price =
            flag "-price" (required string)
              ~doc:"STRING price (0.00 to 1.00)"
          and outcome =
            flag "-outcome" (required string)
              ~doc:"STRING outcome (yes or no)"
          and time_in_force =
            flag "-time-in-force" (optional_with_default "good-til-cancel" string)
              ~doc:"STRING time in force (good-til-cancel, immediate-or-cancel, fill-or-kill)"
          in
          fun () ->
            let config = Cfg.or_default config in
            let side = Side.of_string (String.lowercase side) in
            let outcome = Outcome.of_string (String.lowercase outcome) in
            let request =
              { symbol; order_type = "limit"; side; quantity
              ; price; outcome; time_in_force
              }
            in
            Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
            post config nonce request >>= function
            | `Ok response ->
              printf "Order %Ld placed: %s %s %s @ %s (%s)\n"
                response.order_id
                (Side.to_string response.side)
                response.quantity
                response.symbol
                response.price
                response.status;
              Deferred.unit
            | #Rest.Error.post as err ->
              failwiths ~here:[%here] "place order failed"
                err Rest.Error.sexp_of_post] )
end

module Cancel_order = struct
  module T = struct
    let name = "cancel"
    let path = base_path @ [ "order"; "cancel" ]

    type request =
      { order_id : Int_number.t [@key "orderId"]
      }
    [@@deriving sexp, to_yojson]

    type response =
      { order_id : Int_number.t [@key "orderId"]
      ; status : string
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Post (T)

  let command =
    let open Command.Let_syntax in
    ( "cancel",
      Command.async
        ~summary:"Cancel a prediction market order"
        [%map_open
          let config = Cfg.param
          and order_id =
            flag "-order-id" (required int)
              ~doc:"INT order ID to cancel"
          in
          fun () ->
            let config = Cfg.or_default config in
            let request = { order_id = Int64.of_int order_id } in
            Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
            post config nonce request >>= function
            | `Ok response ->
              printf "Order %Ld cancelled: %s\n"
                response.order_id response.status;
              Deferred.unit
            | #Rest.Error.post as err ->
              failwiths ~here:[%here] "cancel order failed"
                err Rest.Error.sexp_of_post] )
end

module Active_orders = struct
  module T = struct
    let name = "active-orders"
    let path = base_path @ [ "orders"; "active" ]

    type request = unit [@@deriving sexp, to_yojson]

    type response = Place_order.response list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

module Order_history = struct
  module T = struct
    let name = "order-history"
    let path = base_path @ [ "orders"; "history" ]

    type request = unit [@@deriving sexp, to_yojson]

    type response = Place_order.response list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

module Positions = struct
  type position =
    { symbol : string
    ; instrument_id : Int_number.t [@key "instrumentId"]
    ; total_quantity : Decimal_string.t [@key "totalQuantity"]
    ; avg_price : Decimal_string.t [@key "avgPrice"]
    ; outcome : Outcome.t
    ; contract_metadata : Contract_metadata.t option [@default None] [@key "contractMetadata"]
    }
  [@@deriving sexp, of_yojson]

  module T = struct
    let name = "positions"
    let path = base_path @ [ "positions" ]

    type request = unit [@@deriving sexp, to_yojson]

    type response = position list [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg (T)
end

(** {1 Order Book for Prediction Contracts} *)

module Orderbook = struct
  let command =
    let open Command.Let_syntax in
    ( "orderbook",
      Command.async
        ~summary:"Stream prediction market order book"
        [%map_open
          let config = Cfg.param
          and symbol =
            flag "-symbol" (required string)
              ~doc:"STRING instrument symbol (e.g., GEMI-BTC100K-YES)"
          and max_depth =
            flag "--max-depth" (optional_with_default 12 int)
              ~doc:"N maximum bid/ask levels to display (default: 12)"
          in
          fun () ->
            let config = Cfg.or_default config in
            Order_book.Book.pipe_for_instrument config
              ~instrument_symbol:symbol ()
            >>= Pipe.iter ~f:(function
              | `Ok book ->
                let module B = Order_book.Book in
                let module PL = Order_book.Price_level in
                printf "=== %s Prediction Order Book (Epoch: %d) ===\n"
                  (B.symbol book) (B.epoch book);
                let bid_levels = B.best_n_bids book ~n:max_depth () in
                let ask_levels = B.best_n_asks book ~n:max_depth () in
                printf "--- ASKS (lowest first) ---\n";
                List.rev ask_levels |> List.iter ~f:(fun level ->
                  printf "  %.4f  qty=%.2f\n"
                    (PL.price level) (PL.volume level));
                printf "--- BIDS (highest first) ---\n";
                List.iter bid_levels ~f:(fun level ->
                  printf "  %.4f  qty=%.2f\n"
                    (PL.price level) (PL.volume level));
                printf "Spread: %.4f\n\n" (B.spread book);
                Deferred.unit
              | #Market_data.Error.t as e ->
                printf "Error: %s\n"
                  (Sexp.to_string (Market_data.Error.sexp_of_t e));
                Deferred.unit)] )
end

(** {1 CLI Command Group} *)

let command : string * Command.t =
  ( "prediction-markets",
    Command.group
      ~summary:"Gemini Prediction Markets Commands"
      [ List_events.command
      ; Get_event.command
      ; List_categories.command
      ; Place_order.command
      ; Cancel_order.command
      ; Active_orders.command
      ; Order_history.command
      ; Positions.command
      ; Orderbook.command
      ] )

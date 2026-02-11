(** Gemini Prediction Markets API

    CFTC-regulated binary event contracts (YES/NO outcomes) on real-world events.
    API base: /v1/prediction-markets/
    Symbol format: GEMI-{EVENT_TICKER}-{SUFFIX} (e.g., GEMI-BTC100K-YES) *)

open Common

(** {1 Domain Types} *)

module Event_status : sig
  type t =
    [ `Active
    | `Closed
    | `Under_review
    | `Settled
    | `Invalid
    ]
  [@@deriving sexp, enumerate, equal, compare]

  include Json.S with type t := t
end

module Event_type : sig
  type t =
    [ `Binary
    | `Categorical
    ]
  [@@deriving sexp, enumerate, equal, compare]

  include Json.S with type t := t
end

module Outcome : sig
  type t =
    [ `Yes
    | `No
    ]
  [@@deriving sexp, enumerate, equal, compare]

  include Json.S with type t := t
end

module Price_history_entry : sig
  type t =
    { timestamp : string
    ; price : Decimal_string.t
    }
  [@@deriving sexp, of_yojson]
end

module Contract_prices : sig
  type side_prices =
    { yes : Decimal_string.t option
    ; no : Decimal_string.t option
    }
  [@@deriving sexp, of_yojson]

  type t =
    { buy : side_prices option
    ; sell : side_prices option
    ; best_bid : Decimal_string.t option
    ; best_ask : Decimal_string.t option
    ; last_trade_price : Decimal_string.t option
    }
  [@@deriving sexp, of_yojson]
end

module Contract : sig
  type t =
    { id : string
    ; label : string
    ; abbreviated_name : string option
    ; ticker : string
    ; instrument_symbol : string
    ; prices : Contract_prices.t option
    ; total_shares : Decimal_string.t
    ; status : string
    ; color : string
    ; image_url : string option
    ; created_at : string option
    ; expiry_date : string option
    ; effective_date : string option
    ; resolved_at : string option
    ; resolution_side : string option
    ; terms_and_conditions_url : string option
    ; market_state : string option
    ; sort_order : int option
    }
  [@@deriving sexp]

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Event : sig
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

  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Contract_metadata : sig
  type t =
    { contract_id : string option
    ; contract_name : string option
    ; contract_ticker : string option
    ; event_ticker : string option
    ; event_name : string option
    ; category : string option
    ; contract_status : string option
    ; image_url : string option
    ; expiry_date : string option
    ; resolved_at : string option
    ; description : string option
    }
  [@@deriving sexp, of_yojson, to_yojson]
end

(** {1 Public GET Endpoints} *)

module List_events : sig
  type response = Event.t list [@@deriving sexp]

  val get :
    (module Cfg.S) ->
    ?status:Event_status.t list ->
    ?category:string list ->
    ?search:string ->
    ?limit:int ->
    ?offset:int ->
    unit ->
    [ `Ok of response | Rest.Error.get ] Deferred.t

  val command : string * Command.t
end

module Get_event : sig
  type response = Event.t [@@deriving sexp]

  val get :
    (module Cfg.S) ->
    event_ticker:string ->
    unit ->
    [ `Ok of response | Rest.Error.get ] Deferred.t

  val command : string * Command.t
end

module List_categories : sig
  type response = string list [@@deriving sexp]

  val get :
    (module Cfg.S) ->
    unit ->
    [ `Ok of response | Rest.Error.get ] Deferred.t

  val command : string * Command.t
end

(** {1 Authenticated POST Endpoints} *)

module Place_order : sig
  type request =
    { symbol : string
    ; order_type : string
    ; side : Side.t
    ; quantity : Decimal_string.t
    ; price : Decimal_string.t
    ; outcome : Outcome.t
    ; time_in_force : string
    }
  [@@deriving sexp, to_yojson]

  type response =
    { order_id : Int_number.t
    ; status : string
    ; symbol : string
    ; side : Side.t
    ; outcome : Outcome.t
    ; order_type : string
    ; quantity : Decimal_string.t
    ; filled_quantity : Decimal_string.t
    ; remaining_quantity : Decimal_string.t
    ; price : Decimal_string.t
    ; avg_execution_price : Decimal_string.t option
    ; created_at : string option
    ; updated_at : string option
    ; cancelled_at : string option
    ; contract_metadata : Contract_metadata.t option
    }
  [@@deriving sexp, of_yojson]

  val post :
    (module Cfg.S) ->
    Nonce.reader ->
    request ->
    [ `Ok of response | Rest.Error.post ] Deferred.t

  val command : string * Command.t
end

module Cancel_order : sig
  type request =
    { order_id : Int_number.t
    }
  [@@deriving sexp, to_yojson]

  type response =
    { order_id : Int_number.t
    ; status : string
    }
  [@@deriving sexp, of_yojson]

  val post :
    (module Cfg.S) ->
    Nonce.reader ->
    request ->
    [ `Ok of response | Rest.Error.post ] Deferred.t

  val command : string * Command.t
end

module Active_orders : sig
  type request =
    { limit : int option
    ; offset : int option
    ; symbol : string option
    }
  [@@deriving sexp]

  val post :
    (module Cfg.S) ->
    Nonce.reader ->
    request ->
    [ `Ok of Place_order.response list | Rest.Error.post ] Deferred.t

  val get :
    ?limit:int ->
    ?offset:int ->
    ?symbol:string ->
    (module Cfg.S) ->
    Nonce.reader ->
    unit ->
    [ `Ok of Place_order.response list | Rest.Error.post ] Deferred.t

  val command : string * Command.t
end

module Order_history : sig
  type request =
    { limit : int option
    ; offset : int option
    ; symbol : string option
    }
  [@@deriving sexp]

  val post :
    (module Cfg.S) ->
    Nonce.reader ->
    request ->
    [ `Ok of Place_order.response list | Rest.Error.post ] Deferred.t

  val get :
    ?limit:int ->
    ?offset:int ->
    ?symbol:string ->
    (module Cfg.S) ->
    Nonce.reader ->
    unit ->
    [ `Ok of Place_order.response list | Rest.Error.post ] Deferred.t

  val command : string * Command.t
end

module Positions : sig
  type position =
    { symbol : string
    ; instrument_id : Int_number.t
    ; total_quantity : Decimal_string.t
    ; avg_price : Decimal_string.t
    ; outcome : Outcome.t
    ; contract_metadata : Contract_metadata.t option
    }
  [@@deriving sexp, of_yojson]

  val post :
    (module Cfg.S) ->
    Nonce.reader ->
    unit ->
    [ `Ok of position list | Rest.Error.post ] Deferred.t

  val command : string * Command.t
end

(** {1 Order Book for Prediction Contracts} *)

module Book_snapshot : sig
  type level =
    { price : Decimal_string.t
    ; amount : Decimal_string.t
    ; timestamp : string
    }
  [@@deriving sexp, of_yojson]

  type t =
    { bids : level list
    ; asks : level list
    }
  [@@deriving sexp, of_yojson]

  val get :
    (module Cfg.S) ->
    instrument_symbol:string ->
    ?limit:int ->
    unit ->
    [ `Ok of t | Rest.Error.get ] Deferred.t

  val command : string * Command.t
end

module Orderbook : sig
  val command : string * Command.t
end

(** {1 CLI Command Group} *)

val command : string * Command.t

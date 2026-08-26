(** Kraken Unified FIX 4.4 message codecs.

    This module deliberately separates protocol construction from transport. A
    caller can use any TLS connection while keeping sequencing, reconnect, and
    risk policy outside the wire codec. No floating-point values cross this
    interface. *)

module Codec = Exchange_common.Fix_codec
module Decimal = Codec.Decimal

type error =
  [ Codec.error
  | `Invalid_api_key
  | `Invalid_api_secret
  | `Invalid_client_order_id of string
  | `Invalid_decimal of int * string
  | `Invalid_heartbeat_interval of int
  | `Malformed_repeating_group of string
  | `Invalid_nonce of int64
  | `Invalid_request of string
  | `Invalid_sequence_number of int
  | `Invalid_sending_time of string
  | `Invalid_sender_comp_id
  | `Invalid_symbol of string
  | `Missing_required_field of int
  | `Repeating_group_count_mismatch of int * int
  | `Unexpected_message_type of string
  | `Unknown_execution_type of string
  | `Unknown_market_data_entry_type of string
  | `Unknown_market_data_update_action of string
  | `Unknown_order_status of string ]
[@@deriving sexp]

module Endpoint : sig
  type environment = Production | Uat [@@deriving sexp, equal]

  type service = Spot_market_data_l2 | Spot_trading | Spot_market_data_l3
  [@@deriving sexp, equal]

  type t [@@deriving sexp, equal]

  val create : environment:environment -> service:service -> t
  val hostname : t -> string
  val port : t -> int
  val target_comp_id : t -> string
  val environment : t -> environment
  val service : t -> service
end

module Header : sig
  type t

  val create :
    sender_comp_id:string ->
    msg_seq_num:int ->
    sending_time:string ->
    (t, error) Result.t

  val sender_comp_id : t -> string
  val msg_seq_num : t -> int
  val sending_time : t -> string
end

module Credentials : sig
  type t
  (** Secret-bearing values intentionally have no serializer or accessor. *)

  val create : api_key:string -> api_secret_base64:string -> (t, error) Result.t
end

module Auth : sig
  val password :
    credentials:Credentials.t ->
    msg_seq_num:int ->
    sender_comp_id:string ->
    nonce:int64 ->
    (string, error) Result.t
  (** Implements Kraken's exact Logon password construction. [nonce] is Unix
      epoch time in milliseconds and must be generated once, close to send time.
  *)
end

module Session : sig
  type target = Market_data | Trading [@@deriving sexp, equal]
  type cancel_on_disconnect = Cancel | Leave_open [@@deriving sexp, equal]

  val market_data_logon :
    header:Header.t ->
    heartbeat_interval:int ->
    reset_sequence_numbers:bool ->
    (string, error) Result.t

  val trading_logon :
    header:Header.t ->
    credentials:Credentials.t ->
    nonce:int64 ->
    heartbeat_interval:int ->
    reset_sequence_numbers:bool ->
    cancel_on_disconnect:cancel_on_disconnect ->
    ?client_id:int ->
    unit ->
    (string, error) Result.t

  val heartbeat :
    header:Header.t ->
    target:target ->
    ?test_request_id:string ->
    unit ->
    (string, error) Result.t

  val test_request :
    header:Header.t ->
    target:target ->
    test_request_id:string ->
    (string, error) Result.t

  val resend_request :
    header:Header.t ->
    target:target ->
    begin_sequence_number:int ->
    end_sequence_number:int ->
    (string, error) Result.t

  val logout :
    header:Header.t ->
    target:target ->
    ?text:string ->
    unit ->
    (string, error) Result.t

  val sequence_reset_gap_fill :
    header:Header.t ->
    target:target ->
    orig_sending_time:string ->
    new_sequence_number:int ->
    (string, error) Result.t
end

module Market_data : sig
  type depth =
    | Full
    | Top
    | Levels_10
    | Levels_25
    | Levels_100
    | Levels_500
    | Levels_1000
  [@@deriving sexp, equal]

  type entry = Book | Trades [@@deriving sexp, equal]
  type action = Subscribe | Unsubscribe [@@deriving sexp, equal]

  type request = {
    request_id : string;
    action : action;
    depth : depth;
    entries : entry list;
    symbols : string list;
  }
  [@@deriving sexp, equal]

  val request : header:Header.t -> request -> (string, error) Result.t

  module Entry : sig
    type update_action = New_entry | Update_entry | Delete_entry
    [@@deriving sexp, equal]

    type entry_type = Bid | Offer | Trade [@@deriving sexp, equal]
    type t

    val update_action : t -> update_action option
    val entry_type : t -> entry_type
    val id : t -> string
    val price : t -> (Decimal.t, error) Result.t
    val size : t -> (Decimal.t, error) Result.t
    val time : t -> string
    val checksum : t -> string option
  end

  val fold_entries :
    Codec.Frame.t -> init:'a -> f:('a -> Entry.t -> 'a) -> ('a, error) Result.t
  (** Folds entries in wire order. Entry strings remain slices until an accessor
      is called; prices and sizes parse directly from their source fields. *)
end

module Client_order_id : sig
  type t [@@deriving sexp, equal]
  (** Accepts Kraken Spot's positive numeric IDs (at most 18 digits) and v4
      UUID-shaped IDs. Kraken validates UUID timestamp semantics server-side;
      uniqueness remains the caller's duty. *)

  val create : string -> (t, error) Result.t
  val to_string : t -> string
end

module Order : sig
  type side = Buy | Sell [@@deriving sexp, equal]

  type order_kind = Market | Limit of { price : Decimal.t; post_only : bool }
  [@@deriving sexp, equal]

  type time_in_force = Gtc | Ioc | Fok | Gtd of string
  [@@deriving sexp, equal]

  type self_trade_prevention = Cancel_both | Cancel_newest | Cancel_oldest
  [@@deriving sexp, equal]

  type new_order = {
    client_order_id : Client_order_id.t;
    kind : order_kind;
    quantity : Decimal.t;
    side : side;
    symbol : string;
    time_in_force : time_in_force;
    self_trade_prevention : self_trade_prevention option;
  }
  [@@deriving sexp, equal]

  type cancel_target =
    | By_order_id of string
    | By_client_order_id of Client_order_id.t
    | By_both of {
        order_id : string;
        original_client_order_id : Client_order_id.t;
      }
  [@@deriving sexp, equal]

  type cancel_order = {
    client_order_id : Client_order_id.t;
    target : cancel_target;
    side : side;
    symbol : string;
  }
  [@@deriving sexp, equal]

  val new_single : header:Header.t -> new_order -> (string, error) Result.t

  val cancel_single :
    header:Header.t -> cancel_order -> (string, error) Result.t
end

module Inbound : sig
  type t =
    [ `Business_reject of Codec.Frame.t
    | `Execution_report of Codec.Frame.t
    | `Heartbeat of Codec.Frame.t
    | `Logon of Codec.Frame.t
    | `Logout of Codec.Frame.t
    | `Market_data_incremental of Codec.Frame.t
    | `Market_data_reject of Codec.Frame.t
    | `Market_data_snapshot of Codec.Frame.t
    | `Resend_request of Codec.Frame.t
    | `Sequence_reset of Codec.Frame.t
    | `Session_reject of Codec.Frame.t
    | `Test_request of Codec.Frame.t
    | `Unknown of Codec.Frame.t ]

  val classify : Codec.Frame.t -> t
end

module Execution_report : sig
  type event =
    | New_order
    | Cancel
    | Replace
    | Pending_new_order
    | Expire
    | Restated
    | Trade
    | Order_status
  [@@deriving sexp, equal]

  type status =
    | New
    | Partially_filled
    | Filled
    | Canceled
    | Replaced
    | Pending_new
    | Expired
    | Pending_replace
  [@@deriving sexp, equal]

  val event : Codec.Frame.t -> (event, error) Result.t
  val status : Codec.Frame.t -> (status, error) Result.t
  val client_order_id : Codec.Frame.t -> string option
  val order_id : Codec.Frame.t -> string option
  val symbol : Codec.Frame.t -> string option
end

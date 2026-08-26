open Core
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

let codec_error error = (error :> error)
let without_soh value = not (String.mem value Codec.soh)

let nonempty_wire_value value =
  (not (String.is_empty value)) && without_soh value

let valid_fix_timestamp value =
  match String.length value = 21 with
  | false -> false
  | true ->
      let digit_at position = Char.is_digit value.[position] in
      Char.equal value.[8] '-'
      && Char.equal value.[11] ':'
      && Char.equal value.[14] ':'
      && Char.equal value.[17] '.'
      && List.for_all
           [ 0; 1; 2; 3; 4; 5; 6; 7; 9; 10; 12; 13; 15; 16; 18; 19; 20 ]
           ~f:digit_at

let wire_target_comp_id = function
  | `Market_data -> "KRAKEN-MD"
  | `Trading -> "KRAKEN-TRD"

module Endpoint = struct
  type environment = Production | Uat [@@deriving sexp, equal]

  type service = Spot_market_data_l2 | Spot_trading | Spot_market_data_l3
  [@@deriving sexp, equal]

  type t = { environment : environment; service : service }
  [@@deriving sexp, equal]

  let create ~environment ~service = { environment; service }

  let hostname t =
    match t.environment with
    | Production -> "fix.kraken.com"
    | Uat -> "fix.uat.kraken.com"

  let port t =
    match t.service with
    | Spot_market_data_l2 -> 4000
    | Spot_trading -> 4001
    | Spot_market_data_l3 -> 4005

  let target_comp_id t =
    match t.service with
    | Spot_market_data_l2 | Spot_market_data_l3 -> "KRAKEN-MD"
    | Spot_trading -> "KRAKEN-TRD"

  let environment t = t.environment
  let service t = t.service
end

module Header = struct
  type t = { sender_comp_id : string; msg_seq_num : int; sending_time : string }

  let create ~sender_comp_id ~msg_seq_num ~sending_time =
    match nonempty_wire_value sender_comp_id with
    | false -> Error `Invalid_sender_comp_id
    | true -> (
        match msg_seq_num <= 0 with
        | true -> Error (`Invalid_sequence_number msg_seq_num)
        | false -> (
            match valid_fix_timestamp sending_time with
            | false -> Error (`Invalid_sending_time sending_time)
            | true -> Ok { sender_comp_id; msg_seq_num; sending_time }))

  let sender_comp_id t = t.sender_comp_id
  let msg_seq_num t = t.msg_seq_num
  let sending_time t = t.sending_time
end

let encode ~header ~target_comp_id ~msg_type ~body_fields =
  Codec.Encoder.message
    ~sender_comp_id:(Header.sender_comp_id header)
    ~target_comp_id ~msg_type
    ~msg_seq_num:(Header.msg_seq_num header)
    ~sending_time:(Header.sending_time header)
    ~body_fields
  |> Result.map_error ~f:codec_error

let encode_poss_dup ~header ~target_comp_id ~msg_type ~orig_sending_time
    ~body_fields =
  Codec.Encoder.message_poss_dup
    ~sender_comp_id:(Header.sender_comp_id header)
    ~target_comp_id ~msg_type
    ~msg_seq_num:(Header.msg_seq_num header)
    ~sending_time:(Header.sending_time header)
    ~orig_sending_time ~body_fields
  |> Result.map_error ~f:codec_error

module Credentials = struct
  type t = { api_key : string; api_secret : string }

  let create ~api_key ~api_secret_base64 =
    match nonempty_wire_value api_key with
    | false -> Error `Invalid_api_key
    | true -> (
        match Signature.base64_decode api_secret_base64 with
        | Error _ -> Error `Invalid_api_secret
        | Ok api_secret -> (
            match String.is_empty api_secret with
            | true -> Error `Invalid_api_secret
            | false -> Ok { api_key; api_secret }))
end

module Auth = struct
  let password ~credentials ~msg_seq_num ~sender_comp_id ~nonce =
    match msg_seq_num <= 0 with
    | true -> Error (`Invalid_sequence_number msg_seq_num)
    | false -> (
        match nonempty_wire_value sender_comp_id with
        | false -> Error `Invalid_sender_comp_id
        | true -> (
            match Int64.(nonce < 0L) with
            | true -> Error (`Invalid_nonce nonce)
            | false ->
                let separator = String.make 1 Codec.soh in
                let message_input =
                  String.concat
                    [
                      "35=A";
                      separator;
                      "34=";
                      Int.to_string msg_seq_num;
                      separator;
                      "49=";
                      sender_comp_id;
                      separator;
                      "56=KRAKEN-TRD";
                      separator;
                      "553=";
                      credentials.Credentials.api_key;
                      separator;
                    ]
                in
                let digest =
                  Digestif.SHA256.digest_string
                    (message_input ^ Int64.to_string nonce)
                  |> Digestif.SHA256.to_raw_string
                in
                Signature.hmac_sha512 ~secret:credentials.Credentials.api_secret
                  ~message:digest
                |> Signature.base64_encode |> Result.return))
end

module Session = struct
  type target = Market_data | Trading [@@deriving sexp, equal]
  type cancel_on_disconnect = Cancel | Leave_open [@@deriving sexp, equal]

  let target_comp_id = function
    | Market_data -> wire_target_comp_id `Market_data
    | Trading -> wire_target_comp_id `Trading

  let valid_heartbeat heartbeat_interval =
    match heartbeat_interval > 0 with
    | true -> Ok ()
    | false -> Error (`Invalid_heartbeat_interval heartbeat_interval)

  let market_data_logon ~header ~heartbeat_interval ~reset_sequence_numbers =
    let open Result.Let_syntax in
    let%bind () = valid_heartbeat heartbeat_interval in
    encode ~header
      ~target_comp_id:(target_comp_id Market_data)
      ~msg_type:"A"
      ~body_fields:
        [
          (98, "0");
          (108, Int.to_string heartbeat_interval);
          (141, match reset_sequence_numbers with true -> "Y" | false -> "N");
        ]

  let trading_logon ~header ~credentials ~nonce ~heartbeat_interval
      ~reset_sequence_numbers ~cancel_on_disconnect ?client_id () =
    let open Result.Let_syntax in
    let%bind () = valid_heartbeat heartbeat_interval in
    let%bind () =
      match client_id with
      | Some id when id < 0 ->
          Error (`Invalid_request "ClientID must be non-negative")
      | Some _ | None -> Ok ()
    in
    let%bind password =
      Auth.password ~credentials
        ~msg_seq_num:(Header.msg_seq_num header)
        ~sender_comp_id:(Header.sender_comp_id header)
        ~nonce
    in
    let authentication =
      [
        (553, credentials.Credentials.api_key);
        (554, password);
        (5025, Int64.to_string nonce);
      ]
    in
    let client =
      Option.to_list
        (Option.map client_id ~f:(fun id -> (109, Int.to_string id)))
    in
    let body_fields =
      [ (98, "0"); (108, Int.to_string heartbeat_interval) ]
      @ authentication @ client
      @ [
          (141, match reset_sequence_numbers with true -> "Y" | false -> "N");
          ( 8674,
            match cancel_on_disconnect with Cancel -> "0" | Leave_open -> "1" );
        ]
    in
    encode ~header ~target_comp_id:(target_comp_id Trading) ~msg_type:"A"
      ~body_fields

  let heartbeat ~header ~target ?test_request_id () =
    match test_request_id with
    | Some value -> (
        match nonempty_wire_value value with
        | false -> Error (`Invalid_request "TestReqID must be non-empty")
        | true ->
            encode ~header ~target_comp_id:(target_comp_id target) ~msg_type:"0"
              ~body_fields:[ (112, value) ])
    | None ->
        encode ~header ~target_comp_id:(target_comp_id target) ~msg_type:"0"
          ~body_fields:[]

  let test_request ~header ~target ~test_request_id =
    match nonempty_wire_value test_request_id with
    | false -> Error (`Invalid_request "TestReqID must be non-empty")
    | true ->
        encode ~header ~target_comp_id:(target_comp_id target) ~msg_type:"1"
          ~body_fields:[ (112, test_request_id) ]

  let resend_request ~header ~target ~begin_sequence_number ~end_sequence_number
      =
    let valid_end =
      match end_sequence_number with
      | 0 -> true
      | explicit -> explicit >= begin_sequence_number
    in
    match (begin_sequence_number > 0, valid_end) with
    | false, _ | true, false ->
        Error (`Invalid_request "invalid resend sequence range")
    | true, true ->
        encode ~header ~target_comp_id:(target_comp_id target) ~msg_type:"2"
          ~body_fields:
            [
              (7, Int.to_string begin_sequence_number);
              (16, Int.to_string end_sequence_number);
            ]

  let logout ~header ~target ?text () =
    let body_fields =
      Option.to_list (Option.map text ~f:(fun value -> (58, value)))
    in
    encode ~header ~target_comp_id:(target_comp_id target) ~msg_type:"5"
      ~body_fields

  let sequence_reset_gap_fill ~header ~target ~orig_sending_time
      ~new_sequence_number =
    match
      ( valid_fix_timestamp orig_sending_time,
        new_sequence_number > Header.msg_seq_num header )
    with
    | false, _ -> Error (`Invalid_sending_time orig_sending_time)
    | true, false -> Error (`Invalid_sequence_number new_sequence_number)
    | true, true ->
        encode_poss_dup ~orig_sending_time ~header
          ~target_comp_id:(target_comp_id target) ~msg_type:"4"
          ~body_fields:[ (123, "Y"); (36, Int.to_string new_sequence_number) ]
end

module Market_data = struct
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

  let depth_value = function
    | Full -> "0"
    | Top -> "1"
    | Levels_10 -> "10"
    | Levels_25 -> "25"
    | Levels_100 -> "100"
    | Levels_500 -> "500"
    | Levels_1000 -> "1000"

  let action_value = function Subscribe -> "1" | Unsubscribe -> "2"

  let valid_symbol symbol =
    match String.split symbol ~on:'/' with
    | [ base; quote ] -> nonempty_wire_value base && nonempty_wire_value quote
    | _ -> false

  let entry_values entries =
    let has_book = List.mem entries Book ~equal:equal_entry in
    let has_trades = List.mem entries Trades ~equal:equal_entry in
    match (has_book, has_trades) with
    | false, false -> []
    | true, false -> [ "0"; "1" ]
    | false, true -> [ "2" ]
    | true, true -> [ "0"; "1"; "2" ]

  let request ~header request =
    match nonempty_wire_value request.request_id with
    | false -> Error (`Invalid_request "MDReqID must be non-empty")
    | true -> (
        let entries = entry_values request.entries in
        match List.is_empty entries with
        | true ->
            Error
              (`Invalid_request "at least one market-data entry is required")
        | false -> (
            match request.symbols with
            | [] -> Error (`Invalid_request "at least one symbol is required")
            | symbols -> (
                match
                  List.find symbols ~f:(fun symbol -> not (valid_symbol symbol))
                with
                | Some symbol -> Error (`Invalid_symbol symbol)
                | None ->
                    let entry_fields =
                      List.map entries ~f:(fun entry -> (269, entry))
                    in
                    let symbol_fields =
                      List.map symbols ~f:(fun symbol -> (55, symbol))
                    in
                    let body_fields =
                      [
                        (262, request.request_id);
                        (263, action_value request.action);
                        (264, depth_value request.depth);
                        (265, "1");
                        (266, "Y");
                        (267, Int.to_string (List.length entries));
                      ]
                      @ entry_fields
                      @ (146, Int.to_string (List.length symbols))
                        :: symbol_fields
                    in
                    encode ~header
                      ~target_comp_id:(wire_target_comp_id `Market_data)
                      ~msg_type:"V" ~body_fields)))

  module Entry = struct
    type update_action = New_entry | Update_entry | Delete_entry
    [@@deriving sexp, equal]

    type entry_type = Bid | Offer | Trade [@@deriving sexp, equal]

    type t = {
      message : string;
      update_action : update_action option;
      entry_type : entry_type;
      id : Codec.Field.t;
      price : Codec.Field.t;
      size : Codec.Field.t;
      time : Codec.Field.t;
      checksum : Codec.Field.t option;
    }

    let update_action t = t.update_action
    let entry_type t = t.entry_type
    let id t = Codec.Field.value ~message:t.message t.id

    let decimal t tag field =
      Codec.Decimal.of_field ~message:t.message field
      |> Result.map_error ~f:(fun error -> `Invalid_decimal (tag, error))

    let price t = decimal t 270 t.price
    let size t = decimal t 271 t.size
    let time t = Codec.Field.value ~message:t.message t.time

    let checksum t =
      Option.map t.checksum ~f:(Codec.Field.value ~message:t.message)
  end

  let field_between fields ~start ~limit tag =
    let rec loop index =
      match index >= limit with
      | true -> None
      | false -> (
          match Codec.Field.tag fields.(index) = tag with
          | true -> Some fields.(index)
          | false -> loop (index + 1))
    in
    loop start

  let required_between fields ~start ~limit tag =
    match field_between fields ~start ~limit tag with
    | Some field -> Ok field
    | None -> Error (`Missing_required_field tag)

  let value_of_field message field = Codec.Field.value ~message field

  let entry_type ~message field =
    match
      ( Codec.Field.value_equal ~message field "0",
        Codec.Field.value_equal ~message field "1",
        Codec.Field.value_equal ~message field "2" )
    with
    | true, false, false -> Ok Entry.Bid
    | false, true, false -> Ok Entry.Offer
    | false, false, true -> Ok Entry.Trade
    | _ ->
        Error (`Unknown_market_data_entry_type (value_of_field message field))

  let update_action ~message field =
    match
      ( Codec.Field.value_equal ~message field "0",
        Codec.Field.value_equal ~message field "1",
        Codec.Field.value_equal ~message field "2" )
    with
    | true, false, false -> Ok Entry.New_entry
    | false, true, false -> Ok Entry.Update_entry
    | false, false, true -> Ok Entry.Delete_entry
    | _ ->
        Error
          (`Unknown_market_data_update_action (value_of_field message field))

  let group_limit fields ~start ~delimiter =
    let rec loop index =
      match index >= Array.length fields with
      | true -> index
      | false -> (
          match Codec.Field.tag fields.(index) with
          | 10 -> index
          | tag when tag = delimiter -> index
          | _ -> loop (index + 1))
    in
    loop (start + 1)

  let parse_entry ~message ~fields ~start ~limit ~incremental =
    let open Result.Let_syntax in
    let%bind action =
      match incremental with
      | false -> Ok None
      | true ->
          let%bind field = required_between fields ~start ~limit 279 in
          update_action ~message field |> Result.map ~f:Option.some
    in
    let%bind entry_type_field = required_between fields ~start ~limit 269 in
    let%bind parsed_entry_type = entry_type ~message entry_type_field in
    let%bind id_field = required_between fields ~start ~limit 278 in
    let%bind price_field = required_between fields ~start ~limit 270 in
    let%bind size_field = required_between fields ~start ~limit 271 in
    let%bind time_field = required_between fields ~start ~limit 273 in
    let checksum_field = field_between fields ~start ~limit 5041 in
    Ok
      Entry.
        {
          message;
          update_action = action;
          entry_type = parsed_entry_type;
          id = id_field;
          price = price_field;
          size = size_field;
          time = time_field;
          checksum = checksum_field;
        }

  let fold_entries frame ~init ~f =
    let open Result.Let_syntax in
    let message = Codec.Frame.raw frame in
    let fields = Codec.Frame.fields frame in
    let msg_type = Codec.Frame.msg_type frame in
    let%bind incremental, delimiter =
      match msg_type with
      | "W" -> Ok (false, 269)
      | "X" -> Ok (true, 279)
      | other -> Error (`Unexpected_message_type other)
    in
    let%bind count_index, count_field =
      match
        Array.find_mapi fields ~f:(fun index field ->
            match Codec.Field.tag field = 268 with
            | true -> Some (index, field)
            | false -> None)
      with
      | Some located -> Ok located
      | None -> Error (`Missing_required_field 268)
    in
    let%bind declared =
      Codec.Field.int_value ~message count_field
      |> Result.map_error ~f:codec_error
    in
    let rec loop index parsed accumulator =
      match parsed = declared with
      | true -> (
          match
            index < Array.length fields
            && Codec.Field.tag fields.(index) = delimiter
          with
          | true ->
              Error (`Repeating_group_count_mismatch (declared, parsed + 1))
          | false -> Ok accumulator)
      | false -> (
          match index >= Array.length fields with
          | true -> Error (`Repeating_group_count_mismatch (declared, parsed))
          | false -> (
              match Codec.Field.tag fields.(index) with
              | 10 -> Error (`Repeating_group_count_mismatch (declared, parsed))
              | tag when tag <> delimiter ->
                  Error
                    (`Malformed_repeating_group
                       (sprintf "expected group delimiter tag %d, got %d"
                          delimiter tag))
              | _ ->
                  let limit = group_limit fields ~start:index ~delimiter in
                  let%bind entry =
                    parse_entry ~message ~fields ~start:index ~limit
                      ~incremental
                  in
                  loop limit (parsed + 1) (f accumulator entry)))
    in
    loop (count_index + 1) 0 init
end

module Client_order_id = struct
  type t = string [@@deriving sexp, equal]

  let numeric value =
    let length = String.length value in
    length > 0 && length <= 18
    && (not (Char.equal value.[0] '0'))
    && String.for_all value ~f:Char.is_digit

  let uuid value =
    let hyphen position = Char.equal value.[position] '-' in
    let uuid_char position char =
      match List.mem [ 8; 13; 18; 23 ] position ~equal:Int.equal with
      | true -> Char.equal char '-'
      | false -> Char.is_hex_digit char
    in
    match String.length value = 36 with
    | false -> false
    | true ->
        hyphen 8 && hyphen 13 && hyphen 18 && hyphen 23
        && Char.equal value.[14] '4'
        && String.mem "89aAbB" value.[19]
        && String.for_alli value ~f:uuid_char

  let create value =
    match without_soh value && (numeric value || uuid value) with
    | true -> Ok value
    | false -> Error (`Invalid_client_order_id value)

  let to_string t = t
end

module Order = struct
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

  let side_value = function Buy -> "1" | Sell -> "2"

  let valid_symbol symbol =
    match String.split symbol ~on:'/' with
    | [ base; quote ] -> nonempty_wire_value base && nonempty_wire_value quote
    | _ -> false

  let decimal_field tag decimal =
    match Int64.(Decimal.mantissa decimal > 0L) with
    | true -> Ok (tag, Decimal.to_string decimal)
    | false ->
        Error
          (`Invalid_decimal (tag, Sexp.to_string (Decimal.sexp_of_t decimal)))

  let valid_expire_time value =
    match String.length value = 17 with
    | false -> false
    | true ->
        Char.equal value.[8] '-'
        && Char.equal value.[11] ':'
        && Char.equal value.[14] ':'
        && List.for_all [ 0; 1; 2; 3; 4; 5; 6; 7; 9; 10; 12; 13; 15; 16 ]
             ~f:(fun position -> Char.is_digit value.[position])

  let time_in_force_fields = function
    | Gtc -> Ok [ (59, "1") ]
    | Ioc -> Ok [ (59, "3") ]
    | Fok -> Ok [ (59, "4") ]
    | Gtd expire_time -> (
        match valid_expire_time expire_time with
        | true -> Ok [ (59, "6"); (126, expire_time) ]
        | false ->
            Error (`Invalid_request "GTD ExpireTime must be YYYYMMDD-HH:MM:SS"))

  let stp_field = function
    | Cancel_both -> (7928, "0")
    | Cancel_newest -> (7928, "1")
    | Cancel_oldest -> (7928, "2")

  let new_single ~header order =
    let open Result.Let_syntax in
    let%bind quantity = decimal_field 38 order.quantity in
    let%bind tif_fields = time_in_force_fields order.time_in_force in
    let%bind kind_fields, execution_fields =
      match order.kind with
      | Market -> Ok ([ (40, "1") ], [])
      | Limit { price; post_only } -> (
          let%map price = decimal_field 44 price in
          ( [ (40, "2"); price ],
            match post_only with true -> [ (18, "P") ] | false -> [] ))
    in
    match valid_symbol order.symbol with
    | false -> Error (`Invalid_symbol order.symbol)
    | true ->
        let stp_fields =
          Option.to_list (Option.map order.self_trade_prevention ~f:stp_field)
        in
        let body_fields =
          [ (11, Client_order_id.to_string order.client_order_id) ]
          @ kind_fields
          @ [ quantity; (54, side_value order.side); (55, order.symbol) ]
          @ tif_fields
          @ [ (60, Header.sending_time header) ]
          @ execution_fields @ stp_fields
        in
        encode ~header
          ~target_comp_id:(wire_target_comp_id `Trading)
          ~msg_type:"D" ~body_fields

  let cancel_target_fields = function
    | By_order_id order_id -> (
        match nonempty_wire_value order_id with
        | true -> Ok [ (37, order_id) ]
        | false -> Error (`Invalid_request "OrderID must be non-empty"))
    | By_client_order_id client_order_id ->
        Ok [ (41, Client_order_id.to_string client_order_id) ]
    | By_both { order_id; original_client_order_id } -> (
        match nonempty_wire_value order_id with
        | true ->
            Ok
              [
                (37, order_id);
                (41, Client_order_id.to_string original_client_order_id);
              ]
        | false -> Error (`Invalid_request "OrderID must be non-empty"))

  let cancel_single ~header order =
    let open Result.Let_syntax in
    let%bind target_fields = cancel_target_fields order.target in
    match valid_symbol order.symbol with
    | false -> Error (`Invalid_symbol order.symbol)
    | true ->
        let body_fields =
          [ (11, Client_order_id.to_string order.client_order_id) ]
          @ target_fields
          @ [
              (54, side_value order.side);
              (55, order.symbol);
              (60, Header.sending_time header);
            ]
        in
        encode ~header
          ~target_comp_id:(wire_target_comp_id `Trading)
          ~msg_type:"F" ~body_fields
end

module Inbound = struct
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

  let classify frame =
    match Codec.Frame.msg_type frame with
    | "A" -> `Logon frame
    | "0" -> `Heartbeat frame
    | "1" -> `Test_request frame
    | "2" -> `Resend_request frame
    | "3" -> `Session_reject frame
    | "4" -> `Sequence_reset frame
    | "5" -> `Logout frame
    | "8" -> `Execution_report frame
    | "W" -> `Market_data_snapshot frame
    | "X" -> `Market_data_incremental frame
    | "Y" -> `Market_data_reject frame
    | "j" -> `Business_reject frame
    | _ -> `Unknown frame
end

module Execution_report = struct
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

  let field frame tag =
    match Codec.Frame.find frame tag with
    | Some field -> Ok field
    | None -> Error (`Missing_required_field tag)

  let execution_frame frame =
    match String.equal (Codec.Frame.msg_type frame) "8" with
    | true -> Ok ()
    | false -> Error (`Unexpected_message_type (Codec.Frame.msg_type frame))

  let event frame : (event, error) Result.t =
    let open Result.Let_syntax in
    let%bind () = execution_frame frame in
    let%bind field = field frame 150 in
    let message = Codec.Frame.raw frame in
    let value = Codec.Field.value ~message field in
    match value with
    | "0" -> Ok New_order
    | "4" -> Ok Cancel
    | "5" -> Ok Replace
    | "A" -> Ok Pending_new_order
    | "C" -> Ok Expire
    | "D" -> Ok Restated
    | "F" -> Ok Trade
    | "I" -> Ok Order_status
    | unknown -> Error (`Unknown_execution_type unknown)

  let status frame : (status, error) Result.t =
    let open Result.Let_syntax in
    let%bind () = execution_frame frame in
    let%bind field = field frame 39 in
    let message = Codec.Frame.raw frame in
    let value = Codec.Field.value ~message field in
    match value with
    | "0" -> Ok New
    | "1" -> Ok Partially_filled
    | "2" -> Ok Filled
    | "4" -> Ok Canceled
    | "5" -> Ok Replaced
    | "A" -> Ok Pending_new
    | "C" -> Ok Expired
    | "E" -> Ok Pending_replace
    | unknown -> Error (`Unknown_order_status unknown)

  let client_order_id frame = Codec.Frame.value frame 11
  let order_id frame = Codec.Frame.value frame 37
  let symbol frame = Codec.Frame.value frame 55
end

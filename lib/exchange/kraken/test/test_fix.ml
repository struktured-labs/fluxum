open Core
module Fix = Kraken.Fix

let fail_error error = failwith (Sexp.to_string_hum (Fix.sexp_of_error error))
let or_fail = function Ok value -> value | Error error -> fail_error error

let decimal value =
  match Fix.Decimal.of_string value with
  | Ok value -> value
  | Error error -> failwith error

let field_values frame tag =
  Fix.Codec.Frame.find_all frame tag
  |> List.map ~f:(Fix.Codec.Field.value ~message:(Fix.Codec.Frame.raw frame))

let header sequence =
  Fix.Header.create ~sender_comp_id:"CLIENT" ~msg_seq_num:sequence
    ~sending_time:"20260824-12:34:56.123"
  |> or_fail

let test_endpoints () =
  let production =
    Fix.Endpoint.create ~environment:Fix.Endpoint.Production
      ~service:Fix.Endpoint.Spot_trading
  in
  let uat =
    Fix.Endpoint.create ~environment:Fix.Endpoint.Uat
      ~service:Fix.Endpoint.Spot_market_data_l2
  in
  assert (String.equal (Fix.Endpoint.hostname production) "fix.kraken.com");
  assert (Fix.Endpoint.port production = 4001);
  assert (String.equal (Fix.Endpoint.target_comp_id production) "KRAKEN-TRD");
  assert (String.equal (Fix.Endpoint.hostname uat) "fix.uat.kraken.com");
  assert (Fix.Endpoint.port uat = 4000)

let test_authentication_vector () =
  let credentials =
    Fix.Credentials.create ~api_key:"APIKEY" ~api_secret_base64:"c2VjcmV0"
    |> or_fail
  in
  let password =
    Fix.Auth.password ~credentials ~msg_seq_num:7 ~sender_comp_id:"CLIENT"
      ~nonce:1724512345678L
    |> or_fail
  in
  assert (
    String.equal password
      "MwX+M7nvuHXxD5aGI/TFpghXxg2iblxXYyz7lRzLCPMA4UJYI3osNrUKWCiI3fDnOLkEQ5WKSK58Mp3nvu1Klw==");
  (match
     Fix.Auth.password ~credentials ~msg_seq_num:0 ~sender_comp_id:"CLIENT"
       ~nonce:1724512345678L
   with
  | Error (`Invalid_sequence_number 0) -> ()
  | _ -> failwith "zero MsgSeqNum authentication input was accepted");
  let raw =
    Fix.Session.trading_logon ~header:(header 7) ~credentials
      ~nonce:1724512345678L ~heartbeat_interval:60 ~reset_sequence_numbers:true
      ~cancel_on_disconnect:Fix.Session.Cancel ()
    |> or_fail
  in
  let frame =
    Fix.Codec.Frame.decode raw
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  assert (String.equal (Fix.Codec.Frame.msg_type frame) "A");
  assert (
    Option.equal String.equal
      (Fix.Codec.Frame.value frame 56)
      (Some "KRAKEN-TRD"));
  assert (
    Option.equal String.equal (Fix.Codec.Frame.value frame 553) (Some "APIKEY"));
  assert (
    Option.equal String.equal (Fix.Codec.Frame.value frame 554) (Some password));
  assert (
    Option.equal String.equal (Fix.Codec.Frame.value frame 8674) (Some "0"))

let test_sequence_reset_gap_fill () =
  let raw =
    Fix.Session.sequence_reset_gap_fill ~header:(header 7) ~target:Market_data
      ~orig_sending_time:"20260824-12:34:00.000" ~new_sequence_number:10
    |> or_fail
  in
  let frame =
    Fix.Codec.Frame.decode raw
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  assert (String.equal (Fix.Codec.Frame.msg_type frame) "4");
  assert (Fix.Codec.Frame.sequence_number frame = 7);
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 43) (Some "Y"));
  assert (
    Option.equal String.equal
      (Fix.Codec.Frame.value frame 122)
      (Some "20260824-12:34:00.000"));
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 123) (Some "Y"));
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 36) (Some "10"));
  (match
     Fix.Session.sequence_reset_gap_fill ~header:(header 7) ~target:Market_data
       ~orig_sending_time:"20260824-12:34:00.000" ~new_sequence_number:7
   with
  | Error (`Invalid_sequence_number 7) -> ()
  | _ -> failwith "non-advancing gap fill was accepted");
  match
    Fix.Session.sequence_reset_gap_fill ~header:(header 7) ~target:Market_data
      ~orig_sending_time:"bad-time" ~new_sequence_number:10
  with
  | Error (`Invalid_sending_time "bad-time") -> ()
  | _ -> failwith "invalid OrigSendingTime was accepted"

let test_resend_request_range () =
  let open Fix.Session in
  let open Result.Let_syntax in
  let valid =
    let%bind open_ended =
      resend_request ~header:(header 8) ~target:Market_data
        ~begin_sequence_number:10 ~end_sequence_number:0
    in
    let%map explicit =
      resend_request ~header:(header 9) ~target:Market_data
        ~begin_sequence_number:10 ~end_sequence_number:12
    in
    (open_ended, explicit)
  in
  (match valid with
  | Ok _ -> ()
  | Error error -> fail_error error);
  match
    resend_request ~header:(header 10) ~target:Market_data
      ~begin_sequence_number:10 ~end_sequence_number:5
  with
  | Error (`Invalid_request _) -> ()
  | _ -> failwith "descending resend range was accepted"

let test_heartbeat_test_request_id () =
  (match
     Fix.Session.heartbeat ~header:(header 11) ~target:Market_data
       ~test_request_id:"echo-me" ()
   with
  | Ok raw ->
      let frame =
        Fix.Codec.Frame.decode raw
        |> Result.map_error ~f:(fun error -> (error :> Fix.error))
        |> or_fail
      in
      assert (
        Option.equal String.equal
          (Fix.Codec.Frame.value frame 112)
          (Some "echo-me"))
  | Error error -> fail_error error);
  match
    Fix.Session.heartbeat ~header:(header 12) ~target:Market_data
      ~test_request_id:"" ()
  with
  | Error (`Invalid_request _) -> ()
  | _ -> failwith "empty Heartbeat TestReqID was accepted"

let test_market_data_request () =
  let raw =
    Fix.Market_data.request ~header:(header 2)
      {
        request_id = "MDSUB1";
        action = Subscribe;
        depth = Levels_10;
        entries = [ Book; Trades ];
        symbols = [ "BTC/USD"; "ETH/USD" ];
      }
    |> or_fail
  in
  let frame =
    Fix.Codec.Frame.decode raw
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  assert (String.equal (Fix.Codec.Frame.msg_type frame) "V");
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 267) (Some "3"));
  assert (List.equal String.equal (field_values frame 269) [ "0"; "1"; "2" ]);
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 146) (Some "2"));
  assert (
    List.equal String.equal (field_values frame 55) [ "BTC/USD"; "ETH/USD" ])

let test_order_and_cancel () =
  let client_order_id =
    Fix.Client_order_id.create "1744036325000000" |> or_fail
  in
  let raw =
    Fix.Order.new_single ~header:(header 3)
      {
        client_order_id;
        kind = Limit { price = decimal "84000.00"; post_only = true };
        quantity = decimal "0.00100000";
        side = Buy;
        symbol = "BTC/USD";
        time_in_force = Gtc;
        self_trade_prevention = Some Cancel_newest;
      }
    |> or_fail
  in
  let frame =
    Fix.Codec.Frame.decode raw
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  assert (String.equal (Fix.Codec.Frame.msg_type frame) "D");
  assert (
    Option.equal String.equal
      (Fix.Codec.Frame.value frame 38)
      (Some "0.00100000"));
  assert (
    Option.equal String.equal (Fix.Codec.Frame.value frame 44) (Some "84000.00"));
  assert (Option.equal String.equal (Fix.Codec.Frame.value frame 18) (Some "P"));
  assert (
    Option.equal String.equal (Fix.Codec.Frame.value frame 7928) (Some "1"));
  let cancel_id = Fix.Client_order_id.create "1744036325300000" |> or_fail in
  let cancel =
    Fix.Order.cancel_single ~header:(header 4)
      {
        client_order_id = cancel_id;
        target =
          By_both
            {
              order_id = "OQNCZM-NVAVC-AVD2LO";
              original_client_order_id = client_order_id;
            };
        side = Buy;
        symbol = "BTC/USD";
      }
    |> or_fail
  in
  let cancel_frame =
    Fix.Codec.Frame.decode cancel
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  assert (String.equal (Fix.Codec.Frame.msg_type cancel_frame) "F");
  assert (
    Option.equal String.equal
      (Fix.Codec.Frame.value cancel_frame 41)
      (Some "1744036325000000"))

let test_published_execution_report () =
  let published =
    "8=FIX.4.4|9=260|35=8|34=3|49=KRAKEN-TRD|56=CLIENT|52=20260407-14:32:05.122|6=0|11=1744036325000000|14=0|17=EXEC002:TRD001|37=OQNCZM-NVAVC-AVD2LO|38=0.001|39=0|40=2|44=84000|54=1|55=BTC/USD|58=buy \
     0.001 BTC/USD @ limit \
     84000|59=1|60=20260407-14:32:05.000|150=0|151=0.001|381=0|10=144|"
    |> String.map ~f:(function '|' -> Fix.Codec.soh | char -> char)
  in
  let frame =
    Fix.Codec.Frame.decode published
    |> Result.map_error ~f:(fun error -> (error :> Fix.error))
    |> or_fail
  in
  (match Fix.Inbound.classify frame with
  | `Execution_report _ -> ()
  | _ -> failwith "expected an execution report");
  assert (
    Fix.Execution_report.equal_event
      (Fix.Execution_report.event frame |> or_fail)
      New_order);
  assert (
    Fix.Execution_report.equal_status
      (Fix.Execution_report.status frame |> or_fail)
      New);
  assert (
    Option.equal String.equal
      (Fix.Execution_report.order_id frame)
      (Some "OQNCZM-NVAVC-AVD2LO"))

let encode_server ~sequence ~msg_type ~body_fields =
  Fix.Codec.Encoder.message ~sender_comp_id:"KRAKEN-MD" ~target_comp_id:"CLIENT"
    ~msg_type ~msg_seq_num:sequence ~sending_time:"20260825-12:34:56.123"
    ~body_fields
  |> Result.map_error ~f:(fun error -> (error :> Fix.error))
  |> or_fail |> Fix.Codec.Frame.decode
  |> Result.map_error ~f:(fun error -> (error :> Fix.error))
  |> or_fail

let test_published_market_data_groups () =
  let snapshot =
    encode_server ~sequence:21 ~msg_type:"W"
      ~body_fields:
        [
          (55, "BTC/USD");
          (262, "3");
          (268, "2");
          (269, "1");
          (278, "O30300.0");
          (270, "30300.0");
          (271, "8.44867022");
          (273, "13:49:07.307");
          (269, "0");
          (278, "B30299.9");
          (270, "30299.9");
          (271, "0.67373926");
          (273, "13:49:10.179");
        ]
  in
  let entries =
    Fix.Market_data.fold_entries snapshot ~init:[] ~f:(fun entries entry ->
        entry :: entries)
    |> or_fail |> List.rev
  in
  (match entries with
  | [ offer; bid ] ->
      assert (
        Fix.Market_data.Entry.equal_entry_type
          (Fix.Market_data.Entry.entry_type offer)
          Offer);
      assert (
        Fix.Market_data.Entry.equal_entry_type
          (Fix.Market_data.Entry.entry_type bid)
          Bid);
      assert (
        String.equal
          (Fix.Decimal.to_string (Fix.Market_data.Entry.price offer |> or_fail))
          "30300.0");
      assert (
        String.equal
          (Fix.Decimal.to_string (Fix.Market_data.Entry.size bid |> or_fail))
          "0.67373926")
  | _ -> failwith "unexpected snapshot entry count");
  let incremental =
    encode_server ~sequence:100 ~msg_type:"X"
      ~body_fields:
        [
          (55, "BTC/USD");
          (262, "1");
          (268, "2");
          (279, "2");
          (269, "1");
          (278, "O30300.7");
          (270, "30300.7");
          (271, "0.0");
          (273, "13:42:27.208");
          (279, "0");
          (269, "1");
          (278, "O31941.0");
          (270, "31941.0");
          (271, "0.0031746");
          (273, "20:40:00.455");
        ]
  in
  let actions =
    Fix.Market_data.fold_entries incremental ~init:[] ~f:(fun actions entry ->
        Fix.Market_data.Entry.update_action entry :: actions)
    |> or_fail |> List.rev
  in
  assert (
    List.equal
      (Option.equal Fix.Market_data.Entry.equal_update_action)
      actions
      [ Some Delete_entry; Some New_entry ])

let test_fail_closed_validation () =
  (match
     Fix.Header.create ~sender_comp_id:"CLIENT" ~msg_seq_num:0
       ~sending_time:"20260824-12:34:56.123"
   with
  | Error (`Invalid_sequence_number 0) -> ()
  | _ -> failwith "zero MsgSeqNum header was accepted");
  (match Fix.Client_order_id.create "0001" with
  | Error (`Invalid_client_order_id _) -> ()
  | _ -> failwith "leading-zero ClOrdID was accepted");
  (match
     Fix.Market_data.request ~header:(header 1)
       {
         request_id = "bad";
         action = Subscribe;
         depth = Top;
         entries = [];
         symbols = [ "BTC/USD" ];
       }
   with
  | Error (`Invalid_request _) -> ()
  | _ -> failwith "empty market-data entry set was accepted");
  match
    Fix.Order.new_single ~header:(header 1)
      {
        client_order_id = Fix.Client_order_id.create "1" |> or_fail;
        kind = Market;
        quantity = decimal "-1";
        side = Buy;
        symbol = "BTC/USD";
        time_in_force = Ioc;
        self_trade_prevention = None;
      }
  with
  | Error (`Invalid_decimal (38, _)) -> ()
  | _ -> failwith "negative order quantity was accepted"

let () =
  test_endpoints ();
  test_authentication_vector ();
  test_sequence_reset_gap_fill ();
  test_resend_request_range ();
  test_heartbeat_test_request_id ();
  test_market_data_request ();
  test_order_and_cancel ();
  test_published_execution_report ();
  test_published_market_data_groups ();
  test_fail_closed_validation ();
  print_endline "Kraken FIX tests passed"

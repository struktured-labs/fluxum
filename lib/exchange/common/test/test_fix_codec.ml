open Core
open Exchange_common
module Fix = Fix_codec

let or_fail = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (Fix.sexp_of_error error))

let encode ?(seq = 1) ?(body = []) msg_type =
  Fix.Encoder.message ~sender_comp_id:"CLIENT" ~target_comp_id:"KRAKEN-MD"
    ~msg_type ~msg_seq_num:seq ~sending_time:"20260824-12:34:56.123"
    ~body_fields:body
  |> or_fail

let encode_poss_dup ?(seq = 1) ?(body = []) msg_type =
  Fix.Encoder.message_poss_dup ~sender_comp_id:"CLIENT"
    ~target_comp_id:"KRAKEN-MD" ~msg_type ~msg_seq_num:seq
    ~sending_time:"20260824-12:35:00.123"
    ~orig_sending_time:"20260824-12:34:56.123" ~body_fields:body
  |> or_fail

let recompute_checksum raw =
  let checksum_position = String.length raw - 7 in
  let checksum =
    String.prefix raw checksum_position
    |> String.fold ~init:0 ~f:(fun total char -> total + Char.to_int char)
    |> fun total -> total mod 256
  in
  String.prefix raw checksum_position ^ sprintf "10=%03d%c" checksum Fix.soh

let insert_before_checksum raw (tag, value) =
  let separator = String.make 1 Fix.soh in
  let original = Fix.Frame.decode raw |> or_fail in
  let original_body_length = Fix.Frame.value_exn original 9 |> Int.of_string in
  let field = sprintf "%d=%s%c" tag value Fix.soh in
  let checksum_position = String.length raw - 7 in
  let inserted =
    String.prefix raw checksum_position ^ field
    ^ String.drop_prefix raw checksum_position
  in
  let inserted =
    String.substr_replace_first inserted
      ~pattern:("9=" ^ Int.to_string original_body_length ^ separator)
      ~with_:
        ("9=" ^ Int.to_string (original_body_length + String.length field)
       ^ separator)
  in
  recompute_checksum inserted

let%test_module "FIX codec" =
  (module struct
    let%test "round trip validates framing" =
      let raw = encode ~body:[ (262, "book-1"); (55, "BTC/USD") ] "V" in
      let frame = Fix.Frame.decode raw |> or_fail in
      String.equal (Fix.Frame.msg_type frame) "V"
      && Fix.Frame.sequence_number frame = 1
      && Option.equal String.equal (Fix.Frame.value frame 55) (Some "BTC/USD")

    let%test "non-positive sequence numbers fail at both wire boundaries" =
      let encoded =
        Fix.Encoder.message ~sender_comp_id:"CLIENT"
          ~target_comp_id:"KRAKEN-MD" ~msg_type:"0" ~msg_seq_num:0
          ~sending_time:"20260824-12:34:56.123" ~body_fields:[]
      in
      let malicious =
        encode "0"
        |> String.substr_replace_first
             ~pattern:("34=1" ^ String.make 1 Fix.soh)
             ~with_:("34=0" ^ String.make 1 Fix.soh)
        |> recompute_checksum |> Fix.Frame.decode
      in
      match (encoded, malicious) with
      | Error (`Invalid_value (34, "0")), Error (`Invalid_value (34, "0")) ->
          true
      | _ -> false

    let%test "body length corruption is rejected" =
      let raw = encode "0" in
      let body_length =
        Fix.Frame.decode raw |> or_fail |> fun frame ->
        Fix.Frame.value_exn frame 9
      in
      let corrupted =
        String.substr_replace_first raw ~pattern:("9=" ^ body_length)
          ~with_:"9=1"
      in
      match Fix.Frame.decode corrupted with
      | Error (`Body_length_mismatch _) -> true
      | _ -> false

    let%test "checksum corruption is rejected" =
      let raw = encode "0" in
      let checksum_position = String.length raw - 4 in
      let corrupted = Bytes.of_string raw in
      Bytes.set corrupted checksum_position
        (match raw.[checksum_position] with '9' -> '0' | _ -> '9');
      match Fix.Frame.decode (Bytes.to_string corrupted) with
      | Error (`Checksum_mismatch _) -> true
      | _ -> false

    let%test "framer accepts fragmented and coalesced messages" =
      let one = encode ~seq:1 "0" in
      let two = encode ~seq:2 "1" in
      let framer = Fix.Framer.create () in
      let split = String.length one / 2 in
      let first = Fix.Framer.feed framer (String.prefix one split) |> or_fail in
      let second =
        Fix.Framer.feed framer (String.drop_prefix one split ^ two) |> or_fail
      in
      List.is_empty first
      && List.equal Int.equal
           (List.map second ~f:Fix.Frame.sequence_number)
           [ 1; 2 ]
      && Fix.Framer.pending_bytes framer = 0

    let%test "repeating tags preserve order" =
      let raw = encode ~body:[ (267, "2"); (269, "0"); (269, "1") ] "V" in
      let frame = Fix.Frame.decode raw |> or_fail in
      Fix.Frame.find_all frame 269
      |> List.map ~f:(Fix.Field.value ~message:(Fix.Frame.raw frame))
      |> List.equal String.equal [ "0"; "1" ]

    let%test "decimal parsing is exact" =
      match Fix.Decimal.of_string "-123.004500" with
      | Error _ -> false
      | Ok decimal ->
          Int64.equal (Fix.Decimal.mantissa decimal) (-123004500L)
          && Fix.Decimal.scale decimal = 6
          && String.equal (Fix.Decimal.to_string decimal) "-123.004500"

    let%test "sequence gaps fail closed" =
      let state = Fix.Sequence.create () in
      let frame = encode ~seq:2 "0" |> Fix.Frame.decode |> or_fail in
      match Fix.Sequence.accept_incoming state frame with
      | Error (`Gap (1, 2)) -> true
      | _ -> false

    let%test "possible duplicates do not advance sequence" =
      let state = Fix.Sequence.create ~incoming:2 () in
      let duplicate =
        encode_poss_dup ~seq:1 "0" |> Fix.Frame.decode |> or_fail
      in
      match Fix.Sequence.accept_incoming state duplicate with
      | Ok (same_state, `Possible_duplicate) ->
          Fix.Sequence.equal state same_state
      | _ -> false

    let%test "possible duplicates require original sending time" =
      let state = Fix.Sequence.create ~incoming:2 () in
      let raw = encode ~seq:1 "0" in
      let duplicate =
        insert_before_checksum raw (43, "Y")
        |> Fix.Frame.decode |> or_fail
      in
      match Fix.Sequence.accept_incoming state duplicate with
      | Error (`Possible_duplicate_without_orig_sending_time 1) -> true
      | _ -> false

    let%test "replay preserves intent and marks the duplicate" =
      let original =
        encode ~seq:7
          ~body:[ (262, "book-7"); (267, "2"); (269, "0"); (269, "1") ]
          "V"
        |> Fix.Frame.decode |> or_fail
      in
      let replayed =
        Fix.Encoder.replay original ~sending_time:"20260824-12:36:00.000"
        |> or_fail |> Fix.Frame.decode |> or_fail
      in
      Fix.Frame.sequence_number replayed = 7
      && String.equal (Fix.Frame.msg_type replayed) "V"
      && Option.equal String.equal (Fix.Frame.value replayed 43) (Some "Y")
      && Option.equal String.equal
           (Fix.Frame.value replayed 52)
           (Some "20260824-12:36:00.000")
      && Option.equal String.equal
           (Fix.Frame.value replayed 122)
           (Some "20260824-12:34:56.123")
      && List.equal String.equal
           (Fix.Frame.find_all replayed 269
           |> List.map ~f:(Fix.Field.value ~message:(Fix.Frame.raw replayed)))
           [ "0"; "1" ]

    let%test "duplicate control tags cannot be smuggled through the body" =
      match
        Fix.Encoder.message ~sender_comp_id:"CLIENT"
          ~target_comp_id:"KRAKEN-MD" ~msg_type:"0" ~msg_seq_num:1
          ~sending_time:"20260824-12:34:56.123" ~body_fields:[ (43, "Y") ]
      with
      | Error (`Unexpected_field (-1, 43)) -> true
      | _ -> false

    let%test "an earlier checksum field cannot shadow the trailer" =
      let malicious = insert_before_checksum (encode "0") (10, "111") in
      match Fix.Frame.decode malicious with
      | Error (`Duplicate_field 10) -> true
      | _ -> false

    let%test "singleton session fields cannot be duplicated" =
      let malicious = insert_before_checksum (encode "0") (34, "999") in
      match Fix.Frame.decode malicious with
      | Error (`Duplicate_field 34) -> true
      | _ -> false

    let%test "oversized declared frames cannot overflow length arithmetic" =
      let separator = String.make 1 Fix.soh in
      let prefix = "8=FIX.4.4" ^ separator ^ "9=" in
      let raw = prefix ^ Int.to_string Int.max_value ^ separator in
      let framer = Fix.Framer.create ~max_frame_length:128 () in
      match Fix.Framer.feed framer raw with
      | Error (`Frame_too_large (_, 128)) -> true
      | _ -> false

    let%test "frame limit applies per frame, not to a coalesced read" =
      let one = encode ~seq:1 "0" in
      let two = encode ~seq:2 "0" in
      let maximum = Int.max (String.length one) (String.length two) in
      let framer = Fix.Framer.create ~max_frame_length:maximum () in
      match Fix.Framer.feed framer (one ^ two) with
      | Ok frames ->
          List.equal Int.equal
            (List.map frames ~f:Fix.Frame.sequence_number)
            [ 1; 2 ]
      | Error _ -> false
  end)

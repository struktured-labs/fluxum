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

let%test_module "FIX codec" =
  (module struct
    let%test "round trip validates framing" =
      let raw = encode ~body:[ (262, "book-1"); (55, "BTC/USD") ] "V" in
      let frame = Fix.Frame.decode raw |> or_fail in
      String.equal (Fix.Frame.msg_type frame) "V"
      && Fix.Frame.sequence_number frame = 1
      && Option.equal String.equal (Fix.Frame.value frame 55) (Some "BTC/USD")

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
        encode ~seq:1 ~body:[ (43, "Y"); (122, "20260824-12:34:56.000") ] "0"
        |> Fix.Frame.decode |> or_fail
      in
      match Fix.Sequence.accept_incoming state duplicate with
      | Ok (same_state, `Possible_duplicate) ->
          Fix.Sequence.equal state same_state
      | _ -> false

    let%test "possible duplicates require original sending time" =
      let state = Fix.Sequence.create ~incoming:2 () in
      let duplicate =
        encode ~seq:1 ~body:[ (43, "Y") ] "0" |> Fix.Frame.decode |> or_fail
      in
      match Fix.Sequence.accept_incoming state duplicate with
      | Error (`Possible_duplicate_without_orig_sending_time 1) -> true
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

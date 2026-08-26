open Core

let soh = '\x01'

type error =
  [ `Body_length_mismatch of int * int
  | `Checksum_mismatch of int * int
  | `Duplicate_field of int
  | `Frame_too_large of int * int
  | `Invalid_body_length of string
  | `Invalid_checksum of string
  | `Invalid_tag of string
  | `Invalid_value of int * string
  | `Malformed_field of int
  | `Missing_field of int
  | `Trailing_data of int
  | `Unexpected_field of int * int
  | `Unsupported_begin_string of string ]
[@@deriving sexp]

let substring string ~position ~length =
  String.sub string ~pos:position ~len:length

let parse_nonnegative_int string ~position ~length =
  match length <= 0 with
  | true -> Error "empty integer"
  | false ->
      let limit = position + length in
      let rec loop index value =
        match index = limit with
        | true -> Ok value
        | false -> (
            let digit = Char.to_int string.[index] - Char.to_int '0' in
            match digit < 0 || digit > 9 with
            | true -> Error (substring string ~position ~length)
            | false -> (
                match value > (Int.max_value - digit) / 10 with
                | true -> Error (substring string ~position ~length)
                | false -> loop (index + 1) ((value * 10) + digit)))
      in
      loop position 0

module Field = struct
  type t = { tag : int; value_position : int; value_length : int }

  let tag t = t.tag
  let value_length t = t.value_length
  let value_position t = t.value_position

  let value ~message t =
    substring message ~position:t.value_position ~length:t.value_length

  let value_equal ~message t expected =
    let expected_length = String.length expected in
    match expected_length = t.value_length with
    | false -> false
    | true ->
        let rec loop index =
          match index = expected_length with
          | true -> true
          | false -> (
              match
                Char.equal message.[t.value_position + index] expected.[index]
              with
              | false -> false
              | true -> loop (index + 1))
        in
        loop 0

  let int_value ~message t =
    match
      parse_nonnegative_int message ~position:t.value_position
        ~length:t.value_length
    with
    | Ok value -> Ok value
    | Error value -> Error (`Invalid_value (t.tag, value))
end

module Decimal = struct
  type t = { mantissa : int64; scale : int } [@@deriving sexp, equal]

  let mantissa t = t.mantissa
  let scale t = t.scale

  let of_substring value ~position ~length =
    match length = 0 with
    | true -> Error "empty decimal"
    | false -> (
        let limit = position + length in
        let negative, start =
          match value.[position] with
          | '-' -> (true, position + 1)
          | '+' -> (false, position + 1)
          | _ -> (false, position)
        in
        let rec loop index mantissa scale seen_dot seen_digit =
          match index = limit with
          | true -> (
              match seen_digit with
              | false -> Error "decimal has no digits"
              | true ->
                  let mantissa =
                    match negative with
                    | true -> Int64.neg mantissa
                    | false -> mantissa
                  in
                  Ok { mantissa; scale })
          | false -> (
              match value.[index] with
              | '.' -> (
                  match seen_dot with
                  | true -> Error "decimal has multiple points"
                  | false -> loop (index + 1) mantissa scale true seen_digit)
              | char -> (
                  let digit = Char.to_int char - Char.to_int '0' in
                  match digit < 0 || digit > 9 with
                  | true -> Error "decimal contains a non-digit"
                  | false -> (
                      let digit = Int64.of_int digit in
                      match Int64.(mantissa > (max_value - digit) / 10L) with
                      | true -> Error "decimal mantissa overflows int64"
                      | false ->
                          loop (index + 1)
                            Int64.((mantissa * 10L) + digit)
                            (match seen_dot with
                            | true -> scale + 1
                            | false -> scale)
                            seen_dot true)))
        in
        match start = limit with
        | true -> Error "decimal has no digits"
        | false -> loop start 0L 0 false false)

  let of_string value =
    of_substring value ~position:0 ~length:(String.length value)

  let of_field ~message field =
    of_substring message
      ~position:(Field.value_position field)
      ~length:(Field.value_length field)

  let to_string t =
    let negative = Int64.(t.mantissa < 0L) in
    let digits = Int64.abs t.mantissa |> Int64.to_string in
    let length = String.length digits in
    let unsigned =
      match t.scale with
      | 0 -> digits
      | scale when length > scale ->
          String.prefix digits (length - scale)
          ^ "." ^ String.suffix digits scale
      | scale -> "0." ^ String.make (scale - length) '0' ^ digits
    in
    match negative with true -> "-" ^ unsigned | false -> unsigned
end

let checksum string ~length =
  let rec loop index total =
    match index = length with
    | true -> total mod 256
    | false -> loop (index + 1) (total + Char.to_int string.[index])
  in
  loop 0 0

let find_soh string ~position = String.index_from string position soh

let parse_fields raw =
  let raw_length = String.length raw in
  let rec loop position fields =
    match position = raw_length with
    | true -> Ok (Array.of_list_rev fields)
    | false -> (
        match find_soh raw ~position with
        | None -> Error (`Malformed_field position)
        | Some delimiter -> (
            match String.index_from raw position '=' with
            | None -> Error (`Malformed_field position)
            | Some equals when equals >= delimiter ->
                Error (`Malformed_field position)
            | Some equals -> (
                let tag_length = equals - position in
                match
                  parse_nonnegative_int raw ~position ~length:tag_length
                with
                | Error tag -> Error (`Invalid_tag tag)
                | Ok 0 -> Error (`Invalid_tag "0")
                | Ok parsed_tag ->
                    let field =
                      Field.
                        {
                          tag = parsed_tag;
                          value_position = equals + 1;
                          value_length = delimiter - equals - 1;
                        }
                    in
                    loop (delimiter + 1) (field :: fields))))
  in
  loop 0 []

module Frame = struct
  type t = {
    raw : string;
    fields : Field.t array;
    msg_type : string;
    sequence_number : int;
  }

  let raw t = t.raw
  let fields t = t.fields
  let msg_type t = t.msg_type
  let sequence_number t = t.sequence_number
  let find t tag = Array.find t.fields ~f:(fun field -> Field.tag field = tag)

  let find_all t tag =
    Array.fold_right t.fields ~init:[] ~f:(fun field found ->
        match Field.tag field = tag with
        | true -> field :: found
        | false -> found)

  let value t tag = Option.map (find t tag) ~f:(Field.value ~message:t.raw)

  let value_exn t tag =
    Field.value ~message:t.raw (Option.value_exn (find t tag))

  let int_value t tag =
    match find t tag with
    | None -> Error (`Missing_field tag)
    | Some field -> Field.int_value ~message:t.raw field

  let validate_field_at fields index expected =
    match index >= Array.length fields with
    | true -> Error (`Missing_field expected)
    | false -> (
        let actual = Field.tag fields.(index) in
        match actual = expected with
        | true -> Ok fields.(index)
        | false -> Error (`Unexpected_field (expected, actual)))

  let singleton_bit = function
    | 8 -> 1 lsl 0
    | 9 -> 1 lsl 1
    | 10 -> 1 lsl 2
    | 34 -> 1 lsl 3
    | 35 -> 1 lsl 4
    | 43 -> 1 lsl 5
    | 49 -> 1 lsl 6
    | 52 -> 1 lsl 7
    | 56 -> 1 lsl 8
    | 122 -> 1 lsl 9
    | _ -> 0

  let validate_singleton_fields fields =
    let rec loop index seen =
      match index = Array.length fields with
      | true -> (
          match seen land singleton_bit 49 = 0 with
          | true -> Error (`Missing_field 49)
          | false -> (
              match seen land singleton_bit 56 = 0 with
              | true -> Error (`Missing_field 56)
              | false -> (
                  match seen land singleton_bit 52 = 0 with
                  | true -> Error (`Missing_field 52)
                  | false -> Ok ())))
      | false ->
          let field = fields.(index) in
          let tag = Field.tag field in
          let bit = singleton_bit tag in
          (match (bit = 0, seen land bit = 0) with
          | true, _ -> loop (index + 1) seen
          | false, true -> (
              match tag with
              | 49 | 52 | 56 -> (
                  match Field.value_length field = 0 with
                  | true -> Error (`Invalid_value (tag, ""))
                  | false -> loop (index + 1) (seen lor bit))
              | _ -> loop (index + 1) (seen lor bit))
          | false, false -> Error (`Duplicate_field tag))
    in
    loop 0 0

  let decode raw =
    let open Result.Let_syntax in
    let%bind fields = parse_fields raw in
    let%bind begin_field = validate_field_at fields 0 8 in
    let begin_string = Field.value ~message:raw begin_field in
    let%bind () =
      match String.equal begin_string "FIX.4.4" with
      | true -> Ok ()
      | false -> Error (`Unsupported_begin_string begin_string)
    in
    let%bind body_length_field = validate_field_at fields 1 9 in
    let%bind declared_body_length =
      Field.int_value ~message:raw body_length_field
    in
    let%bind msg_type_field = validate_field_at fields 2 35 in
    let%bind () = validate_singleton_fields fields in
    let last_index = Array.length fields - 1 in
    let%bind checksum_field =
      match Array.findi fields ~f:(fun _ field -> Field.tag field = 10) with
      | None -> validate_field_at fields last_index 10
      | Some (index, field) -> (
          match index = last_index with
          | true -> Ok field
          | false ->
              let trailing_position =
                field.value_position + field.value_length + 1
              in
              Error (`Trailing_data (String.length raw - trailing_position)))
    in
    let body_position =
      body_length_field.value_position + body_length_field.value_length + 1
    in
    let checksum_position = checksum_field.value_position - 3 in
    let actual_body_length = checksum_position - body_position in
    let%bind () =
      match declared_body_length = actual_body_length with
      | true -> Ok ()
      | false ->
          Error
            (`Body_length_mismatch (declared_body_length, actual_body_length))
    in
    let checksum_string = Field.value ~message:raw checksum_field in
    let%bind declared_checksum =
      match String.length checksum_string = 3 with
      | false -> Error (`Invalid_checksum checksum_string)
      | true -> (
          match Int.of_string_opt checksum_string with
          | None -> Error (`Invalid_checksum checksum_string)
          | Some checksum -> Ok checksum)
    in
    let actual_checksum = checksum raw ~length:checksum_position in
    let%bind () =
      match declared_checksum = actual_checksum with
      | true -> Ok ()
      | false -> Error (`Checksum_mismatch (declared_checksum, actual_checksum))
    in
    let msg_type = Field.value ~message:raw msg_type_field in
    let%bind () =
      match String.is_empty msg_type with
      | true -> Error (`Invalid_value (35, msg_type))
      | false -> Ok ()
    in
    let sequence_field =
      Array.find fields ~f:(fun field -> Field.tag field = 34)
    in
    let%bind sequence_number =
      match sequence_field with
      | None -> Error (`Missing_field 34)
      | Some field -> Field.int_value ~message:raw field
    in
    (match sequence_number > 0 with
    | true -> Ok { raw; fields; msg_type; sequence_number }
    | false -> Error (`Invalid_value (34, Int.to_string sequence_number)))
end

let validate_encoded_field (tag, value) =
  match tag <= 0 with
  | true -> Error (`Invalid_tag (Int.to_string tag))
  | false -> (
      match String.mem value soh with
      | true -> Error (`Invalid_value (tag, value))
      | false -> Ok ())

module Encoder = struct
  let reserved_tag = function
    | 8 | 9 | 10 | 34 | 35 | 43 | 49 | 52 | 56 | 122 -> true
    | _ -> false

  let add_field buffer (tag, value) =
    Buffer.add_string buffer (Int.to_string tag);
    Buffer.add_char buffer '=';
    Buffer.add_string buffer value;
    Buffer.add_char buffer soh

  let validate_required_values ~sender_comp_id ~target_comp_id ~msg_type
      ~sending_time ~poss_dup_orig_sending_time =
    match String.is_empty msg_type with
    | true -> Error (`Invalid_value (35, msg_type))
    | false -> (
        match String.is_empty sender_comp_id with
        | true -> Error (`Invalid_value (49, sender_comp_id))
        | false -> (
            match String.is_empty target_comp_id with
            | true -> Error (`Invalid_value (56, target_comp_id))
            | false -> (
                match String.is_empty sending_time with
                | true -> Error (`Invalid_value (52, sending_time))
                | false -> (
                    match poss_dup_orig_sending_time with
                    | None -> Ok ()
                    | Some value -> (
                        match String.is_empty value with
                        | true -> Error (`Invalid_value (122, value))
                        | false -> Ok ())))))

  let message_internal ~poss_dup_orig_sending_time ~sender_comp_id
      ~target_comp_id ~msg_type ~msg_seq_num ~sending_time ~body_fields =
    let open Result.Let_syntax in
    let%bind () =
      match msg_seq_num > 0 with
      | false -> Error (`Invalid_value (34, Int.to_string msg_seq_num))
      | true ->
          validate_required_values ~sender_comp_id ~target_comp_id ~msg_type
            ~sending_time ~poss_dup_orig_sending_time
    in
    let header_fields =
      [
        (35, msg_type);
        (34, Int.to_string msg_seq_num);
        (49, sender_comp_id);
        (56, target_comp_id);
        (52, sending_time);
      ]
      @
      match poss_dup_orig_sending_time with
      | None -> []
      | Some original -> [ (43, "Y"); (122, original) ]
    in
    let%bind () =
      Result.all_unit
        (List.map (header_fields @ body_fields) ~f:validate_encoded_field)
    in
    let%bind () =
      match List.find body_fields ~f:(fun (tag, _) -> reserved_tag tag) with
      | None -> Ok ()
      | Some (tag, _) -> Error (`Unexpected_field (-1, tag))
    in
    let body = Buffer.create 256 in
    List.iter (header_fields @ body_fields) ~f:(add_field body);
    let body = Buffer.contents body in
    let prefix =
      sprintf "8=FIX.4.4%c9=%d%c%s" soh (String.length body) soh body
    in
    let check = checksum prefix ~length:(String.length prefix) in
    let raw = sprintf "%s10=%03d%c" prefix check soh in
    Ok raw

  let message ~sender_comp_id ~target_comp_id ~msg_type ~msg_seq_num
      ~sending_time ~body_fields =
    message_internal ~poss_dup_orig_sending_time:None ~sender_comp_id
      ~target_comp_id ~msg_type ~msg_seq_num ~sending_time ~body_fields

  let message_poss_dup ~sender_comp_id ~target_comp_id ~msg_type ~msg_seq_num
      ~sending_time ~orig_sending_time ~body_fields =
    message_internal ~poss_dup_orig_sending_time:(Some orig_sending_time)
      ~sender_comp_id ~target_comp_id ~msg_type ~msg_seq_num ~sending_time
      ~body_fields

  let replay frame ~sending_time =
    let open Result.Let_syntax in
    let%bind sender_comp_id =
      Frame.value frame 49 |> Result.of_option ~error:(`Missing_field 49)
    in
    let%bind target_comp_id =
      Frame.value frame 56 |> Result.of_option ~error:(`Missing_field 56)
    in
    let%bind original_sending_time =
      Frame.value frame 52 |> Result.of_option ~error:(`Missing_field 52)
    in
    let raw = Frame.raw frame in
    let body_fields =
      Frame.fields frame |> Array.to_list
      |> List.filter_map ~f:(fun field ->
          let tag = Field.tag field in
          match reserved_tag tag with
          | true -> None
          | false -> Some (tag, Field.value ~message:raw field))
    in
    message_poss_dup ~orig_sending_time:original_sending_time ~sender_comp_id
      ~target_comp_id ~msg_type:(Frame.msg_type frame)
      ~msg_seq_num:(Frame.sequence_number frame)
      ~sending_time ~body_fields
end

module Framer = struct
  type t = {
    max_frame_length : int;
    mutable pending : string;
    mutable position : int;
  }

  let create ?(max_frame_length = 1024 * 1024) () =
    {
      max_frame_length = Int.max 0 max_frame_length;
      pending = "";
      position = 0;
    }

  let pending_bytes t = String.length t.pending - t.position

  let append t chunk =
    let remaining = pending_bytes t in
    let pending =
      match (remaining, String.is_empty chunk, t.position) with
      | 0, _, _ -> chunk
      | _, true, 0 -> t.pending
      | _, true, _ -> substring t.pending ~position:t.position ~length:remaining
      | _, false, 0 -> t.pending ^ chunk
      | _, false, _ ->
          substring t.pending ~position:t.position ~length:remaining ^ chunk
    in
    t.pending <- pending;
    t.position <- 0

  let next_length t =
    let pending_length = pending_bytes t in
    let incomplete () =
      match pending_length > t.max_frame_length with
      | true -> Error (`Frame_too_large (pending_length, t.max_frame_length))
      | false -> Ok None
    in
    match pending_length < 2 with
    | true -> incomplete ()
    | false -> (
        match
          String.is_substring_at t.pending ~pos:t.position ~substring:"8="
        with
        | false -> Error (`Unexpected_field (8, -1))
        | true -> (
            match find_soh t.pending ~position:t.position with
            | None -> incomplete ()
            | Some first_delimiter -> (
                let second_position = first_delimiter + 1 in
                let second_offset = second_position - t.position in
                match pending_length < second_offset + 2 with
                | true -> incomplete ()
                | false -> (
                    match
                      String.is_substring_at t.pending ~pos:second_position
                        ~substring:"9="
                    with
                    | false -> Error (`Unexpected_field (9, -1))
                    | true -> (
                        match find_soh t.pending ~position:second_position with
                        | None -> incomplete ()
                        | Some second_delimiter -> (
                            let value_position = second_position + 2 in
                            let value_length =
                              second_delimiter - value_position
                            in
                            match
                              parse_nonnegative_int t.pending
                                ~position:value_position ~length:value_length
                            with
                            | Error value -> Error (`Invalid_body_length value)
                            | Ok body_length -> (
                                let framing_length =
                                  second_delimiter - t.position + 8
                                in
                                let too_large =
                                  match framing_length > t.max_frame_length with
                                  | true -> true
                                  | false ->
                                      body_length
                                      > t.max_frame_length - framing_length
                                in
                                match too_large with
                                | false ->
                                    Ok (Some (framing_length + body_length))
                                | true ->
                                    let reported_length =
                                      match
                                        body_length
                                        > Int.max_value - framing_length
                                      with
                                      | true -> Int.max_value
                                      | false -> framing_length + body_length
                                    in
                                    Error
                                      (`Frame_too_large
                                         (reported_length, t.max_frame_length)))
                            ))))))

  let feed t chunk =
    append t chunk;
    let rec loop frames =
      match next_length t with
      | Error _ as error -> error
      | Ok None -> Ok (List.rev frames)
      | Ok (Some length) -> (
          match pending_bytes t < length with
          | true -> Ok (List.rev frames)
          | false -> (
              let raw = substring t.pending ~position:t.position ~length in
              match Frame.decode raw with
              | Error _ as error -> error
              | Ok frame ->
                  t.position <- t.position + length;
                  (match pending_bytes t with
                  | 0 ->
                      t.pending <- "";
                      t.position <- 0
                  | _ -> ());
                  loop (frame :: frames)))
    in
    loop []
end

module Sequence = struct
  type t = { next_outgoing : int; next_incoming : int } [@@deriving sexp, equal]
  type incoming = [ `Accept | `Possible_duplicate ] [@@deriving sexp, equal]

  type sequence_error =
    [ `Duplicate_without_poss_dup of int * int
    | `Gap of int * int
    | `Possible_duplicate_without_orig_sending_time of int ]
  [@@deriving sexp, equal]

  let create ?(outgoing = 1) ?(incoming = 1) () =
    { next_outgoing = outgoing; next_incoming = incoming }

  let next_outgoing t =
    (t.next_outgoing, { t with next_outgoing = t.next_outgoing + 1 })

  let accept_incoming t frame =
    let received = Frame.sequence_number frame in
    let expected = t.next_incoming in
    match Int.compare received expected with
    | 0 -> Ok ({ t with next_incoming = expected + 1 }, `Accept)
    | comparison when comparison > 0 -> Error (`Gap (expected, received))
    | _ -> (
        match Frame.value frame 43 with
        | Some "Y" -> (
            match Frame.find frame 122 with
            | Some _ -> Ok (t, `Possible_duplicate)
            | None ->
                Error (`Possible_duplicate_without_orig_sending_time received))
        | _ -> Error (`Duplicate_without_poss_dup (expected, received)))

  let reset _t ~next_outgoing ~next_incoming = { next_outgoing; next_incoming }
end

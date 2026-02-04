(** Comprehensive tests for Websocket_curl frame encoding and handshake *)

open Core

(* ────────────────────────────────────────────────────────────────────────
   Helpers
   ──────────────────────────────────────────────────────────────────────── *)

(** Decode a WebSocket frame header, returning (fin, opcode, masked, payload_len, header_bytes_consumed).
    Does NOT unmask the payload — caller handles that. *)
let decode_frame_header frame =
  let byte0 = Char.to_int frame.[0] in
  let byte1 = Char.to_int frame.[1] in
  let fin = (byte0 land 0x80) <> 0 in
  let opcode = byte0 land 0x0F in
  let masked = (byte1 land 0x80) <> 0 in
  let len_initial = byte1 land 0x7F in
  let payload_len, header_len =
    match len_initial < 126 with
    | true -> (len_initial, 2)
    | false ->
      match len_initial = 126 with
      | true ->
        let len =
          (Char.to_int frame.[2] lsl 8) lor Char.to_int frame.[3]
        in
        (len, 4)
      | false ->
        (* 8-byte extended length — only use lower 4 bytes *)
        let len =
          (Char.to_int frame.[6] lsl 24)
          lor (Char.to_int frame.[7] lsl 16)
          lor (Char.to_int frame.[8] lsl 8)
          lor Char.to_int frame.[9]
        in
        (len, 10)
  in
  (fin, opcode, masked, payload_len, header_len)

(** Extract the 4-byte masking key from a masked frame *)
let extract_mask frame ~header_len =
  Array.init 4 ~f:(fun i -> Char.to_int frame.[header_len + i])

(** Unmask payload bytes using the masking key *)
let unmask_payload frame ~header_len ~mask ~payload_len =
  let mask_offset = header_len + 4 in
  String.init payload_len ~f:(fun i ->
    let byte = Char.to_int frame.[mask_offset + i] in
    Char.of_int_exn (byte lxor mask.(i mod 4)))

(* ────────────────────────────────────────────────────────────────────────
   Tests: compute_accept_key (RFC 6455 §4.2.2)
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "compute_accept_key" = (module struct
  (* RFC 6455 Section 4.2.2 specifies this exact test vector *)
  let%test "RFC 6455 test vector" =
    let key = "dGhlIHNhbXBsZSBub25jZQ==" in
    let expected = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" in
    String.equal (Websocket_curl.compute_accept_key key) expected

  let%test "different keys produce different accepts" =
    let a = Websocket_curl.compute_accept_key "AAAAAAAAAAAAAAAAAAAAAA==" in
    let b = Websocket_curl.compute_accept_key "BBBBBBBBBBBBBBBBBBBBBB==" in
    not (String.equal a b)

  let%test "empty key still produces output" =
    let result = Websocket_curl.compute_accept_key "" in
    String.length result > 0

  let%test "output is valid base64" =
    let result = Websocket_curl.compute_accept_key "dGhlIHNhbXBsZSBub25jZQ==" in
    match Base64.decode result with
    | Ok _ -> true
    | Error _ -> false
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: encode_frame basics (opcode, FIN, masking)
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "encode_frame" = (module struct
  let%test "text frame has opcode 1" =
    let frame = Websocket_curl.encode_text_frame "hello" in
    let (_fin, opcode, _masked, _len, _hlen) = decode_frame_header frame in
    opcode = 1

  let%test "pong frame has opcode 10" =
    let frame = Websocket_curl.encode_pong_frame "ping-data" in
    let (_fin, opcode, _masked, _len, _hlen) = decode_frame_header frame in
    opcode = 10

  let%test "custom opcode preserved" =
    let frame = Websocket_curl.encode_frame ~opcode:9 "test" in
    let (_fin, opcode, _masked, _len, _hlen) = decode_frame_header frame in
    opcode = 9

  let%test "FIN bit is set" =
    let frame = Websocket_curl.encode_text_frame "hello" in
    let (fin, _opcode, _masked, _len, _hlen) = decode_frame_header frame in
    fin

  let%test "mask bit is set (client-to-server frames must be masked)" =
    let frame = Websocket_curl.encode_text_frame "hello" in
    let (_fin, _opcode, masked, _len, _hlen) = decode_frame_header frame in
    masked

  let%test "pong frames are also masked" =
    let frame = Websocket_curl.encode_pong_frame "pong-payload" in
    let (_fin, _opcode, masked, _len, _hlen) = decode_frame_header frame in
    masked
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: payload recovery (masking round-trip)
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "payload_masking" = (module struct
  let roundtrip_payload ~opcode payload =
    let frame = Websocket_curl.encode_frame ~opcode payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    let mask = extract_mask frame ~header_len in
    let recovered = unmask_payload frame ~header_len ~mask ~payload_len in
    String.equal payload recovered

  let%test "text frame payload round-trips" =
    roundtrip_payload ~opcode:1 "hello world"

  let%test "pong frame payload round-trips" =
    roundtrip_payload ~opcode:10 "ping-echo-data"

  let%test "empty payload round-trips" =
    roundtrip_payload ~opcode:1 ""

  let%test "single byte payload" =
    roundtrip_payload ~opcode:1 "x"

  let%test "binary-like payload with all byte values" =
    let payload = String.init 256 ~f:(fun i -> Char.of_int_exn i) in
    roundtrip_payload ~opcode:2 payload

  let%test "payload with null bytes" =
    roundtrip_payload ~opcode:1 "hello\x00world\x00"

  let%test "JSON payload (typical exchange message)" =
    let json = {|{"event":"heartbeat","channel":"ticker","data":{"price":"50000.00"}}|} in
    roundtrip_payload ~opcode:1 json

  let%test "large JSON payload" =
    let json = String.init 500 ~f:(fun i ->
      match i mod 10 with
      | 0 -> '{'
      | 9 -> '}'
      | _ -> Char.of_int_exn (Char.to_int 'a' + (i mod 26))) in
    roundtrip_payload ~opcode:1 json
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: frame length encoding (7-bit, 16-bit, 64-bit)
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "frame_length_encoding" = (module struct
  let%test "small payload uses 7-bit length (< 126)" =
    let payload = String.make 50 'a' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 50 && header_len = 2

  let%test "125-byte payload uses 7-bit length" =
    let payload = String.make 125 'b' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 125 && header_len = 2

  let%test "126-byte payload uses 16-bit extended length" =
    let payload = String.make 126 'c' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 126 && header_len = 4

  let%test "256-byte payload uses 16-bit extended length" =
    let payload = String.make 256 'd' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 256 && header_len = 4

  let%test "65535-byte payload uses 16-bit extended length" =
    let payload = String.make 65535 'e' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 65535 && header_len = 4

  let%test "65536-byte payload uses 64-bit extended length" =
    let payload = String.make 65536 'f' in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    payload_len = 65536 && header_len = 10

  let%test "payload data round-trips at 126 boundary" =
    let payload = String.init 126 ~f:(fun i -> Char.of_int_exn (i mod 128)) in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    let mask = extract_mask frame ~header_len in
    let recovered = unmask_payload frame ~header_len ~mask ~payload_len in
    String.equal payload recovered

  let%test "payload data round-trips at 65536 boundary" =
    let payload = String.init 65536 ~f:(fun i -> Char.of_int_exn (i mod 128)) in
    let frame = Websocket_curl.encode_text_frame payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    let mask = extract_mask frame ~header_len in
    let recovered = unmask_payload frame ~header_len ~mask ~payload_len in
    String.equal payload recovered
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: total frame size correctness
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "frame_total_size" = (module struct
  (* Frame = header(2/4/10) + mask(4) + payload *)

  let%test "empty frame is 6 bytes (2 header + 4 mask)" =
    let frame = Websocket_curl.encode_text_frame "" in
    String.length frame = 6

  let%test "5-byte payload frame is 11 bytes" =
    let frame = Websocket_curl.encode_text_frame "hello" in
    (* 2 header + 4 mask + 5 payload = 11 *)
    String.length frame = 11

  let%test "125-byte payload frame is 131 bytes" =
    let frame = Websocket_curl.encode_text_frame (String.make 125 'x') in
    (* 2 header + 4 mask + 125 payload = 131 *)
    String.length frame = 131

  let%test "126-byte payload frame is 134 bytes" =
    let frame = Websocket_curl.encode_text_frame (String.make 126 'x') in
    (* 4 header + 4 mask + 126 payload = 134 *)
    String.length frame = 134

  let%test "65536-byte payload frame is 65550 bytes" =
    let frame = Websocket_curl.encode_text_frame (String.make 65536 'x') in
    (* 10 header + 4 mask + 65536 payload = 65550 *)
    String.length frame = 65550
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: pong frame specifics (RFC 6455 compliance)
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "pong_frame" = (module struct
  let%test "pong echoes ping payload exactly" =
    let ping_payload = "server-ping-12345" in
    let frame = Websocket_curl.encode_pong_frame ping_payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    let mask = extract_mask frame ~header_len in
    let recovered = unmask_payload frame ~header_len ~mask ~payload_len in
    String.equal recovered ping_payload

  let%test "pong with empty payload" =
    let frame = Websocket_curl.encode_pong_frame "" in
    let (fin, opcode, masked, payload_len, _hlen) = decode_frame_header frame in
    fin && opcode = 10 && masked && payload_len = 0

  let%test "pong with binary ping payload" =
    let binary_payload = String.init 8 ~f:(fun i -> Char.of_int_exn (i * 31)) in
    let frame = Websocket_curl.encode_pong_frame binary_payload in
    let (_fin, _opc, _masked, payload_len, header_len) = decode_frame_header frame in
    let mask = extract_mask frame ~header_len in
    let recovered = unmask_payload frame ~header_len ~mask ~payload_len in
    String.equal recovered binary_payload

  let%test "pong and text frames differ only in opcode for same payload" =
    let payload = "test" in
    let pong = Websocket_curl.encode_pong_frame payload in
    let text = Websocket_curl.encode_text_frame payload in
    let (_, pong_opc, _, pong_len, _) = decode_frame_header pong in
    let (_, text_opc, _, text_len, _) = decode_frame_header text in
    (* Same payload length, different opcodes *)
    pong_len = text_len && pong_opc = 10 && text_opc = 1
end)

(* ────────────────────────────────────────────────────────────────────────
   Tests: opcode variants
   ──────────────────────────────────────────────────────────────────────── *)

let%test_module "opcodes" = (module struct
  let check_opcode ~opcode =
    let frame = Websocket_curl.encode_frame ~opcode "x" in
    let (_fin, opc, _masked, _len, _hlen) = decode_frame_header frame in
    opc = opcode

  let%test "opcode 0 (continuation)" = check_opcode ~opcode:0
  let%test "opcode 1 (text)" = check_opcode ~opcode:1
  let%test "opcode 2 (binary)" = check_opcode ~opcode:2
  let%test "opcode 8 (close)" = check_opcode ~opcode:8
  let%test "opcode 9 (ping)" = check_opcode ~opcode:9
  let%test "opcode 10 (pong)" = check_opcode ~opcode:10
end)

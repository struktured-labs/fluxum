open Core

(** Common cryptographic utilities for exchange API authentication *)

(** Base64 encoding/decoding *)
let base64_encode data =
  Base64.encode_exn data

let base64_decode data =
  Base64.decode data

(** HMAC-SHA256 implementation using digestif *)
let hmac_sha256 ~secret ~message =
  let block_size = 64 in (* SHA256 block size is 64 bytes *)

  let secret_key =
    match String.length secret > block_size with
    | true -> Digestif.SHA256.digest_string secret |> Digestif.SHA256.to_raw_string
    | false -> secret ^ String.make (block_size - String.length secret) '\x00'
  in

  let ipad = String.init block_size ~f:(fun i ->
    Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x36))
  in
  let opad = String.init block_size ~f:(fun i ->
    Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x5c))
  in

  let inner_msg = ipad ^ message in
  let inner_hash = Digestif.SHA256.digest_string inner_msg |> Digestif.SHA256.to_raw_string in
  let outer_msg = opad ^ inner_hash in
  Digestif.SHA256.digest_string outer_msg

(** HMAC-SHA512 implementation using digestif *)
let hmac_sha512 ~secret ~message =
  let block_size = 128 in (* SHA512 block size is 128 bytes *)

  let secret_key =
    match String.length secret > block_size with
    | true -> Digestif.SHA512.digest_string secret |> Digestif.SHA512.to_raw_string
    | false -> secret ^ String.make (block_size - String.length secret) '\x00'
  in

  let ipad = String.init block_size ~f:(fun i ->
    Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x36))
  in
  let opad = String.init block_size ~f:(fun i ->
    Char.of_int_exn (Char.to_int secret_key.[i] lxor 0x5c))
  in

  let inner_msg = ipad ^ message in
  let inner_hash = Digestif.SHA512.digest_string inner_msg |> Digestif.SHA512.to_raw_string in
  let outer_msg = opad ^ inner_hash in
  let result = Digestif.SHA512.digest_string outer_msg in
  Digestif.SHA512.to_raw_string result

(** Generate timestamp in milliseconds since Unix epoch *)
let generate_timestamp_ms () =
  Int63.to_string (Int63.of_float (Core_unix.gettimeofday () *. 1000.0))

(** Build query string from key-value pairs with URI percent-encoding *)
let build_query_string params =
  String.concat ~sep:"&"
    (List.map params ~f:(fun (k, v) ->
      sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v)))

(** Convert binary string to hex (lowercase) *)
let to_hex binary =
  String.to_list binary
  |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep:""

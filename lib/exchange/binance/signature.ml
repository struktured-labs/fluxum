open Core

(** Binance Signature - HMAC-SHA256 with hex encoding *)

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

(** Generate timestamp in milliseconds since Unix epoch *)
let generate_timestamp () =
  Int63.to_string (Int63.of_float (Core_unix.gettimeofday () *. 1000.0))

(** Build query string from key-value pairs with URI percent-encoding *)
let build_query_string params =
  String.concat ~sep:"&"
    (List.map params ~f:(fun (k, v) ->
      sprintf "%s=%s" (Uri.pct_encode k) (Uri.pct_encode v)))

(** Sign Binance request parameters

    Binance signature = HMAC-SHA256(secret, query_string)
    Returns hex-encoded signature (lowercase)

    Example from Binance docs:
    secret: "NhqPtmdSJYdKjVHjA7PZj4Mge3R5YNiP1e3UZjInClVN65XAbvqqM6A7H5fATj0j"
    params: [("symbol", "LTCBTC"); ("side", "BUY"); ("type", "LIMIT"); ...]
    expected: "c8db56825ae71d6d79447849e617115f4a920fa2acdcab2b053c4b2838bd6b71"
*)
let sign ~api_secret ~params =
  let query_string = build_query_string params in
  let hash = hmac_sha256 ~secret:api_secret ~message:query_string in
  Digestif.SHA256.to_hex hash

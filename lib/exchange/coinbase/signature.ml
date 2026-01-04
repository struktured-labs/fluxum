open Core

(* Pure OCaml HMAC-SHA256 implementation for Coinbase *)

let base64_encode data =
  Base64.encode_exn data

let base64_decode data =
  Base64.decode data

(* HMAC-SHA256 using digestif *)
let hmac_sha256 ~secret ~message =
  (* HMAC(K, M) = SHA256((K' XOR opad) || SHA256((K' XOR ipad) || M))
     where K' is key padded to block size *)
  let block_size = 64 in (* SHA256 block size is 64 bytes *)

  let secret_key =
    if String.length secret > block_size then
      Digestif.SHA256.digest_string secret |> Digestif.SHA256.to_raw_string
    else
      secret ^ String.make (block_size - String.length secret) '\x00'
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
  let result = Digestif.SHA256.digest_string outer_msg in
  Digestif.SHA256.to_raw_string result

(** Generate Coinbase WebSocket authentication signature

    Coinbase requires:
    - timestamp (current Unix timestamp)
    - signature = hex(HMAC-SHA256(timestamp + channel + product_ids, secret))
    - api_key
*)
let coinbase_ws_signature ~api_secret ~timestamp ~channel ~product_ids =
  let open Result.Let_syntax in
  let%bind api_secret_decoded = base64_decode api_secret in

  (* Build the message: timestamp + channel + product_ids *)
  let message = sprintf "%s%s%s" timestamp channel product_ids in

  (* Compute HMAC-SHA256 *)
  let sig_binary = hmac_sha256 ~secret:api_secret_decoded ~message in

  (* Convert to hex string *)
  let hex_sig =
    String.to_list sig_binary
    |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
    |> String.concat ~sep:""
  in
  Ok hex_sig

(** Generate Coinbase REST API authentication signature

    Coinbase Advanced Trade REST API requires:
    - timestamp (current Unix timestamp)
    - signature = hex(HMAC-SHA256(timestamp + method + path + body, secret))
    - api_key
*)
let coinbase_rest_signature ~api_secret ~timestamp ~method_ ~path ~body =
  let open Result.Let_syntax in
  let%bind api_secret_decoded = base64_decode api_secret in

  (* Build the message: timestamp + method + path + body *)
  let message = sprintf "%s%s%s%s" timestamp method_ path body in

  (* Compute HMAC-SHA256 *)
  let sig_binary = hmac_sha256 ~secret:api_secret_decoded ~message in

  (* Convert to hex string *)
  let hex_sig =
    String.to_list sig_binary
    |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
    |> String.concat ~sep:""
  in
  Ok hex_sig

(** Get current timestamp as string *)
let current_timestamp () =
  Int.to_string (Float.to_int (Core_unix.gettimeofday ()))

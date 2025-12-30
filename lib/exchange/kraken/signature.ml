open Core

(* Pure OCaml HMAC-SHA512 implementation *)

let base64_encode data =
  Base64.encode_exn data

let base64_decode data =
  Base64.decode data

(* Simple HMAC-SHA512 using digestif *)
let hmac_sha512 ~secret ~message =
  (* HMAC(K, M) = SHA512((K' XOR opad) || SHA512((K' XOR ipad) || M))
     where K' is key padded to block size *)
  let block_size = 128 in (* SHA512 block size is 128 bytes *)
  
  let secret_key = 
    if String.length secret > block_size then
      Digestif.SHA512.digest_string secret |> Digestif.SHA512.to_raw_string
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
  let inner_hash = Digestif.SHA512.digest_string inner_msg |> Digestif.SHA512.to_raw_string in
  let outer_msg = opad ^ inner_hash in
  let result = Digestif.SHA512.digest_string outer_msg in
  Digestif.SHA512.to_raw_string result

let kraken_signature ~api_secret_b64 ~api_path ~nonce ~post_data =
  let open Result.Let_syntax in
  let%bind api_secret = base64_decode api_secret_b64 in

  (* Compute SHA256(nonce + post_data) *)
  let msg_content = nonce ^ post_data in
  let msg_hash =
    Digestif.SHA256.digest_string msg_content
    |> Digestif.SHA256.to_raw_string
  in

  (* Compute HMAC-SHA512(api_path + msg_hash) *)
  let sign_data = api_path ^ msg_hash in
  let sig_binary = hmac_sha512 ~secret:api_secret ~message:sign_data in

  (* Base64 encode the signature *)
  Ok (base64_encode sig_binary)

(** Kalshi RSA-PSS Authentication

    Signs requests using RSA-PSS with SHA-256 (MGF1-SHA256, salt length =
    digest length, 32 bytes), per docs.kalshi.com "Quick Start: Authenticated
    Requests".

    Message = timestamp_ms ^ UPPERCASE_METHOD ^ path, where [path] is the full
    path from the API root (e.g. "/trade-api/v2/portfolio/balance") WITHOUT
    the query string. Signature is base64-encoded. *)

(* Initialize mirage-crypto's RNG exactly once when this module is loaded.
   Without this, RSA signing raises "The default generator is not yet initialized." *)
let () = Mirage_crypto_rng_unix.use_default ()

(* Kalshi uses RSA-PSS with SHA-256, not PKCS1. Salt length defaults to hlen
   (SHA256 digest size = 32 bytes), which matches Kalshi's expectation. *)
module PSS_SHA256 = Mirage_crypto_pk.Rsa.PSS (Digestif.SHA256)

let load_private_key_pem pem_data =
  match X509.Private_key.decode_pem pem_data with
  | Ok (`RSA key) -> key
  | Ok _ -> failwith "Kalshi: private key must be RSA"
  | Error (`Msg msg) -> failwithf "Kalshi: failed to decode private key: %s" msg ()

let load_private_key path = load_private_key_pem (In_channel.read_all path)

(** Strip any query string / fragment: Kalshi signs the path only. *)
let path_without_query path =
  match String.lsplit2 path ~on:'?' with
  | Some (p, _) -> p
  | None -> path

(** The exact string that gets signed. Pure, for testing. *)
let signing_message ~timestamp_ms ~http_method ~path =
  sprintf "%s%s%s" timestamp_ms (String.uppercase http_method) (path_without_query path)

(** Sign an already-built message with an RSA key; returns base64. *)
let sign_message ~(key : Mirage_crypto_pk.Rsa.priv) message =
  match PSS_SHA256.sign ~key (`Message message) with
  | signature -> Base64.encode_exn signature
  | exception exn ->
    failwithf "Kalshi: RSA-PSS signing failed: %s" (Exn.to_string exn) ()

(** Verify a base64 signature (used by tests). *)
let verify_message ~(pub : Mirage_crypto_pk.Rsa.pub) ~signature_b64 message =
  match Base64.decode signature_b64 with
  | Error _ -> false
  | Ok signature -> PSS_SHA256.verify ~key:pub ~signature (`Message message)

let sign_request ~private_key_path ~timestamp_ms ~http_method ~path =
  let key = load_private_key private_key_path in
  sign_message ~key (signing_message ~timestamp_ms ~http_method ~path)

let now_ms_string () =
  Time_float_unix.now ()
  |> Time_float_unix.to_span_since_epoch
  |> Time_float_unix.Span.to_ms
  |> Float.round_down
  |> Int64.of_float
  |> Int64.to_string

let headers (cfg : (module Cfg.S)) ~http_method ~path =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp_ms = now_ms_string () in
  let signature =
    sign_request ~private_key_path:Cfg.private_key_path ~timestamp_ms ~http_method ~path
  in
  Cohttp.Header.of_list
    [ ("KALSHI-ACCESS-KEY", Cfg.api_key)
    ; ("KALSHI-ACCESS-TIMESTAMP", timestamp_ms)
    ; ("KALSHI-ACCESS-SIGNATURE", signature)
    ; ("Content-Type", "application/json")
    ; ("Accept", "application/json") ]

(** Random RFC 4122 version-4 UUID, used as the default client_order_id so
    every order can be found again (and is de-duplicated by the exchange)
    after an ambiguous outcome such as a timeout. *)
let uuid_v4 () =
  let b = Bytes.of_string (Mirage_crypto_rng.generate 16) in
  Bytes.set b 6 (Char.of_int_exn (Char.to_int (Bytes.get b 6) land 0x0f lor 0x40));
  Bytes.set b 8 (Char.of_int_exn (Char.to_int (Bytes.get b 8) land 0x3f lor 0x80));
  let h = String.concat_map (Bytes.to_string b) ~f:(fun c -> sprintf "%02x" (Char.to_int c)) in
  sprintf "%s-%s-%s-%s-%s" (String.sub h ~pos:0 ~len:8) (String.sub h ~pos:8 ~len:4)
    (String.sub h ~pos:12 ~len:4) (String.sub h ~pos:16 ~len:4) (String.sub h ~pos:20 ~len:12)

(** Kalshi RSA-PSS Authentication

    Signs requests using RSA-PSS with SHA-256.
    Signature = sign(timestamp_ms + method + path_without_query) *)

let load_private_key path =
  let pem_data = In_channel.read_all path in
  match X509.Private_key.decode_pem pem_data with
  | Ok (`RSA key) -> key
  | Ok _ -> failwith "Kalshi: private key must be RSA"
  | Error (`Msg msg) -> failwithf "Kalshi: failed to decode private key: %s" msg ()

let sign_request ~private_key_path ~timestamp_ms ~http_method ~path =
  let key = load_private_key private_key_path in
  let message = sprintf "%s%s%s" timestamp_ms (String.uppercase http_method) path in
  match Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message message) with
  | signature -> Base64.encode_exn signature
  | exception exn ->
    failwithf "Kalshi: RSA signing failed: %s" (Exn.to_string exn) ()

let headers (cfg : (module Cfg.S)) ~http_method ~path =
  let module Cfg = (val cfg : Cfg.S) in
  let timestamp_ms =
    Time_float_unix.now ()
    |> Time_float_unix.to_span_since_epoch
    |> Time_float_unix.Span.to_ms
    |> Float.round_down
    |> Int64.of_float
    |> Int64.to_string
  in
  let signature =
    sign_request ~private_key_path:Cfg.private_key_path ~timestamp_ms ~http_method ~path
  in
  Cohttp.Header.of_list
    [ ("KALSHI-ACCESS-KEY", Cfg.api_key)
    ; ("KALSHI-ACCESS-TIMESTAMP", timestamp_ms)
    ; ("KALSHI-ACCESS-SIGNATURE", signature)
    ; ("Content-Type", "application/json")
    ; ("Accept", "application/json") ]

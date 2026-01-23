(** KuCoin Exchange Configuration

    Configuration for KuCoin API access with three-component authentication:
    - API Key
    - API Secret
    - Passphrase (must be encrypted with HMAC-SHA256 for v2+)

    @see <https://www.kucoin.com/docs/basic-info/connection-method/authentication>
*)

open Core

type t = {
  rest_url : string;
  ws_url : string;
  api_key : string option;
  api_secret : string option;
  passphrase : string option;
  api_version : int; (** API key version: 1, 2, or 3 *)
} [@@deriving sexp]

(** Production configuration (spot trading) *)
let production = {
  rest_url = "https://api.kucoin.com";
  ws_url = "wss://ws-api.kucoin.com";
  api_key = None;
  api_secret = None;
  passphrase = None;
  api_version = 2; (* Default to v2 *)
}

(** Futures trading configuration *)
let futures = {
  production with
  rest_url = "https://api-futures.kucoin.com";
}

(** Mainnet alias (same as production) *)
let mainnet = production

(** Add authentication credentials to configuration *)
let with_auth ~api_key ~api_secret ~passphrase ?(api_version = 2) cfg = {
  cfg with
  api_key = Some api_key;
  api_secret = Some api_secret;
  passphrase = Some passphrase;
  api_version;
}

(** Extract API key (raises if not configured) *)
let api_key_exn t =
  match t.api_key with
  | Some key -> key
  | None -> failwith "API key not configured"

(** Extract API secret (raises if not configured) *)
let api_secret_exn t =
  match t.api_secret with
  | Some secret -> secret
  | None -> failwith "API secret not configured"

(** Extract passphrase (raises if not configured) *)
let passphrase_exn t =
  match t.passphrase with
  | Some pass -> pass
  | None -> failwith "Passphrase not configured"

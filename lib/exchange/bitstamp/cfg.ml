(** Bitstamp Exchange Configuration

    Bitstamp is a European cryptocurrency exchange founded in 2011.
    Known for reliability and regulatory compliance.

    @see <https://www.bitstamp.net/api/>
*)

open Core

(** Configuration for Bitstamp API access *)
type t = {
  rest_url : string;        (** REST API base URL *)
  ws_url : string;          (** WebSocket API URL *)
  api_key : string option;  (** API key for authenticated requests *)
  api_secret : string option; (** API secret for signing *)
  customer_id : string option; (** Customer ID for authentication *)
}
[@@deriving sexp, fields]

(** Production Bitstamp API configuration *)
let production = {
  rest_url = "https://www.bitstamp.net/api/v2";
  ws_url = "wss://ws.bitstamp.net";
  api_key = None;
  api_secret = None;
  customer_id = None;
}

(** Alias for production *)
let mainnet = production

(** Create configuration with authentication credentials *)
let with_auth ~api_key ~api_secret ~customer_id cfg = {
  cfg with
  api_key = Some api_key;
  api_secret = Some api_secret;
  customer_id = Some customer_id;
}

(** Get API key, fail if not configured *)
let api_key_exn t =
  match t.api_key with
  | Some key -> key
  | None -> failwith "Bitstamp API key not configured"

(** Get API secret, fail if not configured *)
let api_secret_exn t =
  match t.api_secret with
  | Some secret -> secret
  | None -> failwith "Bitstamp API secret not configured"

(** Get customer ID, fail if not configured *)
let customer_id_exn t =
  match t.customer_id with
  | Some id -> id
  | None -> failwith "Bitstamp customer ID not configured"

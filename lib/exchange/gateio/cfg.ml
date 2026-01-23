(** Gate.io Exchange Configuration

    Configuration for Gate.io API access.

    @see <https://www.gate.com/docs/developers/apiv4/>
*)

open Core

type t = {
  rest_url : string;
  api_key : string option;
  api_secret : string option;
} [@@deriving sexp]

(** Production configuration *)
let production = {
  rest_url = "https://api.gateio.ws/api/v4";
  api_key = None;
  api_secret = None;
}

(** Mainnet alias *)
let mainnet = production

(** Add authentication *)
let with_auth ~api_key ~api_secret cfg = {
  cfg with
  api_key = Some api_key;
  api_secret = Some api_secret;
}

let api_key_exn t =
  match t.api_key with
  | Some key -> key
  | None -> failwith "API key not configured"

let api_secret_exn t =
  match t.api_secret with
  | Some secret -> secret
  | None -> failwith "API secret not configured"

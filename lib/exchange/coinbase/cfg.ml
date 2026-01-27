(** Coinbase Advanced Trade API Configuration *)

open Core

(** Coinbase endpoints *)
module Endpoint = struct
  let rest_url = "https://api.coinbase.com"
  let ws_url = "wss://advanced-trade-ws.coinbase.com"
end

(** Configuration module signature *)
module type S = sig
  val api_key : string
  val api_secret : string
  val rest_url : string
  val ws_url : string
end

(** Get required environment variable with descriptive error *)
let require_env name =
  match Sys.getenv name with
  | Some v -> v
  | None -> failwith (sprintf "Missing required environment variable: %s" name)

(** Production configuration - requires COINBASE_API_KEY and COINBASE_API_SECRET *)
let production () : (module S) =
  (module struct
    let api_key = require_env "COINBASE_API_KEY"
    let api_secret = require_env "COINBASE_API_SECRET"
    let rest_url = Endpoint.rest_url
    let ws_url = Endpoint.ws_url
  end)

(** Production configuration module (for public endpoints - no auth required) *)
module Production : S = struct
  let api_key = Option.value ~default:"" (Sys.getenv "COINBASE_API_KEY")
  let api_secret = Option.value ~default:"" (Sys.getenv "COINBASE_API_SECRET")
  let rest_url = Endpoint.rest_url
  let ws_url = Endpoint.ws_url
end

(** Sandbox/testnet configuration - requires COINBASE_SANDBOX_API_KEY and COINBASE_SANDBOX_API_SECRET *)
let sandbox () : (module S) =
  (module struct
    let api_key = require_env "COINBASE_SANDBOX_API_KEY"
    let api_secret = require_env "COINBASE_SANDBOX_API_SECRET"
    let rest_url = "https://api-public.sandbox.pro.coinbase.com"
    let ws_url = "wss://ws-feed-public.sandbox.pro.coinbase.com"
  end)

let of_string = function
  | "production" -> production ()
  | "sandbox" -> sandbox ()
  | env -> failwith (sprintf "Unknown Coinbase environment: %s" env)

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

(** Production configuration *)
let production () : (module S) =
  (module struct
    let api_key = Sys.getenv_exn "COINBASE_API_KEY"
    let api_secret = Sys.getenv_exn "COINBASE_API_SECRET"
    let rest_url = Endpoint.rest_url
    let ws_url = Endpoint.ws_url
  end)

(** Sandbox/testnet configuration *)
let sandbox () : (module S) =
  (module struct
    let api_key = Sys.getenv_exn "COINBASE_SANDBOX_API_KEY"
    let api_secret = Sys.getenv_exn "COINBASE_SANDBOX_API_SECRET"
    let rest_url = "https://api-public.sandbox.pro.coinbase.com"
    let ws_url = "wss://ws-feed-public.sandbox.pro.coinbase.com"
  end)

let of_string = function
  | "production" -> production ()
  | "sandbox" -> sandbox ()
  | env -> failwith (sprintf "Unknown Coinbase environment: %s" env)

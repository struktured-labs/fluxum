(** Bitrue Exchange Configuration *)

open Core

(** Bitrue endpoints *)
module Endpoint = struct
  let rest_url = "https://api.bitrue.com"
  let ws_market = "wss://ws.bitrue.com/market/ws"
  let ws_user = "wss://wsapi.bitrue.com"
end

(** Configuration module signature *)
module type S = sig
  val api_key : string
  val api_secret : string
  val rest_url : string
  val ws_market_url : string
  val ws_user_url : string
end

(** Production configuration *)
let production : (module S) =
  (module struct
    let api_key = Sys.getenv_exn "BITRUE_API_KEY"
    let api_secret = Sys.getenv_exn "BITRUE_API_SECRET"
    let rest_url = Endpoint.rest_url
    let ws_market_url = Endpoint.ws_market
    let ws_user_url = Endpoint.ws_user
  end)

let of_string = function
  | "production" -> production
  | env -> failwith (sprintf "Unknown Bitrue environment: %s" env)

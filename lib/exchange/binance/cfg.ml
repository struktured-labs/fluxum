(** Binance Configuration *)

open Core

module type S = sig
  val api_key : string
  val api_secret : string
  val base_url : string
  val ws_url : string
end

let production : (module S) =
  (module struct
    let api_key = Sys.getenv_exn "BINANCE_API_KEY"
    let api_secret = Sys.getenv_exn "BINANCE_API_SECRET"
    let base_url = "https://api.binance.com"
    let ws_url = "wss://stream.binance.com:443"
  end)

let testnet : (module S) =
  (module struct
    let api_key = Sys.getenv_exn "BINANCE_TESTNET_API_KEY"
    let api_secret = Sys.getenv_exn "BINANCE_TESTNET_API_SECRET"
    let base_url = "https://testnet.binance.vision"
    let ws_url = "wss://testnet.binance.vision"
  end)

let of_string = function
  | "production" -> production
  | "testnet" -> testnet
  | env -> failwith (sprintf "Unknown Binance environment: %s" env)

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
    let api_key = Sys.getenv "BINANCE_API_KEY" |> Option.value ~default:""
    let api_secret = Sys.getenv "BINANCE_API_SECRET" |> Option.value ~default:""
    let base_url = "api.binance.com"
    let ws_url = "wss://stream.binance.com:443"
  end)

(** Binance.US configuration - for US-based users *)
let production_us : (module S) =
  (module struct
    let api_key = Sys.getenv "BINANCE_US_API_KEY" |> Option.value ~default:""
    let api_secret = Sys.getenv "BINANCE_US_API_SECRET" |> Option.value ~default:""
    let base_url = "api.binance.us"
    let ws_url = "wss://stream.binance.us:9443"
  end)

let testnet : (module S) =
  (module struct
    let api_key = Sys.getenv "BINANCE_TESTNET_API_KEY" |> Option.value ~default:""
    let api_secret = Sys.getenv "BINANCE_TESTNET_API_SECRET" |> Option.value ~default:""
    let base_url = "testnet.binance.vision"
    let ws_url = "wss://testnet.binance.vision"
  end)

let of_string = function
  | "production" -> production
  | "production_us" | "us" -> production_us
  | "testnet" -> testnet
  | env -> failwith (sprintf "Unknown Binance environment: %s" env)

let arg_type = Command.Arg_type.create of_string

let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:"STRING Binance environment (production|testnet)")

let or_default cfg =
  match cfg with
  | Some cfg -> cfg
  | None -> of_string "production"

(** Jupiter Solana DEX Aggregator Configuration *)

open Core

module type S = sig
  val api_url : string
  val api_key : string option
end

module Production = struct
  let api_url = "https://api.jup.ag/swap/v1"
  let api_key = Sys.getenv "JUPITER_API_KEY"
end

module Testnet = struct
  let api_url = "https://api.jup.ag/swap/v1"  (* Jupiter uses same endpoint *)
  let api_key = Sys.getenv "JUPITER_API_KEY"
end

let or_default cfg_opt =
  Option.value cfg_opt ~default:(module Production : S)

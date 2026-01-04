(** 1inch EVM DEX Aggregator Configuration *)

open Core

module type S = sig
  val api_url : string
  val api_key : string option
  val chain_id : int  (** EVM chain ID: 1=Ethereum, 56=BSC, 137=Polygon, 42161=Arbitrum *)
end

(** Ethereum Mainnet *)
module Ethereum = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/1"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 1
end

(** BNB Smart Chain *)
module Bsc = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/56"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 56
end

(** Polygon *)
module Polygon = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/137"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 137
end

(** Arbitrum One *)
module Arbitrum = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/42161"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 42161
end

(** Optimism *)
module Optimism = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/10"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 10
end

(** Base *)
module Base = struct
  let api_url = "https://api.1inch.dev/swap/v6.0/8453"
  let api_key = Sys.getenv "ONEINCH_API_KEY"
  let chain_id = 8453
end

let or_default cfg_opt =
  Option.value cfg_opt ~default:(module Ethereum : S)

(** Create config for any chain *)
let of_chain_id chain_id =
  let api_key = Sys.getenv "ONEINCH_API_KEY" in
  (module struct
    let api_url = sprintf "https://api.1inch.dev/swap/v6.0/%d" chain_id
    let api_key = api_key
    let chain_id = chain_id
  end : S)

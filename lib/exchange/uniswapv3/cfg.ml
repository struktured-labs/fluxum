(** Uniswap V3 DEX Configuration

    @see <https://docs.uniswap.org/api/subgraph/overview>
*)

open Core

type t = {
  subgraph_url : string;
  api_key : string option;
  rpc_url : string;
  chain_id : int;
  wallet_address : string option;
  private_key_hex : string option;
  swap_router_address : string;
} [@@deriving sexp]

let ethereum_mainnet = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v3";
  api_key = None;
  rpc_url = "https://eth-mainnet.g.alchemy.com/v2/demo";
  chain_id = 1;
  wallet_address = None;
  private_key_hex = None;
  swap_router_address = "0x68b3465833fb72A70ecDF485E0e4C7bD8665Fc45";
}

let production = ethereum_mainnet

let polygon = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/uniswap-v3-polygon";
  api_key = None;
  rpc_url = "https://polygon-rpc.com";
  chain_id = 137;
  wallet_address = None;
  private_key_hex = None;
  swap_router_address = "0x68b3465833fb72A70ecDF485E0e4C7bD8665Fc45";
}

let arbitrum = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/arbitrum-minimal";
  api_key = None;
  rpc_url = "https://arb1.arbitrum.io/rpc";
  chain_id = 42161;
  wallet_address = None;
  private_key_hex = None;
  swap_router_address = "0x68b3465833fb72A70ecDF485E0e4C7bD8665Fc45";
}

let optimism = {
  subgraph_url = "https://api.thegraph.com/subgraphs/name/ianlapham/optimism-post-regenesis";
  api_key = None;
  rpc_url = "https://mainnet.optimism.io";
  chain_id = 10;
  wallet_address = None;
  private_key_hex = None;
  swap_router_address = "0x68b3465833fb72A70ecDF485E0e4C7bD8665Fc45";
}

let with_api_key ~api_key cfg = {
  cfg with api_key = Some api_key;
}

let api_key_opt t = t.api_key

(** Create config from environment variables *)
let of_env ?(base = ethereum_mainnet) () =
  let env_or key default = Option.value (Sys.getenv key) ~default in
  { subgraph_url = env_or "UNISWAP_SUBGRAPH_URL" base.subgraph_url;
    api_key = Sys.getenv "UNISWAP_API_KEY";
    rpc_url = env_or "UNISWAP_RPC_URL" base.rpc_url;
    chain_id = (match Sys.getenv "UNISWAP_CHAIN_ID" with
      | Some s -> Int.of_string s
      | None -> base.chain_id);
    wallet_address = Sys.getenv "UNISWAP_WALLET_ADDRESS";
    private_key_hex = Sys.getenv "UNISWAP_PRIVATE_KEY";
    swap_router_address = env_or "UNISWAP_SWAP_ROUTER" base.swap_router_address;
  }

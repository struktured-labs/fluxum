(** Hyperliquid Configuration *)

(** Hyperliquid endpoints *)
module Endpoint = struct
  let mainnet_rest = "https://api.hyperliquid.xyz"
  let mainnet_ws = "wss://api.hyperliquid.xyz/ws"
  let testnet_rest = "https://api.hyperliquid-testnet.xyz"
  let testnet_ws = "wss://api.hyperliquid-testnet.xyz/ws"
end

type t = {
  rest_url : string;
  ws_url : string;
  wallet_address : string option;  (* For authenticated requests *)
}

let mainnet =
  { rest_url = Endpoint.mainnet_rest
  ; ws_url = Endpoint.mainnet_ws
  ; wallet_address = None
  }

let testnet =
  { rest_url = Endpoint.testnet_rest
  ; ws_url = Endpoint.testnet_ws
  ; wallet_address = None
  }

let with_wallet cfg wallet_address =
  { cfg with wallet_address = Some wallet_address }

let production = mainnet

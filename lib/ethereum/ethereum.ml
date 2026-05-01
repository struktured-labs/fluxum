(** Ethereum Interaction Library

    Reusable primitives for Ethereum blockchain interaction:

    - {!Rpc}: JSON-RPC client for standard Ethereum methods
    - {!Abi}: Solidity ABI encoding for function calls
    - {!Rlp}: Recursive Length Prefix encoding
    - {!Tx}: EIP-1559 transaction builder and signer
    - {!Erc20}: ERC-20 token operations
    - {!Multicall3}: Atomic batched eth_call via the canonical Multicall3 contract
    - {!Wallet}: Read-only wallet introspection (balances, recent transfers) *)

module Rpc = Rpc
module Abi = Abi
module Rlp = Rlp
module Tx = Tx
module Erc20 = Erc20
module Multicall3 = Multicall3
module Wallet = Wallet

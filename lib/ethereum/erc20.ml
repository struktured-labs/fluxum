(** ERC-20 Token Operations

    Standard ERC-20 token contract interactions via JSON-RPC.
    Provides read operations (balance, allowance) and calldata
    builders for write operations (approve, transfer).

    @see <https://eips.ethereum.org/EIPS/eip-20>
*)

open Core

(** Local Ethereum RPC module - aliased before opening Async which shadows Rpc *)
module Eth_rpc = Rpc

open Async

(** ERC-20 function selectors (first 4 bytes of keccak256 of signature) *)
let _balance_of_selector = Abi.function_selector ~signature:"balanceOf(address)"
let _allowance_selector = Abi.function_selector ~signature:"allowance(address,address)"
let _approve_selector = Abi.function_selector ~signature:"approve(address,uint256)"
let _transfer_selector = Abi.function_selector ~signature:"transfer(address,uint256)"
let _decimals_selector = Abi.function_selector ~signature:"decimals()"
let _symbol_selector = Abi.function_selector ~signature:"symbol()"

(** Query ERC-20 token balance for an address *)
let balance_of ~rpc_url ~token ~owner =
  let data = Abi.encode_function_call
    ~selector:_balance_of_selector
    ~args:[Address owner]
  in
  Eth_rpc.eth_call ~rpc_url ~to_:token ~data >>| (function
  | Ok hex_result -> Ok hex_result  (* Returns uint256 as hex *)
  | Error e -> Error e)

(** Query ERC-20 allowance: how much spender can spend of owner's tokens *)
let allowance ~rpc_url ~token ~owner ~spender =
  let data = Abi.encode_function_call
    ~selector:_allowance_selector
    ~args:[Address owner; Address spender]
  in
  Eth_rpc.eth_call ~rpc_url ~to_:token ~data >>| (function
  | Ok hex_result -> Ok hex_result
  | Error e -> Error e)

(** Build calldata for ERC-20 approve(spender, amount) *)
let approve_calldata ~spender ~amount : string =
  Abi.encode_function_call
    ~selector:_approve_selector
    ~args:[Address spender; Uint256 amount]

(** Build calldata for ERC-20 transfer(to, amount) *)
let transfer_calldata ~to_ ~amount : string =
  Abi.encode_function_call
    ~selector:_transfer_selector
    ~args:[Address to_; Uint256 amount]

(** Query token decimals *)
let decimals ~rpc_url ~token =
  let data = Abi.encode_function_call
    ~selector:_decimals_selector
    ~args:[]
  in
  Eth_rpc.eth_call ~rpc_url ~to_:token ~data >>| (function
  | Ok hex_result -> Ok (Eth_rpc.hex_to_int hex_result)
  | Error e -> Error e)

(** Build calldata for unlimited approval (max uint256) *)
let approve_unlimited_calldata ~spender : string =
  approve_calldata ~spender ~amount:Abi.max_uint256

(** Check if allowance is sufficient, and if not, build approve calldata *)
let ensure_allowance ~rpc_url ~token ~owner ~spender ~required_amount =
  let%bind current = allowance ~rpc_url ~token ~owner ~spender in
  match current with
  | Error e -> return (Error e)
  | Ok current_hex ->
    (* Compare as strings - both are 64-char hex, zero-padded *)
    let current_stripped = match String.is_prefix current_hex ~prefix:"0x" with
      | true -> String.drop_prefix current_hex 2
      | false -> current_hex
    in
    let required_padded = Abi.pad_left_32 required_amount in
    match String.( >= ) current_stripped required_padded with
    | true -> return (Ok None)  (* Sufficient allowance *)
    | false -> return (Ok (Some (approve_unlimited_calldata ~spender)))

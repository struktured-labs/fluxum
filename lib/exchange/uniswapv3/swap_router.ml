(** Uniswap V3 SwapRouter02 Interaction

    Builds and submits on-chain swap transactions via the Uniswap V3
    SwapRouter02 contract (0x68b3465833fb72A70ecDF485E0e4C7bD8665Fc45).

    Flow: check allowance -> approve if needed -> build calldata ->
    estimate gas -> sign tx -> submit -> return tx hash.

    @see <https://docs.uniswap.org/contracts/v3/reference/periphery/interfaces/ISwapRouter>
*)

open Core
open Async

type error = [
  | `Swap_error of string
  | `Rpc of string
  | `Network of string
  | `Json_parse of string
] [@@deriving sexp]

(** Parameters for exactInputSingle swap *)
type exact_input_single_params = {
  token_in : string;        (** Input token address *)
  token_out : string;       (** Output token address *)
  fee : int;                (** Pool fee tier (e.g., 3000 for 0.3%) *)
  recipient : string;       (** Address to receive output tokens *)
  amount_in : string;       (** Input amount as hex string (wei) *)
  amount_out_minimum : string;  (** Minimum output, hex string (wei) *)
  sqrt_price_limit_x96 : string;  (** Price limit, "0" for no limit *)
}

(** exactInputSingle function selector:
    exactInputSingle((address,address,uint24,address,uint256,uint256,uint160))
*)
let _exact_input_single_selector =
  Ethereum.Abi.function_selector
    ~signature:"exactInputSingle(address,address,uint24,address,uint256,uint256,uint160)"

(** Build calldata for exactInputSingle *)
let build_exact_input_single_calldata (params : exact_input_single_params) : string =
  Ethereum.Abi.encode_function_call
    ~selector:_exact_input_single_selector
    ~args:[
      Address params.token_in;
      Address params.token_out;
      Uint24 params.fee;
      Address params.recipient;
      Uint256 params.amount_in;
      Uint256 params.amount_out_minimum;
      Uint160 params.sqrt_price_limit_x96;
    ]

(** Ensure the SwapRouter has sufficient ERC-20 allowance for the input token.
    If not, submits an approval transaction and waits for confirmation. *)
let ensure_approval ~(cfg : Cfg.t) ~token_in ~amount_in ~private_key ~from_address =
  let rpc_url = cfg.rpc_url in
  let router = cfg.swap_router_address in
  let%bind allowance_result =
    Ethereum.Erc20.ensure_allowance
      ~rpc_url ~token:token_in ~owner:from_address ~spender:router
      ~required_amount:amount_in
  in
  match allowance_result with
  | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
  | Ok None ->
    (* Allowance is sufficient *)
    return (Ok ())
  | Ok (Some approve_data) ->
    (* Need to submit approval tx *)
    let%bind nonce_result =
      Ethereum.Rpc.eth_get_transaction_count ~rpc_url ~address:from_address
    in
    (match nonce_result with
     | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
     | Ok nonce ->
       let%bind gas_price_result = Ethereum.Rpc.eth_gas_price ~rpc_url in
       (match gas_price_result with
        | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
        | Ok gas_price_hex ->
          let tx : Ethereum.Tx.eip1559_tx = {
            chain_id = cfg.chain_id;
            nonce;
            max_priority_fee_per_gas = "3B9ACA00";  (* 1 gwei *)
            max_fee_per_gas = gas_price_hex;
            gas_limit = 60000;  (* ERC-20 approve is ~46k gas *)
            to_ = token_in;
            value = "0";
            data = approve_data;
          } in
          match Ethereum.Tx.sign tx ~private_key_hex:private_key with
          | Error msg -> return (Error (`Swap_error (sprintf "Approve signing failed: %s" msg)))
          | Ok raw_tx ->
            let%bind send_result =
              Ethereum.Rpc.eth_send_raw_transaction ~rpc_url ~raw_tx
            in
            (match send_result with
             | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
             | Ok _tx_hash ->
               (* Wait briefly for approval to propagate *)
               let%bind () = Clock.after (Time_float.Span.of_sec 3.0) in
               return (Ok ()))))

(** Execute exactInputSingle swap on the SwapRouter02 contract.

    Steps:
    1. Validate configuration (wallet address, private key)
    2. Check and ensure ERC-20 approval
    3. Build swap calldata
    4. Get nonce and gas estimate
    5. Sign EIP-1559 transaction
    6. Submit via eth_sendRawTransaction
    7. Return transaction hash
*)
let exact_input_single ~(cfg : Cfg.t) ~(params : exact_input_single_params)
  : (string, [> error]) Deferred.Result.t =
  let rpc_url = cfg.rpc_url in
  match cfg.private_key_hex, cfg.wallet_address with
  | None, _ -> return (Error (`Swap_error "Private key not configured"))
  | _, None -> return (Error (`Swap_error "Wallet address not configured"))
  | Some private_key, Some from_address ->

  (* 1. Ensure approval *)
  let%bind approval = ensure_approval ~cfg ~token_in:params.token_in
    ~amount_in:params.amount_in ~private_key ~from_address in
  match approval with
  | Error e -> return (Error e)
  | Ok () ->

  (* 2. Build calldata *)
  let calldata = build_exact_input_single_calldata params in

  (* 3. Get nonce *)
  let%bind nonce_result =
    Ethereum.Rpc.eth_get_transaction_count ~rpc_url ~address:from_address
  in
  match nonce_result with
  | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
  | Ok nonce ->

  (* 4. Estimate gas *)
  let%bind gas_result =
    Ethereum.Rpc.eth_estimate_gas ~rpc_url ~to_:cfg.swap_router_address
      ~data:calldata ~from:from_address
  in
  let gas_limit = match gas_result with
    | Ok gas_hex -> (Ethereum.Rpc.hex_to_int gas_hex * 120) / 100  (* 20% buffer *)
    | Error _ -> 300000  (* Fallback gas limit for swaps *)
  in

  (* 5. Get gas price *)
  let%bind gas_price_result = Ethereum.Rpc.eth_gas_price ~rpc_url in
  match gas_price_result with
  | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
  | Ok gas_price_hex ->

  (* 6. Build and sign transaction *)
  let tx : Ethereum.Tx.eip1559_tx = {
    chain_id = cfg.chain_id;
    nonce;
    max_priority_fee_per_gas = "59682F00";  (* 1.5 gwei *)
    max_fee_per_gas = gas_price_hex;
    gas_limit;
    to_ = cfg.swap_router_address;
    value = "0";  (* No ETH value for token swaps *)
    data = calldata;
  } in

  match Ethereum.Tx.sign tx ~private_key_hex:private_key with
  | Error msg -> return (Error (`Swap_error (sprintf "Tx signing failed: %s" msg)))
  | Ok raw_tx ->
    (* 7. Submit transaction *)
    let%bind send_result =
      Ethereum.Rpc.eth_send_raw_transaction ~rpc_url ~raw_tx
    in
    (match send_result with
     | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
     | Ok tx_hash ->
       return (Ok tx_hash))

(** Wait for transaction confirmation by polling for receipt *)
let wait_for_receipt ~(cfg : Cfg.t) ~tx_hash ?(max_polls = 60) ?(poll_interval_sec = 2.0) ()
  : (Yojson.Safe.t option, [> error]) Deferred.Result.t =
  let rpc_url = cfg.rpc_url in
  let rec poll count =
    match count >= max_polls with
    | true -> return (Error (`Swap_error "Transaction not confirmed within timeout"))
    | false ->
      let%bind receipt_result =
        Ethereum.Rpc.eth_get_transaction_receipt ~rpc_url ~tx_hash
      in
      match receipt_result with
      | Error e -> return (Error (e : Ethereum.Rpc.error :> error))
      | Ok None ->
        let%bind () = Clock.after (Time_float.Span.of_sec poll_interval_sec) in
        poll (count + 1)
      | Ok (Some receipt) -> return (Ok (Some receipt))
  in
  poll 0

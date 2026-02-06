(** Hyperliquid EIP-712 Signing Infrastructure

    Implements Ethereum-style EIP-712 structured data signing for Hyperliquid L1 actions.

    Architecture:
    - EIP-712 domain with chainId 1337 (Hyperliquid's phantom agent chain)
    - Keccak-256 hashing (Ethereum-compatible)
    - msgpack serialization for L1 actions
    - secp256k1 ECDSA signing
    - Phantom agent construction for privacy

    References:
    - https://eips.ethereum.org/EIPS/eip-712
    - https://hyperliquid.gitbook.io/hyperliquid-docs/for-developers/api/signing
*)

open Core

(** Hyperliquid EIP-712 Domain Constants *)
module Domain = struct
  (** EIP-712 domain for Hyperliquid L1 actions *)
  type t = {
    name : string;
    version : string;
    chain_id : int;
    verifying_contract : string;
  }

  (** Hyperliquid's L1 domain uses chainId 1337 for phantom agent construction *)
  let hyperliquid_l1 = {
    name = "Exchange";
    version = "1";
    chain_id = 1337;
    verifying_contract = "0x0000000000000000000000000000000000000000";
  }

  (** EIP-712 domain separator hash

      domainSeparator = keccak256(
        abi.encode(
          keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"),
          keccak256(bytes(name)),
          keccak256(bytes(version)),
          chainId,
          verifyingContract
        )
      )
  *)
  let domain_separator (domain : t) : string =
    (* EIP712Domain type hash *)
    let type_hash =
      "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"
      |> Digestif.KECCAK_256.digest_string
      |> Digestif.KECCAK_256.to_raw_string
    in

    (* Hash domain fields *)
    let name_hash =
      domain.name
      |> Digestif.KECCAK_256.digest_string
      |> Digestif.KECCAK_256.to_raw_string
    in

    let version_hash =
      domain.version
      |> Digestif.KECCAK_256.digest_string
      |> Digestif.KECCAK_256.to_raw_string
    in

    (* Chain ID as 32-byte big-endian *)
    let chain_id_bytes =
      let buf = Bytes.make 32 '\x00' in
      Stdlib.Bytes.set_int64_be buf 24 (Int64.of_int domain.chain_id);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
    in

    (* Verifying contract address (already 20 bytes, pad to 32) *)
    let contract_bytes =
      (* Remove 0x prefix if present *)
      let addr =
        if String.is_prefix domain.verifying_contract ~prefix:"0x"
        then String.drop_prefix domain.verifying_contract 2
        else domain.verifying_contract
      in
      (* Convert hex to bytes and pad *)
      let addr_bytes = Hex.to_string (`Hex addr) in
      let padding = String.make (32 - String.length addr_bytes) '\x00' in
      padding ^ addr_bytes
    in

    (* Concatenate all components and hash *)
    let combined = type_hash ^ name_hash ^ version_hash ^ chain_id_bytes ^ contract_bytes in
    let hash = Digestif.KECCAK_256.digest_string combined in
    Digestif.KECCAK_256.to_hex hash
end

(** Keccak-256 utilities *)
module Keccak = struct
  (** Compute Keccak-256 hash of a string *)
  let hash_string (s : string) : string =
    let digest = Digestif.KECCAK_256.digest_string s in
    Digestif.KECCAK_256.to_hex digest

  (** Compute Keccak-256 hash of bytes *)
  let hash_bytes (b : bytes) : string =
    let s = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b in
    hash_string s
end

(** Ethereum address utilities *)
module Address = struct
  (** Derive Ethereum address from secp256k1 public key

      Address is the last 20 bytes of Keccak-256(public_key)
  *)
  let from_public_key (ctx : Secp256k1.Context.t) (pubkey : Secp256k1.Key.public Secp256k1.Key.t) : string =
    (* Get uncompressed public key (65 bytes: 0x04 + 32-byte x + 32-byte y) *)
    let pubkey_buf = Secp256k1.Key.to_bytes ctx ~compress:false pubkey in

    (* Convert Bigarray to bytes and extract the uncompressed key data (skip 0x04 prefix) *)
    let pubkey_bytes = Bytes.create 64 in
    for i = 0 to 63 do
      Bytes.set pubkey_bytes i (Bigarray.Array1.get pubkey_buf (i + 1))
    done;

    (* Hash with Keccak-256 *)
    let hash = Digestif.KECCAK_256.digest_bytes pubkey_bytes in
    let hash_bytes = Digestif.KECCAK_256.to_raw_string hash in

    (* Take last 20 bytes *)
    let address_bytes = String.suffix hash_bytes 20 in

    (* Convert to hex with 0x prefix *)
    "0x" ^ (Hex.of_string address_bytes |> Hex.show)

  (** Normalize Ethereum address to lowercase with 0x prefix *)
  let normalize (addr : string) : string =
    let addr = String.lowercase addr in
    if String.is_prefix addr ~prefix:"0x"
    then addr
    else "0x" ^ addr
end

(** Order types for L1 actions **)
type order_request = {
  asset : int;
  is_buy : bool;
  limit_px : string;
  sz : string;
  reduce_only : bool;
  time_in_force : string;  (* "Alo", "Ioc", "Gtc" *)
  cloid : string option;   (* Client order ID *)
}

type cancel_request = {
  asset : int;
  oid : int64;
}

(** Withdraw request for withdraw3 action *)
type withdraw_request = {
  hyperliquid_chain : string;  (** "Mainnet" or "Testnet" *)
  signature_chain_id : string; (** Hex format, e.g., "0xa4b1" for Arbitrum *)
  destination : string;        (** 42-char hex address *)
  amount : string;             (** USD amount as string *)
  time : int64;                (** Timestamp in milliseconds *)
}

(** msgpack serialization for L1 actions

    Field order is CRITICAL for correct hash computation.
    Must match exact order expected by Hyperliquid.
*)
module Msgpack = struct
  open Msgpck

  (** Serialize order placement action to msgpack

      Field order matters! Must be:
      1. orders array
      2. grouping string
  *)
  let serialize_order_action ~(orders : order_request list) ~grouping : bytes =
    (* Serialize each order *)
    let order_items = List.map orders ~f:(fun (order : order_request) ->
      (* Order fields in exact order *)
      let base_fields = [
        (String "a", Int order.asset);
        (String "b", Bool order.is_buy);
        (String "p", String order.limit_px);
        (String "s", String order.sz);
        (String "r", Bool order.reduce_only);
        (String "t", Map [
          (String "limit", Map [
            (String "tif", String order.time_in_force)
          ])
        ]);
      ] in
      (* Add cloid if present *)
      let fields_with_cloid = match order.cloid with
        | Some cloid -> base_fields @ [(String "c", String cloid)]
        | None -> base_fields @ [(String "c", Nil)]
      in
      Map fields_with_cloid
    ) in

    (* Top-level action *)
    let action = Map [
      (String "orders", List order_items);
      (String "grouping", String grouping);
    ] in

    Msgpck.String.to_string action

  (** Serialize cancel action to msgpack *)
  let serialize_cancel_action ~(cancels : cancel_request list) : bytes =
    let cancel_items = List.map cancels ~f:(fun (cancel : cancel_request) ->
      Map [
        (String "a", Int cancel.asset);
        (String "o", Int64 cancel.oid);
      ]
    ) in

    let action = Map [
      (String "cancels", List cancel_items);
    ] in

    Msgpck.String.to_string action

  (** Serialize withdraw3 action to msgpack

      Field order for withdraw3:
      - hyperliquidChain
      - signatureChainId
      - destination
      - amount
      - time
  *)
  let serialize_withdraw_action ~(req : withdraw_request) : bytes =
    let action = Map [
      (String "hyperliquidChain", String req.hyperliquid_chain);
      (String "signatureChainId", String req.signature_chain_id);
      (String "destination", String req.destination);
      (String "amount", String req.amount);
      (String "time", Int64 req.time);
    ] in
    Msgpck.String.to_string action
end

(** Phantom agent construction

    For privacy, Hyperliquid uses a "phantom agent" - a temporary signing identity
    derived from the hash of the action itself. This prevents linking on-chain
    transactions to the user's main address.
*)
module PhantomAgent = struct
  (** Construct phantom agent hash from action data

      phantom_agent_hash = keccak256(connection_id || action_hash)
  *)
  let construct ~connection_id ~action_hash : string =
    (* Connection ID is typically the user's wallet address *)
    let conn_bytes =
      let addr = Address.normalize connection_id in
      let hex_str = String.drop_prefix addr 2 in
      Hex.to_string (`Hex hex_str)
    in

    (* Action hash is keccak256 of msgpack serialized action *)
    let action_bytes = Hex.to_string (`Hex action_hash) in

    (* Concatenate and hash *)
    let combined = conn_bytes ^ action_bytes in
    Keccak.hash_string combined
end

(** EIP-712 signature creation *)
module Signature = struct
  (** Sign EIP-712 structured data

      digest = keccak256("\x19\x01" || domainSeparator || structHash)
  *)
  let create_digest ~domain_separator ~struct_hash : string =
    (* EIP-712 prefix *)
    let prefix = "\x19\x01" in

    (* Convert hashes from hex to bytes *)
    let domain_bytes = Hex.to_string (`Hex domain_separator) in
    let struct_bytes = Hex.to_string (`Hex struct_hash) in

    (* Combine and hash *)
    let combined = prefix ^ domain_bytes ^ struct_bytes in
    Keccak.hash_string combined

  (** Convert bytes to Bigarray buffer *)
  let bytes_to_buffer (b : bytes) : Secp256k1.buffer =
    let len = Bytes.length b in
    let buf = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout len in
    for i = 0 to len - 1 do
      Bigarray.Array1.set buf i (Bytes.get b i)
    done;
    buf

  (** Sign digest with private key using secp256k1 ECDSA *)
  let sign ~private_key_hex ~digest_hex : (string, string) Result.t =
    try
      (* Create secp256k1 context *)
      let ctx = Secp256k1.Context.create [Sign] in

      (* Parse private key *)
      let privkey_str = Hex.to_string (`Hex private_key_hex) in
      let privkey_bytes = Bytes.of_string privkey_str in
      let privkey_buf = bytes_to_buffer privkey_bytes in
      let privkey = Secp256k1.Key.read_sk_exn ctx privkey_buf in

      (* Parse digest (32 bytes) *)
      let digest_str = Hex.to_string (`Hex digest_hex) in
      let digest_bytes = Bytes.of_string digest_str in
      let digest_buf = bytes_to_buffer digest_bytes in
      let msg = Secp256k1.Sign.msg_of_bytes_exn digest_buf in

      (* Sign with recoverable signature *)
      let rec_sig = Secp256k1.Sign.sign_recoverable_exn ctx ~sk:privkey msg in

      (* Get signature bytes and recovery ID *)
      let sig_bytes, recovery_id = Secp256k1.Sign.to_bytes_recid ctx rec_sig in

      (* Ethereum signature format: r (32) + s (32) + v (1) *)
      (* v = recovery_id + 27 for Ethereum *)
      let v = recovery_id + 27 in

      (* Convert sig_bytes buffer to bytes *)
      let sig_buf_len = Bigarray.Array1.dim sig_bytes in
      let sig_b = Bytes.create sig_buf_len in
      for i = 0 to sig_buf_len - 1 do
        Bytes.set sig_b i (Bigarray.Array1.get sig_bytes i)
      done;

      let v_byte = Bytes.create 1 in
      Bytes.set v_byte 0 (Char.of_int_exn v);

      (* Concatenate signature bytes and v byte *)
      let sig_str = Bytes.to_string sig_b in
      let v_str = Bytes.to_string v_byte in
      let full_sig_str = sig_str ^ v_str in
      Ok (Hex.of_string full_sig_str |> Hex.show)

    with e ->
      Error (sprintf "Signing failed: %s" (Exn.to_string e))
end

(** High-level signing interface *)

(** Sign L1 order placement action *)
let sign_place_order
    ~private_key
    ~orders
    ~grouping
    ~nonce:_
  : (string, string) Result.t =
  try
    (* 1. Serialize action to msgpack *)
    let action_msgpack = Msgpack.serialize_order_action ~orders ~grouping in

    (* 2. Hash the action *)
    let action_hash = Keccak.hash_bytes action_msgpack in

    (* 3. Get domain separator *)
    let domain_sep = Domain.domain_separator Domain.hyperliquid_l1 in

    (* 4. Create EIP-712 digest *)
    let digest = Signature.create_digest ~domain_separator:domain_sep ~struct_hash:action_hash in

    (* 5. Sign the digest *)
    Signature.sign ~private_key_hex:private_key ~digest_hex:digest

  with e ->
    Error (sprintf "sign_place_order failed: %s" (Exn.to_string e))

(** Sign L1 cancel action *)
let sign_cancel_order
    ~private_key
    ~cancels
    ~nonce:_
  : (string, string) Result.t =
  try
    (* 1. Serialize action to msgpack *)
    let action_msgpack = Msgpack.serialize_cancel_action ~cancels in

    (* 2. Hash the action *)
    let action_hash = Keccak.hash_bytes action_msgpack in

    (* 3. Get domain separator *)
    let domain_sep = Domain.domain_separator Domain.hyperliquid_l1 in

    (* 4. Create EIP-712 digest *)
    let digest = Signature.create_digest ~domain_separator:domain_sep ~struct_hash:action_hash in

    (* 5. Sign the digest *)
    Signature.sign ~private_key_hex:private_key ~digest_hex:digest

  with e ->
    Error (sprintf "sign_cancel_order failed: %s" (Exn.to_string e))

(** Sign withdraw3 action

    The withdraw3 action requires EIP-712 signing with a specific type structure.
    Unlike order/cancel which use msgpack, withdraw3 uses direct EIP-712 typed data.
*)
let sign_withdraw
    ~private_key
    ~(req : withdraw_request)
  : (string, string) Result.t =
  try
    (* 1. Serialize action to msgpack *)
    let action_msgpack = Msgpack.serialize_withdraw_action ~req in

    (* 2. Hash the action *)
    let action_hash = Keccak.hash_bytes action_msgpack in

    (* 3. Get domain separator *)
    let domain_sep = Domain.domain_separator Domain.hyperliquid_l1 in

    (* 4. Create EIP-712 digest *)
    let digest = Signature.create_digest ~domain_separator:domain_sep ~struct_hash:action_hash in

    (* 5. Sign the digest *)
    Signature.sign ~private_key_hex:private_key ~digest_hex:digest

  with e ->
    Error (sprintf "sign_withdraw failed: %s" (Exn.to_string e))

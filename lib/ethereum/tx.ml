(** Ethereum Transaction Builder and Signer

    Supports EIP-1559 (Type 2) transactions with secp256k1 ECDSA signing.

    @see <https://eips.ethereum.org/EIPS/eip-1559>
    @see <https://eips.ethereum.org/EIPS/eip-2718>
*)

open Core

(** EIP-1559 transaction fields *)
type eip1559_tx = {
  chain_id : int;
  nonce : int;
  max_priority_fee_per_gas : string;  (** hex string *)
  max_fee_per_gas : string;           (** hex string *)
  gas_limit : int;
  to_ : string;                       (** 20-byte address *)
  value : string;                     (** hex string, wei amount *)
  data : string;                      (** hex-encoded calldata *)
}

(** Convert a hex string to raw bytes, stripping 0x prefix *)
let hex_to_raw (hex : string) : string =
  let hex = match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  (* Ensure even length *)
  let hex = match String.length hex mod 2 = 1 with
    | true -> "0" ^ hex
    | false -> hex
  in
  Hex.to_string (`Hex hex)

(** Convert bytes to a Bigarray buffer for secp256k1 *)
let bytes_to_buffer (b : bytes) : Secp256k1.buffer =
  let len = Bytes.length b in
  let buf = Bigarray.Array1.create Bigarray.Char Bigarray.c_layout len in
  for i = 0 to len - 1 do
    Bigarray.Array1.set buf i (Bytes.get b i)
  done;
  buf

(** Encode EIP-1559 transaction payload for signing (without signature).

    EIP-2718 envelope: 0x02 || rlp([chain_id, nonce, max_priority_fee_per_gas,
    max_fee_per_gas, gas_limit, to, value, data, access_list])
*)
let encode_unsigned (tx : eip1559_tx) : string =
  let rlp_payload = Rlp.encode (List [
    Rlp.encode_int tx.chain_id;
    Rlp.encode_int tx.nonce;
    Rlp.encode_hex tx.max_priority_fee_per_gas;
    Rlp.encode_hex tx.max_fee_per_gas;
    Rlp.encode_int tx.gas_limit;
    Rlp.encode_hex tx.to_;
    Rlp.encode_hex tx.value;
    Rlp.encode_hex tx.data;
    List [];  (* access_list: empty *)
  ]) in
  (* Type 2 envelope prefix *)
  "\x02" ^ rlp_payload

(** Encode signed EIP-1559 transaction.

    0x02 || rlp([chain_id, nonce, max_priority_fee_per_gas,
    max_fee_per_gas, gas_limit, to, value, data, access_list, v, r, s])
*)
let encode_signed (tx : eip1559_tx) ~(v : int) ~(r : string) ~(s : string) : string =
  let rlp_payload = Rlp.encode (List [
    Rlp.encode_int tx.chain_id;
    Rlp.encode_int tx.nonce;
    Rlp.encode_hex tx.max_priority_fee_per_gas;
    Rlp.encode_hex tx.max_fee_per_gas;
    Rlp.encode_int tx.gas_limit;
    Rlp.encode_hex tx.to_;
    Rlp.encode_hex tx.value;
    Rlp.encode_hex tx.data;
    List [];  (* access_list: empty *)
    Rlp.encode_int v;
    Rlp.encode_hex r;
    Rlp.encode_hex s;
  ]) in
  "\x02" ^ rlp_payload

(** Sign an EIP-1559 transaction with a private key.

    Returns the raw signed transaction as a hex string with 0x prefix,
    ready for eth_sendRawTransaction.
*)
let sign (tx : eip1559_tx) ~(private_key_hex : string) : (string, string) Result.t =
  try
    (* 1. Encode unsigned transaction *)
    let unsigned = encode_unsigned tx in

    (* 2. Keccak-256 hash of unsigned payload *)
    let hash = Digestif.KECCAK_256.digest_string unsigned in
    let hash_raw = Digestif.KECCAK_256.to_raw_string hash in

    (* 3. Sign with secp256k1 *)
    let ctx = Secp256k1.Context.create [Sign] in

    let privkey_raw = hex_to_raw private_key_hex in
    let privkey_buf = bytes_to_buffer (Bytes.of_string privkey_raw) in
    let privkey = Secp256k1.Key.read_sk_exn ctx privkey_buf in

    let msg_buf = bytes_to_buffer (Bytes.of_string hash_raw) in
    let msg = Secp256k1.Sign.msg_of_bytes_exn msg_buf in

    let rec_sig = Secp256k1.Sign.sign_recoverable_exn ctx ~sk:privkey msg in
    let sig_bytes, recovery_id = Secp256k1.Sign.to_bytes_recid ctx rec_sig in

    (* 4. Extract r, s from signature (each 32 bytes) *)
    let r_bytes = Bytes.create 32 in
    let s_bytes = Bytes.create 32 in
    for i = 0 to 31 do
      Bytes.set r_bytes i (Bigarray.Array1.get sig_bytes i);
      Bytes.set s_bytes i (Bigarray.Array1.get sig_bytes (i + 32))
    done;

    let r_hex = Hex.of_string (Bytes.to_string r_bytes) |> Hex.show in
    let s_hex = Hex.of_string (Bytes.to_string s_bytes) |> Hex.show in

    (* 5. EIP-1559 uses recovery_id directly as v (0 or 1) *)
    let v = recovery_id in

    (* 6. Encode signed transaction *)
    let signed = encode_signed tx ~v ~r:r_hex ~s:s_hex in
    Ok ("0x" ^ (Hex.of_string signed |> Hex.show))

  with e ->
    Error (sprintf "Transaction signing failed: %s" (Exn.to_string e))

(** Compute transaction hash from raw signed transaction *)
let tx_hash ~(raw_tx : string) : string =
  let raw = hex_to_raw raw_tx in
  let hash = Digestif.KECCAK_256.digest_string raw in
  "0x" ^ Digestif.KECCAK_256.to_hex hash

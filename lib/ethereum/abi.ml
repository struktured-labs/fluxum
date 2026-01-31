(** Solidity ABI Encoding

    Implements Solidity ABI encoding for function calls.
    Supports the subset of types needed for ERC-20 and Uniswap SwapRouter.

    @see <https://docs.soliditylang.org/en/latest/abi-spec.html>
*)

open Core

(** ABI value types *)
type value =
  | Uint256 of string   (** 256-bit unsigned integer as hex string *)
  | Address of string   (** 20-byte Ethereum address *)
  | Bytes of string     (** Dynamic bytes *)
  | Bool of bool
  | Uint24 of int       (** 24-bit unsigned integer (fee tiers) *)
  | Uint160 of string   (** 160-bit unsigned integer (sqrt price limit) *)

(** Compute 4-byte function selector from signature string.
    selector = keccak256(signature)[0:4]
*)
let function_selector ~signature =
  let hash = Digestif.KECCAK_256.digest_string signature in
  let raw = Digestif.KECCAK_256.to_raw_string hash in
  let first_4 = String.prefix raw 4 in
  "0x" ^ (Hex.of_string first_4 |> Hex.show)

(** Left-pad a hex string to 32 bytes (64 hex chars) *)
let pad_left_32 (hex : string) : string =
  let hex = match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  let padded_len = 64 in
  let pad_count = padded_len - String.length hex in
  match pad_count > 0 with
  | true -> String.make pad_count '0' ^ hex
  | false -> hex

(** Encode a single ABI value as 32 bytes (64 hex chars) *)
let encode_value (v : value) : string =
  match v with
  | Uint256 hex ->
    let hex = match String.is_prefix hex ~prefix:"0x" with
      | true -> String.drop_prefix hex 2
      | false -> hex
    in
    pad_left_32 hex
  | Address addr ->
    let addr = match String.is_prefix addr ~prefix:"0x" with
      | true -> String.drop_prefix addr 2
      | false -> addr
    in
    pad_left_32 (String.lowercase addr)
  | Bool b ->
    (match b with true -> pad_left_32 "1" | false -> pad_left_32 "0")
  | Uint24 n ->
    pad_left_32 (sprintf "%x" n)
  | Uint160 hex ->
    let hex = match String.is_prefix hex ~prefix:"0x" with
      | true -> String.drop_prefix hex 2
      | false -> hex
    in
    pad_left_32 hex
  | Bytes _raw_bytes ->
    (* For simplicity in swap router calls, dynamic bytes are not used directly *)
    pad_left_32 "0"

(** Encode a function call: selector + encoded args *)
let encode_function_call ~selector ~args =
  let sel = match String.is_prefix selector ~prefix:"0x" with
    | true -> String.drop_prefix selector 2
    | false -> selector
  in
  let encoded_args = List.map args ~f:encode_value |> String.concat in
  "0x" ^ sel ^ encoded_args

(** Helper: encode uint256 from float, given token decimals *)
let uint256_of_float ~decimals (amount : float) : string =
  (* amount * 10^decimals, as hex *)
  let scale = Float.int_pow 10. decimals in
  let wei = Float.to_int (amount *. scale) in
  sprintf "%x" wei

(** Helper: encode uint256 from int *)
let uint256_of_int (n : int) : string =
  sprintf "%x" n

(** Max uint256 for unlimited approval *)
let max_uint256 : string =
  "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

(** Multicall3 — atomic batch eth_call.

    Wraps the canonical Multicall3 contract deployed at the same address on
    every major EVM chain. Lets us fetch N independent contract reads in a
    single RPC round-trip, with the strong invariant that all reads observe
    the same block — no inter-call drift if a tx lands mid-batch.

    Used by [Wallet.erc20_balances] for portfolio snapshots.

    @see <https://github.com/mds1/multicall> *)

open Core

module Eth_rpc = Rpc

open Async

(** Canonical Multicall3 deployment. Same address on Ethereum mainnet,
    Arbitrum, Optimism, Base, Polygon, BSC, Avalanche, and most other
    EVM chains. *)
let address = "0xcA11bde05977b3631167028862bE2a173976CA11"

(** A single call in a Multicall3 batch. *)
type call =
  { target : string (** Contract address to call. *)
  ; allow_failure : bool (** If true, an inner revert won't abort the batch. *)
  ; call_data : string (** Hex-encoded calldata, with or without "0x" prefix. *)
  }

(** Result of one inner call. *)
type result_item =
  { success : bool
  ; return_data : string (** Hex-encoded returndata, "0x"-prefixed. *)
  }

let aggregate3_selector =
  Abi.function_selector ~signature:"aggregate3((address,bool,bytes)[])"

let strip_hex_prefix (s : string) : string =
  match String.is_prefix s ~prefix:"0x" with
  | true -> String.drop_prefix s 2
  | false -> s

(** Right-pad a hex string to a multiple of 64 hex chars (32 bytes). *)
let pad_right_to_word (hex : string) : string =
  let len = String.length hex in
  let rem = len % 64 in
    match rem with
    | 0 -> hex
    | r -> hex ^ String.make (64 - r) '0'

(** Encode dynamic bytes: [length (32 bytes)][data padded right to word]. *)
let encode_bytes (raw_hex : string) : string =
  let stripped = strip_hex_prefix raw_hex in
  (* Length is in BYTES, not hex chars *)
  let byte_len = String.length stripped / 2 in
  let length_word = Abi.pad_left_32 (sprintf "%x" byte_len) in
  let padded = pad_right_to_word stripped in
    length_word ^ padded

(** Encode one [Call3] tuple inline as the head + dynamic tail.
    Returns (head_size_bytes, head, tail). For (address, bool, bytes):
    - head = address (32) + bool (32) + offset_to_bytes (32)
    - tail = encoded bytes (length + data)
    head_size is fixed at 96 bytes (3 * 32). *)
let encode_call3_parts (c : call) : string * string =
  let target_word = Abi.pad_left_32 (strip_hex_prefix (String.lowercase c.target)) in
  let allow_word =
    Abi.pad_left_32
      (match c.allow_failure with
       | true -> "1"
       | false -> "0")
  in
  (* Offset within this call3's encoding: head is always 96 bytes (3 words). *)
  let offset_word = Abi.pad_left_32 (sprintf "%x" 96) in
  let head = target_word ^ allow_word ^ offset_word in
  let tail = encode_bytes c.call_data in
    (head, tail)

(** Encode the full [aggregate3(calls)] calldata.

    Layout:
    [selector (4)]
    [offset to outer array (32) = 0x20]
    [array length (32)]
    [head_offsets (32 * N) — each pointing into the elements area]
    [element 0 (96 + dynamic bytes)]
    [element 1 ...]
    ... *)
let encode_aggregate3 (calls : call list) : string =
  let n = List.length calls in
  let parts = List.map calls ~f:encode_call3_parts in
  (* Each Call3 element occupies head (96) + tail (variable, length-prefixed). *)
  let elem_sizes =
    List.map parts ~f:(fun (head, tail) ->
      String.length head + String.length tail)
  in
  (* Offsets are RELATIVE to the start of the array elements section
     (i.e. AFTER the array length word). N words of offsets come first,
     then the elements. *)
  let offsets_section_bytes = n * 32 in
  let _, head_offsets_rev =
    List.fold elem_sizes ~init:(offsets_section_bytes, []) ~f:(fun (running, acc) sz ->
      let off_word = Abi.pad_left_32 (sprintf "%x" running) in
        (running + (sz / 2), off_word :: acc))
  in
  let head_offsets = List.rev head_offsets_rev |> String.concat in
  let elements =
    List.map parts ~f:(fun (head, tail) -> head ^ tail) |> String.concat
  in
  let outer_offset = Abi.pad_left_32 (sprintf "%x" 32) in
  let array_length = Abi.pad_left_32 (sprintf "%x" n) in
  let selector_stripped = strip_hex_prefix aggregate3_selector in
    "0x" ^ selector_stripped ^ outer_offset ^ array_length ^ head_offsets ^ elements

(** Decode the response of [aggregate3]: a [Result[]] where
    [Result = (bool success, bytes returnData)].

    Layout mirrors encoding:
    [outer offset (32) = 0x20]
    [array length N (32)]
    [head_offsets (32 * N)]
    [element 0: success (32) + offset_to_returnData (32) + length (32) + data]
    ...

    @return List of [result_item] in batch order. *)
let decode_aggregate3_result (hex : string) : (result_item list, [> `Json_parse of string]) Result.t =
  let stripped = strip_hex_prefix hex in
  let total_chars = String.length stripped in
    match total_chars < 128 with
    | true ->
      Error
        (`Json_parse
           (sprintf "Multicall3 response too short: %d hex chars" total_chars))
    | false ->
      (* Read array length at offset 32 bytes (chars 64..128). *)
      let length_hex = String.sub stripped ~pos:64 ~len:64 in
      let n =
        try Int.of_string ("0x" ^ length_hex) with
        | _ -> -1
      in
        (match n < 0 with
         | true ->
           Error
             (`Json_parse (sprintf "Bad Multicall3 array length: %s" length_hex))
         | false ->
           (* Elements section starts at char 128 + N*64 (offsets table).
              Each element's offset is RELATIVE to start of elements section
              (i.e. char 128 + offset*2). *)
           let head_offsets_start = 128 in
           let elements_start = head_offsets_start + (n * 64) in
             (try
                let items =
                  List.init n ~f:(fun i ->
                    let off_pos = head_offsets_start + (i * 64) in
                    let off_hex = String.sub stripped ~pos:off_pos ~len:64 in
                    let elem_off_bytes = Int.of_string ("0x" ^ off_hex) in
                    (* Elements offsets in this layout are relative to AFTER the
                       array-length word but the offsets table itself is part of
                       the array data. mds1's Multicall3 encodes offsets from
                       the start of the offsets table (= after array length). *)
                    let elem_pos = head_offsets_start + (elem_off_bytes * 2) in
                    let success_hex = String.sub stripped ~pos:elem_pos ~len:64 in
                    let success = Int.of_string ("0x" ^ success_hex) <> 0 in
                    (* Skip success word, read returnData offset (relative to
                       start of this element). *)
                    let rd_off_pos = elem_pos + 64 in
                    let rd_off_hex = String.sub stripped ~pos:rd_off_pos ~len:64 in
                    let rd_off_bytes = Int.of_string ("0x" ^ rd_off_hex) in
                    let rd_pos = elem_pos + (rd_off_bytes * 2) in
                    let rd_len_hex = String.sub stripped ~pos:rd_pos ~len:64 in
                    let rd_len_bytes = Int.of_string ("0x" ^ rd_len_hex) in
                    let rd_data_pos = rd_pos + 64 in
                    let rd_data_chars = rd_len_bytes * 2 in
                    let return_data =
                      match rd_data_chars with
                      | 0 -> "0x"
                      | _ -> "0x" ^ String.sub stripped ~pos:rd_data_pos ~len:rd_data_chars
                    in
                      { success; return_data })
                in
                let _ = elements_start in
                  Ok items
              with
              | Invalid_argument m ->
                Error (`Json_parse (sprintf "Multicall3 decode error: %s" m))
              | _ as exn ->
                Error
                  (`Json_parse
                     (sprintf "Multicall3 decode error: %s" (Exn.to_string exn)))))

(** Execute an aggregate3 batch via [eth_call].
    Returns one [result_item] per input call, in order. *)
let aggregate3 ~rpc_url (calls : call list) :
  (result_item list, [> Eth_rpc.error | `Json_parse of string ]) Result.t Deferred.t =
  let data = encode_aggregate3 calls in
    Eth_rpc.eth_call ~rpc_url ~to_:address ~data
    >>| function
    | Error e -> Error (e :> [> Eth_rpc.error | `Json_parse of string ])
    | Ok hex_result -> decode_aggregate3_result hex_result

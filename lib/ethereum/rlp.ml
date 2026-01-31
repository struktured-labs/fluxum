(** Recursive Length Prefix (RLP) Encoding

    Implements Ethereum's RLP encoding scheme for serializing
    nested binary data structures.

    @see <https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/>
*)

open Core

(** RLP item: either a raw string or a list of items *)
type item =
  | String of string
  | List of item list

(** Encode a single byte length prefix *)
let encode_length len offset =
  match len < 56 with
  | true ->
    String.make 1 (Char.of_int_exn (len + offset))
  | false ->
    (* For lengths >= 56, encode as: (offset + 55 + len_of_len) :: big-endian len bytes *)
    let rec to_bytes n acc =
      match n = 0 with
      | true -> acc
      | false -> to_bytes (n / 256) (String.make 1 (Char.of_int_exn (n mod 256)) ^ acc)
    in
    let len_bytes = to_bytes len "" in
    let len_of_len = String.length len_bytes in
    String.make 1 (Char.of_int_exn (offset + 55 + len_of_len)) ^ len_bytes

(** Encode an RLP item to binary string *)
let rec encode (item : item) : string =
  match item with
  | String s ->
    let len = String.length s in
    (match len with
     | 1 when Char.to_int (String.get s 0) < 0x80 ->
       (* Single byte in [0x00, 0x7f] range is its own RLP encoding *)
       s
     | _ ->
       encode_length len 0x80 ^ s)
  | List items ->
    let encoded_items = List.map items ~f:encode |> String.concat in
    let len = String.length encoded_items in
    encode_length len 0xc0 ^ encoded_items

(** Encode a hex string (with or without 0x prefix) as an RLP string item.
    Strips leading zero bytes for compact encoding. *)
let encode_hex (hex : string) : item =
  let hex = match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  (* Ensure even length *)
  let hex = match String.length hex mod 2 = 1 with
    | true -> "0" ^ hex
    | false -> hex
  in
  let raw = Hex.to_string (`Hex hex) in
  (* Strip leading zero bytes *)
  let stripped = match String.lstrip raw ~drop:(fun c -> Char.equal c '\x00') with
    | "" -> ""
    | s -> s
  in
  String stripped

(** Encode an integer as an RLP string item *)
let encode_int (n : int) : item =
  match n with
  | 0 -> String ""
  | _ -> encode_hex (sprintf "%x" n)

(** Encode an int64 as an RLP string item *)
let encode_int64 (n : int64) : item =
  match Int64.equal n 0L with
  | true -> String ""
  | false -> encode_hex (sprintf "%Lx" n)

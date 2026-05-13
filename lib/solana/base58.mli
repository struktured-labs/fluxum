(** Base58 (Bitcoin alphabet, also used by Solana for pubkeys, signatures,
    and transaction signatures).

    Solana uses the same alphabet Bitcoin/IPFS use:
    [123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz] — no
    0/O/I/l to avoid ambiguity. *)

(** Encode a byte string as base58.
    The result has no fixed length; leading zero bytes in the input become
    leading '1' characters in the output. *)
val encode : bytes -> string

(** Decode a base58 string to bytes.
    Returns [`Invalid_char c] if the input contains a non-base58 character. *)
val decode : string -> (bytes, [> `Invalid_char of char ]) result

(** [decode_exn s] is [decode s] but raises on error. *)
val decode_exn : string -> bytes

(** Convenience: parse a base58 pubkey string and verify it decodes to
    exactly 32 bytes (Solana Ed25519 public key length). *)
val decode_pubkey : string -> (bytes, [> `Invalid_char of char | `Wrong_length of int ]) result

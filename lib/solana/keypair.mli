(** Solana Ed25519 keypair: load from Solana CLI JSON, sign messages,
    extract pubkey. *)

(** An Ed25519 keypair. The [secret] field is the 32-byte seed; the [public]
    field is the 32-byte derived public key. Solana wire format and the CLI
    JSON file both store [secret] || [public] = 64 bytes. *)
type t

(** Load a keypair from the standard Solana CLI keypair JSON file format
    (a JSON array of 64 bytes). The first 32 bytes are the Ed25519 seed,
    the second 32 are the derived pubkey.

    The file is expected to be mode 600 — caller is responsible for that. *)
val of_file : string -> (t, [> `Parse_error of string | `Io_error of string ]) result

(** [of_file_exn] raises on any error. *)
val of_file_exn : string -> t

(** Generate a fresh keypair using a cryptographically secure RNG.
    Intended for tests; production keypairs should come from solana-keygen. *)
val generate : unit -> t

(** The 32-byte raw public key. *)
val pubkey_bytes : t -> bytes

(** The base58-encoded public key — the canonical Solana "address" form. *)
val pubkey_base58 : t -> string

(** [sign t ~message] returns a 64-byte Ed25519 signature over [message]. *)
val sign : t -> message:bytes -> bytes

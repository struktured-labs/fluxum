(** Sign a Solana transaction returned by Jupiter's [/swap] endpoint.

    Jupiter returns a fully-built, base64-encoded transaction with zeroed
    placeholder signatures. The user's job is to sign the message portion
    with their Ed25519 key, write the 64-byte signature into the first
    signature slot, and submit.

    This module handles steps 1 (parse) and 2 (sign) only — submission lives
    in [Rpc.send_transaction]. *)

(** Errors that can arise from parsing or signing. *)
type error =
  [ `Base64_decode of string
  | `Truncated of string
  | `Unsupported_sig_count of int ]

val sexp_of_error : error -> Sexplib.Sexp.t

(** [sign_jupiter_swap_b64 ~keypair tx_base64] takes the base64 string Jupiter
    returns under [.swapTransaction], signs the message with [keypair], and
    returns the signed transaction as a fresh base64 string.

    Requires exactly 1 signature slot (the user). If the transaction declares
    multiple signers we return [`Unsupported_sig_count]; callers needing
    multi-sig must handle them explicitly. *)
val sign_jupiter_swap_b64
  :  keypair:Keypair.t
  -> string
  -> (string, error) result

(** [signature_of_signed_b64 b64] extracts the first 64-byte signature of a
    signed base64 transaction and returns its base58 encoding — the canonical
    Solana transaction id you can look up on solscan. *)
val signature_of_signed_b64 : string -> (string, error) result

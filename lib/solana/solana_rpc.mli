(** Minimal Solana JSON-RPC client.

    Supports the four methods needed for a quote→swap→submit→confirm flow
    plus balance reads. Single-batch synchronous calls — no batching, no
    websocket subscriptions yet.

    Default RPC URL points to the public mainnet-beta endpoint. For
    production use, swap in a paid RPC (Helius / Triton / QuickNode) via
    [~rpc_url] to avoid rate-limits and MEV. *)

(** Default RPC URL. Free, no key, rate-limited. *)
val default_rpc_url : string

(** Error type for all Solana RPC calls. *)
type error =
  [ `Network of string
  | `Http of int * string
  | `Json_parse of string
  | `Rpc of int * string ]

val sexp_of_error : error -> Sexplib.Sexp.t

(** Native SOL balance in lamports. 1 SOL = 1_000_000_000 lamports. *)
val get_balance
  :  ?rpc_url:string
  -> address:string
  -> unit
  -> (int, error) Result.t Async.Deferred.t

(** Submit a signed base64-encoded transaction. Returns the base58
    transaction signature on success.

    [~skip_preflight] (default false): skip simulation. Set true to save
    one round-trip when you're confident the tx will succeed.

    [~max_retries] (default 0): RPC-side retry count for tx forwarding. *)
val send_transaction
  :  ?rpc_url:string
  -> ?skip_preflight:bool
  -> ?max_retries:int
  -> tx_base64:string
  -> unit
  -> (string, error) Result.t Async.Deferred.t

(** Status of a single signature: confirmation level + slot.
    The variant captures the documented set:
      - [`Processed]: included in a leader's block, not yet voted.
      - [`Confirmed]: cluster supermajority voted.
      - [`Finalized]: rooted; will not be rolled back. *)
type confirmation =
  [ `Processed
  | `Confirmed
  | `Finalized ]

(** Signature status. [None] for [status] means the signature is not yet
    visible to this RPC (might still be propagating, or might never be). *)
type signature_status =
  { slot : int option
  ; confirmation : confirmation option
  ; err : string option
  }

(** Check the status of one signature. *)
val get_signature_status
  :  ?rpc_url:string
  -> signature_base58:string
  -> unit
  -> (signature_status option, error) Result.t Async.Deferred.t

(** Poll until [signature] reaches [until] (default [`Confirmed]) or
    [timeout] elapses (default 30s, poll every 1s). Returns the final status
    or [Timeout]. *)
val confirm
  :  ?rpc_url:string
  -> ?until:confirmation
  -> ?timeout:Core.Time_float.Span.t
  -> ?poll:Core.Time_float.Span.t
  -> signature_base58:string
  -> unit
  -> ([ `Confirmed of signature_status
      | `Tx_error of string
      | `Timeout ], error) Result.t Async.Deferred.t

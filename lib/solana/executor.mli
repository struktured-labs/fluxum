(** End-to-end Jupiter swap executor.

    Wraps the full quote → swap-build → sign → submit → confirm flow into a
    single [swap] call. Returns the typed quote response, the signed
    transaction's base58 signature, and the final confirmation status.

    All steps share a polymorphic-variant error union so callers can
    pattern-match on the failure point if they want. *)

(** Combined error type — any leg can fail. *)
type error =
  [ `Quote_error of Jupiter.error
  | `Swap_build_error of Jupiter.error
  | `Sign_error of Transaction.error
  | `Submit_error of Solana_rpc.error
  | `Confirm_error of Solana_rpc.error
  ]

val sexp_of_error : error -> Sexplib.Sexp.t

(** Result of a successful executor invocation. *)
type ok =
  { quote : Jupiter.quote
  ; signature : string                   (* base58 tx signature *)
  ; confirmation : Solana_rpc.signature_status option   (* [None] on timeout *)
  ; tx_error : string option             (* set if the chain rejected post-submit *)
  }

(** Execute a Jupiter swap end-to-end.

    [~amount] is in raw mint units (multiply by 10^decimals before calling).
    [~slippage_bps] defaults to 50 (0.5%).
    [~confirm_until] defaults to [`Confirmed].
    [~confirm_timeout] defaults to 60s, [~poll] to 1s.
    [~skip_preflight] passes through to [Solana_rpc.send_transaction].

    Returns [`Confirm_error] iff the post-submit status polling hits an
    RPC error; the tx itself may have succeeded on-chain regardless. Check
    the [signature] in solscan to verify. *)
val swap
  :  ?rpc_url:string
  -> ?slippage_bps:int
  -> ?confirm_until:Solana_rpc.confirmation
  -> ?confirm_timeout:Core.Time_float.Span.t
  -> ?poll:Core.Time_float.Span.t
  -> ?skip_preflight:bool
  -> keypair:Keypair.t
  -> input_mint:string
  -> output_mint:string
  -> amount:int
  -> unit
  -> (ok, error) Result.t Async.Deferred.t

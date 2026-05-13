(** Minimal Jupiter swap client — no API key, no extra abstractions.

    Two endpoints:
      - [/quote]: simulated swap quote at a given notional
      - [/swap]:  fully-built (but unsigned) swap transaction, base64

    The cohort-recorder script [scripts/dex_quote_recorder.sh] exercises the
    same endpoints from bash; this is the OCaml equivalent, suitable for
    composing into a sign-and-submit executor. *)

(** Errors that can arise from Jupiter API calls. *)
type error =
  [ `Network of string
  | `Http of int * string
  | `Json_parse of string ]

val sexp_of_error : error -> Sexplib.Sexp.t

(** A typed quote response. The raw JSON has more fields; only the ones the
    executor needs are surfaced here. The full JSON is preserved as
    [raw_json] so the executor can pass it back to /swap unmodified. *)
type quote =
  { input_mint : string
  ; output_mint : string
  ; in_amount : int        (* raw units of input mint (e.g., lamports for SOL) *)
  ; out_amount : int       (* raw units of output mint *)
  ; other_amount_threshold : int   (* minimum acceptable out at given slippage *)
  ; slippage_bps : int
  ; price_impact_pct : float
  ; raw_json : Yojson.Safe.t
  }

(** [get_quote ~input_mint ~output_mint ~amount ~slippage_bps] requests a
    swap quote at [amount] raw units (already multiplied by 10^decimals). *)
val get_quote
  :  input_mint:string
  -> output_mint:string
  -> amount:int
  -> ?slippage_bps:int
  -> unit
  -> (quote, error) Result.t Async.Deferred.t

(** [get_swap_tx ~quote ~user_pubkey] returns the base64 swap transaction
    string. The transaction is fully built (instructions, blockhash, account
    keys) with a placeholder signature; the user signs and submits. *)
val get_swap_tx
  :  quote:quote
  -> user_pubkey:string
  -> (string, error) Result.t Async.Deferred.t

(** Default mints (held here so callers don't repeat the address strings). *)
module Mint : sig
  val sol  : string  (* native SOL — wrapped SOL mint *)
  val usdc : string  (* Solana USDC (Circle-issued) *)
end

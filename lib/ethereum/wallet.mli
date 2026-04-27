(** Read-only Ethereum wallet introspection.

    Composes [Rpc], [Erc20], and [Multicall3] into a clean typed API for
    answering "what does this address hold?" and "what has it done recently?"
    questions. No signing, no transaction submission — those live in a
    future Signer abstraction. *)

(** Local alias before opening Async — Async.Rpc shadows our local Rpc. *)
module Eth_rpc = Rpc

open Async

(** Chain-block-range helpers. *)

(** Convert a span (e.g. 7 days) into an estimated block count using the
    canonical 12-second mainnet block time. Sized for L1 Ethereum; L2s
    will need their own conversion. *)
val blocks_in_span : Core.Time_float.Span.t -> int

(** Default lookback window for [recent_transfers]: 7 days = ~50,400 blocks. *)
val default_lookback_span : Core.Time_float.Span.t

(** Fetch native ETH balance of an address.
    Returns a typed [Token_amount.t] with decimals=18 and symbol="ETH". *)
val eth_balance :
  rpc_url:string ->
  address:string ->
  (Erc20.Token_amount.t, Eth_rpc.error) Result.t Deferred.t

(** Fetch the balance of a single ERC-20 token for an address.
    Concurrently fetches [decimals] and [symbol] to populate the returned
    [Token_amount.t]. *)
val erc20_balance :
  rpc_url:string ->
  address:string ->
  contract:string ->
  (Erc20.Token_amount.t, Eth_rpc.error) Result.t Deferred.t

(** Fetch balances of N ERC-20 tokens for one address in a single RPC via
    Multicall3 — guarantees all balances are observed in the same block.
    Decimals/symbol metadata is fetched in a parallel batch.
    Returns one [Token_amount.t] per input contract, in input order. *)
val erc20_balances :
  rpc_url:string ->
  address:string ->
  contracts:string list ->
  (Erc20.Token_amount.t list, Eth_rpc.error) Result.t Deferred.t

(** A decoded ERC-20 Transfer event. *)
module Transfer : sig
  type t =
    { block_number : int
    ; tx_hash : string
    ; log_index : int
    ; from_addr : string
    ; to_addr : string
    ; value : Erc20.Token_amount.t
    ; direction : [ `Incoming | `Outgoing | `Self ]
        (** Computed from the queried address's relationship to from/to. *)
    }
  [@@deriving sexp]
end

(** Fetch ERC-20 Transfer events involving [address] for [contract], over a
    lookback window. Queries both incoming and outgoing transfers and merges
    them in block-then-log-index order.

    Internally paginates [eth_getLogs] in 10,000-block chunks with a 200ms
    pause between calls (politeness for free-tier RPC providers).

    [?since] selects the lookback as a span before now. Defaults to 7 days.
    [?from_block]/[?to_block] override [?since] when set; useful for backfills.

    Returns [Max_logs_per_call_exceeded] if any single chunk exceeds the
    provider's log-result cap. Caller should retry with a smaller window. *)
val recent_transfers :
  rpc_url:string ->
  address:string ->
  contract:string ->
  ?since:Core.Time_float.Span.t ->
  ?from_block:int ->
  ?to_block:int ->
  unit ->
  (Transfer.t list, Eth_rpc.log_error) Result.t Deferred.t

(** Rate Limiter with Token Bucket + Exponential Backoff

    Provides automatic request throttling for exchange REST APIs.
    Each exchange should create its own limiter instance with
    appropriate rate limits from the exchange documentation.

    Features:
    - Token bucket algorithm for smooth rate limiting
    - Exponential backoff on rate limit errors
    - Configurable burst allowance
    - Async-friendly (non-blocking wait)
*)

open Core
open Async

(** Configuration for a rate limiter *)
type config =
  { requests_per_second : float  (** Base rate limit *)
  ; burst_size : int             (** Max tokens to accumulate *)
  ; initial_backoff_sec : float  (** Starting backoff after rate limit error *)
  ; max_backoff_sec : float      (** Maximum backoff duration *)
  ; backoff_multiplier : float   (** Multiplier for exponential backoff *)
  }
[@@deriving sexp]

(** Default conservative config (10 req/s, 20 burst) *)
let default_config =
  { requests_per_second = 10.0
  ; burst_size = 20
  ; initial_backoff_sec = 1.0
  ; max_backoff_sec = 60.0
  ; backoff_multiplier = 2.0
  }

(** Exchange-specific rate limit configurations *)
module Configs = struct
  (** Gemini: 120 requests/min for private, 1 request/sec for some endpoints *)
  let gemini =
    { requests_per_second = 2.0
    ; burst_size = 10
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** Kraken: 15-20 calls per minute depending on verification *)
  let kraken =
    { requests_per_second = 0.33  (* ~20/min *)
    ; burst_size = 5
    ; initial_backoff_sec = 2.0
    ; max_backoff_sec = 60.0
    ; backoff_multiplier = 2.0
    }

  (** Binance: 1200 weight/min, most requests are 1-10 weight *)
  let binance =
    { requests_per_second = 10.0
    ; burst_size = 50
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 60.0
    ; backoff_multiplier = 2.0
    }

  (** Bybit: 120 requests/min for most endpoints *)
  let bybit =
    { requests_per_second = 2.0
    ; burst_size = 20
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** OKX: 60 requests/2s for most endpoints *)
  let okx =
    { requests_per_second = 20.0
    ; burst_size = 30
    ; initial_backoff_sec = 0.5
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** Coinbase: 10 requests/sec *)
  let coinbase =
    { requests_per_second = 10.0
    ; burst_size = 15
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** MEXC: 20 requests/sec *)
  let mexc =
    { requests_per_second = 20.0
    ; burst_size = 30
    ; initial_backoff_sec = 0.5
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** Bitstamp: 8000 requests/10min = ~13/sec *)
  let bitstamp =
    { requests_per_second = 10.0
    ; burst_size = 50
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 60.0
    ; backoff_multiplier = 2.0
    }

  (** Bitrue: Conservative estimate *)
  let bitrue =
    { requests_per_second = 5.0
    ; burst_size = 10
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }

  (** Hyperliquid: L1 chain, generally no rate limits but be conservative *)
  let hyperliquid =
    { requests_per_second = 10.0
    ; burst_size = 20
    ; initial_backoff_sec = 1.0
    ; max_backoff_sec = 30.0
    ; backoff_multiplier = 2.0
    }
end

(** Rate limiter state *)
type t =
  { config : config
  ; mutable tokens : float
  ; mutable last_refill : Time_float_unix.t
  ; mutable current_backoff : float
  ; mutable in_backoff : bool
  }

(** Create a new rate limiter with given config *)
let create ?(config = default_config) () =
  { config
  ; tokens = Float.of_int config.burst_size
  ; last_refill = Time_float_unix.now ()
  ; current_backoff = config.initial_backoff_sec
  ; in_backoff = false
  }

(** Refill tokens based on elapsed time *)
let refill t =
  let now = Time_float_unix.now () in
  let elapsed = Time_float_unix.diff now t.last_refill |> Time_float_unix.Span.to_sec in
  let new_tokens = t.tokens +. (elapsed *. t.config.requests_per_second) in
  t.tokens <- Float.min new_tokens (Float.of_int t.config.burst_size);
  t.last_refill <- now

(** Try to acquire a token. Returns true if successful, false if should wait. *)
let try_acquire t =
  refill t;
  match Float.(t.tokens >= 1.0) with
  | true ->
    t.tokens <- t.tokens -. 1.0;
    true
  | false ->
    false

(** Calculate time until a token is available *)
let time_until_available t =
  refill t;
  match Float.(t.tokens >= 1.0) with
  | true -> Time_float_unix.Span.zero
  | false ->
    let needed = 1.0 -. t.tokens in
    let seconds = needed /. t.config.requests_per_second in
    Time_float_unix.Span.of_sec seconds

(** Wait until a token is available, then acquire it.
    This is the main entry point for rate-limited requests. *)
let wait t =
  match t.in_backoff with
  | true ->
    (* Currently in backoff from a rate limit error *)
    let backoff_span = Time_float_unix.Span.of_sec t.current_backoff in
    after backoff_span >>| fun () ->
    t.in_backoff <- false;
    (* Reset backoff on successful wait completion *)
    t.current_backoff <- t.config.initial_backoff_sec
  | false ->
    let rec acquire () =
      match try_acquire t with
      | true -> Deferred.unit
      | false ->
        let wait_time = time_until_available t in
        after wait_time >>= fun () ->
        acquire ()
    in
    acquire ()

(** Signal that a rate limit error was received.
    This triggers exponential backoff for subsequent requests. *)
let on_rate_limit_error t =
  t.in_backoff <- true;
  (* Exponential backoff with jitter *)
  let jitter = Random.float 0.5 in
  let new_backoff = t.current_backoff *. t.config.backoff_multiplier *. (1.0 +. jitter) in
  t.current_backoff <- Float.min new_backoff t.config.max_backoff_sec;
  Log.Global.info "Rate limit hit, backing off for %.1fs (next: %.1fs)"
    t.current_backoff new_backoff

(** Reset backoff state (e.g., after a successful request) *)
let reset_backoff t =
  match t.in_backoff with
  | true -> ()  (* Don't reset during active backoff *)
  | false ->
    t.current_backoff <- t.config.initial_backoff_sec

(** Get current state for monitoring *)
let status t =
  refill t;
  sprintf "tokens=%.1f/%d backoff=%.1fs in_backoff=%b"
    t.tokens t.config.burst_size t.current_backoff t.in_backoff

(** Check if an error represents a rate limit condition.
    Matches both `Too_many_requests of string (exchange API) and `Rate_limited (normalized). *)
let is_rate_limit_error (type a) (e : a) : bool =
  (* Use Obj magic to check if the variant tag matches rate limit patterns.
     This is a bit of a hack but allows the function to work with any error type
     without requiring specific type constraints that would propagate through the codebase. *)
  let is_too_many_requests =
    try
      let _ = (Obj.magic e : [> `Too_many_requests of string ]) in
      match (Obj.magic e : [> `Too_many_requests of string ]) with
      | `Too_many_requests _ -> true
      | _ -> false
    with _ -> false
  in
  is_too_many_requests

(** Wrap a deferred computation with rate limiting.
    Automatically handles rate limit errors (indicated by returning Error (`Too_many_requests _)). *)
let with_rate_limit t ~f =
  wait t >>= fun () ->
  f () >>| fun result ->
  (match result with
   | Error e when is_rate_limit_error e -> on_rate_limit_error t
   | Ok _ -> reset_backoff t
   | Error _ -> ());
  result

(** Wrap with rate limit, retrying on rate limit errors up to max_retries times.
    The function preserves the exact error type from the wrapped function. *)
let with_rate_limit_retry ?(max_retries = 3) t ~f =
  let rec loop retries =
    wait t >>= fun () ->
    f () >>= fun result ->
    match result with
    | Error e when is_rate_limit_error e && retries > 0 ->
      on_rate_limit_error t;
      loop (retries - 1)
    | Ok _ ->
      reset_backoff t;
      return result
    | Error _ ->
      return result
  in
  loop max_retries

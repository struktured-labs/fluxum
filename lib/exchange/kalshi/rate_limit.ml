(** Client-side throttling and retry for the Kalshi REST API.

    Kalshi meters requests with token buckets (docs: "Rate Limits and Tiers"):
    a default request costs 10 tokens; Read and Write (order create / amend /
    decrease / cancel, order groups) have separate buckets; cancels cost 2
    tokens; GET /portfolio/balance has its own bucket. A 429 carries no
    Retry-After header, so we back off exponentially with full jitter.

    The bucket core is pure (explicit [~now] in seconds) so it is unit
    testable; [Limiter] wraps it for Async. *)

module Bucket = struct
  type t =
    { capacity : float
    ; refill_per_s : float
    ; mutable tokens : float
    ; mutable last : float
    }
  [@@deriving sexp]

  let create ~capacity ~refill_per_s ~now = { capacity; refill_per_s; tokens = capacity; last = now }

  let refill t ~now =
    if Float.(now > t.last) then begin
      t.tokens <- Float.min t.capacity (t.tokens +. ((now -. t.last) *. t.refill_per_s));
      t.last <- now
    end

  (** Take [cost] tokens if available and return 0., otherwise take nothing
      and return the seconds to wait until enough tokens accrue. A cost above
      capacity is clamped to capacity so it can eventually proceed. *)
  let try_take t ~now ~cost =
    refill t ~now;
    let cost = Float.min cost t.capacity in
    if Float.(t.tokens >= cost) then (t.tokens <- t.tokens -. cost; 0.)
    else (cost -. t.tokens) /. t.refill_per_s
end

module Tier = struct
  type t = Basic | Advanced | Expert | Premier | Paragon | Prime | Prestige
  [@@deriving sexp, equal, enumerate]

  let of_string s =
    List.find all ~f:(fun t ->
      String.equal (String.lowercase (Sexp.to_string (sexp_of_t t))) (String.lowercase s))

  (** (refill tokens/s, burst capacity) for Read and Write buckets. Basic &
      Advanced Read and above-Basic Write buckets hold 3 s of burst; higher
      Read tiers and the Basic Write bucket hold 1 s. *)
  let read = function
    | Basic -> (200., 600.) | Advanced -> (300., 900.) | Expert -> (600., 600.)
    | Premier -> (1200., 1200.) | Paragon -> (2400., 2400.) | Prime -> (4800., 4800.)
    | Prestige -> (12000., 12000.)

  let write = function
    | Basic -> (100., 100.) | Advanced -> (300., 900.) | Expert -> (600., 1800.)
    | Premier -> (1200., 3600.) | Paragon -> (2400., 7200.) | Prime -> (4800., 14400.)
    | Prestige -> (9600., 28800.)

  (** Balance endpoint, requests per second. *)
  let balance_rps = function Basic | Advanced -> 1. | _ -> 5.
end

type kind = Read | Write | Balance [@@deriving sexp, equal]

let default_cost = 10.
let cancel_cost = 2.

(** Classify a request by method + path (path relative to /trade-api/v2). *)
let classify ~meth ~path =
  let p = match String.lsplit2 path ~on:'?' with Some (a, _) -> a | None -> path in
  let is_order_write =
    String.is_prefix p ~prefix:"/portfolio/events/orders"
    || String.is_prefix p ~prefix:"/portfolio/orders"
    || String.is_prefix p ~prefix:"/portfolio/order_groups"
  in
  match meth with
  | `GET when String.equal p "/portfolio/balance" -> (Balance, 1.)
  | `GET -> (Read, default_cost)
  | `DELETE when is_order_write -> (Write, cancel_cost)
  | (`POST | `PUT | `DELETE) when is_order_write -> (Write, default_cost)
  | _ -> (Read, default_cost)

(** Exponential backoff with full jitter: uniform in
    [0, min(cap, base * 2^attempt)]. [rand] in [0,1). *)
let backoff_delay ?(base = 0.25) ?(cap = 8.0) ~attempt ~rand () =
  let ceiling = Float.min cap (base *. (2. ** Float.of_int attempt)) in
  (* never 0: keep a small floor so we don't hammer *)
  Float.max 0.01 (rand *. ceiling)

module Limiter = struct
  type t = { read : Bucket.t; write : Bucket.t; balance : Bucket.t; tier : Tier.t }

  let now () = Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_sec

  let create ?(now = now ()) tier =
    let mk (r, c) = Bucket.create ~capacity:c ~refill_per_s:r ~now in
    let b = Tier.balance_rps tier in
    { read = mk (Tier.read tier); write = mk (Tier.write tier)
    ; balance = Bucket.create ~capacity:b ~refill_per_s:b ~now; tier }

  let bucket t = function Read -> t.read | Write -> t.write | Balance -> t.balance

  (** Wait (asynchronously) until the request may be sent. *)
  let rec acquire t kind ~cost =
    let wait = Bucket.try_take (bucket t kind) ~now:(now ()) ~cost in
    if Float.(wait <= 0.) then return ()
    else
      let%bind () = Clock.after (Time_float.Span.of_sec wait) in
      acquire t kind ~cost
end

(** Process-wide default limiter, Basic tier unless KALSHI_TIER is set. *)
let default =
  lazy
    (let tier =
       Option.bind (Sys.getenv "KALSHI_TIER") ~f:Tier.of_string |> Option.value ~default:Tier.Basic
     in
     Limiter.create tier)

(** Run [f ()] retrying while [is_rate_limited] holds, up to [max_retries]
    extra attempts, sleeping [backoff_delay] between tries. [sleep]/[rand]
    are injectable for tests. *)
let with_retry ?(max_retries = 5) ?(sleep = fun d -> Clock.after (Time_float.Span.of_sec d))
      ?(rand = fun () -> Random.float 1.0) ?(on_retry = fun ~attempt:_ ~delay:_ -> ())
      ~is_rate_limited f =
  let rec go attempt =
    let%bind r = f () in
    if is_rate_limited r && attempt < max_retries then begin
      let delay = backoff_delay ~attempt ~rand:(rand ()) () in
      on_retry ~attempt ~delay;
      let%bind () = sleep delay in
      go (attempt + 1)
    end
    else return r
  in
  go 0

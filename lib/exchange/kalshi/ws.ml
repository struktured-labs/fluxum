(** Kalshi Trade API WebSocket client (authenticated handshake).

    Channels: orderbook_delta (snapshot + incremental), ticker, trade, fill,
    user_orders. Prices on the book channel are in each side's own pricing
    (yes_dollars_fp / no_dollars_fp are bids), matching the REST orderbook.

    Consistency: every message on a subscription carries [sid] + [seq]; a seq
    gap means a dropped message, so we drop the connection and resubscribe,
    which yields a fresh snapshot. Disconnects reconnect with exponential
    backoff + jitter. *)

let ws_path = "/trade-api/ws/v2"

(** A maintained book for one market. Keys are prices in centicents (1e-4 $),
    values are contract counts. *)
module Local_book = struct
  type t =
    { ticker : string
    ; mutable yes : float Int.Map.t
    ; mutable no : float Int.Map.t
    ; mutable updates : int
    }

  (** Raised (and caught by [handle]) for any malformed book message. *)
  exception Bad_message of string

  let bad fmt = Printf.ksprintf (fun m -> raise (Bad_message m)) fmt

  let create ticker = { ticker; yes = Int.Map.empty; no = Int.Map.empty; updates = 0 }

  (** Strict: full decimal syntax, at most 4 significant dp, within [0, 1]. *)
  let key_of_price s =
    match Trade_v2.Fp.parse_scaled ~decimals:4 s with
    | Some k when k >= 0 && k <= 10_000 -> k
    | _ -> bad "bad price %S" s

  let num j =
    let f =
      match j with
      | `String s -> (match Float.of_string_opt (String.strip s) with Some f -> f | None -> bad "bad number %S" s)
      | `Float f -> f
      | `Int i -> Float.of_int i
      | _ -> bad "bad number %s" (Yojson.Safe.to_string j)
    in
    if Float.is_finite f then f else bad "non-finite number %s" (Yojson.Safe.to_string j)

  let levels_of_json = function
    | `Null -> Int.Map.empty
    | `List l ->
      List.fold l ~init:Int.Map.empty ~f:(fun m lv ->
        match lv with
        | `List [ `String p; q ] ->
          let p = key_of_price p in
          let q = num q in
          if Float.(q > 0.) then Map.set m ~key:p ~data:q else m
        | j -> bad "bad level %s" (Yojson.Safe.to_string j))
    | j -> bad "bad levels %s" (Yojson.Safe.to_string j)

  (** Parses both sides before mutating, so a malformed snapshot leaves the
      book untouched (and [handle] resyncs). *)
  let apply_snapshot t (msg : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let yes = levels_of_json (member "yes_dollars_fp" msg) in
    let no = levels_of_json (member "no_dollars_fp" msg) in
    t.yes <- yes;
    t.no <- no;
    t.updates <- t.updates + 1

  (** Apply a delta; a level whose size reaches <= 0 is removed. *)
  let apply_delta_raw t ~side ~price_key ~delta =
    let upd m =
      let cur = Option.value (Map.find m price_key) ~default:0. in
      let n = cur +. delta in
      if Float.(n <= 1e-9) then Map.remove m price_key else Map.set m ~key:price_key ~data:n
    in
    (match side with `Yes -> t.yes <- upd t.yes | `No -> t.no <- upd t.no);
    t.updates <- t.updates + 1

  let side_of_json = function
    | `String "yes" -> `Yes
    | `String "no" -> `No
    | j -> bad "bad side %s" (Yojson.Safe.to_string j)

  (** Validates every field before touching the book. *)
  let apply_delta t (msg : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let side = side_of_json (member "side" msg) in
    let price_key =
      match member "price_dollars" msg with
      | `String s -> key_of_price s
      | j -> bad "bad price_dollars %s" (Yojson.Safe.to_string j)
    in
    let delta = num (member "delta_fp" msg) in
    apply_delta_raw t ~side ~price_key ~delta

  let best m = Map.max_elt m  (* highest bid *)

  (** (yes_bid, yes_ask) as (price_key, size); yes ask = 1 - best no bid. *)
  let top t =
    let bid = best t.yes in
    let ask = Option.map (best t.no) ~f:(fun (p, q) -> (10_000 - p, q)) in
    (bid, ask)

  let to_levels m = Map.to_alist m

  (** Compare with REST orderbook JSON ([orderbook_fp] or [orderbook] with
      yes_dollars/no_dollars). Returns list of discrepancies (empty = equal). *)
  let diff_with_rest t (rest : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let ob = match member "orderbook_fp" rest with `Null -> member "orderbook" rest | o -> o in
    let pick a b = match member a ob with `Null -> member b ob | x -> x in
    match
      (levels_of_json (pick "yes_dollars" "yes_dollars_fp"),
       levels_of_json (pick "no_dollars" "no_dollars_fp"))
    with
    | exception Bad_message m -> [ "rest decode error: " ^ m ]
    | ry, rn ->
    let cmp name local remote =
      Map.symmetric_diff local remote ~data_equal:(fun a b -> Float.(abs (a -. b) < 1e-6))
      |> Sequence.to_list
      |> List.map ~f:(fun (k, d) ->
        let s = function Some q -> Float.to_string q | None -> "-" in
        match d with
        | `Left q -> sprintf "%s %d: ws=%s rest=-" name k (s (Some q))
        | `Right q -> sprintf "%s %d: ws=- rest=%s" name k (s (Some q))
        | `Unequal (a, b) -> sprintf "%s %d: ws=%s rest=%s" name k (s (Some a)) (s (Some b)))
    in
    cmp "yes" t.yes ry @ cmp "no" t.no rn
end

(** Per-subscription sequence tracking.

    Kalshi numbers messages per subscription ([sid]), not per market: one
    orderbook_delta subscription covering several markets shares a single
    [seq] stream, so a dropped delta for market A is only detectable as a gap
    in that shared stream. A snapshot therefore does {b not} reset the
    expectation for a sid that is already being tracked (it refreshes one
    market only); it must be in sequence like any other message. Only the
    first message seen on a sid (after connect / resubscribe) is accepted
    unconditionally. *)
module Seq_tracker = struct
  type t = int Int.Table.t  (* sid -> last seq *)

  let create () : t = Int.Table.create ()

  (** [`Ok] when seq = last+1 (or the sid is new), [`Gap (expected, got)]
      otherwise. [is_snapshot] is accepted for API compatibility but no
      longer resets an existing sid (see module doc). *)
  let check (t : t) ~sid ~seq ~is_snapshot:(_ : bool) =
    match Hashtbl.find t sid with
    | None -> Hashtbl.set t ~key:sid ~data:seq; `Ok
    | Some last when seq = last + 1 -> Hashtbl.set t ~key:sid ~data:seq; `Ok
    | Some last -> `Gap (last + 1, seq)

  let reset (t : t) = Hashtbl.clear t
end

type event =
  | Connected of int  (** connection number, 1-based *)
  | Subscribed of string * int  (** channel, sid *)
  | Book of Local_book.t * [ `Snapshot | `Delta ]
  | Ticker of Yojson.Safe.t
  | Trade of Yojson.Safe.t
  | Fill of Yojson.Safe.t
  | User_order of Yojson.Safe.t
  | Gap of { sid : int; expected : int; got : int }
  | Decode_error of string  (** malformed message; the client resyncs *)
  | Disconnected of string
  | Error_msg of Yojson.Safe.t
  | Other of Yojson.Safe.t

let subscribe_msg ~id ~channels ?market_tickers () =
  `Assoc
    [ ("id", `Int id)
    ; ("cmd", `String "subscribe")
    ; ( "params"
      , `Assoc
          (("channels", `List (List.map channels ~f:(fun c -> `String c)))
           :: (match market_tickers with
               | Some ts -> [ ("market_tickers", `List (List.map ts ~f:(fun t -> `String t))) ]
               | None -> [])) ) ]

let market_channels = [ "orderbook_delta"; "ticker"; "trade" ]
let user_channels = [ "fill"; "user_orders" ]

(** Route one decoded message. Returns events to emit and whether a resync
    (reconnect) is required. Pure apart from mutating [books]/[seqs].
    Never raises on bad input: malformed book data yields [Decode_error] and
    [`Resync] (the book is left unmodified by the bad message). *)
let handle ~(books : (string, Local_book.t) Hashtbl.t) ~seqs (j : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc _ ->
    let ty = match member "type" j with `String s -> s | _ -> "" in
    let msg = member "msg" j in
    let sid = match member "sid" j with `Int i -> Some i | _ -> None in
    let seq = match member "seq" j with `Int i -> Some i | _ -> None in
    let gap =
      match sid, seq with
      | Some sid, Some seq ->
        (match Seq_tracker.check seqs ~sid ~seq ~is_snapshot:(String.equal ty "orderbook_snapshot") with
         | `Ok -> None
         | `Gap (expected, got) -> Some (Gap { sid; expected; got }))
      | _ -> None
    in
    (match gap with
     | Some g -> ([ g ], `Resync)
     | None ->
       let book_of () =
         match member "market_ticker" msg with
         | `String t when not (String.is_empty t) ->
           Hashtbl.find_or_add books t ~default:(fun () -> Local_book.create t)
         | _ -> Local_book.bad "missing market_ticker"
       in
       (match
          (match ty with
           | "orderbook_snapshot" -> let b = book_of () in Local_book.apply_snapshot b msg; Book (b, `Snapshot)
           | "orderbook_delta" -> let b = book_of () in Local_book.apply_delta b msg; Book (b, `Delta)
           | "ticker" | "ticker_v2" -> Ticker msg
           | "trade" -> Trade msg
           | "fill" -> Fill msg
           | "user_order" -> User_order msg
           | "subscribed" ->
             Subscribed ((match member "channel" msg with `String c -> c | _ -> "?"),
                         (match member "sid" msg with `Int i -> i | _ -> -1))
           | "error" -> Error_msg msg
           | _ -> Other j)
        with
        | ev -> ([ ev ], `Continue)
        | exception (Local_book.Bad_message m) -> ([ Decode_error (ty ^ ": " ^ m) ], `Resync)
        | exception (Type_error (m, _)) -> ([ Decode_error (ty ^ ": " ^ m) ], `Resync)))
  | _ -> ([ Decode_error "message is not a JSON object" ], `Resync)

(** Minimal transport interface so the reconnect / shutdown logic can be
    tested without a network. Contract relied on by [run_with]: [close] is
    never called while a [receive] on the same connection is in flight
    (with libcurl that would free the handle under the worker thread). *)
module type Transport = sig
  type t

  val connect : url:string -> headers:(string * string) list -> t Deferred.Or_error.t
  val send : t -> string -> unit Deferred.t
  val receive : t -> string option Deferred.t
  val close : t -> unit Deferred.t
end

module Curl_transport : Transport with type t = Websocket_curl.t = struct
  type t = Websocket_curl.t

  let connect ~url ~headers = Websocket_curl.connect ~url ~headers ()
  let send = Websocket_curl.send
  let receive = Websocket_curl.receive
  let close = Websocket_curl.close
end

let auth_headers cfg =
  Auth.headers cfg ~http_method:"GET" ~path:ws_path
  |> Cohttp.Header.to_list
  |> List.filter ~f:(fun (k, _) -> String.is_prefix (String.uppercase k) ~prefix:"KALSHI-")

(** Reconnect policy. Every reconnect (failed handshake or lost connection)
    waits [Rate_limit.backoff_delay ~base ~cap ~attempt:failures]; the
    failure count is only reset once a connection has stayed up for
    [healthy_after] seconds, so a server that accepts the upgrade and then
    drops immediately is backed off exponentially, not hammered. *)
type policy =
  { base : float
  ; cap : float
  ; healthy_after : float
  ; stop_grace : float
    (** on stop, how long [run] waits for an in-flight receive to return
        so the transport can be closed before returning; the close still
        happens (later) if the receive outlives the grace period *)
  }

let default_policy = { base = 0.5; cap = 30.; healthy_after = 10.; stop_grace = 2. }

let run_with (type c) (module T : Transport with type t = c) ~headers ~url ~tickers ?(user = true)
      ?(stop = Deferred.never ()) ?(policy = default_policy) ?(rand = fun () -> Random.float 1.)
      ~(on_event : event -> unit) () =
  let books = String.Table.create () in
  let seqs = Seq_tracker.create () in
  let sec = Time_float.Span.of_sec in
  let now () = Time_float.now () in
  (* wait d seconds or until stop; true = stopped *)
  let wait d =
    let%map () = Deferred.any_unit [ Clock.after (sec d); stop ] in
    Deferred.is_determined stop
  in
  let delay failures =
    Rate_limit.backoff_delay ~base:policy.base ~cap:policy.cap ~attempt:failures ~rand:(rand ()) ()
  in
  let rec connect_loop n failures =
    if Deferred.is_determined stop then return ()
    else
      let conn = T.connect ~url ~headers:(headers ()) in
      match%bind
        Deferred.choose [ Deferred.choice stop (fun () -> `Stop); Deferred.choice conn (fun r -> `Conn r) ]
      with
      | `Stop ->
        (* A handshake that completes after stop must still be closed (no
           receive was started on it, so closing is safe). *)
        upon conn (function Ok ws -> don't_wait_for (T.close ws) | Error _ -> ());
        return ()
      | `Conn (Error e) ->
        on_event (Disconnected (Error.to_string_hum e));
        if%bind wait (delay failures) then return () else connect_loop n (failures + 1)
      | `Conn (Ok ws) ->
        let started = now () in
        Seq_tracker.reset seqs;
        Hashtbl.clear books;
        let finish_lost why =
          on_event (Disconnected why);
          let healthy = Float.(Time_float.Span.to_sec (Time_float.diff (now ()) started) >= policy.healthy_after) in
          let failures = if healthy then 0 else failures + 1 in
          if%bind wait (delay failures) then return () else connect_loop (n + 1) failures
        in
        let emit evs =
          (* if a callback raises, don't leak the connection (no receive is
             in flight here), then re-raise *)
          try List.iter evs ~f:on_event
          with exn -> don't_wait_for (T.close ws); raise exn
        in
        emit [ Connected n ];
        let send j = T.send ws (Yojson.Safe.to_string j) in
        let%bind () = send (subscribe_msg ~id:1 ~channels:market_channels ~market_tickers:tickers ()) in
        let%bind () = if user then send (subscribe_msg ~id:2 ~channels:user_channels ()) else return () in
        let rec read_loop () =
          if Deferred.is_determined stop then (let%map () = T.close ws in `Stop) else
          let pending = T.receive ws in
          match%bind
            Deferred.choose
              [ Deferred.choice stop (fun () -> `Stop); Deferred.choice pending (fun m -> `Msg m) ]
          with
          | `Stop ->
            (* Never free the transport under an in-flight receive: close
               only once it has returned. *)
            let closed = pending >>= fun _ -> T.close ws in
            let%map () = Deferred.any_unit [ closed; Clock.after (sec policy.stop_grace) ] in
            `Stop
          | `Msg None ->
            (* EOF / read error: the receive has returned, so close is safe
               and releases the transport before reconnecting. *)
            let%map () = T.close ws in
            `Lost "connection closed"
          | `Msg (Some s) ->
            let evs, action =
              match Yojson.Safe.from_string s with
              | exception _ -> ([ Decode_error "invalid JSON" ], `Resync)
              | j -> handle ~books ~seqs j
            in
            emit evs;
            (match action with
             | `Continue -> read_loop ()
             | `Resync ->
               let%map () = T.close ws in
               `Lost (match evs with [ Gap _ ] -> "seq gap; resubscribing" | _ -> "decode error; resubscribing"))
        in
        (match%bind read_loop () with
         | `Stop -> return ()
         | `Lost why -> finish_lost why)
  in
  connect_loop 1 0

(** Run until [stop] is determined. Reconnects with backoff on disconnect,
    on sequence gaps and on malformed messages (fresh snapshot after
    resubscribe). Auth headers are re-signed for every connection attempt. *)
let run ~cfg ~url ~tickers ?user ?stop ?policy ~(on_event : event -> unit) () =
  run_with (module Curl_transport) ~headers:(fun () -> auth_headers cfg) ~url ~tickers ?user ?stop ?policy
    ~on_event ()

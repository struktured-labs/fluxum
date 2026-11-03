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

  let create ticker = { ticker; yes = Int.Map.empty; no = Int.Map.empty; updates = 0 }

  let key_of_price s =
    match Trade_v2.Fp.parse_scaled ~decimals:4 s with
    | Some k -> k
    | None -> failwithf "bad price %s" s ()

  let num j = match j with
    | `String s -> Float.of_string s | `Float f -> f | `Int i -> Float.of_int i
    | _ -> failwith "bad number"

  let levels_of_json = function
    | `List l ->
      List.fold l ~init:Int.Map.empty ~f:(fun m lv ->
        match lv with
        | `List [ p; q ] ->
          let p = match p with `String s -> key_of_price s | j -> Float.iround_nearest_exn (num j *. 10_000.) in
          let q = num q in
          if Float.(q > 0.) then Map.set m ~key:p ~data:q else m
        | _ -> m)
    | _ -> Int.Map.empty

  let apply_snapshot t (msg : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    t.yes <- levels_of_json (member "yes_dollars_fp" msg);
    t.no <- levels_of_json (member "no_dollars_fp" msg);
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

  let apply_delta t (msg : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let side = match member "side" msg with `String "no" -> `No | _ -> `Yes in
    let price_key = key_of_price (to_string (member "price_dollars" msg)) in
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
    let ry = levels_of_json (pick "yes_dollars" "yes_dollars_fp") in
    let rn = levels_of_json (pick "no_dollars" "no_dollars_fp") in
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

(** Per-subscription sequence tracking. *)
module Seq_tracker = struct
  type t = int Int.Table.t  (* sid -> last seq *)

  let create () : t = Int.Table.create ()

  (** [`Ok] when seq = last+1 (or first seen), [`Gap (expected, got)]
      otherwise. A snapshot resets the expectation. *)
  let check (t : t) ~sid ~seq ~is_snapshot =
    match Hashtbl.find t sid with
    | _ when is_snapshot -> Hashtbl.set t ~key:sid ~data:seq; `Ok
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
    (reconnect) is required. Pure apart from mutating [books]/[seqs]. *)
let handle ~(books : (string, Local_book.t) Hashtbl.t) ~seqs (j : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
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
  match gap with
  | Some g -> ([ g ], `Resync)
  | None ->
    let book_of () =
      let t = match member "market_ticker" msg with `String s -> s | _ -> "" in
      Hashtbl.find_or_add books t ~default:(fun () -> Local_book.create t)
    in
    let ev =
      match ty with
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
      | _ -> Other j
    in
    ([ ev ], `Continue)

let auth_headers cfg =
  Auth.headers cfg ~http_method:"GET" ~path:ws_path
  |> Cohttp.Header.to_list
  |> List.filter ~f:(fun (k, _) -> String.is_prefix (String.uppercase k) ~prefix:"KALSHI-")

(** Run until [stop] is determined. Reconnects with backoff on disconnect and
    on sequence gaps (fresh snapshot after resubscribe). *)
let run ~cfg ~url ~tickers ?(user = true) ?(stop = Deferred.never ()) ~(on_event : event -> unit) () =
  let books = String.Table.create () in
  let seqs = Seq_tracker.create () in
  let stopped () = Deferred.is_determined stop in
  let rec connect_loop n attempt =
    if stopped () then return ()
    else
      match%bind Websocket_curl.connect ~url ~headers:(auth_headers cfg) () with
      | Error e ->
        on_event (Disconnected (Error.to_string_hum e));
        let d = Rate_limit.backoff_delay ~base:0.5 ~cap:30. ~attempt ~rand:(Random.float 1.) () in
        let%bind () = Deferred.any [ Clock.after (Time_float.Span.of_sec d); stop ] in
        connect_loop n (attempt + 1)
      | Ok ws ->
        Seq_tracker.reset seqs;
        Hashtbl.clear books;
        on_event (Connected n);
        let send j = Websocket_curl.send ws (Yojson.Safe.to_string j) in
        let%bind () = send (subscribe_msg ~id:1 ~channels:market_channels ~market_tickers:tickers ()) in
        let%bind () = if user then send (subscribe_msg ~id:2 ~channels:user_channels ()) else return () in
        let rec read_loop () =
          match%bind Deferred.choose
                       [ Deferred.choice stop (fun () -> `Stop)
                       ; Deferred.choice (Websocket_curl.receive ws) (fun m -> `Msg m) ] with
          | `Stop -> let%map () = Websocket_curl.close ws in `Stop
          | `Msg None -> return (`Lost "connection closed")
          | `Msg (Some s) ->
            (match Yojson.Safe.from_string s with
             | exception _ -> read_loop ()
             | j ->
               let evs, action = handle ~books ~seqs j in
               List.iter evs ~f:on_event;
               (match action with
                | `Continue -> read_loop ()
                | `Resync -> let%map () = Websocket_curl.close ws in `Lost "seq gap; resubscribing"))
        in
        (match%bind read_loop () with
         | `Stop -> return ()
         | `Lost why ->
           on_event (Disconnected why);
           let%bind () = Clock.after (Time_float.Span.of_sec 0.5) in
           connect_loop (n + 1) 0)
  in
  connect_loop 1 0

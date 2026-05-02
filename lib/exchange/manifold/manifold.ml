(** Manifold Markets — read-only data feed for the play-money
    prediction market.

    Implements {!Feed_only_adapter.S} against [api.manifold.markets/v0].
    All endpoints are unauthenticated for read; trading on Manifold uses
    play-money "mana" (no real-money risk; legal anywhere).

    Manifold is particularly valuable for {b empirical calibration
    research}: the platform has tens of thousands of resolved markets
    spanning many years and categories, with active forecasters whose
    predictions can be benchmarked against {!Prediction_analysis.Calibration}.

    {b Market types}: Manifold has BINARY, MULTIPLE_CHOICE, FREE_RESPONSE,
    NUMERIC, and a few others. This adapter handles BINARY (always 2
    outcomes — YES/NO with implied probability) and MULTIPLE_CHOICE (N
    outcomes). FREE_RESPONSE and NUMERIC are not modeled cleanly by the
    Event/Outcome shape and are returned with a single placeholder
    outcome. *)

module Feed = Feed_only_adapter

let name = "manifold"
let homepage = "https://manifold.markets"
let us_geo_restricted = false

let api_host = "api.manifold.markets"

(* --- HTTP --- *)

let http_get ~path ?query () =
  let uri = Uri.make ~scheme:"https" ~host:api_host ~path ?query () in
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
    |> Deferred.bind ~f:(function
      | Error err -> return (Error (`Network (Core.Error.to_string_hum err)))
      | Ok (response, body) ->
        let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
        Cohttp_async.Body.to_string body
        >>| fun body_str ->
          (match code with
           | 200 ->
             (try Ok (Yojson.Safe.from_string body_str) with
              | Yojson.Json_error msg ->
                Error (`Json_parse (sprintf "JSON parse error: %s" msg)))
           | 404 -> Error (`Not_found body_str)
           | 429 -> Error (`Rate_limited body_str)
           | _ -> Error (`Http (code, body_str))))

let field name j =
  match j with
  | `Assoc fs -> List.Assoc.find fs ~equal:String.equal name
  | _ -> None

let str_field n j =
  match field n j with
  | Some (`String s) -> Some s
  | _ -> None

let bool_field n j =
  match field n j with
  | Some (`Bool b) -> Some b
  | _ -> None

let float_field n j =
  match field n j with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (Float.of_int i)
  | _ -> None

let parse_ts_ms_opt n j =
  match field n j with
  | Some (`Int ms) ->
    Some
      (Time_float_unix.of_span_since_epoch
         (Time_float.Span.of_sec (Float.of_int ms /. 1000.)))
  | Some (`Float ms) ->
    Some
      (Time_float_unix.of_span_since_epoch
         (Time_float.Span.of_sec (ms /. 1000.)))
  | _ -> None

(** Convert a Manifold market JSON to a Feed_only_adapter Event. *)
let parse_event (json : Yojson.Safe.t) : Feed.Event.t option =
  match str_field "id" json with
  | None -> None
  | Some id ->
    let title = Option.value (str_field "question" json) ~default:"" in
    let description = str_field "description" json in
    let outcome_type = Option.value (str_field "outcomeType" json) ~default:"" in
    let outcomes =
      match outcome_type with
      | "BINARY" ->
        let prob = float_field "probability" json in
        let mk label price =
          { Feed.Outcome.id = sprintf "%s:%s" id label
          ; label
          ; token_id = Option.map price ~f:Float.to_string
          }
        in
          [| mk "YES" prob
           ; mk "NO" (Option.map prob ~f:(fun p -> 1. -. p))
          |]
      | "MULTIPLE_CHOICE" | "MULTI_NUMERIC" ->
        (* Manifold returns answers under "answers" array each with id, text, probability *)
        (match field "answers" json with
         | Some (`List items) ->
           List.filter_map items ~f:(fun a ->
             match str_field "id" a, str_field "text" a with
             | Some aid, Some txt ->
               let prob = float_field "probability" a in
               Some
                 { Feed.Outcome.id = sprintf "%s:%s" id aid
                 ; label = txt
                 ; token_id = Option.map prob ~f:Float.to_string
                 }
             | _ -> None)
           |> Array.of_list
         | _ -> [||])
      | _ ->
        (* Free-response / numeric / pseudo-numeric — use single placeholder *)
        [| { Feed.Outcome.id = sprintf "%s:current" id
           ; label = "current"
           ; token_id = None
           }
        |]
    in
    let resolution_ts = parse_ts_ms_opt "closeTime" json in
    let is_resolved =
      Option.value (bool_field "isResolved" json) ~default:false
    in
    let resolved_outcome_id =
      match str_field "resolution" json with
      | Some r -> Some (sprintf "%s:%s" id r)
      | None -> None
    in
    let ts_listed = parse_ts_ms_opt "createdTime" json in
      Some
        { Feed.Event.id
        ; title
        ; description
        ; category = None
        ; outcomes
        ; resolution_ts
        ; is_resolved
        ; resolved_outcome_id
        ; ts_listed
        }

(* --- Public API --- *)

let list_events ?(filter = Feed.List_filter.default) () =
  let limit_str =
    match filter.limit with
    | Some n -> Int.to_string n
    | None -> "50"
  in
  let query = [ ("limit", [ limit_str ]) ] in
    http_get ~path:"/v0/markets" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      let events =
        List.filter_map items ~f:parse_event
        |> List.filter ~f:(fun (e : Feed.Event.t) ->
          match filter.include_resolved with
          | true -> true
          | false -> not e.is_resolved)
      in
        Ok events
    | Ok _ -> Error (`Json_parse "Expected JSON array from /v0/markets")

let get_event ~event_id =
  http_get ~path:(sprintf "/v0/market/%s" event_id) ()
  >>| function
  | Error e -> Error e
  | Ok json ->
    (match parse_event json with
     | Some e -> Ok e
     | None -> Error (`Json_parse "Failed to parse Manifold market"))

(** Manifold doesn't expose orderbook depth — it's a CFMM (CPMM-1).
    We construct a degenerate Quote from the implied probability. *)
let get_quote ~outcome_id =
  (* outcome_id encoded as "<event_id>:<outcome_label_or_id>" *)
  let event_id, outcome_key =
    match String.lsplit2 outcome_id ~on:':' with
    | Some (a, b) -> a, b
    | None -> outcome_id, "YES"
  in
  let%bind r = get_event ~event_id in
    match r with
    | Error e -> return (Error (e :> [> Feed.error ]))
    | Ok ev ->
      let prob_opt =
        Array.find_map ev.outcomes ~f:(fun (o : Feed.Outcome.t) ->
          (* outcome.id is "<event>:<key>"; match the key part *)
          match String.lsplit2 o.id ~on:':' with
          | Some (_, k) when String.equal k outcome_key ->
            Option.bind o.token_id ~f:(fun s ->
              try Some (Float.of_string s) with _ -> None)
          | _ -> None)
      in
        return
          (Ok
             { Feed.Quote.outcome_id
             ; last_price = prob_opt
             ; bid = prob_opt
             ; ask = prob_opt
             ; bid_size = None
             ; ask_size = None
             ; ts = Time_float_unix.now ()
             })

let get_book ~outcome_id ?depth () =
  let _ = depth in
  let%bind q = get_quote ~outcome_id in
    return
      (match q with
       | Error e -> Error e
       | Ok (q : Feed.Quote.t) ->
         (* Single-level "book" derived from the CPMM mid *)
         let bids =
           match q.bid with
           | Some p -> [| { Feed.Book_level.price = p; size = 0. } |]
           | None -> [||]
         in
         let asks =
           match q.ask with
           | Some p -> [| { Feed.Book_level.price = p; size = 0. } |]
           | None -> [||]
         in
           Ok { Feed.Book.outcome_id; bids; asks; ts = q.ts })

(** Recent bets for an event (Manifold's analog of trades). *)
let parse_trade event_id (json : Yojson.Safe.t) : Feed.Trade.t option =
  let outcome =
    match str_field "outcome" json with
    | Some o -> sprintf "%s:%s" event_id o
    | None -> event_id
  in
  let amount = Option.value (float_field "amount" json) ~default:0. in
  let prob_after = float_field "probAfter" json in
  let ts = parse_ts_ms_opt "createdTime" json in
  let trade_id = str_field "id" json in
  let aggressor =
    match str_field "outcome" json with
    | Some "YES" -> Some `Buy
    | Some "NO" -> Some `Sell
    | _ -> None
  in
    Some
      { Feed.Trade.outcome_id = outcome
      ; price = Option.value prob_after ~default:0.
      ; size = amount
      ; aggressor
      ; ts = Option.value ts ~default:(Time_float_unix.now ())
      ; trade_id
      }

let get_recent_trades ~outcome_id ?(limit = 50) () =
  let event_id, _ =
    match String.lsplit2 outcome_id ~on:':' with
    | Some (a, b) -> a, b
    | None -> outcome_id, ""
  in
  let query =
    [ ("contractId", [ event_id ]); ("limit", [ Int.to_string limit ]) ]
  in
    http_get ~path:"/v0/bets" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      Ok (List.filter_map items ~f:(parse_trade event_id))
    | Ok _ -> Error (`Json_parse "Expected JSON array from /v0/bets")

let get_resolved_events ?since ?(limit = 100) ?category () =
  let _ = since in
  let _ = category in
  let query =
    [ ("limit", [ Int.to_string limit ])
    ; ("sort", [ "resolved-newest" ])
    ]
  in
    http_get ~path:"/v0/markets" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      Ok
        (List.filter_map items ~f:parse_event
         |> List.filter ~f:(fun (e : Feed.Event.t) -> e.is_resolved))
    | Ok _ -> Error (`Json_parse "Expected JSON array")

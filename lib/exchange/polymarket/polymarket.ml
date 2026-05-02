(** Polymarket — read-only data feed.

    Implements {!Feed_only_adapter.S} against Polymarket's three public APIs:

    - {b Gamma API} ([gamma-api.polymarket.com]): market metadata,
      categories, listings.
    - {b CLOB API} ([clob.polymarket.com]): orderbook snapshots, midpoints.
    - {b Data API} ([data-api.polymarket.com]): trade history.

    All three are unauthenticated for read access. Polymarket trading
    itself is geo-restricted from US persons (CFTC settlement Jan 2022,
    $1.4M fine for unregistered swap exchange) — this adapter is
    feed-only by design and surfaces that fact via [us_geo_restricted = true].

    {b Polymarket structure mapping to Feed_only_adapter}: each
    Polymarket market is one binary event with two outcomes (typically
    YES/NO). The market's [clobTokenIds] array gives the on-chain ERC-1155
    token IDs for each outcome — those token_ids are also used as the
    canonical [outcome_id] in this adapter, since CLOB endpoints query
    by token_id directly. *)

module Feed = Feed_only_adapter

let name = "polymarket"
let homepage = "https://polymarket.com"
let us_geo_restricted = true

(* --- API hosts --- *)

let gamma_host = "gamma-api.polymarket.com"
let clob_host = "clob.polymarket.com"
let data_host = "data-api.polymarket.com"

(* --- HTTP helpers --- *)

(** Common error normalization: HTTP status codes → [Feed.error]. *)
let http_get ~host ~path ?query () =
  let uri = Uri.make ~scheme:"https" ~host ~path ?query () in
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
    |> Deferred.bind ~f:(function
      | Error err ->
        return (Error (`Network (Core.Error.to_string_hum err)))
      | Ok (response, body) ->
        let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
        Cohttp_async.Body.to_string body
        >>| fun body_str ->
          (match code with
           | 200 ->
             (try Ok (Yojson.Safe.from_string body_str) with
              | Yojson.Json_error msg ->
                Error (`Json_parse (sprintf "JSON parse error: %s" msg)))
           | 404 ->
             Error
               (`Not_found
                  (sprintf "%s%s returned 404" host path))
           | 429 ->
             Error (`Rate_limited body_str)
           | _ -> Error (`Http (code, body_str))))

(* --- JSON decoding helpers --- *)

let field name json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let str_field name json =
  match field name json with
  | Some (`String s) -> Some s
  | _ -> None

let bool_field name json =
  match field name json with
  | Some (`Bool b) -> Some b
  | _ -> None

let float_field name json =
  match field name json with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (Float.of_int i)
  | Some (`String s) ->
    (try Some (Float.of_string s) with _ -> None)
  | _ -> None

let parse_json_array_in_string s =
  try
    match Yojson.Safe.from_string s with
    | `List items -> Some items
    | _ -> None
  with _ -> None

let strs_of_jsonstring s =
  match parse_json_array_in_string s with
  | None -> []
  | Some items ->
    List.filter_map items ~f:(function
      | `String s -> Some s
      | _ -> None)

let parse_iso_ts s =
  try Some (Time_float_unix.of_string s) with _ -> None

(** Parse one Polymarket market JSON into a [Feed.Event.t].
    Each market has a [clobTokenIds] array (JSON-encoded string) of
    length 2 corresponding to [outcomes] (also JSON-encoded string array).
    We pair them in order to build [Outcome.t]s. *)
let parse_event (json : Yojson.Safe.t) : Feed.Event.t option =
  match str_field "id" json with
  | None -> None
  | Some id ->
    let title = Option.value (str_field "question" json) ~default:"" in
    let description = str_field "description" json in
    let category = str_field "groupItemTitle" json in
    let outcomes_str =
      Option.value (str_field "outcomes" json) ~default:"[]"
    in
    let token_ids_str =
      Option.value (str_field "clobTokenIds" json) ~default:"[]"
    in
    let outcome_labels = strs_of_jsonstring outcomes_str in
    let token_ids = strs_of_jsonstring token_ids_str in
    let outcomes =
      List.mapi outcome_labels ~f:(fun i label ->
        let token_id = List.nth token_ids i in
        let id =
          match token_id with
          | Some t -> t (* Use token_id as canonical outcome id *)
          | None -> sprintf "%s-outcome-%d" id i
        in
          { Feed.Outcome.id; label; token_id })
      |> Array.of_list
    in
    let resolution_ts =
      Option.bind (str_field "endDate" json) ~f:parse_iso_ts
    in
    let is_resolved = Option.value (bool_field "closed" json) ~default:false in
    let ts_listed =
      Option.bind (str_field "startDate" json) ~f:parse_iso_ts
    in
      Some
        { Feed.Event.id
        ; title
        ; description
        ; category
        ; outcomes
        ; resolution_ts
        ; is_resolved
        ; resolved_outcome_id = None
            (* Polymarket signals resolution via "closed" but doesn't
               directly name the winning outcome in /markets payload;
               UMA-resolved questions need separate lookup. *)
        ; ts_listed
        }

(* --- Public API surface --- *)

let list_events ?(filter = Feed.List_filter.default) () =
  let limit_str =
    match filter.limit with
    | Some n -> Int.to_string n
    | None -> "100"
  in
  let query =
    [ ("limit", [ limit_str ])
    ; ("active", [ "true" ])
    ; ("closed",
       [ match filter.include_resolved with
         | true -> "true"
         | false -> "false"
       ])
    ]
  in
    http_get ~host:gamma_host ~path:"/markets" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      let events =
        List.filter_map items ~f:parse_event
        |> (fun all ->
          match filter.category with
          | None -> all
          | Some cat ->
            List.filter all ~f:(fun (e : Feed.Event.t) ->
              match e.category with
              | Some c -> String.equal c cat
              | None -> false))
      in
        Ok events
    | Ok _ ->
      Error (`Json_parse "Expected JSON array from /markets")

let get_event ~event_id =
  http_get
    ~host:gamma_host
    ~path:(sprintf "/markets/%s" event_id)
    ()
  >>| function
  | Error e -> Error e
  | Ok json ->
    (match parse_event json with
     | Some ev -> Ok ev
     | None ->
       Error
         (`Json_parse
            (sprintf "Could not parse Polymarket market response for %s" event_id)))

(* --- CLOB calls (orderbook + midpoint) --- *)

(** Decode a single Polymarket book level: [{"price": "0.55", "size": "100"}]. *)
let parse_book_level (json : Yojson.Safe.t) : Feed.Book_level.t option =
  match float_field "price" json, float_field "size" json with
  | Some price, Some size -> Some { Feed.Book_level.price; size }
  | _ -> None

let parse_book ~outcome_id (json : Yojson.Safe.t) : (Feed.Book.t, [> Feed.error ]) Result.t =
  let bids_arr =
    match field "bids" json with
    | Some (`List items) ->
      List.filter_map items ~f:parse_book_level |> Array.of_list
    | _ -> [||]
  in
  let asks_arr =
    match field "asks" json with
    | Some (`List items) ->
      List.filter_map items ~f:parse_book_level |> Array.of_list
    | _ -> [||]
  in
  (* Polymarket bids come ascending (low → high price); we want descending
     by price for canonical bids. Asks come ascending, which is canonical. *)
  Array.sort bids_arr ~compare:(fun (a : Feed.Book_level.t) b ->
    Float.compare b.price a.price);
  Array.sort asks_arr ~compare:(fun (a : Feed.Book_level.t) b ->
    Float.compare a.price b.price);
  let ts =
    match field "timestamp" json with
    | Some (`String s) ->
      (try
         let ms = Int64.of_string s in
         let secs = Int64.to_float ms /. 1000. in
           Time_float_unix.of_span_since_epoch (Time_float.Span.of_sec secs)
       with _ -> Time_float_unix.now ())
    | _ -> Time_float_unix.now ()
  in
    Ok { Feed.Book.outcome_id; bids = bids_arr; asks = asks_arr; ts }

let get_book ~outcome_id ?depth () =
  let _ = depth in (* Polymarket returns full depth; depth ignored for v1 *)
  http_get
    ~host:clob_host
    ~path:"/book"
    ~query:[ ("token_id", [ outcome_id ]) ]
    ()
  >>| function
  | Error e -> Error e
  | Ok json -> parse_book ~outcome_id json

let get_quote ~outcome_id =
  (* Polymarket /book contains everything we need for a quote without
     a second round-trip; just derive from top-of-book. /midpoint is
     a cleaner endpoint but loses the bid/ask split. *)
  get_book ~outcome_id ()
  >>| function
  | Error e -> Error e
  | Ok (book : Feed.Book.t) ->
    let top side =
      match Array.length side with
      | 0 -> None, None
      | _ ->
        let l : Feed.Book_level.t = side.(0) in
          Some l.price, Some l.size
    in
    let bid, bid_size = top book.bids in
    let ask, ask_size = top book.asks in
    let last_price =
      match bid, ask with
      | Some b, Some a -> Some ((b +. a) /. 2.)
      | _ -> None
    in
      Ok
        { Feed.Quote.outcome_id
        ; last_price
        ; bid
        ; ask
        ; bid_size
        ; ask_size
        ; ts = book.ts
        }

(* --- Data API (trades) --- *)

(** A Polymarket trade response refers to outcomes by [asset] (token_id)
    or [outcomeIndex]. We pass through the asset id as outcome_id. *)
let parse_trade (json : Yojson.Safe.t) : Feed.Trade.t option =
  match str_field "asset" json with
  | None -> None
  | Some asset ->
    let price = Option.value (float_field "price" json) ~default:0. in
    let size = Option.value (float_field "size" json) ~default:0. in
    let aggressor =
      match str_field "side" json with
      | Some "BUY" -> Some `Buy
      | Some "SELL" -> Some `Sell
      | _ -> None
    in
    let ts =
      match field "timestamp" json with
      | Some (`Int i) ->
        Time_float_unix.of_span_since_epoch
          (Time_float.Span.of_sec (Float.of_int i))
      | Some (`Float f) ->
        Time_float_unix.of_span_since_epoch (Time_float.Span.of_sec f)
      | _ -> Time_float_unix.now ()
    in
    let trade_id = str_field "transactionHash" json in
      Some
        { Feed.Trade.outcome_id = asset
        ; price
        ; size
        ; aggressor
        ; ts
        ; trade_id
        }

let get_recent_trades ~outcome_id ?(limit = 50) () =
  (* Polymarket data-api expects market (conditionId) not token_id. We
     don't have conditionId in scope here directly — but the data-api
     also accepts ?asset_id=<token_id> for filtering. Use that. *)
  let query =
    [ ("market", [ outcome_id ])
        (* Polymarket also accepts asset_id; market here is overloaded
           to accept either. Token_id works for asset filtering. *)
    ; ("limit", [ Int.to_string limit ])
    ]
  in
    http_get ~host:data_host ~path:"/trades" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      let trades =
        List.filter_map items ~f:parse_trade
        |> List.filter ~f:(fun (t : Feed.Trade.t) ->
          (* Server might return all trades for the market across both
             outcomes; filter to the requested outcome. *)
          String.equal t.outcome_id outcome_id)
      in
        Ok trades
    | Ok _ ->
      Error (`Json_parse "Expected JSON array from /trades")

let get_resolved_events ?since ?(limit = 100) ?category () =
  let _ = since in (* TODO: filter by start_date_min with since-conversion *)
  let query =
    [ ("limit", [ Int.to_string limit ])
    ; ("active", [ "false" ])
    ; ("closed", [ "true" ])
    ]
  in
  let query =
    match category with
    | None -> query
    | Some _ -> query (* server doesn't filter by groupItemTitle; client-side below *)
  in
    http_get ~host:gamma_host ~path:"/markets" ~query ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      let events =
        List.filter_map items ~f:parse_event
        |> List.filter ~f:(fun (e : Feed.Event.t) ->
          match category with
          | None -> true
          | Some cat ->
            (match e.category with
             | Some c -> String.equal c cat
             | None -> false))
      in
        Ok events
    | Ok _ ->
      Error (`Json_parse "Expected JSON array from /markets (resolved)")

(** Pyth Network — read-only oracle price feed.

    Implements {!Feed_only_adapter.Spot_feed.S} against Hermes
    ([hermes.pyth.network]), Pyth's HTTP+SSE price-update gateway.
    Sub-second updates from the Pyth on-chain oracle, public no-auth.

    Pyth identifies feeds by 64-char hex {b feed_id}. Most consumers
    will know feeds by their {b symbol} (e.g. "Crypto.BTC/USD"); use
    {!list_symbols} to discover the feed_id from the symbol, or pass the
    feed_id directly to {!get_price} via the canonical [symbol] arg.

    {b Note on price scaling}: Pyth returns prices as scaled integers
    plus an [expo] field (e.g. price=7845450486315, expo=-8 means
    $78,454.50486315). This adapter normalizes to a plain [float] in
    {!Feed_only_adapter.Spot_feed.Price.t}. *)

module Spot = Feed_only_adapter.Spot_feed

let name = "pyth"
let homepage = "https://pyth.network"
let us_geo_restricted = false

let hermes_host = "hermes.pyth.network"

(* --- HTTP --- *)

let http_get ~path ~query () =
  let uri = Uri.make ~scheme:"https" ~host:hermes_host ~path ~query () in
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

(* --- JSON helpers --- *)

let field name json =
  match json with
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let str_field n j =
  match field n j with
  | Some (`String s) -> Some s
  | _ -> None

(** Pyth's price is encoded as a string-int with an exponent.
    [price * 10^expo] yields the human-readable value. *)
let scaled_to_float ~price_str ~expo =
  try
    let p = Float.of_string price_str in
      p *. (10. ** Float.of_int expo)
  with _ -> Float.nan

(** Decode the parsed price block from Hermes /v2/updates/price/latest.
    Shape: {parsed: [{id, price: {price, conf, expo, publish_time}, ema_price: ...}]}. *)
let parse_price_entry ~symbol json =
  let id =
    match str_field "id" json with
    | Some s -> s
    | None -> symbol
  in
  match field "price" json with
  | None -> None
  | Some price_obj ->
    let price_str = Option.value (str_field "price" price_obj) ~default:"0" in
    let conf_str = str_field "conf" price_obj in
    let expo =
      match field "expo" price_obj with
      | Some (`Int i) -> i
      | _ -> 0
    in
    let publish_time =
      match field "publish_time" price_obj with
      | Some (`Int i) -> i
      | _ -> 0
    in
    let price = scaled_to_float ~price_str ~expo in
    let confidence =
      Option.map conf_str ~f:(fun c -> scaled_to_float ~price_str:c ~expo)
    in
    let ts =
      Time_float_unix.of_span_since_epoch
        (Time_float.Span.of_sec (Float.of_int publish_time))
    in
    let ema_price =
      match field "ema_price" json with
      | Some ema ->
        let p = Option.value (str_field "price" ema) ~default:"0" in
        let e =
          match field "expo" ema with
          | Some (`Int i) -> i
          | _ -> 0
        in
          [ ("ema_price", Float.to_string (scaled_to_float ~price_str:p ~expo:e)) ]
      | None -> []
    in
      Some
        { Spot.Price.symbol = id
        ; price
        ; confidence
        ; ts
        ; venue_metadata = [ ("expo", Int.to_string expo) ] @ ema_price
        }

(* --- Public API --- *)

let normalize_feed_id s =
  match String.is_prefix s ~prefix:"0x" with
  | true -> String.drop_prefix s 2
  | false -> s

let list_symbols ?(query = "") ?(limit = 50) () =
  let q =
    [ ("query", [ query ]); ("limit", [ Int.to_string limit ]) ]
  in
    http_get ~path:"/v2/price_feeds" ~query:q ()
    >>| function
    | Error e -> Error e
    | Ok (`List items) ->
      let symbols =
        List.filter_map items ~f:(fun item ->
          let id = str_field "id" item in
          let attrs = field "attributes" item in
          match id, attrs with
          | Some feed_id, Some attrs ->
            let symbol =
              Option.value (str_field "symbol" attrs) ~default:feed_id
            in
            let asset_class = str_field "asset_type" attrs in
            let quote_currency = str_field "quote_currency" attrs in
            let description = str_field "description" attrs in
              Some
                { Spot.Symbol_info.symbol
                ; asset_class
                ; quote_currency
                ; description
                }
          | _ -> None)
      in
        Ok symbols
    | Ok _ ->
      Error (`Json_parse "Expected JSON array from /v2/price_feeds")

let get_price ~symbol =
  let feed_id = normalize_feed_id symbol in
  let q = [ ("ids[]", [ feed_id ]) ] in
    http_get ~path:"/v2/updates/price/latest" ~query:q ()
    >>| function
    | Error e -> Error e
    | Ok json ->
      (match field "parsed" json with
       | Some (`List (entry :: _)) ->
         (match parse_price_entry ~symbol entry with
          | Some p -> Ok p
          | None -> Error (`Json_parse "Could not decode Pyth price entry"))
       | Some (`List []) -> Error (`Symbol_unknown symbol)
       | _ -> Error (`Json_parse "Missing parsed[] in Hermes response"))

let get_prices ~symbols =
  let feed_ids = List.map symbols ~f:normalize_feed_id in
  let q = List.map feed_ids ~f:(fun id -> ("ids[]", [ id ])) in
    http_get ~path:"/v2/updates/price/latest" ~query:q ()
    >>| function
    | Error e -> Error e
    | Ok json ->
      (match field "parsed" json with
       | Some (`List entries) ->
         let prices =
           List.filter_map entries ~f:(fun entry ->
             parse_price_entry ~symbol:"" entry)
         in
           Ok prices
       | _ -> Error (`Json_parse "Missing parsed[] in Hermes response"))

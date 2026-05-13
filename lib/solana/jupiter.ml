open Core
open Async

let api_base = "https://api.jup.ag/swap/v1"

type error =
  [ `Network of string
  | `Http of int * string
  | `Json_parse of string ]

let sexp_of_error = function
  | `Network s -> Sexp.List [ Sexp.Atom "Network"; Sexp.Atom s ]
  | `Http (c, s) ->
    Sexp.List [ Sexp.Atom "Http"; Sexp.Atom (Int.to_string c); Sexp.Atom s ]
  | `Json_parse s -> Sexp.List [ Sexp.Atom "Json_parse"; Sexp.Atom s ]

type quote =
  { input_mint : string
  ; output_mint : string
  ; in_amount : int
  ; out_amount : int
  ; other_amount_threshold : int
  ; slippage_bps : int
  ; price_impact_pct : float
  ; raw_json : Yojson.Safe.t
  }

module Mint = struct
  let sol  = "So11111111111111111111111111111111111111112"
  let usdc = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
end

let parse_int_str s =
  match Int.of_string_opt s with
  | Some n -> Ok n
  | None -> Error (sprintf "not an int: %s" s)

let parse_float_str s =
  match Float.of_string_opt s with
  | Some f -> Ok f
  | None -> Error (sprintf "not a float: %s" s)

let quote_of_json json : (quote, string) Result.t =
  let open Yojson.Safe.Util in
  try
    let input_mint = json |> member "inputMint" |> to_string in
    let output_mint = json |> member "outputMint" |> to_string in
    let in_amount_s = json |> member "inAmount" |> to_string in
    let out_amount_s = json |> member "outAmount" |> to_string in
    let threshold_s = json |> member "otherAmountThreshold" |> to_string in
    let slippage_bps = json |> member "slippageBps" |> to_int in
    let price_impact_s = json |> member "priceImpactPct" |> to_string in
    let%bind.Result in_amount = parse_int_str in_amount_s in
    let%bind.Result out_amount = parse_int_str out_amount_s in
    let%bind.Result other_amount_threshold = parse_int_str threshold_s in
    let%bind.Result price_impact_pct = parse_float_str price_impact_s in
    Ok
      { input_mint
      ; output_mint
      ; in_amount
      ; out_amount
      ; other_amount_threshold
      ; slippage_bps
      ; price_impact_pct
      ; raw_json = json
      }
  with
  | Type_error (msg, _) -> Error (sprintf "field type: %s" msg)
  | exn -> Error (Exn.to_string exn)

let http_get ~uri =
  let%bind result =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
    |> Deferred.map
         ~f:(Result.map_error ~f:(fun err ->
                `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    (match code = 200 with
     | false -> return (Error (`Http (code, body_str)))
     | true ->
       (match Yojson.Safe.from_string body_str with
        | exception Yojson.Json_error m -> return (Error (`Json_parse m))
        | json -> return (Ok json)))

let http_post ~uri ~body_json =
  let body_str = Yojson.Safe.to_string body_json in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post ~body:(Cohttp_async.Body.of_string body_str) ~headers uri)
    |> Deferred.map
         ~f:(Result.map_error ~f:(fun err ->
                `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    (match code = 200 with
     | false -> return (Error (`Http (code, body_str)))
     | true ->
       (match Yojson.Safe.from_string body_str with
        | exception Yojson.Json_error m -> return (Error (`Json_parse m))
        | json -> return (Ok json)))

let get_quote ~input_mint ~output_mint ~amount ?(slippage_bps = 50) () =
  let query =
    [ ("inputMint", input_mint)
    ; ("outputMint", output_mint)
    ; ("amount", Int.to_string amount)
    ; ("slippageBps", Int.to_string slippage_bps)
    ]
  in
  let uri = Uri.with_query' (Uri.of_string (api_base ^ "/quote")) query in
  match%bind http_get ~uri with
  | Error _ as err -> return err
  | Ok json ->
    (match quote_of_json json with
     | Error msg -> return (Error (`Json_parse msg))
     | Ok q -> return (Ok q))

let get_swap_tx ~quote ~user_pubkey =
  let uri = Uri.of_string (api_base ^ "/swap") in
  let body =
    `Assoc
      [ ("quoteResponse", quote.raw_json)
      ; ("userPublicKey", `String user_pubkey)
      ; ("wrapAndUnwrapSol", `Bool true)
      ; ("dynamicComputeUnitLimit", `Bool true)
      ]
  in
  match%bind http_post ~uri ~body_json:body with
  | Error _ as err -> return err
  | Ok json ->
    let open Yojson.Safe.Util in
    (try
       let s = json |> member "swapTransaction" |> to_string in
       return (Ok s)
     with exn ->
       return
         (Error
            (`Json_parse
              (sprintf "swap response missing .swapTransaction: %s"
                 (Exn.to_string exn)))))

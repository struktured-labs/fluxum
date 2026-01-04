(** Jupiter Solana DEX Aggregator REST API

    Jupiter is the dominant DEX aggregator on Solana, routing trades
    through all major DEXs (Raydium, Orca, etc.) for optimal pricing.

    API requires an API key from https://dev.jup.ag
*)

open Core
open Async

(* ============================================================ *)
(* Error Types *)
(* ============================================================ *)

module Error = struct
  type http = [ `Http of int * string ]
  type json = [ `Json_parse of string ]
  type api = [ `Api_error of string ]
  type unauthorized = [ `Unauthorized ]
  type t = [ http | json | api | unauthorized ]

  let sexp_of_t = function
    | `Http (code, msg) ->
      Sexp.List [Sexp.Atom "Http"; Sexp.Atom (sprintf "%d: %s" code msg)]
    | `Json_parse msg ->
      Sexp.List [Sexp.Atom "Json_parse"; Sexp.Atom msg]
    | `Api_error msg ->
      Sexp.List [Sexp.Atom "Api_error"; Sexp.Atom msg]
    | `Unauthorized ->
      Sexp.List [Sexp.Atom "Unauthorized"; Sexp.Atom "API key required"]
end

(* ============================================================ *)
(* Token Constants - Common Solana tokens *)
(* ============================================================ *)

module Tokens = struct
  let sol = "So11111111111111111111111111111111111111112"
  let usdc = "EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v"
  let usdt = "Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB"
  let bonk = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263"
  let jup = "JUPyiwrYJFskUPiHa7hkeR8VUtAeFoSYbKedZNsDvCN"
end

(* ============================================================ *)
(* Response Types *)
(* ============================================================ *)

module Types = struct
  (** Swap info within a route *)
  type swap_info = {
    ammKey : string;
    label : string;
    inputMint : string;
    outputMint : string;
    inAmount : string;
    outAmount : string;
    feeAmount : string; [@default "0"]
    feeMint : string; [@default ""]
  } [@@deriving yojson { strict = false }, sexp]

  (** Route plan step *)
  type route_step = {
    swapInfo : swap_info;
    percent : int;
  } [@@deriving yojson { strict = false }, sexp]

  (** Quote response *)
  type quote = {
    inputMint : string;
    inAmount : string;
    outputMint : string;
    outAmount : string;
    otherAmountThreshold : string;
    swapMode : string;
    slippageBps : int;
    priceImpactPct : string;
    routePlan : route_step list;
    contextSlot : int option; [@default None]
    timeTaken : float option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Swap transaction response *)
  type swap_transaction = {
    swapTransaction : string;  (* base64-encoded transaction *)
    lastValidBlockHeight : int;
    prioritizationFeeLamports : int option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Price response (from Price API) *)
  type price_data = {
    id : string;
    mintSymbol : string option; [@default None]
    vsToken : string option; [@default None]
    vsTokenSymbol : string option; [@default None]
    price : float;
  } [@@deriving yojson { strict = false }, sexp]

  type prices_response = {
    data : (string * price_data) list;
    timeTaken : float option; [@default None]
  } [@@deriving sexp]

  let prices_response_of_yojson json =
    match json with
    | `Assoc [("data", `Assoc prices); _] | `Assoc [_; ("data", `Assoc prices)] ->
      let parsed = List.filter_map prices ~f:(fun (id, price_json) ->
        match price_data_of_yojson price_json with
        | Ok p -> Some (id, p)
        | Error _ -> None
      ) in
      Ok { data = parsed; timeTaken = None }
    | `Assoc [("data", `Assoc prices)] ->
      let parsed = List.filter_map prices ~f:(fun (id, price_json) ->
        match price_data_of_yojson price_json with
        | Ok p -> Some (id, p)
        | Error _ -> None
      ) in
      Ok { data = parsed; timeTaken = None }
    | _ -> Error "Expected {data: {...}} object"

  (** Token info *)
  type token_info = {
    address : string;
    chainId : int option; [@default None]
    decimals : int;
    name : string;
    symbol : string;
    logoURI : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]
end

(* ============================================================ *)
(* HTTP Client *)
(* ============================================================ *)

let get (module Cfg : Cfg.S) ~path ?params () : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  match Cfg.api_key with
  | None -> Deferred.return (Error `Unauthorized)
  | Some api_key ->
    let base_uri = Uri.of_string Cfg.api_url in
    let uri = Uri.with_path base_uri path in
    let uri = match params with
      | None -> uri
      | Some p -> Uri.add_query_params' uri p
    in
    let headers = Cohttp.Header.of_list [
      ("x-api-key", api_key);
      ("Accept", "application/json");
    ] in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.get ~headers uri
      >>= fun (response, body) ->
      Cohttp_async.Body.to_string body
      >>| fun body_str ->
      (response, body_str)
    )
    >>| function
    | Error exn ->
      Error (`Http (0, Exn.to_string exn))
    | Ok (response, body_str) ->
      let status = Cohttp.Response.status response in
      let code = Cohttp.Code.code_of_status status in
      match code with
      | 401 -> Error `Unauthorized
      | c when c >= 400 -> Error (`Http (code, body_str))
      | _ ->
        match Yojson.Safe.from_string body_str with
        | exception _ -> Error (`Json_parse body_str)
        | json -> Ok json

let post (module Cfg : Cfg.S) ~path ~body : (Yojson.Safe.t, [> Error.t ]) result Deferred.t =
  match Cfg.api_key with
  | None -> Deferred.return (Error `Unauthorized)
  | Some api_key ->
    let base_uri = Uri.of_string Cfg.api_url in
    let uri = Uri.with_path base_uri path in
    let headers = Cohttp.Header.of_list [
      ("x-api-key", api_key);
      ("Content-Type", "application/json");
      ("Accept", "application/json");
    ] in
    let body_str = Yojson.Safe.to_string body in
    Monitor.try_with (fun () ->
      Cohttp_async.Client.post ~headers
        ~body:(Cohttp_async.Body.of_string body_str) uri
      >>= fun (response, body) ->
      Cohttp_async.Body.to_string body
      >>| fun body_str ->
      (response, body_str)
    )
    >>| function
    | Error exn ->
      Error (`Http (0, Exn.to_string exn))
    | Ok (response, body_str) ->
      let status = Cohttp.Response.status response in
      let code = Cohttp.Code.code_of_status status in
      match code with
      | 401 -> Error `Unauthorized
      | c when c >= 400 -> Error (`Http (code, body_str))
      | _ ->
        match Yojson.Safe.from_string body_str with
        | exception _ -> Error (`Json_parse body_str)
        | json -> Ok json

(* ============================================================ *)
(* API Endpoints *)
(* ============================================================ *)

(** Get a swap quote *)
let quote cfg ~input_mint ~output_mint ~amount ?(slippage_bps=50) ()
    : (Types.quote, [> Error.t ]) result Deferred.t =
  let params = [
    ("inputMint", input_mint);
    ("outputMint", output_mint);
    ("amount", Int.to_string amount);
    ("slippageBps", Int.to_string slippage_bps);
  ] in
  get cfg ~path:"/quote" ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.quote_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok quote -> Ok quote

(** Build swap transaction from quote *)
let swap cfg ~(quote : Types.quote) ~user_public_key
    : (Types.swap_transaction, [> Error.t ]) result Deferred.t =
  let body = `Assoc [
    ("quoteResponse", Types.quote_to_yojson quote);
    ("userPublicKey", `String user_public_key);
    ("wrapUnwrapSOL", `Bool true);
    ("computeUnitPriceMicroLamports", `String "auto");
  ] in
  post cfg ~path:"/swap" ~body >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.swap_transaction_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok tx -> Ok tx

(** Get prices for multiple tokens *)
let prices cfg ~mints
    : (Types.prices_response, [> Error.t ]) result Deferred.t =
  let ids = String.concat ~sep:"," mints in
  let params = [("ids", ids)] in
  (* Price API uses different endpoint *)
  let cfg_with_price_url = (module struct
    let api_url = "https://api.jup.ag/price/v2"
    let api_key = let (module C : Cfg.S) = cfg in C.api_key
  end : Cfg.S) in
  get cfg_with_price_url ~path:"" ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.prices_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Get quote as a synthetic order book (best bid/ask) *)
let order_book cfg ~base_mint ~quote_mint ~amount
    : ((Types.quote * Types.quote), [> Error.t ]) result Deferred.t =
  (* Get "buy" quote (base -> quote, what you get for selling base) *)
  let%bind sell_quote = quote cfg ~input_mint:base_mint ~output_mint:quote_mint ~amount () in
  match sell_quote with
  | Error _ as err -> Deferred.return err
  | Ok sq ->
    (* Get "sell" quote (quote -> base, what you need to buy base) *)
    let quote_amount =
      match Int.of_string_opt sq.outAmount with
      | Some a -> a
      | None -> amount
    in
    let%bind buy_quote = quote cfg ~input_mint:quote_mint ~output_mint:base_mint ~amount:quote_amount () in
    match buy_quote with
    | Error _ as err -> Deferred.return err
    | Ok bq -> Deferred.return (Ok (sq, bq))

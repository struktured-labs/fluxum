(** 1inch EVM DEX Aggregator REST API

    1inch is the leading DEX aggregator across EVM chains (Ethereum,
    BSC, Polygon, Arbitrum, etc.), routing trades through 400+ liquidity
    sources for optimal pricing.

    API requires an API key from https://portal.1inch.dev
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
(* Token Constants - Common EVM tokens *)
(* ============================================================ *)

module Tokens = struct
  (* Ethereum Mainnet *)
  let eth = "0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE"  (* Native ETH *)
  let weth = "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"
  let usdc = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  let usdt = "0xdAC17F958D2ee523a2206206994597C13D831ec7"
  let dai = "0x6B175474E89094C44Da98b954EeseDCDE5aE0D3CE7"
  let wbtc = "0x2260FAC5E5542a773Aa44fBCfeDf7C193bc2C599"

  (* BSC *)
  let bnb = "0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE"
  let wbnb = "0xbb4CdB9CBd36B01bD1cBaEBF2De08d9173bc095c"
  let busd = "0xe9e7CEA3DedcA5984780Bafc599bD69ADd087D56"

  (* Polygon *)
  let matic = "0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE"
  let wmatic = "0x0d500B1d8E8eF31E21C99d1Db9A6444d3ADf1270"
  let usdc_polygon = "0x2791Bca1f2de4661ED88A30C99A7a9449Aa84174"
end

(* ============================================================ *)
(* Response Types *)
(* ============================================================ *)

module Types = struct
  (** Token info *)
  type token_info = {
    address : string;
    symbol : string;
    name : string;
    decimals : int;
    logoURI : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Protocol info in route *)
  type protocol_step = {
    name : string;
    part : float;
    fromTokenAddress : string;
    toTokenAddress : string;
  } [@@deriving yojson { strict = false }, sexp]

  (** Quote response *)
  type quote = {
    fromToken : token_info;
    toToken : token_info;
    toAmount : string;
    gas : int option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Swap transaction data *)
  type swap_tx = {
    from : string;
    to_ : string; [@key "to"]
    data : string;
    value : string;
    gas : int option; [@default None]
    gasPrice : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  (** Swap response *)
  type swap = {
    fromToken : token_info;
    toToken : token_info;
    toAmount : string;
    tx : swap_tx;
  } [@@deriving yojson { strict = false }, sexp]

  (** Token list response *)
  type tokens_response = {
    tokens : (string * token_info) list;
  } [@@deriving sexp]

  let tokens_response_of_yojson json =
    match json with
    | `Assoc [("tokens", `Assoc tokens)] ->
      let parsed = List.filter_map tokens ~f:(fun (addr, token_json) ->
        match token_info_of_yojson token_json with
        | Ok t -> Some (addr, t)
        | Error _ -> None
      ) in
      Ok { tokens = parsed }
    | _ -> Error "Expected {tokens: {...}} object"

  (** Liquidity sources *)
  type liquidity_source = {
    id : string;
    title : string;
    img : string option; [@default None]
  } [@@deriving yojson { strict = false }, sexp]

  type liquidity_sources_response = {
    protocols : liquidity_source list;
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
    let uri = Uri.with_path base_uri (Uri.path base_uri ^ path) in
    let uri = match params with
      | None -> uri
      | Some p -> Uri.add_query_params' uri p
    in
    let headers = Cohttp.Header.of_list [
      ("Authorization", sprintf "Bearer %s" api_key);
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

(* ============================================================ *)
(* API Endpoints *)
(* ============================================================ *)

(** Get a swap quote *)
let quote cfg ~src ~dst ~amount ?(slippage=1.0) ()
    : (Types.quote, [> Error.t ]) result Deferred.t =
  let params = [
    ("src", src);
    ("dst", dst);
    ("amount", amount);
    ("slippage", Float.to_string slippage);
  ] in
  get cfg ~path:"/quote" ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.quote_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok quote -> Ok quote

(** Build swap transaction *)
let swap cfg ~src ~dst ~amount ~from_address ?(slippage=1.0) ()
    : (Types.swap, [> Error.t ]) result Deferred.t =
  let params = [
    ("src", src);
    ("dst", dst);
    ("amount", amount);
    ("from", from_address);
    ("slippage", Float.to_string slippage);
  ] in
  get cfg ~path:"/swap" ~params () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.swap_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok swap -> Ok swap

(** Get list of supported tokens *)
let tokens cfg : (Types.tokens_response, [> Error.t ]) result Deferred.t =
  get cfg ~path:"/tokens" () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.tokens_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Get list of liquidity sources *)
let liquidity_sources cfg : (Types.liquidity_sources_response, [> Error.t ]) result Deferred.t =
  get cfg ~path:"/liquidity-sources" () >>| function
  | Error _ as err -> err
  | Ok json ->
    match Types.liquidity_sources_response_of_yojson json with
    | Error e -> Error (`Json_parse e)
    | Ok resp -> Ok resp

(** Get synthetic order book from quote *)
let order_book cfg ~base_token ~quote_token ~amount
    : ((Types.quote * Types.quote), [> Error.t ]) result Deferred.t =
  (* Get "sell" quote (base -> quote) *)
  let%bind sell_quote = quote cfg ~src:base_token ~dst:quote_token ~amount () in
  match sell_quote with
  | Error _ as err -> Deferred.return err
  | Ok sq ->
    (* Get "buy" quote (quote -> base) *)
    let%bind buy_quote = quote cfg ~src:quote_token ~dst:base_token ~amount:sq.toAmount () in
    match buy_quote with
    | Error _ as err -> Deferred.return err
    | Ok bq -> Deferred.return (Ok (sq, bq))

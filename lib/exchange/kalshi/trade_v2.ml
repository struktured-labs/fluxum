(** Kalshi Trade API v2 helpers: environment guard, fixed-point helpers, fee
    math, order-book quoting and the V2 order-entry endpoints
    (/portfolio/events/orders).

    Generic exchange plumbing only; no account- or strategy-specific logic. *)

let base_path = Prediction_markets.base_path

(* ------------------------------------------------------------------------- *)
(* Environment selection with an explicit production guard                    *)
(* ------------------------------------------------------------------------- *)

module Env = struct
  type t = Demo | Production [@@deriving sexp, equal, compare]

  let to_string = function Demo -> "demo" | Production -> "production"
  let rest_host = function
    | Demo -> "external-api.demo.kalshi.co"
    | Production -> "external-api.kalshi.com"

  let ws_url = function
    | Demo -> "wss://external-api-ws.demo.kalshi.co/trade-api/ws/v2"
    | Production -> "wss://external-api-ws.kalshi.com/trade-api/ws/v2"

  let allow_production_env_var = "KALSHI_ALLOW_PRODUCTION"

  let production_allowed_by_env () =
    match Sys.getenv allow_production_env_var with
    | Some v -> List.mem [ "1"; "yes"; "true" ] (String.lowercase v) ~equal:String.equal
    | None -> false

  let strict_param name =
    match Sys.getenv name with
    | Some v when not (String.is_empty (String.strip v)) -> Ok v
    | _ -> Or_error.errorf "%s is not set" name

  (** Authenticated config for [t]. Only the env-specific variables are read
      (KALSHI_DEMO_API_KEY / KALSHI_DEMO_PRIVATE_KEY_PATH, or the
      KALSHI_PRODUCTION_* pair) -- never the generic KALSHI_API_KEY fallback,
      so a production key can't be picked up by a demo run. Production
      additionally requires BOTH [~allow_production:true] (an explicit caller
      flag) AND KALSHI_ALLOW_PRODUCTION=1 in the environment. *)
  let authed_cfg ?(allow_production = false) t : (module Cfg.S) Or_error.t =
    let open Or_error.Let_syntax in
    let%bind () =
      match t with
      | Demo -> Ok ()
      | Production ->
        if allow_production && production_allowed_by_env () then Ok ()
        else
          Or_error.errorf
            "Kalshi production trading refused: pass the explicit production \
             flag AND set %s=1"
            allow_production_env_var
    in
    let prefix = match t with Demo -> "KALSHI_DEMO_" | Production -> "KALSHI_PRODUCTION_" in
    let%bind api_key = strict_param (prefix ^ "API_KEY") in
    let%bind private_key_path = strict_param (prefix ^ "PRIVATE_KEY_PATH") in
    let%bind () =
      if Stdlib.Sys.file_exists private_key_path then Ok ()
      else Or_error.errorf "%sPRIVATE_KEY_PATH does not point to a file" prefix
    in
    let api_host = rest_host t in
    Ok (module struct
      let api_host = api_host
      let api_key = api_key
      let private_key_path = private_key_path
    end : Cfg.S)

  (** Unauthenticated (public market data) config; any env is fine. *)
  let public_cfg ?host t : (module Cfg.S) =
    let api_host = Option.value host ~default:(rest_host t) in
    (module struct
      let api_host = api_host
      let api_key = ""
      let private_key_path = ""
    end : Cfg.S)
end

(* ------------------------------------------------------------------------- *)
(* Fixed-point strings                                                        *)
(* ------------------------------------------------------------------------- *)

module Fp = struct
  (** Parse a decimal string into an integer scaled by 10^decimals,
      truncating extra digits. "0.5600" ~decimals:4 -> 5600. *)
  let parse_scaled ~decimals s =
    let s = String.strip s in
    let neg, s =
      match String.chop_prefix s ~prefix:"-" with Some r -> (true, r) | None -> (false, s)
    in
    let ip, fp = match String.lsplit2 s ~on:'.' with Some (a, b) -> (a, b) | None -> (s, "") in
    let fp =
      if String.length fp >= decimals then String.prefix fp decimals
      else fp ^ String.make (decimals - String.length fp) '0'
    in
    let ip = if String.is_empty ip then "0" else ip in
    match Int.of_string_opt ip, (if decimals = 0 then Some 0 else Int.of_string_opt fp) with
    | Some i, Some f ->
      let v = (i * Int.pow 10 decimals) + f in
      Some (if neg then -v else v)
    | _ -> None

  let price_to_string_scaled p = sprintf "%d.%04d" (p / 10_000) (p mod 10_000)

  (** Dollar price as a 4-dp wire string, e.g. 0.56 -> "0.5600". *)
  let price_to_string (p : float) =
    price_to_string_scaled (Float.iround_nearest_exn (p *. 10_000.))

  (** Contract count as a 2-dp wire string, e.g. 10 -> "10.00". *)
  let count_to_string (c : float) =
    let h = Float.iround_nearest_exn (c *. 100.) in
    sprintf "%d.%02d" (h / 100) (h mod 100)

  let to_float s = Float.of_string_opt (String.strip s)
end

(* ------------------------------------------------------------------------- *)
(* Fees                                                                       *)
(* ------------------------------------------------------------------------- *)

module Fees = struct
  (** Kalshi fee schedule (effective 2026-07-07):
        taker = round_up(M * 0.07   * C * P * (1 - P))
        maker = round_up(M * 0.0175 * C * P * (1 - P))   (series-dependent)
      P is the contract price in dollars, C the contract count, M the series
      [fee_multiplier]. Maker fees apply only to series whose [fee_type] is
      [quadratic_with_maker_fees] (0.0175) or
      [quadratic_with_combo_maker_fees] (0.035); plain [quadratic] series charge
      no maker fee. Kalshi's exact settlement rounding has sub-cent components
      (a fee accumulator); [Cent] (whole-cent ceiling per order) is the
      conservative default for pre-trade estimates. *)

  type rounding = Cent | Centicent | Exact [@@deriving sexp, equal]

  type fee_type =
    | Quadratic
    | Quadratic_with_maker_fees
    | Quadratic_with_combo_maker_fees
    | Flat
    | Unknown of string
  [@@deriving sexp, equal]

  let fee_type_of_string = function
    | "quadratic" -> Quadratic
    | "quadratic_with_maker_fees" -> Quadratic_with_maker_fees
    | "quadratic_with_combo_maker_fees" -> Quadratic_with_combo_maker_fees
    | "flat" -> Flat
    | s -> Unknown s

  let taker_rate = 0.07
  let maker_rate = function
    | Quadratic_with_maker_fees -> 0.0175
    | Quadratic_with_combo_maker_fees -> 0.035
    | Quadratic | Flat | Unknown _ -> 0.0

  let raw ~rate ~multiplier ~count ~price =
    Float.max 0. (rate *. multiplier *. count *. price *. (1. -. price))

  let round_up ~rounding x =
    match rounding with
    | Exact -> x
    | Cent | Centicent ->
      let q = match rounding with Cent -> 100. | _ -> 10_000. in
      (* small epsilon so exact values like 1.75 don't round up to 1.76 *)
      let r = Float.round_up ((x *. q) -. 1e-7) /. q in
      if Float.(r <= 0.) then 0. else r

  let taker_fee ?(multiplier = 1.0) ?(rounding = Cent) ~count ~price () =
    round_up ~rounding (raw ~rate:taker_rate ~multiplier ~count ~price)

  let maker_fee ?(fee_type = Quadratic) ?(multiplier = 1.0) ?(rounding = Cent) ~count ~price () =
    round_up ~rounding (raw ~rate:(maker_rate fee_type) ~multiplier ~count ~price)
end

(* ------------------------------------------------------------------------- *)
(* Order book + fee-inclusive quoting                                         *)
(* ------------------------------------------------------------------------- *)

type outcome = Yes | No [@@deriving sexp, equal, compare]

let outcome_of_string s =
  match String.lowercase s with
  | "yes" | "y" -> Ok Yes
  | "no" | "n" -> Ok No
  | _ -> Or_error.errorf "outcome must be yes|no, got %S" s

let outcome_to_string = function Yes -> "yes" | No -> "no"

let round4 x = Float.round_nearest (x *. 10_000.) /. 10_000.

module Book = struct
  type level = { price : float; size : float } [@@deriving sexp, equal]

  (** Bids only (Kalshi returns only bids). Best (highest) first. *)
  type t = { yes_bids : level list; no_bids : level list } [@@deriving sexp]

  let parse_levels (ls : (string * string) list) =
    List.filter_map ls ~f:(fun (p, q) ->
      match Fp.to_float p, Fp.to_float q with
      | Some price, Some size when Float.(size > 0.) -> Some { price; size }
      | _ -> None)
    |> List.sort ~compare:(fun a b -> Float.compare b.price a.price)

  let of_orderbook (ob : Prediction_markets.Orderbook.t) =
    { yes_bids = parse_levels ob.yes; no_bids = parse_levels ob.no }

  (** Bids for [outcome], best first. *)
  let bids t = function Yes -> t.yes_bids | No -> t.no_bids

  (** Asks to BUY [outcome], best (lowest) first. A YES ask at p is a NO bid at
      1 - p and vice versa. *)
  let asks t outcome =
    let opp = match outcome with Yes -> t.no_bids | No -> t.yes_bids in
    List.map opp ~f:(fun l -> { l with price = round4 (1. -. l.price) })

  let best_bid t o = List.hd (bids t o)
  let best_ask t o = List.hd (asks t o)
end

module Quote = struct
  type t =
    { outcome : outcome
    ; requested : float
    ; filled : float  (** contracts fillable within the limit *)
    ; avg_price : float
    ; worst_price : float
    ; notional : float  (** sum price*size, before fees *)
    ; fee : float  (** estimated taker fee *)
    ; total_cost : float
    ; payout : float  (** $1 per contract if the outcome wins *)
    ; net_profit_if_win : float
    ; multiple : float  (** payout / total_cost *)
    ; breakeven_prob : float  (** total_cost / payout *)
    ; levels : Book.level list  (** fills per level *)
    }
  [@@deriving sexp]

  (** Walk the asks for [outcome] buying [count] contracts as a taker, stopping
      at [limit] (inclusive) if given. The fee is computed on the whole order
      (sum of raw per-level fees, rounded once). *)
  let buy ?limit ?(multiplier = 1.0) ?(rounding = Fees.Cent) ~book ~outcome ~count () =
    let rec walk remaining acc = function
      | [] -> List.rev acc
      | _ when Float.(remaining <= 0.) -> List.rev acc
      | (l : Book.level) :: rest ->
        (match limit with
         | Some lim when Float.(l.price > lim +. 1e-9) -> List.rev acc
         | _ ->
           let take = Float.min remaining l.size in
           walk (remaining -. take) ({ l with size = take } :: acc) rest)
    in
    let levels = walk count [] (Book.asks book outcome) in
    let filled = List.sum (module Float) levels ~f:(fun l -> l.size) in
    let notional = List.sum (module Float) levels ~f:(fun l -> l.price *. l.size) in
    let raw_fee =
      List.sum (module Float) levels ~f:(fun l ->
        Fees.raw ~rate:Fees.taker_rate ~multiplier ~count:l.size ~price:l.price)
    in
    let fee = Fees.round_up ~rounding raw_fee in
    let total_cost = notional +. fee in
    let payout = filled in
    let div a b = if Float.(b > 0.) then a /. b else Float.nan in
    { outcome; requested = count; filled
    ; avg_price = div notional filled
    ; worst_price = (match List.last levels with Some l -> l.price | None -> Float.nan)
    ; notional; fee; total_cost; payout
    ; net_profit_if_win = payout -. total_cost
    ; multiple = div payout total_cost
    ; breakeven_prob = div total_cost payout
    ; levels }

  (** Resting (maker) limit buy at [price]: cost/payout if fully filled. *)
  let limit_buy ?(multiplier = 1.0) ?(fee_type = Fees.Quadratic) ?(rounding = Fees.Cent)
        ~outcome ~count ~price () =
    let notional = price *. count in
    let fee = Fees.maker_fee ~fee_type ~multiplier ~rounding ~count ~price () in
    let total_cost = notional +. fee in
    let div a b = if Float.(b > 0.) then a /. b else Float.nan in
    { outcome; requested = count; filled = count; avg_price = price; worst_price = price
    ; notional; fee; total_cost; payout = count
    ; net_profit_if_win = count -. total_cost
    ; multiple = div count total_cost
    ; breakeven_prob = div total_cost count
    ; levels = [ { Book.price; size = count } ] }
end

(* ------------------------------------------------------------------------- *)
(* V2 order request                                                           *)
(* ------------------------------------------------------------------------- *)

module Order = struct
  type book_side = Bid | Ask [@@deriving sexp, equal]
  type tif = Gtc | Ioc | Fok [@@deriving sexp, equal]
  type stp = Taker_at_cross | Maker [@@deriving sexp, equal]

  let book_side_to_string = function Bid -> "bid" | Ask -> "ask"
  let tif_to_string = function
    | Gtc -> "good_till_canceled" | Ioc -> "immediate_or_cancel" | Fok -> "fill_or_kill"
  let tif_of_string s =
    match String.lowercase s with
    | "gtc" | "good_till_canceled" -> Ok Gtc
    | "ioc" | "immediate_or_cancel" -> Ok Ioc
    | "fok" | "fill_or_kill" -> Ok Fok
    | _ -> Or_error.errorf "time-in-force must be gtc|ioc|fok, got %S" s
  let stp_to_string = function Taker_at_cross -> "taker_at_cross" | Maker -> "maker"

  type request =
    { ticker : string
    ; side : book_side  (** on the YES book: Bid = buy YES, Ask = sell YES / buy NO *)
    ; count : float
    ; price : float  (** YES price in dollars *)
    ; time_in_force : tif
    ; self_trade_prevention : stp
    ; post_only : bool
    ; reduce_only : bool
    ; client_order_id : string option
    ; expiration_time : int option  (** unix seconds, GTC only *)
    ; subaccount : int option
    }
  [@@deriving sexp]

  (** Build a request to BUY [outcome] at [price] (the price of that outcome).
      Buying NO at q == asking (selling) YES at 1 - q. *)
  let buy ?(time_in_force = Gtc) ?(self_trade_prevention = Taker_at_cross)
        ?(post_only = false) ?client_order_id ?expiration_time ?subaccount
        ~ticker ~outcome ~count ~price () =
    let side, yes_price =
      match outcome with Yes -> (Bid, price) | No -> (Ask, round4 (1. -. price))
    in
    { ticker; side; count; price = yes_price; time_in_force; self_trade_prevention
    ; post_only; reduce_only = false; client_order_id; expiration_time; subaccount }

  let validate r =
    let open Or_error.Let_syntax in
    let%bind () =
      if Float.(r.price > 0. && r.price < 1.) then Ok ()
      else Or_error.errorf "price %.4f outside (0,1)" r.price
    in
    let%bind () =
      if Float.(r.count > 0.) then Ok () else Or_error.errorf "count must be > 0"
    in
    let%bind () =
      if r.reduce_only && not (equal_tif r.time_in_force Ioc)
      then Or_error.error_string "reduce_only requires immediate_or_cancel" else Ok ()
    in
    if Option.is_some r.expiration_time && not (equal_tif r.time_in_force Gtc)
    then Or_error.error_string "expiration_time requires good_till_canceled"
    else Ok ()

  let to_yojson r : Yojson.Safe.t =
    let opt k f = function Some v -> [ (k, f v) ] | None -> [] in
    `Assoc
      ([ ("ticker", `String r.ticker)
       ; ("side", `String (book_side_to_string r.side))
       ; ("count", `String (Fp.count_to_string r.count))
       ; ("price", `String (Fp.price_to_string r.price))
       ; ("time_in_force", `String (tif_to_string r.time_in_force))
       ; ("self_trade_prevention_type", `String (stp_to_string r.self_trade_prevention))
       ]
       @ (if r.post_only then [ ("post_only", `Bool true) ] else [])
       @ (if r.reduce_only then [ ("reduce_only", `Bool true) ] else [])
       @ opt "client_order_id" (fun s -> `String s) r.client_order_id
       @ opt "expiration_time" (fun i -> `Int i) r.expiration_time
       @ opt "subaccount" (fun i -> `Int i) r.subaccount)

  type response =
    { order_id : string
    ; client_order_id : string option
    ; fill_count : string option
    ; remaining_count : string option
    ; ts_ms : int option
    }
  [@@deriving sexp]

  let response_of_yojson (j : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    match j |> member "order_id" with
    | `String order_id ->
      let str k = match member k j with `String s -> Some s | _ -> None in
      Ok { order_id; client_order_id = str "client_order_id"
         ; fill_count = str "fill_count"; remaining_count = str "remaining_count"
         ; ts_ms = (match member "ts_ms" j with `Int i -> Some i | _ -> None) }
    | _ -> Or_error.errorf "unexpected order response: %s" (Yojson.Safe.to_string j)
end

(* ------------------------------------------------------------------------- *)
(* HTTP                                                                       *)
(* ------------------------------------------------------------------------- *)

type error =
  [ `Http of int * string
  | `Json_parse_error of string
  | `Unauthorized
  | `Not_found
  | `Rate_limited
  | `Network of string ]
[@@deriving sexp]

let error_to_string (e : error) = Sexp.to_string_hum (sexp_of_error e)

let parse_response code body : (Yojson.Safe.t, error) Result.t =
  match code with
  | c when c >= 200 && c < 300 ->
    if String.is_empty (String.strip body) then Ok `Null
    else (
      match Yojson.Safe.from_string body with
      | j -> Ok j
      | exception Yojson.Json_error m -> Error (`Json_parse_error m))
  | 401 -> Error `Unauthorized
  | 404 when String.is_substring body ~substring:"insufficient_shard_balance" ->
    (* user not provisioned / unfunded on the market's exchange shard *)
    Error (`Http (404, body))
  | 404 -> Error `Not_found
  | 429 -> Error `Rate_limited  (* no Retry-After; caller should back off *)
  | c -> Error (`Http (c, body))

(** One request. [authed] signs with the cfg's key; the signature covers the
    path only (never the query string). *)
let call ?(authed = true) ?(query = []) ?body ~(cfg : (module Cfg.S)) ~meth path
  : (Yojson.Safe.t, error) Result.t Deferred.t =
  let module C = (val cfg : Cfg.S) in
  let path = base_path ^ path in
  let uri = Uri.make ~scheme:"https" ~host:C.api_host ~path ~query () in
  let method_s = Cohttp.Code.string_of_method meth in
  Monitor.try_with (fun () ->
    let headers =
      if authed then Auth.headers cfg ~http_method:method_s ~path
      else Cohttp.Header.of_list [ ("Accept", "application/json") ]
    in
    let headers =
      if Option.is_some body then Cohttp.Header.replace headers "Content-Type" "application/json"
      else headers
    in
    let body =
      Option.map body ~f:(fun j -> Cohttp_async.Body.of_string (Yojson.Safe.to_string j))
    in
    let%bind resp, rb = Cohttp_async.Client.call ~headers ?body meth uri in
    let%map s = Cohttp_async.Body.to_string rb in
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp), s))
  >>| function
  | Ok (code, s) -> parse_response code s
  | Error exn -> Error (`Network (Exn.to_string exn))

let q_opt k f = function Some v -> [ (k, [ f v ]) ] | None -> []

(* Public market data *)
let get_series ~cfg series_ticker = call ~authed:false ~cfg ~meth:`GET ("/series/" ^ series_ticker)
let get_market_json ~cfg ticker = call ~authed:false ~cfg ~meth:`GET ("/markets/" ^ ticker)
let get_event_json ~cfg ?(nested = true) event_ticker =
  call ~authed:false ~cfg ~meth:`GET
    ~query:[ ("with_nested_markets", [ Bool.to_string nested ]) ]
    ("/events/" ^ event_ticker)
let get_orderbook_json ~cfg ?depth ticker =
  call ~authed:false ~cfg ~meth:`GET ~query:(q_opt "depth" Int.to_string depth)
    ("/markets/" ^ ticker ^ "/orderbook")
let exchange_status ~cfg = call ~authed:false ~cfg ~meth:`GET "/exchange/status"

let get_orderbook ~cfg ?depth ticker =
  get_orderbook_json ~cfg ?depth ticker
  >>| Result.bind ~f:(fun j ->
    match Prediction_markets.Orderbook.of_yojson j with
    | Ok ob -> Ok (Book.of_orderbook ob)
    | Error m -> Error (`Json_parse_error m))

(** (fee_type, fee_multiplier) for a series. *)
let series_fee_params ~cfg series_ticker =
  get_series ~cfg series_ticker
  >>| Result.map ~f:(fun j ->
    let open Yojson.Safe.Util in
    let s = member "series" j in
    let ft = match member "fee_type" s with `String s -> Fees.fee_type_of_string s | _ -> Fees.Quadratic in
    let m = match member "fee_multiplier" s with
      | `Int i -> Float.of_int i | `Float f -> f
      | `String x -> Option.value (Float.of_string_opt x) ~default:1.0
      | _ -> 1.0 in
    (ft, m))

(* Authenticated portfolio / account *)
let get_balance ~cfg = call ~cfg ~meth:`GET "/portfolio/balance"
let get_positions ~cfg ?ticker ?(limit = 100) () =
  call ~cfg ~meth:`GET
    ~query:(("limit", [ Int.to_string limit ]) :: q_opt "ticker" Fn.id ticker)
    "/portfolio/positions"
let get_orders ~cfg ?ticker ?status () =
  call ~cfg ~meth:`GET ~query:(q_opt "ticker" Fn.id ticker @ q_opt "status" Fn.id status)
    "/portfolio/orders"
let get_order ~cfg order_id = call ~cfg ~meth:`GET ("/portfolio/orders/" ^ order_id)
let get_fills ~cfg ?ticker ?(limit = 100) () =
  call ~cfg ~meth:`GET
    ~query:(("limit", [ Int.to_string limit ]) :: q_opt "ticker" Fn.id ticker)
    "/portfolio/fills"
let get_account_limits ~cfg = call ~cfg ~meth:`GET "/account/limits"

(* V2 order entry *)
let create_order ~cfg (r : Order.request) =
  match Order.validate r with
  | Error e -> return (Error (`Http (0, Error.to_string_hum e)))
  | Ok () ->
    call ~cfg ~meth:`POST ~body:(Order.to_yojson r) "/portfolio/events/orders"
    >>| Result.bind ~f:(fun j ->
      Order.response_of_yojson j
      |> Result.map_error ~f:(fun e -> `Json_parse_error (Error.to_string_hum e)))

(** Cancel. [market_ticker] lets the exchange auto-route (recommended). *)
let cancel_order ~cfg ?market_ticker order_id =
  call ~cfg ~meth:`DELETE ~query:(q_opt "market_ticker" Fn.id market_ticker)
    ("/portfolio/events/orders/" ^ order_id)

let amend_order ~cfg ~order_id ~ticker ~side ~price ~count =
  call ~cfg ~meth:`POST
    ~body:(`Assoc [ ("ticker", `String ticker)
                  ; ("side", `String (Order.book_side_to_string side))
                  ; ("price", `String (Fp.price_to_string price))
                  ; ("count", `String (Fp.count_to_string count)) ])
    ("/portfolio/events/orders/" ^ order_id ^ "/amend")

let decrease_order ~cfg ~order_id ?market_ticker (by : [ `Reduce_by of float | `Reduce_to of float ]) =
  let k, v = match by with `Reduce_by c -> ("reduce_by", c) | `Reduce_to c -> ("reduce_to", c) in
  call ~cfg ~meth:`POST
    ~body:(`Assoc ((k, `String (Fp.count_to_string v))
                   :: (match market_ticker with Some t -> [ ("market_ticker", `String t) ] | None -> [])))
    ("/portfolio/events/orders/" ^ order_id ^ "/decrease")

(* ------------------------------------------------------------------------- *)
(* Ticker / URL resolution                                                    *)
(* ------------------------------------------------------------------------- *)

(** Accept a market ticker, event ticker, or a kalshi.com URL and return the
    candidate ticker (upper-cased last path segment). *)
let ticker_of_input s =
  let s = String.strip s in
  let s =
    if String.is_substring s ~substring:"kalshi.com/" then
      let path = Uri.path (Uri.of_string s) in
      String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty) |> List.last
      |> Option.value ~default:s
    else s
  in
  String.uppercase s

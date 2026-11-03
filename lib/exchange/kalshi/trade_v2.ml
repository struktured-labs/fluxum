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
  (** Strictly parse a decimal string into an integer scaled by 10^decimals.
      Accepts [-]?digits[.digits] (at least one digit overall, surrounding
      whitespace ignored). Digits beyond [decimals] are accepted only when they
      are all zero, so nothing is silently truncated. Anything else (empty,
      "-", trailing garbage, exponents, overflow) is [None].
      "0.5600" ~decimals:4 -> Some 5600; "0.5000garbage" -> None. *)
  let parse_scaled ~decimals s =
    let s = String.strip s in
    let neg, s =
      match String.chop_prefix s ~prefix:"-" with Some r -> (true, r) | None -> (false, s)
    in
    let ip, fp = match String.lsplit2 s ~on:'.' with Some (a, b) -> (a, b) | None -> (s, "") in
    let all_digits x = String.for_all x ~f:Char.is_digit in
    if String.is_empty ip && String.is_empty fp then None
    else if not (all_digits ip && all_digits fp) then None
    else if String.length ip > 15 then None (* keeps the scaled value far from overflow *)
    else
      let fp_kept, fp_extra =
        if String.length fp > decimals then (String.prefix fp decimals, String.drop_prefix fp decimals)
        else (fp ^ String.make (decimals - String.length fp) '0', "")
      in
      if not (String.for_all fp_extra ~f:(Char.equal '0')) then None
      else
        let ip = if String.is_empty ip then "0" else ip in
        match Int.of_string_opt ip, (if decimals = 0 then Some 0 else Int.of_string_opt fp_kept) with
        | Some i, Some f ->
          (* checked: i * 10^d + f must not exceed max_int (63-bit ints) *)
          let m = Int.pow 10 decimals in
          if i > (Int.max_value - f) / m then None
          else
            let v = (i * m) + f in
            Some (if neg then -v else v)
        | _ -> None

  (** Checked float -> integer scaled by 10^decimals (round to nearest).
      Rejects NaN, infinities and magnitudes that can't be represented. *)
  let scale_checked ~what ~decimals (x : float) =
    if not (Float.is_finite x) then Or_error.errorf "%s is not a finite number" what
    else
      let v = x *. (10. ** Float.of_int decimals) in
      if Float.(abs v > 1e15) then Or_error.errorf "%s %g is out of range" what x
      else Ok (Float.iround_nearest_exn v)

  (** A YES price in dollars as centicents, required to be strictly inside
      (0, 1) {i after} rounding to 4 dp (0.00001 -> 0 and 0.99999 -> 10000
      are rejected). *)
  let price_key_checked (p : float) =
    let open Or_error.Let_syntax in
    let%bind k = scale_checked ~what:"price" ~decimals:4 p in
    if k > 0 && k < 10_000 then Ok k
    else Or_error.errorf "price %g rounds to %d centicents, outside (0, 1)" p k

  (** A contract count in hundredths. Must be > 0 after rounding to 2 dp
      (>= 0 when [allow_zero]). *)
  let count_hundredths_checked ?(allow_zero = false) (c : float) =
    let open Or_error.Let_syntax in
    let%bind h = scale_checked ~what:"count" ~decimals:2 c in
    if h > 0 || (allow_zero && h = 0) then Ok h
    else Or_error.errorf "count %g rounds to %d hundredths; must be %s 0" c h
           (if allow_zero then ">=" else ">")

  let price_to_string_scaled p = sprintf "%d.%04d" (p / 10_000) (p mod 10_000)

  (** Dollar price as a 4-dp wire string, e.g. 0.56 -> "0.5600". *)
  let price_to_string (p : float) =
    price_to_string_scaled (Float.iround_nearest_exn (p *. 10_000.))

  (** Contract count as a 2-dp wire string, e.g. 10 -> "10.00". *)
  let count_to_string_scaled h = sprintf "%d.%02d" (h / 100) (h mod 100)

  let count_to_string (c : float) = count_to_string_scaled (Float.iround_nearest_exn (c *. 100.))

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

  (** Taker rate for a series' fee_type. [None] for [Flat] (per-contract
      amounts published only in Kalshi's "Specific Trading Fees Table", not
      in the API) and for fee types this client does not know. *)
  let taker_rate_of = function
    | Quadratic | Quadratic_with_maker_fees | Quadratic_with_combo_maker_fees -> Some taker_rate
    | Flat | Unknown _ -> None

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

  (** Strict decode of a REST [/markets/{t}/orderbook] response. Every level
      must be [["0.dddd", "n.nn"]] with 0 < price < 1 and size > 0 (fixed-point
      strings); an absent/null side is empty. Anything else is an error rather
      than a silently partial book. *)
  let of_json (j : Yojson.Safe.t) : t Or_error.t =
    let open Or_error.Let_syntax in
    let level = function
      | `List [ `String p; `String q ] ->
        (match Fp.parse_scaled ~decimals:4 p, Fp.parse_scaled ~decimals:2 q with
         | Some pk, Some qh when pk > 0 && pk < 10_000 && qh > 0 ->
           Ok { price = Float.of_int pk /. 10_000.; size = Float.of_int qh /. 100. }
         | _ -> Or_error.errorf "bad orderbook level [%S, %S]" p q)
      | l -> Or_error.errorf "bad orderbook level %s" (Yojson.Safe.to_string l)
    in
    let side ob k =
      match List.Assoc.find ob ~equal:String.equal k with
      | None | Some `Null -> Ok []
      | Some (`List ls) ->
        let%bind ls = List.map ls ~f:level |> Or_error.all in
        let ls = List.sort ls ~compare:(fun a b -> Float.compare b.price a.price) in
        (match List.find_consecutive_duplicate ls ~equal:(fun a b -> Float.equal a.price b.price) with
         | Some (a, _) -> Or_error.errorf "orderbook %s has duplicate price level %.4f" k a.price
         | None -> Ok ls)
      | Some _ -> Or_error.errorf "orderbook %s is not a list" k
    in
    match j with
    | `Assoc a ->
      (match List.Assoc.find a ~equal:String.equal "orderbook_fp" with
       | Some (`Assoc ob) ->
         let%bind yes_bids = side ob "yes_dollars" in
         let%map no_bids = side ob "no_dollars" in
         { yes_bids; no_bids }
       | _ -> Or_error.error_string "orderbook response lacks an orderbook_fp object")
    | _ -> Or_error.error_string "orderbook response is not a JSON object"

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
  let buy ?limit ?(multiplier = 1.0) ?(fee_type = Fees.Quadratic) ?(rounding = Fees.Cent) ~book ~outcome ~count () =
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
    (* [fee] is NaN when the series' fee_type has no known formula (Flat /
       Unknown): callers must treat the cost as unknown, not fee-free. *)
    let fee =
      match Fees.taker_rate_of fee_type with
      | None -> Float.nan
      | Some rate ->
        Fees.round_up ~rounding
          (List.sum (module Float) levels ~f:(fun l ->
             Fees.raw ~rate ~multiplier ~count:l.size ~price:l.price))
    in
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
    (* Validate the values that will actually go on the wire (rounded to
       4 dp price / 2 dp count), not just the raw floats. *)
    let%bind (_ : int) = Fp.price_key_checked r.price in
    let%bind (_ : int) = Fp.count_hundredths_checked r.count in
    let%bind () =
      if r.reduce_only && not (equal_tif r.time_in_force Ioc)
      then Or_error.error_string "reduce_only requires immediate_or_cancel" else Ok ()
    in
    let%bind () =
      if r.post_only && not (equal_tif r.time_in_force Gtc)
      then Or_error.error_string "post_only requires good_till_canceled (not IOC/FOK)" else Ok ()
    in
    let%bind () =
      if String.is_empty (String.strip r.ticker) then Or_error.error_string "empty ticker" else Ok ()
    in
    match r.expiration_time with
    | None -> Ok ()
    | Some _ when not (equal_tif r.time_in_force Gtc) ->
      Or_error.error_string "expiration_time requires good_till_canceled"
    | Some t when t <= 0 -> Or_error.errorf "expiration_time %d must be a positive unix time" t
    | Some _ -> Ok ()

  (** Fill in a fresh UUID [client_order_id] if none is set, so the order can
      be looked up after an ambiguous outcome (e.g. a timeout). *)
  let ensure_client_order_id r =
    match r.client_order_id with
    | Some id when not (String.is_empty (String.strip id)) -> r
    | _ -> { r with client_order_id = Some (Auth.uuid_v4 ()) }

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
    match j with
    | `Assoc _ ->
      (match member "order_id" j with
       | `String order_id when not (String.is_empty order_id) ->
      let str k = match member k j with `String s -> Some s | _ -> None in
      Ok { order_id; client_order_id = str "client_order_id"
         ; fill_count = str "fill_count"; remaining_count = str "remaining_count"
         ; ts_ms = (match member "ts_ms" j with `Int i -> Some i | _ -> None) }
       | _ -> Or_error.errorf "unexpected order response: %s" (Yojson.Safe.to_string j))
    | _ -> Or_error.errorf "order response is not a JSON object: %s" (Yojson.Safe.to_string j)
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
  | `Timeout of string
    (** no response within the deadline. The request may or may not have
        been applied by the exchange; it is never retried automatically. *)
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

(** The raw HTTP exchange (status code, body). Replaceable for tests. *)
let http_impl
  : (headers:Cohttp.Header.t -> ?body:Cohttp_async.Body.t -> Cohttp.Code.meth -> Uri.t
     -> (int * string) Deferred.t) ref =
  ref (fun ~headers ?body meth uri ->
    let%bind resp, rb = Cohttp_async.Client.call ~headers ?body meth uri in
    let%map s = Cohttp_async.Body.to_string rb in
    (Cohttp.Code.code_of_status (Cohttp.Response.status resp), s))

(** Default per-request deadline (seconds). *)
let default_timeout = 10.

(** One HTTP request with a deadline of [timeout] seconds. On expiry returns
    [`Timeout]; the abandoned request may still reach the exchange. *)
let once ?(authed = true) ?(query = []) ?body ?(timeout = default_timeout) ~(cfg : (module Cfg.S)) ~meth path
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
    Clock.with_timeout (Time_float.Span.of_sec timeout) (!http_impl ~headers ?body meth uri))
  >>| function
  | Ok (`Result (code, s)) -> parse_response code s
  | Ok `Timeout -> Error (`Timeout (sprintf "%s %s: no response within %gs" method_s path timeout))
  | Error exn -> Error (`Network (Exn.to_string exn))

(** Hook for observing retries (tests / logging). *)
let on_retry : (attempt:int -> delay:float -> path:string -> unit) ref =
  ref (fun ~attempt:_ ~delay:_ ~path:_ -> ())

(** One logical request: client-side token-bucket throttling (per
    [Rate_limit.default] tier, override with [?limiter]) then up to
    [max_retries] retries on 429 with exponential backoff + full jitter.
    Only 429 (which the exchange rejected without applying) is retried: a
    [`Timeout] or [`Network] error is returned as is, since the request may
    have been applied (never blindly re-send a POST). Signs the path only
    (never the query string). *)
let call ?authed ?(query = []) ?body ?limiter ?(max_retries = 5) ?timeout ~(cfg : (module Cfg.S)) ~meth path
  : (Yojson.Safe.t, error) Result.t Deferred.t =
  let limiter = match limiter with Some l -> l | None -> Lazy.force Rate_limit.default in
  let kind, cost = Rate_limit.classify ~meth ~path in
  Rate_limit.with_retry ~max_retries
    ~on_retry:(fun ~attempt ~delay -> !on_retry ~attempt ~delay ~path)
    ~is_rate_limited:(function Error `Rate_limited -> true | _ -> false)
    (fun () ->
       let%bind () = Rate_limit.Limiter.acquire limiter kind ~cost in
       once ?authed ~query ?body ?timeout ~cfg ~meth path)

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
    Book.of_json j |> Result.map_error ~f:(fun e -> `Json_parse_error (Error.to_string_hum e)))

(** Strictly decode (fee_type, fee_multiplier) from a [/series/{t}] response.
    The response and its [series] must be objects. An absent/null fee_type
    defaults to Quadratic and an absent/null multiplier to 1.0; a present but
    wrongly typed, unparseable, non-finite or negative value is an error. *)
let series_fee_params_of_json (j : Yojson.Safe.t) =
  let field a k = List.Assoc.find a ~equal:String.equal k in
  match j with
  | `Assoc a ->
    (match field a "series" with
     | Some (`Assoc s) ->
       let ft =
         match field s "fee_type" with
         | None | Some `Null -> Ok Fees.Quadratic
         | Some (`String x) -> Ok (Fees.fee_type_of_string x)
         | Some v -> Or_error.errorf "series fee_type is not a string: %s" (Yojson.Safe.to_string v)
       in
       let m =
         let chk f = if Float.is_finite f && Float.(f >= 0.) then Ok f
           else Or_error.errorf "series fee_multiplier %g out of range" f in
         match field s "fee_multiplier" with
         | None | Some `Null -> Ok 1.0
         | Some (`Int i) -> chk (Float.of_int i)
         | Some (`Float f) -> chk f
         | Some (`String x) ->
           (match Float.of_string_opt (String.strip x) with
            | Some f -> chk f
            | None -> Or_error.errorf "series fee_multiplier %S is not a number" x)
         | Some v -> Or_error.errorf "series fee_multiplier has bad type: %s" (Yojson.Safe.to_string v)
       in
       Or_error.both ft m
     | _ -> Or_error.error_string "series response lacks a series object")
  | _ -> Or_error.error_string "series response is not a JSON object"

(** (fee_type, fee_multiplier) for a series. *)
let series_fee_params ~cfg series_ticker =
  get_series ~cfg series_ticker
  >>| Result.bind ~f:(fun j ->
    series_fee_params_of_json j
    |> Result.map_error ~f:(fun e -> `Json_parse_error (Error.to_string_hum e)))

(* Authenticated portfolio / account *)
(** Balance; [exchange_index] restricts to one shard. Response includes
    [balance_breakdown : {exchange_index; balance}] per shard. *)
let get_balance ?exchange_index ~cfg () =
  call ~cfg ~meth:`GET ~query:(q_opt "exchange_index" Int.to_string exchange_index)
    "/portfolio/balance"

(** Per-shard balances in {b centicents} (1e-4 dollars) from a balance
    response's [balance_breakdown], sorted by index. The live API sends each
    entry's [balance] as a fixed-point dollar string (e.g. ["989.7662"]); a
    JSON integer is taken as cents (x100). Strict: the response must be an
    object; an absent/null breakdown is empty; every entry must be an object
    with a unique non-negative integer [exchange_index] and a balance that
    parses exactly (checked, no floats). Malformed entries are an error
    rather than being dropped. *)
let balance_breakdown (j : Yojson.Safe.t) : (int * int) list Or_error.t =
  let open Or_error.Let_syntax in
  let field a k = List.Assoc.find a ~equal:String.equal k in
  let int_of what = function
    | Some (`Int i) -> Ok i
    | Some (`String s) ->
      let s' = String.strip s in
      let digits = match String.chop_prefix s' ~prefix:"-" with Some r -> r | None -> s' in
      if (not (String.is_empty digits)) && String.for_all digits ~f:Char.is_digit
      then (match Int.of_string_opt s' with Some i -> Ok i | None -> Or_error.errorf "%s %S out of range" what s)
      else Or_error.errorf "%s %S is not an integer" what s
    | Some v -> Or_error.errorf "%s has bad value %s" what (Yojson.Safe.to_string v)
    | None -> Or_error.errorf "%s missing" what
  in
  match j with
  | `Assoc a ->
    (match field a "balance_breakdown" with
     | None | Some `Null -> Ok []
     | Some (`List l) ->
       let%bind entries =
         List.map l ~f:(function
           | `Assoc e ->
             let%bind i = int_of "exchange_index" (field e "exchange_index") in
             let%bind b =
               match field e "balance" with
               | Some (`String s) ->
                 (match Fp.parse_scaled ~decimals:4 s with
                  | Some cc -> Ok cc
                  | None -> Or_error.errorf "balance %S is not a fixed-point dollar amount" s)
               | Some (`Int c) when abs c <= Int.max_value / 100 -> Ok (c * 100)
               | v -> int_of "balance" v |> Or_error.bind ~f:(fun _ -> Or_error.error_string "balance out of range")
             in
             if i < 0 then Or_error.errorf "negative exchange_index %d" i else Ok (i, b)
           | v -> Or_error.errorf "bad balance_breakdown entry %s" (Yojson.Safe.to_string v))
         |> Or_error.all
       in
       let sorted = List.sort entries ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
       (match List.find_consecutive_duplicate sorted ~equal:(fun (a, _) (b, _) -> a = b) with
        | Some ((i, _), _) -> Or_error.errorf "duplicate exchange_index %d in balance_breakdown" i
        | None -> Ok sorted)
     | Some _ -> Or_error.error_string "balance_breakdown is not a list")
  | _ -> Or_error.error_string "balance response is not a JSON object"

(* ---- cursor pagination ---- *)

(** Cursor from a page; [None] when empty/absent (= last page). *)
let page_cursor (j : Yojson.Safe.t) =
  match Yojson.Safe.Util.member "cursor" j with
  | `String "" | `Null -> None
  | `String c -> Some c
  | _ -> None

(** Strictly decode one page: must be an object whose [key] is a list (an
    explicit null counts as empty) and whose cursor, if present, is a string
    or null. Returns (items, next_cursor). *)
let page_of_json ~key (j : Yojson.Safe.t) =
  match j with
  | `Assoc a ->
    let items =
      match List.Assoc.find a ~equal:String.equal key with
      | Some (`List l) -> Ok l
      | Some `Null -> Ok []
      | None -> Or_error.errorf "page has no %S field" key
      | Some _ -> Or_error.errorf "page field %S is not a list" key
    in
    let cursor =
      match List.Assoc.find a ~equal:String.equal "cursor" with
      | None | Some `Null | Some (`String "") -> Ok None
      | Some (`String c) -> Ok (Some c)
      | Some _ -> Or_error.error_string "page cursor is not a string"
    in
    Or_error.both items cursor
  | _ -> Or_error.error_string "page is not a JSON object"

(** Fetch pages via [fetch ?cursor] until the cursor is empty or [max_pages]
    reached, concatenating the list field [key]. Returns (items, next_cursor). *)
let paginate ?(max_pages = 50) ~key
      (fetch : ?cursor:string -> unit -> (Yojson.Safe.t, error) Result.t Deferred.t) =
  let rec go acc cursor n =
    match%bind fetch ?cursor () with
    | Error e -> return (Error e)
    | Ok j ->
      (match page_of_json ~key j with
       | Error e -> return (Error (`Json_parse_error (Error.to_string_hum e)))
       | Ok (items, cursor) ->
         let acc = List.rev_append items acc in
         (match cursor with
          | Some c when n + 1 < max_pages -> go acc (Some c) (n + 1)
          | next -> return (Ok (List.rev acc, next))))
  in
  go [] None 0

(** A scheduled per-event fee change (GET /events/fee_changes). A [None]
    override means "use the series value". *)
type fee_change =
  { fc_event_ticker : string
  ; fee_type_override : Fees.fee_type option
  ; fee_multiplier_override : float option
  ; scheduled_ts : Time_float.t
  }

let fee_change_of_json (j : Yojson.Safe.t) : fee_change Or_error.t =
  let open Or_error.Let_syntax in
  match j with
  | `Assoc a ->
    let field k = List.Assoc.find a ~equal:String.equal k in
    let%bind fc_event_ticker =
      match field "event_ticker" with
      | Some (`String s) -> Ok s
      | _ -> Or_error.error_string "fee change lacks event_ticker"
    in
    let%bind fee_type_override =
      match field "fee_type_override" with
      | None | Some `Null -> Ok None
      | Some (`String s) -> Ok (Some (Fees.fee_type_of_string s))
      | Some v -> Or_error.errorf "bad fee_type_override %s" (Yojson.Safe.to_string v)
    in
    let%bind fee_multiplier_override =
      let chk f = if Float.is_finite f && Float.(f >= 0.) then Ok (Some f)
        else Or_error.errorf "fee_multiplier_override %g out of range" f in
      match field "fee_multiplier_override" with
      | None | Some `Null -> Ok None
      | Some (`Int i) -> chk (Float.of_int i)
      | Some (`Float f) -> chk f
      | Some (`String x) ->
        (match Float.of_string_opt (String.strip x) with
         | Some f -> chk f
         | None -> Or_error.errorf "fee_multiplier_override %S is not a number" x)
      | Some v -> Or_error.errorf "bad fee_multiplier_override %s" (Yojson.Safe.to_string v)
    in
    let%map scheduled_ts =
      match field "scheduled_ts" with
      | Some (`String s) ->
        Or_error.try_with (fun () -> Time_float.of_string_with_utc_offset s)
        |> Or_error.tag ~tag:"scheduled_ts"
      | Some (`Int i) -> Ok (Time_float.of_span_since_epoch (Time_float.Span.of_int_sec i))
      | _ -> Or_error.error_string "fee change lacks scheduled_ts"
    in
    { fc_event_ticker; fee_type_override; fee_multiplier_override; scheduled_ts }
  | _ -> Or_error.error_string "fee change is not an object"

(** Fee params in force at [now]: the most recent change scheduled at or
    before [now] for [event_ticker] (if any) overrides the series values
    field by field; a null override falls back to the series value. *)
let effective_fee_params ~series:(series_ft, series_mult) ~event_ticker ~(changes : fee_change list) ~now =
  let applicable =
    List.filter changes ~f:(fun c ->
      String.equal c.fc_event_ticker event_ticker && Time_float.( <= ) c.scheduled_ts now)
  in
  match List.max_elt applicable ~compare:(fun a b -> Time_float.compare a.scheduled_ts b.scheduled_ts) with
  | None -> (series_ft, series_mult)
  | Some c ->
    ( Option.value c.fee_type_override ~default:series_ft
    , Option.value c.fee_multiplier_override ~default:series_mult )

let get_event_fee_changes ~cfg event_ticker =
  paginate ~max_pages:10 ~key:"event_fee_changes" (fun ?cursor () ->
    call ~authed:false ~cfg ~meth:`GET
      ~query:(("event_ticker", [ event_ticker ]) :: q_opt "cursor" Fn.id cursor)
      "/events/fee_changes")
  >>| Result.bind ~f:(fun (l, _) ->
    List.map l ~f:fee_change_of_json |> Or_error.all
    |> Result.map_error ~f:(fun e -> `Json_parse_error (Error.to_string_hum e)))

(** (fee_type, fee_multiplier) in force now for a market of [event_ticker]
    in [series_ticker]: series values plus any per-event override. *)
let event_fee_params ?(now = Time_float.now ()) ~cfg ~series_ticker event_ticker =
  match%bind series_fee_params ~cfg series_ticker with
  | Error e -> return (Error e)
  | Ok series ->
    get_event_fee_changes ~cfg event_ticker
    >>| Result.map ~f:(fun changes -> effective_fee_params ~series ~event_ticker ~changes ~now)

let get_positions ~cfg ?ticker ?event_ticker ?count_filter ?settlement_status ?exchange_index
      ?cursor ?(limit = 100) () =
  call ~cfg ~meth:`GET
    ~query:(("limit", [ Int.to_string limit ]) :: q_opt "ticker" Fn.id ticker
            @ q_opt "event_ticker" Fn.id event_ticker @ q_opt "count_filter" Fn.id count_filter
            @ q_opt "settlement_status" Fn.id settlement_status
            @ q_opt "exchange_index" Int.to_string exchange_index @ q_opt "cursor" Fn.id cursor)
    "/portfolio/positions"

let get_fills ~cfg ?ticker ?order_id ?min_ts ?max_ts ?exchange_index ?cursor ?(limit = 100) () =
  call ~cfg ~meth:`GET
    ~query:(("limit", [ Int.to_string limit ]) :: q_opt "ticker" Fn.id ticker
            @ q_opt "order_id" Fn.id order_id @ q_opt "min_ts" Int.to_string min_ts
            @ q_opt "max_ts" Int.to_string max_ts
            @ q_opt "exchange_index" Int.to_string exchange_index @ q_opt "cursor" Fn.id cursor)
    "/portfolio/fills"

let all_fills ?max_pages ~cfg ?ticker ?order_id ?min_ts ?max_ts ?exchange_index ?limit () =
  paginate ?max_pages ~key:"fills" (fun ?cursor () ->
    get_fills ~cfg ?ticker ?order_id ?min_ts ?max_ts ?exchange_index ?cursor ?limit ())

let all_market_positions ?max_pages ~cfg ?ticker ?event_ticker ?count_filter ?settlement_status
      ?exchange_index ?limit () =
  paginate ?max_pages ~key:"market_positions" (fun ?cursor () ->
    get_positions ~cfg ?ticker ?event_ticker ?count_filter ?settlement_status ?exchange_index
      ?cursor ?limit ())

(* ---- shard transfers (POST /portfolio/intra_exchange_instance_transfer) ---- *)

module Transfer = struct
  (** Dollars -> centicents (1/10000 dollar), as the API's [amount]. *)
  let centicents_of_dollars d = Float.iround_nearest_exn (d *. 10_000.)

  (** Checked: finite, representable, and > 0 after rounding to centicents. *)
  let centicents_checked d =
    let open Or_error.Let_syntax in
    let%bind k = Fp.scale_checked ~what:"amount" ~decimals:4 d in
    if k > 0 then Ok k else Or_error.errorf "amount %g rounds to %d centicents; must be > 0" d k

  let body ~amount_dollars ~source_shard ~destination_shard ?client_transfer_id () =
    match centicents_checked amount_dollars with
    | Error e -> Error e
    | Ok amount ->
    if source_shard < 0 || source_shard > 100 || destination_shard < 0 || destination_shard > 100
    then Or_error.error_string "shard index must be 0..100"
    else if source_shard = destination_shard then Or_error.error_string "source = destination shard"
    else
      Ok (`Assoc ([ ("source", `String "event_contract")
                  ; ("destination", `String "event_contract")
                  ; ("amount", `Int amount)
                  ; ("source_exchange_shard", `Int source_shard)
                  ; ("destination_exchange_shard", `Int destination_shard) ]
                  @ (match client_transfer_id with
                     | Some id -> [ ("client_transfer_id", `String id) ] | None -> [])))
end

(** Move funds between exchange shards of the event-contract instance.
    Processed asynchronously; returns [{transfer_id}]. *)
let transfer_between_shards ~cfg ~amount_dollars ~source_shard ~destination_shard
      ?client_transfer_id () =
  match Transfer.body ~amount_dollars ~source_shard ~destination_shard ?client_transfer_id () with
  | Error e -> return (Error (`Http (0, Error.to_string_hum e)))
  | Ok body -> call ~cfg ~meth:`POST ~body "/portfolio/intra_exchange_instance_transfer"

let get_shard_transfers ~cfg ?cursor ?(limit = 50) () =
  call ~cfg ~meth:`GET ~query:(("limit", [ Int.to_string limit ]) :: q_opt "cursor" Fn.id cursor)
    "/portfolio/intra_exchange_instance_transfers"

let get_shard_transfer ~cfg transfer_id =
  call ~cfg ~meth:`GET ("/portfolio/intra_exchange_instance_transfers/" ^ transfer_id)

let get_orders ~cfg ?ticker ?status ?min_ts ?cursor ?limit () =
  call ~cfg ~meth:`GET
    ~query:(q_opt "ticker" Fn.id ticker @ q_opt "status" Fn.id status
            @ q_opt "min_ts" Int.to_string min_ts @ q_opt "cursor" Fn.id cursor
            @ q_opt "limit" Int.to_string limit)
    "/portfolio/orders"
let get_order ~cfg order_id = call ~cfg ~meth:`GET ("/portfolio/orders/" ^ order_id)
let get_account_limits ~cfg = call ~cfg ~meth:`GET "/account/limits"

(** The order (JSON) whose [client_order_id] is [id], if any, among the
    orders on [ticker] (all statuses) created at or after [min_ts]. GET
    /portfolio/orders has no client_order_id filter, so this pages through
    and filters client-side. Use after a [`Timeout] on [create_order]. *)
let find_order_by_client_order_id ?(max_pages = 10) ?fetch ~cfg ~ticker ?min_ts client_order_id =
  let fetch = match fetch with Some f -> f | None -> (fun ?cursor () -> get_orders ~cfg ~ticker ?min_ts ?cursor ~limit:200 ()) in
  paginate ~max_pages ~key:"orders" (fun ?cursor () -> fetch ?cursor ())
  >>| Result.map ~f:(fun (orders, _) ->
    List.find orders ~f:(fun o ->
      match Yojson.Safe.Util.member "client_order_id" o with
      | `String c -> String.equal c client_order_id
      | _ -> false))

(* V2 order entry *)

(** Place an order. A missing [client_order_id] is filled with a fresh UUID
    (use [Order.ensure_client_order_id] first to know it). A [`Timeout] is
    ambiguous and never retried: look the order up with
    [find_order_by_client_order_id] before deciding to re-send. *)
let create_order ?timeout ~cfg (r : Order.request) =
  let r = Order.ensure_client_order_id r in
  match Order.validate r with
  | Error e -> return (Error (`Http (0, Error.to_string_hum e)))
  | Ok () ->
    call ?timeout ~cfg ~meth:`POST ~body:(Order.to_yojson r) "/portfolio/events/orders"
    >>| Result.bind ~f:(fun j ->
      Order.response_of_yojson j
      |> Result.map_error ~f:(fun e -> `Json_parse_error (Error.to_string_hum e)))

(** Cancel. [market_ticker] lets the exchange auto-route (recommended). *)
let cancel_order ~cfg ?market_ticker order_id =
  call ~cfg ~meth:`DELETE ~query:(q_opt "market_ticker" Fn.id market_ticker)
    ("/portfolio/events/orders/" ^ order_id)

module Amend = struct
  let body ~ticker ~side ~price ~count =
    let open Or_error.Let_syntax in
    let%bind p = Fp.price_key_checked price in
    let%bind c = Fp.count_hundredths_checked count in
    Ok (`Assoc [ ("ticker", `String ticker)
               ; ("side", `String (Order.book_side_to_string side))
               ; ("price", `String (Fp.price_to_string_scaled p))
               ; ("count", `String (Fp.count_to_string_scaled c)) ])

  (** [Reduce_by] must be > 0; [Reduce_to] may be 0 (= cancel the remainder). *)
  let decrease_body ?market_ticker (by : [ `Reduce_by of float | `Reduce_to of float ]) =
    let open Or_error.Let_syntax in
    let k, v, allow_zero =
      match by with `Reduce_by c -> ("reduce_by", c, false) | `Reduce_to c -> ("reduce_to", c, true)
    in
    let%bind h = Fp.count_hundredths_checked ~allow_zero v in
    Ok (`Assoc ((k, `String (Fp.count_to_string_scaled h))
                :: (match market_ticker with Some t -> [ ("market_ticker", `String t) ] | None -> [])))
end

let amend_order ~cfg ~order_id ~ticker ~side ~price ~count =
  match Amend.body ~ticker ~side ~price ~count with
  | Error e -> return (Error (`Http (0, Error.to_string_hum e)))
  | Ok body -> call ~cfg ~meth:`POST ~body ("/portfolio/events/orders/" ^ order_id ^ "/amend")

let decrease_order ~cfg ~order_id ?market_ticker by =
  match Amend.decrease_body ?market_ticker by with
  | Error e -> return (Error (`Http (0, Error.to_string_hum e)))
  | Ok body -> call ~cfg ~meth:`POST ~body ("/portfolio/events/orders/" ^ order_id ^ "/decrease")

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

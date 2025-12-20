open Core
open Async

(** Kraken balance types *)
module Balance = struct
  type t = string * float [@@deriving sexp]
end

(** Kraken order types *)
module Order_descr = struct
  type t =
    { pair : string
    ; type_ : string [@key "type"]
    ; ordertype : string
    ; price : string
    ; price2 : string
    ; leverage : string
    ; order : string
    ; close : string
    }
  [@@deriving sexp]
end

module Order = struct
  type t =
    { refid : string
    ; userref : int option
    ; status : string
    ; reason : string option
    ; opentm : float
    ; closetm : float option
    ; starttm : float
    ; expiretm : float
    ; descr : Order_descr.t
    ; vol : float
    ; vol_exec : float
    ; cost : float
    ; fee : float
    ; price : float
    ; stopprice : float
    ; limitprice : float
    ; misc : string
    ; oflags : string
    }
  [@@deriving sexp]
end

(** Make an authenticated Kraken API call using pure OCaml signature generation *)
let api_call ~api_key ~api_secret ~endpoint ~data () =
  (* Generate nonce from current Unix timestamp in milliseconds *)
  let nonce = Int.to_string (Int.of_float (Unix.gettimeofday () *. 1000.0)) in
  
  (* Build post data *)
  let post_pairs = ("nonce", nonce) :: data in
  let post_data = String.concat ~sep:"&" (List.map post_pairs ~f:(fun (k, v) -> 
    sprintf "%s=%s" k v))
  in
  
  (* API path for signing *)
  let api_path = sprintf "/0/private/%s" endpoint in
  
  (* Generate Kraken signature using pure OCaml *)
  let signature = Signature.kraken_signature ~api_secret_b64:api_secret ~api_path ~nonce ~post_data in
  
  (* Make HTTP request via curl *)
  In_thread.run (fun () ->
    let tmp_data = Filename_unix.temp_file "kraken" ".data" in
    Out_channel.write_all tmp_data ~data:post_data;
    
    let response =
      try
        let curl_cmd = sprintf {|curl -s -X POST \
          -H "API-Key: %s" \
          -H "API-Sign: %s" \
          -d @%s \
          https://api.kraken.com%s|} api_key signature tmp_data api_path in
        let process = Core_unix.open_process_in curl_cmd in
        let output = In_channel.input_all process in
        let _ = Core_unix.close_process_in process in
        output
      with e ->
        sprintf "{\"error\": [\"%s\"]}" (Exn.to_string e)
    in
    (try Sys_unix.remove tmp_data with _ -> ());
    response
  ) >>= fun response_str ->
  try
    let json = Yojson.Safe.from_string response_str in
    Deferred.return (`Ok json)
  with e ->
    Deferred.return (`Error (sprintf "JSON parse error: %s" (Exn.to_string e)))

(** Get account balances *)
let balances (module Cfg : Cfg.S) () =
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"Balance" ~data:[] ()

(** Get open orders
    Returns: JSON with open orders keyed by order ID
*)
let open_orders ?(trades=false) (module Cfg : Cfg.S) () =
  let data = [("trades", if trades then "true" else "false")] in
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"OpenOrders" ~data ()

(** Add a new order
    Required: pair, type, ordertype
    Optional: price, price2, volume, leverage, oflags, starttm, expiretm, closetm, etc.
*)
let add_order 
    ?(price="")
    ?(price2="")
    ?(leverage="")
    ?(oflags="")
    ?(starttm="0")
    ?(expiretm="0")
    ?(timeinforce="")
    (module Cfg : Cfg.S)
    ~pair
    ~type_
    ~ordertype
    ~volume
    () =
  let data = [
    ("pair", pair);
    ("type", type_);
    ("ordertype", ordertype);
    ("volume", Float.to_string volume);
  ] in
  let data = if String.is_empty price then data else ("price", price) :: data in
  let data = if String.is_empty price2 then data else ("price2", price2) :: data in
  let data = if String.is_empty leverage then data else ("leverage", leverage) :: data in
  let data = if String.is_empty oflags then data else ("oflags", oflags) :: data in
  let data = if String.equal starttm "0" then data else ("starttm", starttm) :: data in
  let data = if String.equal expiretm "0" then data else ("expiretm", expiretm) :: data in
  let data = if String.is_empty timeinforce then data else ("timeinforce", timeinforce) :: data in
  
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"AddOrder" ~data ()

(** Cancel an open order by ID *)
let cancel_order (module Cfg : Cfg.S) ~txid () =
  let data = [("txid", txid)] in
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"CancelOrder" ~data ()

(** Cancel multiple orders by ID *)
let cancel_orders (module Cfg : Cfg.S) ~txids () =
  let data = [("txid", String.concat ~sep:"," txids)] in
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"CancelOrder" ~data ()

(** Query orders info by ID *)
let query_orders ?(trades=false) (module Cfg : Cfg.S) ~txids () =
  let data = [
    ("txid", String.concat ~sep:"," txids);
    ("trades", if trades then "true" else "false");
  ] in
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"QueryOrders" ~data ()

(** Get closed orders *)
let closed_orders ?(trades=false) ?(userref="") (module Cfg : Cfg.S) () =
  let data = [("trades", if trades then "true" else "false")] in
  let data = if String.is_empty userref then data else ("userref", userref) :: data in
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"ClosedOrders" ~data ()

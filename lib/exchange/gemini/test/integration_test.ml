(** Gemini Integration Tests - Public REST APIs (no auth required) *)

open Core
open Async

let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let pass msg =
  incr tests_run;
  incr tests_passed;
  printf "  * %s\n" msg

let fail msg =
  incr tests_run;
  incr tests_failed;
  printf "  X FAIL: %s\n" msg

(* ============================================================ *)
(* Helper for public GET requests *)
(* ============================================================ *)

let get_json path =
  (* Build URI from path string directly to preserve query params *)
  let uri = Uri.of_string (sprintf "https://api.gemini.com%s" path) in
  Monitor.try_with (fun () ->
    Cohttp_async.Client.get uri >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
  ) >>| function
  | Error exn -> Error (Exn.to_string exn)
  | Ok body ->
    match Yojson.Safe.from_string body with
    | exception _ -> Error body
    | json -> Ok json

(* ============================================================ *)
(* REST API Integration Tests *)
(* ============================================================ *)

let test_symbols () =
  printf "\n[REST] Symbols\n";
  get_json "/v1/symbols" >>| function
  | Ok (`List symbols) ->
    let count = List.length symbols in
    pass (sprintf "Got %d symbols" count);
    (match List.hd symbols with
     | Some (`String s) -> pass (sprintf "First symbol: %s" s)
     | _ -> ())
  | Ok _ -> fail "Unexpected response format"
  | Error e -> fail (sprintf "Error: %s" e)

let test_pubticker () =
  printf "\n[REST] Public Ticker (btcusd)\n";
  get_json "/v1/pubticker/btcusd" >>| function
  | Ok json ->
    let open Yojson.Safe.Util in
    (try
       let bid = json |> member "bid" |> to_string in
       let ask = json |> member "ask" |> to_string in
       let last = json |> member "last" |> to_string in
       pass (sprintf "Bid: %s | Ask: %s | Last: %s" bid ask last);
       let volume = json |> member "volume" in
       let btc_vol = volume |> member "BTC" |> to_string in
       pass (sprintf "24h volume: %s BTC" btc_vol)
     with _ -> fail "Failed to parse ticker response")
  | Error e -> fail (sprintf "Error: %s" e)

let test_book () =
  printf "\n[REST] Order Book (ethusd)\n";
  get_json "/v1/book/ethusd?limit_bids=5&limit_asks=5" >>| function
  | Ok json ->
    (try
       match json with
       | `Assoc fields ->
         let bids = match List.Assoc.find fields ~equal:String.equal "bids" with
           | Some (`List l) -> l | _ -> []
         in
         let asks = match List.Assoc.find fields ~equal:String.equal "asks" with
           | Some (`List l) -> l | _ -> []
         in
         let bid_count = List.length bids in
         let ask_count = List.length asks in
         pass (sprintf "Bids: %d levels | Asks: %d levels" bid_count ask_count);
         (match List.hd bids with
          | Some (`Assoc bid_fields) ->
            let price = match List.Assoc.find bid_fields ~equal:String.equal "price" with
              | Some (`String s) -> s | _ -> "?"
            in
            let amount = match List.Assoc.find bid_fields ~equal:String.equal "amount" with
              | Some (`String s) -> s | _ -> "?"
            in
            pass (sprintf "Best bid: %s @ $%s" amount price)
          | _ -> ());
         (match List.hd asks with
          | Some (`Assoc ask_fields) ->
            let price = match List.Assoc.find ask_fields ~equal:String.equal "price" with
              | Some (`String s) -> s | _ -> "?"
            in
            let amount = match List.Assoc.find ask_fields ~equal:String.equal "amount" with
              | Some (`String s) -> s | _ -> "?"
            in
            pass (sprintf "Best ask: %s @ $%s" amount price)
          | _ -> ())
       | _ -> fail "Unexpected JSON structure"
     with e -> fail (sprintf "Failed to parse book response: %s" (Exn.to_string e)))
  | Error e -> fail (sprintf "Error: %s" e)

let test_trades () =
  printf "\n[REST] Recent Trades (btcusd)\n";
  get_json "/v1/trades/btcusd?limit_trades=5" >>| function
  | Ok json ->
    let trades = match json with `List l -> l | _ -> [] in
    let count = List.length trades in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d trades" count);
       (match List.hd trades with
        | Some trade ->
          let open Yojson.Safe.Util in
          (try
             let price = trade |> member "price" |> to_string in
             let amount = trade |> member "amount" |> to_string in
             let side = trade |> member "type" |> to_string in
             (* tid can be a large int64, use to_string or to_int_exn with care *)
             let tid_str = match trade |> member "tid" with
               | `Int n -> Int.to_string n
               | `Intlit s -> s
               | _ -> "?"
             in
             pass (sprintf "Latest: %s %s @ $%s (tid: %s)" side amount price tid_str)
           with e -> fail (sprintf "Failed to parse trade: %s" (Exn.to_string e)))
        | None -> ())
     | false -> fail "No trades returned")
  | Error e -> fail (sprintf "Error: %s" e)

let test_symbol_details () =
  printf "\n[REST] Symbol Details (btcusd)\n";
  get_json "/v1/symbols/details/btcusd" >>| function
  | Ok json ->
    let open Yojson.Safe.Util in
    (try
       let symbol = json |> member "symbol" |> to_string in
       let base = json |> member "base_currency" |> to_string in
       let quote = json |> member "quote_currency" |> to_string in
       let status = json |> member "status" |> to_string in
       let min_order = json |> member "min_order_size" |> to_string in
       pass (sprintf "Symbol: %s (%s/%s)" symbol base quote);
       pass (sprintf "Status: %s | Min order: %s" status min_order)
     with _ -> fail "Failed to parse symbol details")
  | Error e -> fail (sprintf "Error: %s" e)


(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "Gemini Integration Tests (Public APIs)\n";
  printf "===========================================\n";

  (* Direct REST API tests - public endpoints only *)
  test_symbols () >>= fun () ->
  test_pubticker () >>= fun () ->
  test_book () >>= fun () ->
  test_trades () >>= fun () ->
  test_symbol_details () >>= fun () ->

  (* Summary *)
  printf "\n===========================================\n";
  printf "Integration Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  match !tests_failed > 0 with
  | true -> exit 1
  | false -> return ()

let () =
  don't_wait_for (run_tests ());
  never_returns (Scheduler.go ())

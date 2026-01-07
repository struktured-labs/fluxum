(** Coinbase Integration Tests - Public REST APIs (no auth required)
    Uses the Coinbase Exchange API (api.exchange.coinbase.com) which has public endpoints *)

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
(* Helper for public GET requests to Exchange API *)
(* ============================================================ *)

let get_json path =
  (* Build URI from path string directly to preserve query params *)
  let uri = Uri.of_string (sprintf "https://api.exchange.coinbase.com%s" path) in
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
(* REST API Integration Tests (Exchange API - Public Endpoints) *)
(* ============================================================ *)

let test_products () =
  printf "\n[REST] Products (All Trading Pairs)\n";
  get_json "/products" >>| function
  | Ok (`List products) ->
    let count = List.length products in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d products" count);
       let open Yojson.Safe.Util in
       (match List.find products ~f:(fun p ->
          String.equal (p |> member "id" |> to_string) "BTC-USD") with
        | Some p ->
          let status = p |> member "status" |> to_string in
          pass (sprintf "BTC-USD: status=%s" status);
          let base = p |> member "base_currency" |> to_string in
          let quote = p |> member "quote_currency" |> to_string in
          pass (sprintf "BTC-USD: %s/%s" base quote)
        | None -> fail "BTC-USD not found in products")
     | false -> fail "No products returned")
  | Ok _ -> fail "Unexpected response format"
  | Error e -> fail (sprintf "Error: %s" e)

let test_product_book () =
  printf "\n[REST] Order Book (BTC-USD)\n";
  get_json "/products/BTC-USD/book?level=2" >>| function
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
         (match bid_count > 0 && ask_count > 0 with
         | true ->
           pass (sprintf "Got %d bids, %d asks" bid_count ask_count);
           (* Coinbase book format: [price, size, num_orders] *)
           (match List.hd bids with
            | Some (`List (price :: size :: _)) ->
              let p = match price with `String s -> s | _ -> "?" in
              let s = match size with `String s -> s | _ -> "?" in
              pass (sprintf "Best bid: %s @ $%s" s p)
            | _ -> pass "Could not parse bid level");
           (match List.hd asks with
            | Some (`List (price :: size :: _)) ->
              let p = match price with `String s -> s | _ -> "?" in
              let s = match size with `String s -> s | _ -> "?" in
              pass (sprintf "Best ask: %s @ $%s" s p)
            | _ -> pass "Could not parse ask level")
         | false -> fail (sprintf "Empty order book (bids=%d, asks=%d)" bid_count ask_count))
       | _ -> fail "Unexpected JSON structure"
     with e -> fail (sprintf "Parse error: %s" (Exn.to_string e)))
  | Error e -> fail (sprintf "Error: %s" e)

let test_ticker () =
  printf "\n[REST] Ticker (ETH-USD)\n";
  get_json "/products/ETH-USD/ticker" >>| function
  | Ok json ->
    let open Yojson.Safe.Util in
    (try
       let price = json |> member "price" |> to_string in
       let bid = json |> member "bid" |> to_string in
       let ask = json |> member "ask" |> to_string in
       let volume = json |> member "volume" |> to_string in
       pass (sprintf "Price: $%s" price);
       pass (sprintf "Bid: $%s | Ask: $%s" bid ask);
       pass (sprintf "24h Volume: %s" volume)
     with e -> fail (sprintf "Parse error: %s" (Exn.to_string e)))
  | Error e -> fail (sprintf "Error: %s" e)

let test_trades () =
  printf "\n[REST] Recent Trades (BTC-USD)\n";
  get_json "/products/BTC-USD/trades?limit=5" >>| function
  | Ok json ->
    let trades = match json with `List l -> l | _ -> [] in
    let count = List.length trades in
    (match count > 0 with
     | true ->
       pass (sprintf "Got %d trades" count);
       let open Yojson.Safe.Util in
       (match List.hd trades with
        | Some trade ->
          (try
             let price = trade |> member "price" |> to_string in
             let size = trade |> member "size" |> to_string in
             let side = trade |> member "side" |> to_string in
             let trade_id_str = match trade |> member "trade_id" with
               | `Int n -> Int.to_string n
               | `Intlit s -> s
               | _ -> "?"
             in
             pass (sprintf "Latest: %s %s @ $%s (id: %s)" side size price trade_id_str)
           with e -> fail (sprintf "Parse error: %s" (Exn.to_string e)))
        | None -> ())
     | false -> fail "No trades returned")
  | Error e -> fail (sprintf "Error: %s" e)

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let run_tests () =
  printf "===========================================\n";
  printf "Coinbase Integration Tests (Public APIs)\n";
  printf "Using: api.exchange.coinbase.com\n";
  printf "===========================================\n";

  (* REST API tests *)
  test_products () >>= fun () ->
  test_product_book () >>= fun () ->
  test_ticker () >>= fun () ->
  test_trades () >>= fun () ->

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

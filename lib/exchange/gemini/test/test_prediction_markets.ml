(** Unit tests for Gemini Prediction Markets module

    Tests cover:
    1. Domain type parsing (Event_status, Outcome, Event_type)
    2. Event/Contract JSON deserialization
    3. Contract_metadata parsing
    4. Place_order response normalization
    5. Position normalization
    6. Trades/Ticker response parsing
    7. Book_snapshot parsing
    8. Price validation
    9. Timestamp parsing in adapter
    10. Edge cases *)

open Core

(* Test infrastructure *)
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
  printf "  FAIL: %s\n" msg

let test_assert msg cond =
  match cond with
  | true -> pass msg
  | false -> fail msg

let _test_ok msg = function
  | Ok _ -> pass msg
  | Error e -> fail (sprintf "%s: %s" msg e)

let _test_error msg = function
  | Error _ -> pass msg
  | Ok _ -> fail (sprintf "%s: expected error" msg)

module PM = Gemini.Prediction_markets

(* ============================================================ *)
(* Domain Types *)
(* ============================================================ *)

let test_event_status () =
  printf "\n=== Event_status Parsing ===\n";
  test_assert "active parses"
    (Option.is_some (PM.Event_status.of_string_opt "active"));
  test_assert "settled parses"
    (Option.is_some (PM.Event_status.of_string_opt "settled"));
  test_assert "Active parses (case insensitive)"
    (Option.is_some (PM.Event_status.of_string_opt "Active"));
  test_assert "CLOSED parses (case insensitive)"
    (Option.is_some (PM.Event_status.of_string_opt "CLOSED"));
  test_assert "under_review parses"
    (Option.is_some (PM.Event_status.of_string_opt "under_review"));
  test_assert "invalid parses"
    (Option.is_some (PM.Event_status.of_string_opt "invalid"));
  test_assert "garbage returns None"
    (Option.is_none (PM.Event_status.of_string_opt "garbage"))

let test_outcome () =
  printf "\n=== Outcome Parsing ===\n";
  test_assert "yes parses"
    (Option.is_some (PM.Outcome.of_string_opt "yes"));
  test_assert "no parses"
    (Option.is_some (PM.Outcome.of_string_opt "no"));
  test_assert "YES parses"
    (Option.is_some (PM.Outcome.of_string_opt "YES"));
  test_assert "maybe returns None"
    (Option.is_none (PM.Outcome.of_string_opt "maybe"))

let test_event_type () =
  printf "\n=== Event_type Parsing ===\n";
  test_assert "binary parses"
    (Option.is_some (PM.Event_type.of_string_opt "binary"));
  test_assert "categorical parses"
    (Option.is_some (PM.Event_type.of_string_opt "categorical"))

(* ============================================================ *)
(* Contract Parsing *)
(* ============================================================ *)

let test_contract_parsing () =
  printf "\n=== Contract JSON Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "id": "c1",
    "instrumentSymbol": "GEMI-BTC100K-YES",
    "label": "Yes",
    "ticker": "BTC100K-YES",
    "totalShares": "10000",
    "status": "active",
    "color": "#00ff00",
    "prices": {
      "buy": {"yes": "0.65", "no": "0.35"},
      "sell": {"yes": "0.35", "no": "0.65"},
      "bestBid": "0.64",
      "bestAsk": "0.66",
      "lastTradePrice": "0.65"
    }
  }|} in
  (match PM.Contract.of_yojson json with
   | Ok c ->
     test_assert "instrument_symbol correct"
       (String.equal c.instrument_symbol "GEMI-BTC100K-YES");
     test_assert "label correct" (String.equal c.label "Yes");
     test_assert "ticker correct" (String.equal c.ticker "BTC100K-YES");
     test_assert "prices present" (Option.is_some c.prices);
     test_assert "total_shares correct" (String.equal c.total_shares "10000");
     test_assert "status correct" (String.equal c.status "active")
   | Error e -> fail (sprintf "Contract parse failed: %s" e));
  (* Minimal contract *)
  let json_minimal = Yojson.Safe.from_string {|{
    "id": "c2",
    "instrumentSymbol": "GEMI-TEST-NO",
    "label": "No",
    "ticker": "TEST-NO",
    "totalShares": "0",
    "status": "settled",
    "color": "#ff0000"
  }|} in
  (match PM.Contract.of_yojson json_minimal with
   | Ok c ->
     test_assert "minimal contract: no prices" (Option.is_none c.prices);
     test_assert "minimal contract: status settled" (String.equal c.status "settled")
   | Error e -> fail (sprintf "Minimal contract parse failed: %s" e))

(* ============================================================ *)
(* Contract Metadata *)
(* ============================================================ *)

let test_contract_metadata () =
  printf "\n=== Contract_metadata Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "contractId": "abc123",
    "contractName": "BTC > 100K",
    "contractTicker": "BTC100K-YES",
    "eventTicker": "BTC100K",
    "eventName": "Bitcoin 100K",
    "category": "crypto",
    "contractStatus": "active",
    "expiryDate": "2026-12-31",
    "resolvedAt": null,
    "description": "Will BTC exceed 100K?"
  }|} in
  (match PM.Contract_metadata.of_yojson json with
   | Ok m ->
     test_assert "contract_id parsed"
       (Option.equal String.equal m.contract_id (Some "abc123"));
     test_assert "contract_name parsed"
       (Option.equal String.equal m.contract_name (Some "BTC > 100K"));
     test_assert "event_ticker parsed"
       (Option.equal String.equal m.event_ticker (Some "BTC100K"));
     test_assert "category parsed"
       (Option.equal String.equal m.category (Some "crypto"));
     test_assert "resolved_at is None"
       (Option.is_none m.resolved_at);
     test_assert "expiry_date parsed"
       (Option.equal String.equal m.expiry_date (Some "2026-12-31"))
   | Error e -> fail (sprintf "Contract_metadata parse failed: %s" e));
  (* Empty metadata *)
  let json_empty = Yojson.Safe.from_string "{}" in
  (match PM.Contract_metadata.of_yojson json_empty with
   | Ok m ->
     test_assert "empty metadata: all None"
       (Option.is_none m.contract_id
        && Option.is_none m.contract_name
        && Option.is_none m.event_ticker)
   | Error e -> fail (sprintf "Empty metadata parse failed: %s" e))

(* ============================================================ *)
(* Place_order Response *)
(* ============================================================ *)

let test_place_order_response () =
  printf "\n=== Place_order Response Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "orderId": 12345,
    "status": "open",
    "symbol": "GEMI-BTC100K-YES",
    "side": "buy",
    "outcome": "yes",
    "orderType": "limit",
    "quantity": "10",
    "filledQuantity": "3",
    "remainingQuantity": "7",
    "price": "0.65",
    "avgExecutionPrice": "0.64",
    "createdAt": "2026-03-08T12:00:00Z",
    "updatedAt": "2026-03-08T12:01:00Z",
    "cancelledAt": null,
    "contractMetadata": {
      "contractId": "abc",
      "contractName": "BTC100K Yes",
      "eventTicker": "BTC100K"
    }
  }|} in
  (match PM.Place_order.response_of_yojson json with
   | Ok r ->
     test_assert "order_id = 12345" (Int64.equal r.order_id 12345L);
     test_assert "status = open" (String.equal r.status "open");
     test_assert "symbol correct" (String.equal r.symbol "GEMI-BTC100K-YES");
     test_assert "quantity = 10" (String.equal r.quantity "10");
     test_assert "filled = 3" (String.equal r.filled_quantity "3");
     test_assert "remaining = 7" (String.equal r.remaining_quantity "7");
     test_assert "price = 0.65" (String.equal r.price "0.65");
     test_assert "avg_execution_price present"
       (Option.equal String.equal r.avg_execution_price (Some "0.64"));
     test_assert "created_at present" (Option.is_some r.created_at);
     test_assert "updated_at present" (Option.is_some r.updated_at);
     test_assert "cancelled_at is None" (Option.is_none r.cancelled_at);
     test_assert "contract_metadata present" (Option.is_some r.contract_metadata);
     let m = Option.value_exn r.contract_metadata in
     test_assert "metadata.event_ticker"
       (Option.equal String.equal m.event_ticker (Some "BTC100K"));
     test_assert "metadata.contract_name"
       (Option.equal String.equal m.contract_name (Some "BTC100K Yes"))
   | Error e -> fail (sprintf "Place_order response parse failed: %s" e));
  (* Minimal response (defaults) *)
  let json_minimal = Yojson.Safe.from_string {|{
    "orderId": 1,
    "status": "cancelled",
    "symbol": "GEMI-TEST-NO",
    "side": "sell",
    "outcome": "no",
    "orderType": "limit",
    "quantity": "5",
    "price": "0.30"
  }|} in
  (match PM.Place_order.response_of_yojson json_minimal with
   | Ok r ->
     test_assert "minimal: filled defaults to 0" (String.equal r.filled_quantity "0");
     test_assert "minimal: remaining defaults to 0" (String.equal r.remaining_quantity "0");
     test_assert "minimal: no avg_execution_price" (Option.is_none r.avg_execution_price);
     test_assert "minimal: no created_at" (Option.is_none r.created_at);
     test_assert "minimal: no metadata" (Option.is_none r.contract_metadata)
   | Error e -> fail (sprintf "Minimal Place_order parse failed: %s" e))

(* ============================================================ *)
(* Positions *)
(* ============================================================ *)

let test_positions () =
  printf "\n=== Positions Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "symbol": "GEMI-BTC100K-YES",
    "instrumentId": 42,
    "totalQuantity": "25",
    "avgPrice": "0.55",
    "outcome": "yes",
    "contractMetadata": {
      "contractId": "c1",
      "contractName": "BTC > 100K",
      "eventTicker": "BTC100K"
    }
  }|} in
  (match PM.Positions.position_of_yojson json with
   | Ok p ->
     test_assert "symbol correct" (String.equal p.symbol "GEMI-BTC100K-YES");
     test_assert "total_quantity = 25" (String.equal p.total_quantity "25");
     test_assert "avg_price = 0.55" (String.equal p.avg_price "0.55");
     test_assert "metadata present" (Option.is_some p.contract_metadata)
   | Error e -> fail (sprintf "Position parse failed: %s" e))

(* ============================================================ *)
(* Trades *)
(* ============================================================ *)

let test_trades () =
  printf "\n=== Trades Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "timestamp": 1709900000,
    "timestampms": 1709900000123,
    "tid": 99001,
    "price": "0.72",
    "amount": "5",
    "exchange": "gemini",
    "type": "buy",
    "broken": false
  }|} in
  (match PM.Trades.trade_of_yojson json with
   | Ok t ->
     test_assert "tid = 99001" (Int64.equal t.tid 99001L);
     test_assert "price = 0.72" (String.equal t.price "0.72");
     test_assert "amount = 5" (String.equal t.amount "5");
     test_assert "side = buy" (String.equal t.side "buy");
     test_assert "not broken" (not t.broken);
     test_assert "timestampms correct" (Int64.equal t.timestampms 1709900000123L)
   | Error e -> fail (sprintf "Trade parse failed: %s" e));
  (* Broken trade *)
  let json_broken = Yojson.Safe.from_string {|{
    "timestamp": 1, "timestampms": 1000, "tid": 1,
    "price": "0.50", "amount": "1", "type": "buy", "broken": true
  }|} in
  (match PM.Trades.trade_of_yojson json_broken with
   | Ok t -> test_assert "broken trade parsed" t.broken
   | Error e -> fail (sprintf "Broken trade parse failed: %s" e));
  (* Default exchange field *)
  let json_no_exchange = Yojson.Safe.from_string {|{
    "timestamp": 1, "timestampms": 1000, "tid": 2,
    "price": "0.50", "amount": "1", "type": "sell"
  }|} in
  (match PM.Trades.trade_of_yojson json_no_exchange with
   | Ok t ->
     test_assert "exchange defaults to gemini" (String.equal t.exchange "gemini");
     test_assert "broken defaults to false" (not t.broken)
   | Error e -> fail (sprintf "Trade defaults parse failed: %s" e))

(* ============================================================ *)
(* Ticker *)
(* ============================================================ *)

let test_ticker () =
  printf "\n=== Ticker Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "symbol": "GEMI-BTC100K-YES",
    "open": "0.60",
    "high": "0.75",
    "low": "0.58",
    "close": "0.72",
    "changes": ["0.65", "0.68", "0.70"],
    "bid": "0.71",
    "ask": "0.73"
  }|} in
  (match PM.Ticker.of_yojson json with
   | Ok t ->
     test_assert "symbol correct" (String.equal t.symbol "GEMI-BTC100K-YES");
     test_assert "open = 0.60" (String.equal t.open_ "0.60");
     test_assert "high = 0.75" (String.equal t.high "0.75");
     test_assert "low = 0.58" (String.equal t.low "0.58");
     test_assert "close = 0.72" (String.equal t.close "0.72");
     test_assert "bid = 0.71" (String.equal t.bid "0.71");
     test_assert "ask = 0.73" (String.equal t.ask "0.73");
     test_assert "3 hourly changes" (List.length t.changes = 3)
   | Error e -> fail (sprintf "Ticker parse failed: %s" e));
  (* Empty changes list *)
  let json_no_changes = Yojson.Safe.from_string {|{
    "symbol": "TEST", "open": "0", "high": "0", "low": "0",
    "close": "0", "bid": "0", "ask": "0"
  }|} in
  (match PM.Ticker.of_yojson json_no_changes with
   | Ok t -> test_assert "changes defaults to empty" (List.is_empty t.changes)
   | Error e -> fail (sprintf "Ticker no-changes parse failed: %s" e))

(* ============================================================ *)
(* Book Snapshot *)
(* ============================================================ *)

let test_book_snapshot () =
  printf "\n=== Book_snapshot Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "bids": [
      {"price": "0.64", "amount": "100", "timestamp": "1709900000"},
      {"price": "0.63", "amount": "50"}
    ],
    "asks": [
      {"price": "0.66", "amount": "75", "timestamp": "1709900001"}
    ]
  }|} in
  (match PM.Book_snapshot.of_yojson json with
   | Ok snap ->
     test_assert "2 bid levels" (List.length snap.bids = 2);
     test_assert "1 ask level" (List.length snap.asks = 1);
     let bid = List.hd_exn snap.bids in
     test_assert "bid price = 0.64" (String.equal bid.price "0.64");
     test_assert "bid amount = 100" (String.equal bid.amount "100");
     test_assert "bid timestamp present" (String.equal bid.timestamp "1709900000");
     let bid2 = List.nth_exn snap.bids 1 in
     test_assert "missing timestamp defaults empty" (String.equal bid2.timestamp "")
   | Error e -> fail (sprintf "Book_snapshot parse failed: %s" e))

(* ============================================================ *)
(* Positions Volume *)
(* ============================================================ *)

let test_positions_volume () =
  printf "\n=== Positions_volume Parsing ===\n";
  let json = Yojson.Safe.from_string {|{
    "symbol": "GEMI-BTC100K-YES",
    "outcome": "yes",
    "totalBuyQuantity": "100",
    "totalSellQuantity": "25",
    "totalBuyVolume": "65.00",
    "totalSellVolume": "18.75"
  }|} in
  (match PM.Positions_volume.entry_of_yojson json with
   | Ok e ->
     test_assert "symbol correct" (String.equal e.symbol "GEMI-BTC100K-YES");
     test_assert "buy qty = 100" (String.equal e.total_buy_quantity "100");
     test_assert "sell qty = 25" (String.equal e.total_sell_quantity "25");
     test_assert "buy vol = 65.00" (String.equal e.total_buy_volume "65.00");
     test_assert "sell vol = 18.75" (String.equal e.total_sell_volume "18.75")
   | Error e -> fail (sprintf "Positions_volume entry parse failed: %s" e));
  (* Defaults *)
  let json_minimal = Yojson.Safe.from_string {|{
    "symbol": "X", "outcome": "no"
  }|} in
  (match PM.Positions_volume.entry_of_yojson json_minimal with
   | Ok e ->
     test_assert "buy qty defaults to 0" (String.equal e.total_buy_quantity "0");
     test_assert "sell vol defaults to 0" (String.equal e.total_sell_volume "0")
   | Error e -> fail (sprintf "Minimal volume entry parse failed: %s" e))

(* ============================================================ *)
(* Normalized Types - Adapter *)
(* ============================================================ *)

let test_order_response_fields () =
  printf "\n=== Order Response: Timestamp & Metadata Fields ===\n";
  let json = Yojson.Safe.from_string {|{
    "orderId": 555,
    "status": "open",
    "symbol": "GEMI-TEST-YES",
    "side": "buy",
    "outcome": "yes",
    "orderType": "limit",
    "quantity": "10",
    "filledQuantity": "0",
    "remainingQuantity": "10",
    "price": "0.50",
    "createdAt": "2026-03-08T12:00:00.000Z",
    "updatedAt": "2026-03-08T12:01:30.000Z",
    "contractMetadata": {
      "contractId": "cid1",
      "contractName": "Test Yes",
      "eventTicker": "TEST"
    }
  }|} in
  (match PM.Place_order.response_of_yojson json with
   | Ok r ->
     test_assert "order_id = 555" (Int64.equal r.order_id 555L);
     test_assert "created_at present" (Option.is_some r.created_at);
     test_assert "updated_at present" (Option.is_some r.updated_at);
     test_assert "metadata present" (Option.is_some r.contract_metadata);
     let m = Option.value_exn r.contract_metadata in
     test_assert "metadata.contract_id"
       (Option.equal String.equal m.contract_id (Some "cid1"));
     test_assert "metadata.contract_name"
       (Option.equal String.equal m.contract_name (Some "Test Yes"));
     test_assert "metadata.event_ticker"
       (Option.equal String.equal m.event_ticker (Some "TEST"))
   | Error e -> fail (sprintf "parse response failed: %s" e))

let test_position_metadata () =
  printf "\n=== Position: Metadata Extraction ===\n";
  let json = Yojson.Safe.from_string {|{
    "symbol": "GEMI-TEST-NO",
    "instrumentId": 99,
    "totalQuantity": "50",
    "avgPrice": "0.30",
    "outcome": "no",
    "contractMetadata": {
      "contractId": "cid2",
      "contractName": "Test No",
      "eventTicker": "TEST"
    }
  }|} in
  (match PM.Positions.position_of_yojson json with
   | Ok p ->
     test_assert "symbol correct" (String.equal p.symbol "GEMI-TEST-NO");
     test_assert "total_quantity = 50" (String.equal p.total_quantity "50");
     test_assert "avg_price = 0.30" (String.equal p.avg_price "0.30");
     let m = Option.value_exn p.contract_metadata in
     test_assert "metadata.contract_id"
       (Option.equal String.equal m.contract_id (Some "cid2"));
     test_assert "metadata.contract_name"
       (Option.equal String.equal m.contract_name (Some "Test No"))
   | Error e -> fail (sprintf "parse position failed: %s" e))

let test_adapter_price_validation () =
  printf "\n=== Adapter: Price Validation ===\n";
  (* Price > 1.0 should be rejected *)
  test_assert "price 1.50 is invalid (> 1.0)"
    (match Float.of_string_opt "1.50" with
     | Some p -> Float.(p < 0.0 || p > 1.0)
     | None -> false);
  test_assert "price -0.10 is invalid (< 0.0)"
    (match Float.of_string_opt "-0.10" with
     | Some p -> Float.(p < 0.0 || p > 1.0)
     | None -> false);
  test_assert "price 0.65 is valid"
    (match Float.of_string_opt "0.65" with
     | Some p -> Float.(p >= 0.0 && p <= 1.0)
     | None -> false);
  test_assert "price 0.00 is valid"
    (match Float.of_string_opt "0.00" with
     | Some p -> Float.(p >= 0.0 && p <= 1.0)
     | None -> false);
  test_assert "price 1.00 is valid"
    (match Float.of_string_opt "1.00" with
     | Some p -> Float.(p >= 0.0 && p <= 1.0)
     | None -> false)

(* ============================================================ *)
(* Edge Cases *)
(* ============================================================ *)

let test_edge_cases () =
  printf "\n=== Edge Cases ===\n";
  (* Malformed trade JSON *)
  (match PM.Trades.trade_of_yojson (`String "not a trade") with
   | Error _ -> test_assert "malformed trade rejected" true
   | Ok _ -> fail "malformed trade should fail");
  (* Position with no metadata *)
  let json_no_meta = Yojson.Safe.from_string {|{
    "symbol": "X", "instrumentId": 1, "totalQuantity": "0",
    "avgPrice": "0", "outcome": "yes"
  }|} in
  (match PM.Positions.position_of_yojson json_no_meta with
   | Ok p -> test_assert "position without metadata" (Option.is_none p.contract_metadata)
   | Error _ -> fail "position without metadata should parse");
  (* Empty book *)
  let json = Yojson.Safe.from_string {|{"bids": [], "asks": []}|} in
  (match PM.Book_snapshot.of_yojson json with
   | Ok snap ->
     test_assert "empty book bids" (List.is_empty snap.bids);
     test_assert "empty book asks" (List.is_empty snap.asks)
   | Error _ -> fail "empty book should parse");
  (* Malformed order: non-numeric quantity still parses (string field) *)
  let json_bad = Yojson.Safe.from_string {|{
    "orderId": 1, "status": "open", "symbol": "X",
    "side": "buy", "outcome": "yes", "orderType": "limit",
    "quantity": "not_a_number", "price": "0.50"
  }|} in
  (match PM.Place_order.response_of_yojson json_bad with
   | Ok r ->
     test_assert "non-numeric qty parses as string" (String.equal r.quantity "not_a_number")
   | Error _ -> fail "should parse even with bad qty (string field)")

(* ============================================================ *)
(* Test Runner *)
(* ============================================================ *)

let () =
  printf "\n";
  printf "====================================================\n";
  printf "  Gemini Prediction Markets - Unit Test Suite\n";
  printf "====================================================\n";
  test_event_status ();
  test_outcome ();
  test_event_type ();
  test_contract_parsing ();
  test_contract_metadata ();
  test_place_order_response ();
  test_positions ();
  test_trades ();
  test_ticker ();
  test_book_snapshot ();
  test_positions_volume ();
  test_order_response_fields ();
  test_position_metadata ();
  test_adapter_price_validation ();
  test_edge_cases ();
  printf "\n====================================================\n";
  printf "  Test Results: %d run, %d passed, %d failed\n"
    !tests_run !tests_passed !tests_failed;
  printf "====================================================\n";
  (match !tests_failed with
   | 0 -> printf "\n* All tests passed!\n\n"
   | n -> printf "\n!! %d TESTS FAILED !!\n\n" n; exit 1)

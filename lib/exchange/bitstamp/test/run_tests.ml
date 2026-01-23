(** Bitstamp Unit Tests *)

open Core

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

let test_assert name condition =
  match condition with true -> pass name | false -> fail name

(* ============================================================ *)
(* Configuration Tests *)
(* ============================================================ *)

let test_cfg () =
  printf "\n[Configuration]\n";

  (* Test production config *)
  test_assert "Production REST URL"
    (String.is_prefix Bitstamp.Cfg.production.rest_url ~prefix:"https://www.bitstamp.net");

  test_assert "Production WS URL"
    (String.is_prefix Bitstamp.Cfg.production.ws_url ~prefix:"wss://ws.bitstamp.net");

  test_assert "Production has no API key by default"
    (Option.is_none Bitstamp.Cfg.production.api_key);

  (* Test with_auth *)
  let cfg = Bitstamp.Cfg.with_auth
    ~api_key:"test_key"
    ~api_secret:"test_secret"
    ~customer_id:"12345"
    Bitstamp.Cfg.production
  in
  test_assert "with_auth sets API key"
    (Option.equal String.equal cfg.api_key (Some "test_key"));
  test_assert "with_auth sets API secret"
    (Option.equal String.equal cfg.api_secret (Some "test_secret"));
  test_assert "with_auth sets customer ID"
    (Option.equal String.equal cfg.customer_id (Some "12345"));

  (* Test mainnet alias *)
  test_assert "Mainnet is production alias"
    (String.equal Bitstamp.Cfg.mainnet.rest_url Bitstamp.Cfg.production.rest_url)

(* ============================================================ *)
(* Types Tests *)
(* ============================================================ *)

let test_types () =
  printf "\n[Types]\n";

  (* Test ticker parsing *)
  let ticker_json = `Assoc [
    ("high", `String "50000.00");
    ("last", `String "49500.00");
    ("timestamp", `String "1699000000");
    ("bid", `String "49450.00");
    ("vwap", `String "49700.00");
    ("volume", `String "1234.56");
    ("low", `String "48000.00");
    ("ask", `String "49550.00");
    ("open", `String "49000.00");
  ] in
  (match Bitstamp.Types.ticker_of_yojson ticker_json with
   | Ok ticker ->
     test_assert "Ticker parses correctly" true;
     test_assert "Ticker last price" (String.equal ticker.last "49500.00");
     test_assert "Ticker bid" (String.equal ticker.bid "49450.00");
     test_assert "Ticker ask" (String.equal ticker.ask "49550.00")
   | Error _ -> fail "Ticker parsing failed");

  (* Test order book entry parsing *)
  let entry_json = `List [`String "50000.0"; `String "1.5"] in
  (match Bitstamp.Types.order_book_entry_of_yojson entry_json with
   | Ok (price, amount) ->
     test_assert "Order book entry parses" true;
     test_assert "Entry price" Float.(abs (price - 50000.0) < 0.01);
     test_assert "Entry amount" Float.(abs (amount - 1.5) < 0.01)
   | Error _ -> fail "Order book entry parsing failed");

  (* Test order book parsing *)
  let book_json = `Assoc [
    ("timestamp", `String "1699000000");
    ("microtimestamp", `String "1699000000000000");
    ("bids", `List [
      `List [`String "50000.0"; `String "1.5"];
      `List [`String "49999.0"; `String "2.0"];
    ]);
    ("asks", `List [
      `List [`String "50001.0"; `String "1.0"];
      `List [`String "50002.0"; `String "0.5"];
    ]);
  ] in
  (match Bitstamp.Types.order_book_of_yojson book_json with
   | Ok book ->
     test_assert "Order book parses" true;
     test_assert "Order book has 2 bids" (List.length book.bids = 2);
     test_assert "Order book has 2 asks" (List.length book.asks = 2)
   | Error _ -> fail "Order book parsing failed");

  (* Test trade parsing *)
  let trade_json = `Assoc [
    ("date", `String "1699000000");
    ("tid", `Int 123456);
    ("price", `String "50000.0");
    ("amount", `String "0.5");
    ("type", `Int 0);
  ] in
  (match Bitstamp.Types.trade_of_yojson trade_json with
   | Ok trade ->
     test_assert "Trade parses" true;
     test_assert "Trade price" (String.equal trade.price "50000.0");
     test_assert "Trade amount" (String.equal trade.amount "0.5");
     test_assert "Trade type buy" (trade.type_ = 0)
   | Error _ -> fail "Trade parsing failed")

(* ============================================================ *)
(* WebSocket Channel Tests *)
(* ============================================================ *)

let test_ws_channels () =
  printf "\n[WebSocket Channels]\n";

  let open Bitstamp.Ws.Channel in

  (* Test channel to string *)
  test_assert "Live trades channel"
    (String.equal (to_string (Live_trades "btcusd")) "live_trades_btcusd");

  test_assert "Live orders channel"
    (String.equal (to_string (Live_orders "ethusd")) "live_orders_ethusd");

  test_assert "Order book channel"
    (String.equal (to_string (Order_book "btceur")) "order_book_btceur");

  test_assert "Diff order book channel"
    (String.equal (to_string (Diff_order_book "xrpusd")) "diff_order_book_xrpusd");

  (* Test channel from string *)
  (match of_string "live_trades_btcusd" with
   | Some (Live_trades "btcusd") -> pass "Parse live_trades channel"
   | _ -> fail "Parse live_trades channel");

  (match of_string "order_book_ethusd" with
   | Some (Order_book "ethusd") -> pass "Parse order_book channel"
   | _ -> fail "Parse order_book channel");

  (match of_string "invalid_channel" with
   | None -> pass "Reject invalid channel"
   | _ -> fail "Should reject invalid channel")

(* ============================================================ *)
(* WebSocket Message Parsing Tests *)
(* ============================================================ *)

let test_ws_message_parsing () =
  printf "\n[WebSocket Message Parsing]\n";

  (* Test connection established *)
  let conn_msg = {|{"event":"bts:connection_established","data":{}}|} in
  (match Bitstamp.Ws.parse_message conn_msg with
   | Bitstamp.Ws.Message.Connected -> pass "Parse connection established"
   | _ -> fail "Parse connection established");

  (* Test subscription succeeded *)
  let sub_msg = {|{"event":"bts:subscription_succeeded","channel":"live_trades_btcusd","data":{}}|} in
  (match Bitstamp.Ws.parse_message sub_msg with
   | Bitstamp.Ws.Message.Subscribed "live_trades_btcusd" -> pass "Parse subscription succeeded"
   | _ -> fail "Parse subscription succeeded");

  (* Test error message *)
  let err_msg = {|{"event":"bts:request_reconnect","data":{}}|} in
  (match Bitstamp.Ws.parse_message err_msg with
   | Bitstamp.Ws.Message.Error _ -> pass "Parse error message"
   | _ -> fail "Parse error message");

  (* Test malformed JSON *)
  let malformed = "not valid json{" in
  (match Bitstamp.Ws.parse_message malformed with
   | Bitstamp.Ws.Message.Error _ -> pass "Handle malformed JSON"
   | _ -> fail "Should handle malformed JSON")

(* ============================================================ *)
(* REST Signature Tests *)
(* ============================================================ *)

let test_rest_signature () =
  printf "\n[REST Signature]\n";

  (* Test nonce generation *)
  let nonce1 = Bitstamp.Rest.generate_nonce () in
  let nonce2 = Bitstamp.Rest.generate_nonce () in
  test_assert "Nonce is numeric string"
    (String.for_all nonce1 ~f:Char.is_digit);
  test_assert "Nonces increase"
    (Int.of_string nonce2 >= Int.of_string nonce1);

  (* Test signature creation *)
  let signature = Bitstamp.Rest.create_signature
    ~api_secret:"test_secret"
    ~nonce:"1699000000000000"
    ~customer_id:"12345"
    ~api_key:"test_key"
  in
  test_assert "Signature is hex string"
    (String.for_all signature ~f:(fun c ->
      Char.is_digit c || (Char.is_alpha c && Char.is_uppercase c)));
  test_assert "Signature is 64 characters (SHA256)"
    (String.length signature = 64);

  (* Test signature determinism *)
  let sig1 = Bitstamp.Rest.create_signature
    ~api_secret:"secret"
    ~nonce:"123"
    ~customer_id:"456"
    ~api_key:"key"
  in
  let sig2 = Bitstamp.Rest.create_signature
    ~api_secret:"secret"
    ~nonce:"123"
    ~customer_id:"456"
    ~api_key:"key"
  in
  test_assert "Signatures are deterministic"
    (String.equal sig1 sig2)

(* ============================================================ *)
(* Error Type Tests *)
(* ============================================================ *)

let test_error_types () =
  printf "\n[Error Types]\n";

  let http_err : Bitstamp.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Bitstamp.Rest.Error.to_string http_err in
  test_assert "HTTP error to_string"
    (String.is_substring err_str ~substring:"404");

  let json_err : Bitstamp.Rest.Error.t = `Json_parse "bad json" in
  let err_str = Bitstamp.Rest.Error.to_string json_err in
  test_assert "JSON error to_string"
    (String.is_substring err_str ~substring:"JSON");

  let api_err : Bitstamp.Rest.Error.t = `Api_error "rate limited" in
  let err_str = Bitstamp.Rest.Error.to_string api_err in
  test_assert "API error to_string"
    (String.is_substring err_str ~substring:"API")

(* ============================================================ *)
(* Main *)
(* ============================================================ *)

let () =
  printf "===========================================\n";
  printf "Bitstamp Unit Tests\n";
  printf "===========================================\n";

  test_cfg ();
  test_types ();
  test_ws_channels ();
  test_ws_message_parsing ();
  test_rest_signature ();
  test_error_types ();

  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";

  (match !tests_failed > 0 with true -> exit 1 | false -> ())

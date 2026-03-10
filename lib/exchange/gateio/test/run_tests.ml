(** Gate.io Unit Tests *)

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
  match condition with
  | true -> pass name
  | false -> fail name

let test_cfg () =
  printf "\n[Configuration]\n";
  test_assert
    "Production REST URL"
    (String.is_prefix Gateio.Cfg.production.rest_url ~prefix:"https://api.gateio.ws");
  test_assert "Production has no API key" (Option.is_none Gateio.Cfg.production.api_key);
  let cfg =
    Gateio.Cfg.with_auth
      ~api_key:"test_key"
      ~api_secret:"test_secret"
      Gateio.Cfg.production
  in
    test_assert
      "with_auth sets API key"
      (Option.equal String.equal cfg.api_key (Some "test_key"));
    test_assert
      "with_auth sets API secret"
      (Option.equal String.equal cfg.api_secret (Some "test_secret"));
    test_assert
      "Mainnet is production alias"
      (String.equal Gateio.Cfg.mainnet.rest_url Gateio.Cfg.production.rest_url)

let test_types () =
  printf "\n[Types]\n";
  let ticker_json =
    `Assoc
      [ ("currency_pair", `String "BTC_USDT")
      ; ("last", `String "50000")
      ; ("lowest_ask", `String "50050")
      ; ("highest_bid", `String "49950")
      ; ("high_24h", `String "51000")
      ; ("low_24h", `String "48000")
      ; ("base_volume", `String "1234.5")
      ; ("quote_volume", `String "62000000") ]
  in
    (match Gateio.Types.ticker_of_yojson ticker_json with
     | Ok ticker ->
       test_assert "Ticker parses" true;
       test_assert "Ticker pair" (String.equal ticker.currency_pair "BTC_USDT");
       test_assert "Ticker last" (String.equal ticker.last "50000")
     | Error _ -> fail "Ticker parsing failed");
    let entry_json = `List [`String "50000"; `String "1.5"] in
      (match Gateio.Types.order_book_entry_of_yojson entry_json with
       | Ok (price, amount) ->
         test_assert "Order book entry parses" true;
         test_assert "Entry price" (String.equal price "50000");
         test_assert "Entry amount" (String.equal amount "1.5")
       | Error _ -> fail "Order book entry failed");
      let book_json =
        `Assoc
          [ ("current", `Int 1699000000)
          ; ("update", `Int 123456)
          ; ("asks", `List [`List [`String "50001"; `String "1.0"]])
          ; ("bids", `List [`List [`String "49999"; `String "2.0"]]) ]
      in
        (match Gateio.Types.order_book_of_yojson book_json with
         | Ok book ->
           test_assert "Order book parses" true;
           test_assert "Book has asks" (List.length book.asks = 1);
           test_assert "Book has bids" (List.length book.bids = 1)
         | Error _ -> fail "Order book parsing failed");
        let trade_json =
          `Assoc
            [ ("id", `Int 123456)
            ; ("create_time", `Int 1699000000)
            ; ("side", `String "buy")
            ; ("amount", `String "0.5")
            ; ("price", `String "50000") ]
        in
          (match Gateio.Types.trade_of_yojson trade_json with
           | Ok trade ->
             test_assert "Trade parses" true;
             test_assert "Trade side" (String.equal trade.side "buy");
             test_assert "Trade price" (String.equal trade.price "50000")
           | Error _ -> fail "Trade parsing failed");
          let balance_json =
            `Assoc
              [ ("currency", `String "BTC")
              ; ("available", `String "10.0")
              ; ("locked", `String "0.5") ]
          in
            match Gateio.Types.balance_of_yojson balance_json with
            | Ok balance ->
              test_assert "Balance parses" true;
              test_assert "Balance currency" (String.equal balance.currency "BTC");
              test_assert "Balance available" (String.equal balance.available "10.0")
            | Error _ -> fail "Balance parsing failed"

let test_rest_signature () =
  printf "\n[REST Signature]\n";
  let ts1 = Gateio.Rest.generate_timestamp () in
  let ts2 = Gateio.Rest.generate_timestamp () in
    test_assert "Timestamp is numeric" (String.for_all ts1 ~f:Char.is_digit);
    test_assert "Timestamps increase or equal" (Int.of_string ts2 >= Int.of_string ts1);
    let hash = Gateio.Rest.sha512_hash "test" in
      test_assert
        "SHA512 hash is hex lowercase"
        (String.for_all hash ~f:(fun c ->
           Char.is_digit c || (Char.is_alpha c && Char.is_lowercase c)));
      test_assert "SHA512 hash is 128 chars" (String.length hash = 128);
      let sig1 =
        Gateio.Rest.create_signature
          ~api_secret:"secret"
          ~method_:"GET"
          ~url_path:"/api/v4/spot/accounts"
          ~query_string:""
          ~body:""
          ~timestamp:"1699000000"
      in
      let sig2 =
        Gateio.Rest.create_signature
          ~api_secret:"secret"
          ~method_:"GET"
          ~url_path:"/api/v4/spot/accounts"
          ~query_string:""
          ~body:""
          ~timestamp:"1699000000"
      in
        test_assert "Signatures are deterministic" (String.equal sig1 sig2);
        test_assert
          "Signature is hex lowercase"
          (String.for_all sig1 ~f:(fun c ->
             Char.is_digit c || (Char.is_alpha c && Char.is_lowercase c)))

let test_error_types () =
  printf "\n[Error Types]\n";
  let http_err : Gateio.Rest.Error.t = `Http (404, "Not found") in
  let err_str = Gateio.Rest.Error.to_string http_err in
    test_assert "HTTP error to_string" (String.is_substring err_str ~substring:"404");
    let json_err : Gateio.Rest.Error.t = `Json_parse "bad json" in
    let err_str = Gateio.Rest.Error.to_string json_err in
      test_assert "JSON error to_string" (String.is_substring err_str ~substring:"JSON")

let test_normalization () =
  printf "\n[Normalization]\n";
  (* Ticker normalization *)
  let native_ticker : Gateio.Types.ticker =
    { currency_pair= "BTC_USDT"
    ; last= "67500.50"
    ; lowest_ask= "67501.00"
    ; highest_bid= "67500.00"
    ; high_24h= "68000.00"
    ; low_24h= "66000.00"
    ; base_volume= "1234.5678"
    ; quote_volume= "83333333.00" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.ticker native_ticker ~symbol:"BTC_USDT" with
   | Ok t ->
     test_assert "Ticker normalize: venue"
       (Fluxum.Types.Venue.equal t.venue Gateio);
     test_assert "Ticker normalize: symbol"
       (String.equal t.symbol "BTC_USDT");
     test_assert "Ticker normalize: last_price"
       (Float.(=) t.last_price 67500.50);
     test_assert "Ticker normalize: bid"
       (Float.(=) t.bid_price 67500.00);
     test_assert "Ticker normalize: ask"
       (Float.(=) t.ask_price 67501.00);
     test_assert "Ticker normalize: high"
       (Float.(=) t.high_24h 68000.00);
     test_assert "Ticker normalize: low"
       (Float.(=) t.low_24h 66000.00);
     test_assert "Ticker normalize: volume"
       (Float.(=) t.volume_24h 1234.5678);
     test_assert "Ticker normalize: quote_volume"
       (Option.is_some t.quote_volume)
   | Error e -> fail (sprintf "Ticker normalization failed: %s" e));
  (* Balance normalization *)
  let native_balance : Gateio.Types.balance =
    { currency= "eth"; available= "10.5"; locked= "2.3" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.balance native_balance with
   | Ok b ->
     test_assert "Balance normalize: venue"
       (Fluxum.Types.Venue.equal b.venue Gateio);
     test_assert "Balance normalize: currency uppercase"
       (String.equal b.currency "ETH");
     test_assert "Balance normalize: total"
       (Float.(=) b.total 12.8);
     test_assert "Balance normalize: available"
       (Float.(=) b.available 10.5);
     test_assert "Balance normalize: locked"
       (Float.(=) b.locked 2.3)
   | Error e -> fail (sprintf "Balance normalization failed: %s" e));
  (* Trade normalization *)
  let native_trade : Gateio.Types.trade =
    { id= 123456L; create_time= 1699000000L; side= "buy"
    ; amount= "0.5"; price= "50000.00" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.public_trade native_trade ~symbol:"BTC_USDT" with
   | Ok t ->
     test_assert "Trade normalize: venue"
       (Fluxum.Types.Venue.equal t.venue Gateio);
     test_assert "Trade normalize: price"
       (Float.(=) t.price 50000.00);
     test_assert "Trade normalize: qty"
       (Float.(=) t.qty 0.5);
     test_assert "Trade normalize: side is Buy"
       (match t.side with Some Buy -> true | _ -> false);
     test_assert "Trade normalize: trade_id"
       (Option.equal String.equal t.trade_id (Some "123456"));
     test_assert "Trade normalize: has timestamp"
       (Option.is_some t.ts)
   | Error e -> fail (sprintf "Trade normalization failed: %s" e));
  (* Sell trade *)
  let sell_trade : Gateio.Types.trade =
    { id= 789L; create_time= 1699000001L; side= "sell"
    ; amount= "1.0"; price= "50100.00" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.public_trade sell_trade ~symbol:"BTC_USDT" with
   | Ok t ->
     test_assert "Sell trade: side is Sell"
       (match t.side with Some Sell -> true | _ -> false)
   | Error e -> fail (sprintf "Sell trade normalization failed: %s" e));
  (* Order response normalization *)
  let native_order : Gateio.Types.order_response =
    { id= "order-42"; text= ""; currency_pair= "ETH_USDT"
    ; type_= "limit"; account= "spot"; side= "buy"
    ; amount= "5.0"; price= "2000.00"
    ; time_in_force= "gtc"; status= "open" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.order_response native_order with
   | Ok o ->
     test_assert "Order normalize: venue"
       (Fluxum.Types.Venue.equal o.venue Gateio);
     test_assert "Order normalize: id"
       (String.equal o.id "order-42");
     test_assert "Order normalize: symbol"
       (String.equal o.symbol "ETH_USDT");
     test_assert "Order normalize: side is Buy"
       (Fluxum.Types.Side.equal o.side Buy);
     test_assert "Order normalize: qty"
       (Float.(=) o.qty 5.0);
     test_assert "Order normalize: status is New"
       (match o.status with Fluxum.Types.Order_status.New -> true | _ -> false);
     test_assert "Order normalize: kind is Limit"
       (match o.kind with Fluxum.Types.Order_kind.Basic (Limit p) -> Float.(=) p 2000.0 | _ -> false)
   | Error e -> fail (sprintf "Order normalization failed: %s" e));
  (* Closed order -> Filled status *)
  let closed_order : Gateio.Types.order_response =
    { native_order with status= "closed"; side= "sell" }
  in
  (match Gateio.Fluxum_adapter.Adapter.Normalize.order_response closed_order with
   | Ok o ->
     test_assert "Closed order -> Filled"
       (match o.status with Fluxum.Types.Order_status.Filled -> true | _ -> false);
     test_assert "Sell side"
       (Fluxum.Types.Side.equal o.side Sell)
   | Error e -> fail (sprintf "Closed order normalization failed: %s" e));
  (* Error normalization *)
  let open Gateio.Fluxum_adapter.Adapter.Normalize in
  test_assert "HTTP error -> Exchange_specific"
    (match error (`Http (429, "rate limited")) with
     | Fluxum.Types.Error.Exchange_specific {code= "429"; _} -> true
     | _ -> false);
  test_assert "JSON error -> Exchange_specific"
    (match error (`Json_parse "bad") with
     | Fluxum.Types.Error.Exchange_specific {code= "json"; _} -> true
     | _ -> false);
  test_assert "Network error -> Exchange_specific"
    (match error (`Network "timeout") with
     | Fluxum.Types.Error.Exchange_specific {code= "network"; _} -> true
     | _ -> false);
  (* Invalid numeric string *)
  test_assert "Bad float returns Error"
    (match Gateio.Fluxum_adapter.Adapter.Normalize.float_of_string_safe "abc" with
     | Error _ -> true
     | Ok _ -> false)

let () =
  printf "===========================================\n";
  printf "Gate.io Unit Tests\n";
  printf "===========================================\n";
  test_cfg ();
  test_types ();
  test_rest_signature ();
  test_error_types ();
  test_normalization ();
  printf "\n===========================================\n";
  printf "Test Summary\n";
  printf "===========================================\n";
  printf "Total tests:  %d\n" !tests_run;
  printf "Passed:       %d *\n" !tests_passed;
  printf "Failed:       %d X\n" !tests_failed;
  printf "===========================================\n";
  match !tests_failed > 0 with
  | true -> exit 1
  | false -> ()

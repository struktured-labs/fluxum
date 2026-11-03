open Core

let passed = ref 0
let failed = ref 0

let _assert_ok msg result =
  match result with
  | Ok _ ->
    incr passed;
    printf "✓ %s\n" msg
  | Error e ->
    incr failed;
    printf "✗ %s: %s\n" msg e

let assert_true msg b =
  match b with
  | true -> incr passed; printf "✓ %s\n" msg
  | false -> incr failed; printf "✗ %s\n" msg

let assert_equal msg ~expected actual =
  match String.equal expected actual with
  | true -> incr passed; printf "✓ %s\n" msg
  | false -> incr failed; printf "✗ %s: expected %S got %S\n" msg expected actual

let () =
  printf "\n=== Kalshi Prediction Markets Tests ===\n";

  (* Market Status parsing *)
  printf "\n--- Market Status ---\n";
  assert_true "active status"
    (Option.is_some (Kalshi.Prediction_markets.Market_status.of_string "active"));
  assert_true "closed status"
    (Option.is_some (Kalshi.Prediction_markets.Market_status.of_string "closed"));
  assert_true "settled status"
    (Option.is_some (Kalshi.Prediction_markets.Market_status.of_string "settled"));
  assert_true "determined status"
    (Option.is_some (Kalshi.Prediction_markets.Market_status.of_string "determined"));
  assert_true "finalized status"
    (Option.is_some (Kalshi.Prediction_markets.Market_status.of_string "finalized"));
  assert_true "unknown status returns None"
    (Option.is_none (Kalshi.Prediction_markets.Market_status.of_string "bogus"));
  assert_equal "active to_string"
    ~expected:"active"
    (Kalshi.Prediction_markets.Market_status.to_string `Active);

  (* Side parsing *)
  printf "\n--- Side ---\n";
  assert_true "yes side" (Option.is_some (Kalshi.Prediction_markets.Side.of_string "yes"));
  assert_true "no side" (Option.is_some (Kalshi.Prediction_markets.Side.of_string "no"));
  assert_true "invalid side" (Option.is_none (Kalshi.Prediction_markets.Side.of_string "maybe"));
  assert_equal "yes to_string" ~expected:"yes" (Kalshi.Prediction_markets.Side.to_string `Yes);

  (* Action parsing *)
  printf "\n--- Action ---\n";
  assert_true "buy action" (Option.is_some (Kalshi.Prediction_markets.Action.of_string "buy"));
  assert_true "sell action" (Option.is_some (Kalshi.Prediction_markets.Action.of_string "sell"));
  assert_true "invalid action" (Option.is_none (Kalshi.Prediction_markets.Action.of_string "hold"));

  (* Market JSON parsing *)
  printf "\n--- Market JSON ---\n";
  let market_json = Yojson.Safe.from_string {|{
    "ticker": "KXBTC-26MAR14-T67500",
    "event_ticker": "KXBTC-26MAR14",
    "market_type": "binary",
    "title": "Bitcoin above 67500?",
    "subtitle": "BTC > $67,500",
    "status": "active",
    "yes_bid_dollars": "0.45",
    "yes_ask_dollars": "0.47",
    "no_bid_dollars": "0.53",
    "no_ask_dollars": "0.55",
    "last_price_dollars": "0.46",
    "volume_fp": "10000.00",
    "volume_24h_fp": "5000.00",
    "open_interest_fp": "8000.00",
    "result": "",
    "category": "Crypto"
  }|} in
  (match Kalshi.Prediction_markets.Market.of_yojson market_json with
   | Ok m ->
     assert_equal "market ticker" ~expected:"KXBTC-26MAR14-T67500" m.ticker;
     assert_equal "market event_ticker" ~expected:"KXBTC-26MAR14" m.event_ticker;
     assert_equal "market status" ~expected:"active" m.status;
     assert_equal "market yes_bid" ~expected:"0.45" m.yes_bid;
     assert_equal "market yes_ask" ~expected:"0.47" m.yes_ask;
     assert_equal "market last_price" ~expected:"0.46" m.last_price;
     assert_equal "market volume" ~expected:"10000.00" m.volume;
     assert_equal "market category" ~expected:"Crypto" m.category
   | Error e -> incr failed; printf "✗ market parsing failed: %s\n" e);

  (* Market with missing fields *)
  let sparse_market_json = Yojson.Safe.from_string {|{
    "ticker": "SPARSE-TEST",
    "status": "closed"
  }|} in
  (match Kalshi.Prediction_markets.Market.of_yojson sparse_market_json with
   | Ok m ->
     assert_equal "sparse market ticker" ~expected:"SPARSE-TEST" m.ticker;
     assert_equal "sparse market defaults" ~expected:"0" m.yes_bid
   | Error e -> incr failed; printf "✗ sparse market: %s\n" e);

  (* Event JSON parsing *)
  printf "\n--- Event JSON ---\n";
  let event_json = Yojson.Safe.from_string {|{
    "event_ticker": "KXBTC-26MAR14",
    "series_ticker": "KXBTC",
    "title": "Bitcoin Price Range",
    "sub_title": "Where will BTC close?",
    "category": "Crypto",
    "mutually_exclusive": true,
    "markets": [
      {"ticker": "KXBTC-26MAR14-T67500", "status": "active", "title": "Above 67500"}
    ]
  }|} in
  (match Kalshi.Prediction_markets.Event.of_yojson event_json with
   | Ok e ->
     assert_equal "event ticker" ~expected:"KXBTC-26MAR14" e.event_ticker;
     assert_equal "event series" ~expected:"KXBTC" e.series_ticker;
     assert_true "event has markets" (List.length e.markets = 1);
     assert_true "event mutually exclusive" e.mutually_exclusive
   | Error e -> incr failed; printf "✗ event parsing failed: %s\n" e);

  (* Trade JSON parsing *)
  printf "\n--- Trade JSON ---\n";
  let trade_json = Yojson.Safe.from_string {|{
    "trade_id": "abc123",
    "ticker": "KXBTC-26MAR14-T67500",
    "count_fp": "100.00",
    "yes_price_dollars": "0.45",
    "no_price_dollars": "0.55",
    "taker_side": "yes",
    "created_time": "2026-03-09T12:00:00Z"
  }|} in
  (match Kalshi.Prediction_markets.Trade.of_yojson trade_json with
   | Ok t ->
     assert_equal "trade id" ~expected:"abc123" t.trade_id;
     assert_equal "trade ticker" ~expected:"KXBTC-26MAR14-T67500" t.ticker;
     assert_equal "trade count" ~expected:"100.00" t.count;
     assert_equal "trade yes_price" ~expected:"0.45" t.yes_price;
     assert_equal "trade taker_side" ~expected:"yes" t.taker_side
   | Error e -> incr failed; printf "✗ trade parsing failed: %s\n" e);

  (* Orderbook JSON parsing *)
  printf "\n--- Orderbook JSON ---\n";
  let ob_json = Yojson.Safe.from_string {|{
    "orderbook_fp": {
      "yes_dollars": [["0.45", "1000.00"], ["0.44", "500.00"]],
      "no_dollars": [["0.55", "1000.00"], ["0.56", "500.00"]]
    }
  }|} in
  (match Kalshi.Prediction_markets.Orderbook.of_yojson ob_json with
   | Ok ob ->
     assert_true "orderbook has yes levels" (List.length ob.yes = 2);
     assert_true "orderbook has no levels" (List.length ob.no = 2);
     (match List.hd ob.yes with
      | Some (price, qty) ->
        assert_equal "yes level price" ~expected:"0.45" price;
        assert_equal "yes level qty" ~expected:"1000.00" qty
      | None -> incr failed; printf "✗ no yes levels\n")
   | Error e -> incr failed; printf "✗ orderbook parsing failed: %s\n" e);

  (* Order JSON parsing *)
  printf "\n--- Order JSON ---\n";
  let order_json = Yojson.Safe.from_string {|{
    "order_id": "order-xyz",
    "ticker": "KXBTC-26MAR14-T67500",
    "action": "buy",
    "side": "yes",
    "type": "limit",
    "status": "resting",
    "yes_price": 45,
    "no_price": 55,
    "count": 10,
    "remaining_count": 5,
    "filled_count": 5,
    "created_time": "2026-03-09T12:00:00Z",
    "client_order_id": "client-123"
  }|} in
  (match Kalshi.Prediction_markets.Order.of_yojson order_json with
   | Ok o ->
     assert_equal "order id" ~expected:"order-xyz" o.order_id;
     assert_equal "order action" ~expected:"buy" o.action;
     assert_equal "order side" ~expected:"yes" o.side;
     assert_true "order yes_price" (o.yes_price = 45);
     assert_true "order quantity" (o.quantity = 10);
     assert_true "order filled" (o.filled_count = 5);
     assert_true "order remaining" (o.remaining_count = 5)
   | Error e -> incr failed; printf "✗ order parsing failed: %s\n" e);

  (* Position JSON parsing *)
  printf "\n--- Position JSON ---\n";
  let pos_json = Yojson.Safe.from_string {|{
    "ticker": "KXBTC-26MAR14-T67500",
    "market_exposure": 500,
    "position": 10,
    "resting_orders_count": 2,
    "total_traded": 1000,
    "realized_pnl": 50,
    "fees_paid": 5
  }|} in
  (match Kalshi.Prediction_markets.Position.of_yojson pos_json with
   | Ok p ->
     assert_equal "position ticker" ~expected:"KXBTC-26MAR14-T67500" p.ticker;
     assert_true "position" (p.position = 10);
     assert_true "position pnl" (p.realized_pnl = 50)
   | Error e -> incr failed; printf "✗ position parsing failed: %s\n" e);

  (* Balance JSON parsing *)
  printf "\n--- Balance JSON ---\n";
  let bal_json = Yojson.Safe.from_string {|{
    "balance": 50000,
    "portfolio_value": 150000
  }|} in
  (match Kalshi.Prediction_markets.Balance.of_yojson bal_json with
   | Ok b ->
     assert_true "balance" (b.balance = 50000);
     assert_true "portfolio_value" (b.portfolio_value = 150000)
   | Error e -> incr failed; printf "✗ balance parsing failed: %s\n" e);

  (* Normalization tests *)
  printf "\n--- Fluxum Adapter Normalization ---\n";
  let market : Kalshi.Prediction_markets.Market.t =
    { ticker= "KXBTC-T70000"; event_ticker= "KXBTC"; market_type= "binary"
    ; title= "BTC above 70k"; subtitle= ""; status= "active"
    ; yes_bid= "0.65"; yes_ask= "0.67"; no_bid= "0.33"; no_ask= "0.35"
    ; last_price= "0.66"; volume= "25000"; volume_24h= "12000"
    ; open_interest= "8500"; result= ""; close_time= None; open_time= None
    ; category= "Crypto" }
  in
  (match Kalshi.Fluxum_adapter.Adapter.Normalize.prediction_contract_of_market market with
   | Ok c ->
     assert_equal "contract symbol" ~expected:"KXBTC-T70000" c.instrument_symbol;
     assert_true "contract has last_price" (Option.is_some c.last_price);
     assert_true "contract has best_bid" (Option.is_some c.best_bid);
     assert_true "contract has best_ask" (Option.is_some c.best_ask)
   | Error e -> incr failed; printf "✗ contract normalization failed: %s\n" e);

  let event : Kalshi.Prediction_markets.Event.t =
    { event_ticker= "KXBTC"; series_ticker= "KXBTC"; title= "Bitcoin Range"
    ; subtitle= ""; category= "Crypto"; mutually_exclusive= true
    ; markets= [market] }
  in
  (match Kalshi.Fluxum_adapter.Adapter.Normalize.prediction_event_of_event event with
   | Ok e ->
     assert_equal "event venue" ~expected:"Kalshi"
       (Fluxum.Types.Venue.to_string e.venue);
     assert_equal "event id" ~expected:"KXBTC" e.id;
     assert_true "event is_live" e.is_live;
     assert_true "event has contracts" (List.length e.contracts = 1)
   | Error e -> incr failed; printf "✗ event normalization failed: %s\n" e);

  let order : Kalshi.Prediction_markets.Order.t =
    { order_id= "ord-1"; ticker= "KXBTC-T70000"; action= "buy"; side= "yes"
    ; order_type= "limit"; status= "resting"; yes_price= 65; no_price= 35
    ; quantity= 100; remaining_count= 50; filled_count= 50
    ; created_time= None; updated_time= None; client_order_id= "" }
  in
  (match Kalshi.Fluxum_adapter.Adapter.Normalize.prediction_order order with
   | Ok o ->
     assert_equal "order id" ~expected:"ord-1" o.id;
     assert_true "order side is Buy" (Fluxum.Types.Side.equal o.side Buy);
     assert_true "order outcome is Yes"
       (Fluxum.Types.Prediction_outcome.equal o.outcome Yes);
     assert_true "order price is 0.65" (Float.(=) o.price 0.65);
     assert_true "order qty is 100" (Float.(=) o.qty 100.0)
   | Error e -> incr failed; printf "✗ order normalization failed: %s\n" e);

  let position : Kalshi.Prediction_markets.Position.t =
    { ticker= "KXBTC-T70000"; market_exposure= 500; position= 10
    ; resting_orders_count= 0; total_traded= 1000; realized_pnl= 25; fees_paid= 3 }
  in
  (match Kalshi.Fluxum_adapter.Adapter.Normalize.prediction_position position with
   | Ok p ->
     assert_equal "position symbol" ~expected:"KXBTC-T70000" p.symbol;
     assert_true "position outcome is Yes"
       (Fluxum.Types.Prediction_outcome.equal p.outcome Yes);
     assert_true "position qty is 10" (Float.(=) p.qty 10.0)
   | Error e -> incr failed; printf "✗ position normalization failed: %s\n" e);

  (* Negative position = No outcome *)
  let neg_position : Kalshi.Prediction_markets.Position.t =
    { ticker= "KXBTC-T70000"; market_exposure= 500; position= -5
    ; resting_orders_count= 0; total_traded= 1000; realized_pnl= 0; fees_paid= 0 }
  in
  (match Kalshi.Fluxum_adapter.Adapter.Normalize.prediction_position neg_position with
   | Ok p ->
     assert_true "negative position is No"
       (Fluxum.Types.Prediction_outcome.equal p.outcome No);
     assert_true "negative position qty is 5" (Float.(=) p.qty 5.0)
   | Error e -> incr failed; printf "✗ negative position: %s\n" e);

  (* Error normalization *)
  printf "\n--- Error Normalization ---\n";
  let open Kalshi.Fluxum_adapter.Adapter.Normalize in
  assert_true "401 -> Auth_failed"
    (match error `Unauthorized with Fluxum.Types.Error.Auth_failed -> true | _ -> false);
  assert_true "404 -> Exchange_specific"
    (match error `Not_found with
     | Fluxum.Types.Error.Exchange_specific {code= "404"; _} -> true
     | _ -> false);
  assert_true "429 -> rate_limited"
    (match error `Rate_limited with
     | Fluxum.Types.Error.Exchange_specific {code= "429"; _} -> true
     | _ -> false);

  (* Cfg tests *)
  printf "\n--- Configuration ---\n";
  assert_equal "production host"
    ~expected:"external-api.kalshi.com" (Kalshi.Cfg.host ~env:"production");
  assert_equal "demo host"
    ~expected:"external-api.demo.kalshi.co" (Kalshi.Cfg.host ~env:"demo");
  assert_equal "sandbox host"
    ~expected:"external-api.demo.kalshi.co" (Kalshi.Cfg.host ~env:"sandbox");

  (* Place_order request serialization *)
  printf "\n--- Request Serialization ---\n";
  let req : Kalshi.Prediction_markets.Place_order.request =
    { ticker= "KXBTC-T70000"; action= "buy"; side= "yes"; count= 10
    ; order_type= "limit"; yes_price= Some 45; no_price= None
    ; client_order_id= Some "test-123" }
  in
  let json = Kalshi.Prediction_markets.Place_order.request_to_yojson req in
  (match json with
   | `Assoc assoc ->
     assert_true "request has ticker"
       (Option.is_some (List.Assoc.find assoc ~equal:String.equal "ticker"));
     assert_true "request has yes_price"
       (Option.is_some (List.Assoc.find assoc ~equal:String.equal "yes_price"));
     assert_true "request has client_order_id"
       (Option.is_some (List.Assoc.find assoc ~equal:String.equal "client_order_id"));
     assert_true "request has no no_price"
       (Option.is_none (List.Assoc.find assoc ~equal:String.equal "no_price"))
   | _ -> incr failed; printf "✗ request serialization\n");

  (* ---------------- Trade API v2: signing ---------------- *)
  printf "\n--- Signing (RSA-PSS SHA256) ---\n";
  assert_equal "signing message format"
    ~expected:"1700000000000GET/trade-api/v2/portfolio/balance"
    (Kalshi.Auth.signing_message ~timestamp_ms:"1700000000000" ~http_method:"get"
       ~path:"/trade-api/v2/portfolio/balance");
  assert_equal "signing message strips query"
    ~expected:"1700000000000DELETE/trade-api/v2/portfolio/events/orders/abc"
    (Kalshi.Auth.signing_message ~timestamp_ms:"1700000000000" ~http_method:"DELETE"
       ~path:"/trade-api/v2/portfolio/events/orders/abc?market_ticker=X");
  let key = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let pub = Mirage_crypto_pk.Rsa.pub_of_priv key in
  let msg = "1700000000000POST/trade-api/v2/portfolio/events/orders" in
  let sig_b64 = Kalshi.Auth.sign_message ~key msg in
  assert_true "signature is base64 of 256 bytes"
    (match Base64.decode sig_b64 with Ok s -> String.length s = 256 | Error _ -> false);
  assert_true "PSS signature verifies" (Kalshi.Auth.verify_message ~pub ~signature_b64:sig_b64 msg);
  assert_true "PSS signature rejects tampered message"
    (not (Kalshi.Auth.verify_message ~pub ~signature_b64:sig_b64 (msg ^ "x")));
  assert_true "PSS is randomized (salted)"
    (not (String.equal sig_b64 (Kalshi.Auth.sign_message ~key msg)));
  (* PEM round trip through the same loader used for real keys *)
  let pem = X509.Private_key.encode_pem (`RSA key) in
  let key' = Kalshi.Auth.load_private_key_pem pem in
  assert_true "PEM-loaded key signs verifiably"
    (Kalshi.Auth.verify_message ~pub ~signature_b64:(Kalshi.Auth.sign_message ~key:key' msg) msg);
  (* Optional cross-check artifact for an external verifier (python/openssl) *)
  (match Sys.getenv "KALSHI_SIGN_XCHECK_DIR" with
   | Some dir ->
     Out_channel.write_all (dir ^ "/pub.pem")
       ~data:(X509.Public_key.encode_pem (`RSA pub));
     Out_channel.write_all (dir ^ "/msg.txt") ~data:msg;
     Out_channel.write_all (dir ^ "/sig.b64") ~data:sig_b64
   | None -> ());

  (* ---------------- Trade API v2: fixed point ---------------- *)
  printf "\n--- Fixed point ---\n";
  let module T = Kalshi.Trade_v2 in
  assert_true "parse 0.5600" (Option.equal Int.equal (T.Fp.parse_scaled ~decimals:4 "0.5600") (Some 5600));
  assert_true "parse 10.00 count" (Option.equal Int.equal (T.Fp.parse_scaled ~decimals:2 "10.00") (Some 1000));
  assert_equal "price_to_string" ~expected:"0.0300" (T.Fp.price_to_string 0.03);
  assert_equal "count_to_string" ~expected:"25.00" (T.Fp.count_to_string 25.);

  (* ---------------- Fees ---------------- *)
  printf "\n--- Fees ---\n";
  let feq name ~expected got =
    assert_true (sprintf "%s (expected %.4f got %.4f)" name expected got)
      Float.(abs (expected - got) < 1e-9)
  in
  feq "taker 100 @ 0.50 = 1.75" ~expected:1.75 (T.Fees.taker_fee ~count:100. ~price:0.50 ());
  feq "taker 1 @ 0.50 rounds up to 0.02" ~expected:0.02 (T.Fees.taker_fee ~count:1. ~price:0.50 ());
  feq "taker 100 @ 0.10 = 0.63" ~expected:0.63 (T.Fees.taker_fee ~count:100. ~price:0.10 ());
  feq "taker 100 @ 0.01 = 0.07" ~expected:0.07 (T.Fees.taker_fee ~count:100. ~price:0.01 ());
  feq "taker centicent 1 @ 0.50" ~expected:0.0175
    (T.Fees.taker_fee ~rounding:T.Fees.Centicent ~count:1. ~price:0.50 ());
  feq "taker multiplier 0.5" ~expected:0.88 (T.Fees.taker_fee ~multiplier:0.5 ~count:100. ~price:0.50 ());
  feq "maker on plain quadratic = 0" ~expected:0. (T.Fees.maker_fee ~count:100. ~price:0.50 ());
  feq "maker on maker-fee series 100 @ 0.50" ~expected:0.44
    (T.Fees.maker_fee ~fee_type:T.Fees.Quadratic_with_maker_fees ~count:100. ~price:0.50 ());

  (* ---------------- Book / quote ---------------- *)
  printf "\n--- Book / quote ---\n";
  let ob : Kalshi.Prediction_markets.Orderbook.t =
    { yes = [ ("0.4000", "50.00"); ("0.4200", "10.00") ]
    ; no = [ ("0.5500", "20.00"); ("0.5000", "100.00") ] }
  in
  let book = T.Book.of_orderbook ob in
  (match T.Book.best_ask book T.Yes with
   | Some l -> feq "best YES ask = 1 - best NO bid" ~expected:0.45 l.price
   | None -> assert_true "best YES ask exists" false);
  (match T.Book.best_ask book T.No with
   | Some l -> feq "best NO ask = 1 - best YES bid" ~expected:0.58 l.price
   | None -> assert_true "best NO ask exists" false);
  let q = T.Quote.buy ~book ~outcome:T.Yes ~count:30. () in
  feq "quote fills 30" ~expected:30. q.filled;
  feq "quote notional 20*.45+10*.50" ~expected:14.0 q.notional;
  (* raw fee = .07*(20*.45*.55 + 10*.5*.5) = .07*(4.95+2.5) = .5215 -> .53 *)
  feq "quote fee rounds once" ~expected:0.53 q.fee;
  feq "quote total" ~expected:14.53 q.total_cost;
  feq "quote multiple" ~expected:(30. /. 14.53) q.multiple;
  let ql = T.Quote.buy ~book ~outcome:T.Yes ~count:30. ~limit:0.45 () in
  feq "limit stops at 0.45" ~expected:20. ql.filled;

  (* ---------------- V2 order JSON ---------------- *)
  printf "\n--- V2 order request ---\n";
  let r = T.Order.buy ~ticker:"KXTEST-1" ~outcome:T.No ~count:5. ~price:0.30 () in
  let j = Yojson.Safe.to_string (T.Order.to_yojson r) in
  assert_equal "buy NO @0.30 -> ask YES @0.70"
    ~expected:{|{"ticker":"KXTEST-1","side":"ask","count":"5.00","price":"0.7000","time_in_force":"good_till_canceled","self_trade_prevention_type":"taker_at_cross"}|}
    j;
  assert_true "validate ok" (Result.is_ok (T.Order.validate r));
  assert_true "reduce_only w/ GTC rejected"
    (Result.is_error (T.Order.validate { r with reduce_only = true }));
  assert_true "price 1.0 rejected" (Result.is_error (T.Order.validate { r with price = 1.0 }));

  (* ---------------- Env guard / URL parsing ---------------- *)
  printf "\n--- Env guard ---\n";
  assert_true "production refused without flag"
    (Result.is_error (T.Env.authed_cfg T.Env.Production));
  assert_true "production refused with flag but no env confirmation"
    (match Sys.getenv T.Env.allow_production_env_var with
     | Some _ -> true
     | None -> Result.is_error (T.Env.authed_cfg ~allow_production:true T.Env.Production));
  assert_equal "url -> ticker" ~expected:"KXBTCD-26NOV0217"
    (T.ticker_of_input "https://kalshi.com/markets/kxbtcd/bitcoin-price-abovebelow/kxbtcd-26nov0217");
  assert_equal "plain ticker upper" ~expected:"KXFED-26DEC" (T.ticker_of_input " kxfed-26dec ");


  (* ---------------- Rate limiting ---------------- *)
  printf "\n--- Rate limit ---\n";
  let module R = Kalshi.Rate_limit in
  let b = R.Bucket.create ~capacity:100. ~refill_per_s:100. ~now:0. in
  assert_true "bucket: burst of 10x10 ok" (List.for_all (List.init 10 ~f:Fn.id) ~f:(fun _ -> Float.(R.Bucket.try_take b ~now:0. ~cost:10. = 0.)));
  let w = R.Bucket.try_take b ~now:0. ~cost:10. in
  assert_true "bucket: empty -> wait 0.1s" Float.(abs (w -. 0.1) < 1e-9);
  assert_true "bucket: refused take consumes nothing" Float.(b.tokens = 0.);
  assert_true "bucket: refills after 0.1s" Float.(R.Bucket.try_take b ~now:0.1 ~cost:10. = 0.);
  assert_true "bucket: refill capped at capacity" (ignore (R.Bucket.try_take b ~now:100. ~cost:0.); Float.(b.tokens = 100.));
  assert_true "bucket: cost > capacity clamps" Float.(R.Bucket.try_take b ~now:100. ~cost:500. = 0.);
  assert_true "classify GET read" (Poly.equal (R.classify ~meth:`GET ~path:"/markets") (R.Read, 10.));
  assert_true "classify balance" (Poly.equal (R.classify ~meth:`GET ~path:"/portfolio/balance") (R.Balance, 1.));
  assert_true "classify create write" (Poly.equal (R.classify ~meth:`POST ~path:"/portfolio/events/orders") (R.Write, 10.));
  assert_true "classify amend write" (Poly.equal (R.classify ~meth:`POST ~path:"/portfolio/events/orders/x/amend") (R.Write, 10.));
  assert_true "classify cancel 2 tokens" (Poly.equal (R.classify ~meth:`DELETE ~path:"/portfolio/events/orders/x") (R.Write, 2.));
  assert_true "classify transfer default read bucket" (Poly.equal (R.classify ~meth:`POST ~path:"/portfolio/intra_exchange_instance_transfer") (R.Read, 10.));
  assert_true "tier basic read" (Poly.equal (R.Tier.read Basic) (200., 600.));
  assert_true "tier basic write" (Poly.equal (R.Tier.write Basic) (100., 100.));
  assert_true "tier of_string" (Poly.equal (R.Tier.of_string "expert") (Some R.Tier.Expert));
  assert_true "backoff grows" Float.(R.backoff_delay ~attempt:3 ~rand:0.999 () > R.backoff_delay ~attempt:1 ~rand:0.999 ());
  assert_true "backoff capped" Float.(R.backoff_delay ~attempt:30 ~rand:0.999 () <= 8.0);
  assert_true "backoff jitter range" Float.(R.backoff_delay ~attempt:2 ~rand:0.5 () = 0.5);
  assert_true "backoff floor" Float.(R.backoff_delay ~attempt:0 ~rand:0. () >= 0.01);
  let retry_run ~fails ~max_retries =
    let calls = ref 0 and sleeps = ref [] in
    let r = Async.Thread_safe.block_on_async_exn (fun () ->
      R.with_retry ~max_retries ~sleep:(fun d -> sleeps := d :: !sleeps; Async.return ()) ~rand:(fun () -> 0.999)
        ~is_rate_limited:(fun r -> Poly.equal r `Rl)
        (fun () -> incr calls; Async.return (if !calls <= fails then `Rl else `Ok))) in
    (r, !calls, List.rev !sleeps)
  in
  let r, n, sl = retry_run ~fails:2 ~max_retries:5 in
  assert_true "retry: succeeds after 2 429s" (Poly.equal r `Ok && n = 3);
  assert_true "retry: exponential sleeps" (match sl with [ a; b ] -> Float.(b > a) | _ -> false);
  let r, n, _ = retry_run ~fails:10 ~max_retries:3 in
  assert_true "retry: gives up after max" (Poly.equal r `Rl && n = 4);
  let lim = R.Limiter.create R.Tier.Basic in
  let t0 = Time_float_unix.now () in
  Async.Thread_safe.block_on_async_exn (fun () ->
    Async.Deferred.List.iter ~how:`Sequential (List.init 12 ~f:Fn.id) ~f:(fun _ -> R.Limiter.acquire lim R.Write ~cost:10.));
  let el = Time_float.Span.to_sec (Time_float.diff (Time_float_unix.now ()) t0) in
  assert_true (sprintf "limiter throttles write burst (%.3fs >= 0.15s)" el) Float.(el >= 0.15 && el < 2.);

  (* ---------------- Pagination / balance / transfer ---------------- *)
  printf "\n--- Pagination & shards ---\n";
  assert_true "cursor empty -> None" (Option.is_none (T.page_cursor (Yojson.Safe.from_string {|{"cursor":""}|})));
  assert_true "cursor present" (Poly.equal (T.page_cursor (Yojson.Safe.from_string {|{"cursor":"abc"}|})) (Some "abc"));
  let pages = [| {|{"fills":[{"a":1},{"a":2}],"cursor":"c1"}|}; {|{"fills":[{"a":3}],"cursor":"c2"}|}; {|{"fills":[],"cursor":""}|} |] in
  let seen = ref [] in
  let fetch ?cursor () =
    seen := cursor :: !seen;
    let i = match cursor with None -> 0 | Some "c1" -> 1 | _ -> 2 in
    Async.return (Ok (Yojson.Safe.from_string pages.(i))) in
  (match Async.Thread_safe.block_on_async_exn (fun () -> T.paginate ~key:"fills" fetch) with
   | Ok (items, next) ->
     assert_true "paginate: all 3 items in order" (List.length items = 3 && Poly.equal (List.hd items) (Some (`Assoc [("a", `Int 1)])));
     assert_true "paginate: ends with no cursor" (Option.is_none next);
     assert_true "paginate: passes cursors" (Poly.equal (List.rev !seen) [ None; Some "c1"; Some "c2" ])
   | Error _ -> assert_true "paginate ok" false);
  (match Async.Thread_safe.block_on_async_exn (fun () -> T.paginate ~max_pages:1 ~key:"fills" fetch) with
   | Ok (items, next) -> assert_true "paginate: max_pages returns next cursor" (List.length items = 2 && Poly.equal next (Some "c1"))
   | Error _ -> assert_true "paginate max" false);
  assert_true "balance_breakdown" (Poly.equal
    (T.balance_breakdown (Yojson.Safe.from_string {|{"balance":100000,"balance_breakdown":[{"exchange_index":2,"balance":1000},{"exchange_index":0,"balance":99000}]}|}))
    (Ok [ (0, 99000); (2, 1000) ]));
  assert_true "centicents $10" (T.Transfer.centicents_of_dollars 10. = 100_000);
  (match T.Transfer.body ~amount_dollars:10. ~source_shard:0 ~destination_shard:2 () with
   | Ok j -> assert_equal "transfer body" ~expected:{|{"source":"event_contract","destination":"event_contract","amount":100000,"source_exchange_shard":0,"destination_exchange_shard":2}|} (Yojson.Safe.to_string j)
   | Error _ -> assert_true "transfer body" false);
  assert_true "transfer rejects same shard" (Result.is_error (T.Transfer.body ~amount_dollars:1. ~source_shard:2 ~destination_shard:2 ()));
  assert_true "transfer rejects <=0" (Result.is_error (T.Transfer.body ~amount_dollars:0. ~source_shard:0 ~destination_shard:2 ()));
  assert_true "transfer rejects shard>100" (Result.is_error (T.Transfer.body ~amount_dollars:1. ~source_shard:0 ~destination_shard:101 ()));

  (* ---------------- WebSocket book maintenance ---------------- *)
  printf "\n--- WS book ---\n";
  let module W = Kalshi.Ws in
  let books = String.Table.create () and seqs = W.Seq_tracker.create () in
  let feed s = W.handle ~books ~seqs (Yojson.Safe.from_string s) in
  let snap = {|{"type":"orderbook_snapshot","sid":2,"seq":1,"msg":{"market_ticker":"M","yes_dollars_fp":[["0.0800","300.00"],["0.2200","333.00"]],"no_dollars_fp":[["0.5400","20.00"],["0.5600","146.00"]]}}|} in
  let _, a = feed snap in
  assert_true "snapshot continues" (Poly.equal a `Continue);
  let bk = Hashtbl.find_exn books "M" in
  assert_true "snapshot top" (Poly.equal (W.Local_book.top bk) (Some (2200, 333.), Some (4400, 146.)));
  let _, a = feed {|{"type":"orderbook_delta","sid":2,"seq":2,"msg":{"market_ticker":"M","price_dollars":"0.2200","delta_fp":"-333.00","side":"yes"}}|} in
  assert_true "delta removes level" (Poly.equal a `Continue && Poly.equal (fst (W.Local_book.top bk)) (Some (800, 300.)));
  let _ = feed {|{"type":"orderbook_delta","sid":2,"seq":3,"msg":{"market_ticker":"M","price_dollars":"0.5700","delta_fp":"5.00","side":"no"}}|} in
  assert_true "delta adds level" (Poly.equal (snd (W.Local_book.top bk)) (Some (4300, 5.)));
  let _ = feed {|{"type":"orderbook_delta","sid":2,"seq":4,"msg":{"market_ticker":"M","price_dollars":"0.5700","delta_fp":"2.50","side":"no"}}|} in
  assert_true "delta accumulates" (Poly.equal (snd (W.Local_book.top bk)) (Some (4300, 7.5)));
  let evs, a = feed {|{"type":"orderbook_delta","sid":2,"seq":6,"msg":{"market_ticker":"M","price_dollars":"0.5700","delta_fp":"1.00","side":"no"}}|} in
  assert_true "seq gap -> resync" (Poly.equal a `Resync && (match evs with [ W.Gap { expected = 5; got = 6; _ } ] -> true | _ -> false));
  assert_true "gap delta not applied" (Poly.equal (snd (W.Local_book.top bk)) (Some (4300, 7.5)));
  (* after a gap the client reconnects, which resets sequence tracking *)
  W.Seq_tracker.reset seqs;
  let _, a = feed {|{"type":"orderbook_snapshot","sid":2,"seq":1,"msg":{"market_ticker":"M","yes_dollars_fp":[],"no_dollars_fp":[]}}|} in
  assert_true "snapshot after resubscribe replaces book" (Poly.equal a `Continue && Poly.equal (W.Local_book.top bk) (None, None));
  let _, a = feed {|{"type":"ticker","sid":3,"msg":{"market_ticker":"M"}}|} in
  assert_true "unsequenced ticker ok" (Poly.equal a `Continue);
  let other = W.Seq_tracker.create () in
  assert_true "independent sids" (Poly.equal (W.Seq_tracker.check other ~sid:1 ~seq:5 ~is_snapshot:false) `Ok
    && Poly.equal (W.Seq_tracker.check other ~sid:9 ~seq:1 ~is_snapshot:false) `Ok
    && Poly.equal (W.Seq_tracker.check other ~sid:1 ~seq:6 ~is_snapshot:false) `Ok);
  let b2 = W.Local_book.create "M" in
  W.Local_book.apply_snapshot b2 (Yojson.Safe.Util.member "msg" (Yojson.Safe.from_string snap));
  assert_true "diff_with_rest equal" (List.is_empty (W.Local_book.diff_with_rest b2 (Yojson.Safe.from_string
    {|{"orderbook_fp":{"yes_dollars":[["0.0800","300.00"],["0.2200","333.00"]],"no_dollars":[["0.5400","20.00"],["0.5600","146.00"]]}}|})));
  assert_true "diff_with_rest detects mismatch" (List.length (W.Local_book.diff_with_rest b2 (Yojson.Safe.from_string
    {|{"orderbook_fp":{"yes_dollars":[["0.0800","301.00"]],"no_dollars":[["0.5400","20.00"],["0.5600","146.00"]]}}|})) = 2);
  assert_equal "subscribe msg" ~expected:{|{"id":1,"cmd":"subscribe","params":{"channels":["orderbook_delta"],"market_tickers":["M"]}}|}
    (Yojson.Safe.to_string (W.subscribe_msg ~id:1 ~channels:[ "orderbook_delta" ] ~market_tickers:[ "M" ] ()));
  (match feed {|{"type":"fill","sid":13,"msg":{"order_id":"o"}}|} with
   | [ W.Fill _ ], `Continue -> assert_true "fill routed" true | _ -> assert_true "fill routed" false);
  (match feed {|{"type":"user_order","sid":22,"msg":{"order_id":"o"}}|} with
   | [ W.User_order _ ], `Continue -> assert_true "user_order routed" true | _ -> assert_true "user_order routed" false);


  (* ---------------- Copilot round-1 fixes ---------------- *)
  printf "\n--- Strict fixed-point parsing ---\n";
  let ps ?(d = 4) x = T.Fp.parse_scaled ~decimals:d x in
  let none x = Option.is_none (ps x) in
  assert_true "parse rejects trailing garbage" (none "0.5000garbage");
  assert_true "parse rejects empty" (none "");
  assert_true "parse rejects lone -" (none "-");
  assert_true "parse rejects lone ." (none ".");
  assert_true "parse rejects exponent" (none "5e-1");
  assert_true "parse rejects inner space" (none "0. 5");
  assert_true "parse rejects double dot" (none "0.5.1");
  assert_true "parse rejects +" (none "+0.5");
  assert_true "parse rejects nonzero excess digits" (none "0.56001");
  assert_true "parse accepts zero excess digits" (Poly.equal (ps "0.560000") (Some 5600));
  assert_true "parse rejects overflow" (none "99999999999999999999.0");
  assert_true "parse .5" (Poly.equal (ps ".5") (Some 5000));
  assert_true "parse 1." (Poly.equal (ps "1.") (Some 10000));
  assert_true "parse -0.25" (Poly.equal (ps "-0.25") (Some (-2500)));
  printf "\n--- Checked order values ---\n";
  let err r = Result.is_error r in
  let base = T.Order.buy ~ticker:"M" ~outcome:T.Yes ~count:1. ~price:0.5 () in
  assert_true "order ok baseline" (Result.is_ok (T.Order.validate base));
  assert_true "order price 0.00001 (rounds to 0) rejected" (err (T.Order.validate { base with price = 0.00001 }));
  assert_true "order price 0.99999 (rounds to 1) rejected" (err (T.Order.validate { base with price = 0.99999 }));
  assert_true "order price 0.99994 (rounds to 0.9999) ok" (Result.is_ok (T.Order.validate { base with price = 0.99994 }));
  assert_true "order price NaN rejected" (err (T.Order.validate { base with price = Float.nan }));
  assert_true "order count 0.001 (rounds to 0) rejected" (err (T.Order.validate { base with count = 0.001 }));
  assert_true "order count inf rejected" (err (T.Order.validate { base with count = Float.infinity }));
  assert_true "order count nan rejected" (err (T.Order.validate { base with count = Float.nan }));
  assert_true "order count 1e300 rejected" (err (T.Order.validate { base with count = 1e300 }));
  assert_true "order count -1 rejected" (err (T.Order.validate { base with count = -1. }));
  let ab ~price ~count = T.Amend.body ~ticker:"M" ~side:T.Order.Bid ~price ~count in
  (match ab ~price:0.42 ~count:3. with
   | Ok j -> assert_equal "amend body" ~expected:{|{"ticker":"M","side":"bid","price":"0.4200","count":"3.00"}|} (Yojson.Safe.to_string j)
   | Error e -> assert_true ("amend body: " ^ Error.to_string_hum e) false);
  assert_true "amend NaN price rejected" (err (ab ~price:Float.nan ~count:1.));
  assert_true "amend inf price rejected" (err (ab ~price:Float.infinity ~count:1.));
  assert_true "amend price rounds to 1 rejected" (err (ab ~price:0.99999 ~count:1.));
  assert_true "amend inf count rejected" (err (ab ~price:0.5 ~count:Float.infinity));
  assert_true "amend huge count rejected" (err (ab ~price:0.5 ~count:1e300));
  assert_true "amend zero count rejected" (err (ab ~price:0.5 ~count:0.004));
  assert_true "reduce_by 0 rejected" (err (T.Amend.decrease_body (`Reduce_by 0.)));
  assert_true "reduce_by NaN rejected" (err (T.Amend.decrease_body (`Reduce_by Float.nan)));
  assert_true "reduce_by inf rejected" (err (T.Amend.decrease_body (`Reduce_by Float.infinity)));
  assert_true "reduce_to -1 rejected" (err (T.Amend.decrease_body (`Reduce_to (-1.))));
  (match T.Amend.decrease_body (`Reduce_to 0.) with
   | Ok j -> assert_equal "reduce_to 0 allowed" ~expected:{|{"reduce_to":"0.00"}|} (Yojson.Safe.to_string j)
   | Error _ -> assert_true "reduce_to 0 allowed" false);
  (match T.Amend.decrease_body ~market_ticker:"M" (`Reduce_by 2.) with
   | Ok j -> assert_equal "reduce_by body" ~expected:{|{"reduce_by":"2.00","market_ticker":"M"}|} (Yojson.Safe.to_string j)
   | Error _ -> assert_true "reduce_by body" false);
  printf "\n--- Checked transfer amounts ---\n";
  let tb a = T.Transfer.body ~amount_dollars:a ~source_shard:0 ~destination_shard:2 () in
  assert_true "transfer NaN -> Error (no exn)" (err (tb Float.nan));
  assert_true "transfer +inf -> Error" (err (tb Float.infinity));
  assert_true "transfer -inf -> Error" (err (tb Float.neg_infinity));
  assert_true "transfer 1e300 -> Error" (err (tb 1e300));
  assert_true "transfer 0.00001 (rounds to 0) -> Error" (err (tb 0.00001));
  assert_true "transfer -5 -> Error" (err (tb (-5.)));
  (match tb 0.0001 with
   | Ok (`Assoc l) -> assert_true "transfer 0.0001 -> 1 centicent" (Poly.equal (List.Assoc.find l ~equal:String.equal "amount") (Some (`Int 1)))
   | _ -> assert_true "transfer 0.0001" false);

  printf "\n--- WS strict decoding ---\n";
  let books = String.Table.create () and seqs = W.Seq_tracker.create () in
  let feed s = W.handle ~books ~seqs (Yojson.Safe.from_string s) in
  let is_decode_resync = function [ W.Decode_error _ ], `Resync -> true | _ -> false in
  ignore (feed {|{"type":"orderbook_snapshot","sid":1,"seq":1,"msg":{"market_ticker":"A","yes_dollars_fp":[["0.4000","10.00"]],"no_dollars_fp":[["0.5000","5.00"]]}}|});
  let a = Hashtbl.find_exn books "A" in
  let top0 = W.Local_book.top a in
  let bad_delta seq fields =
    sprintf {|{"type":"orderbook_delta","sid":1,"seq":%d,"msg":{"market_ticker":"A"%s}}|} seq fields
  in
  (* each bad message consumes one seq, so keep them in sequence *)
  assert_true "unknown side -> decode error + resync"
    (is_decode_resync (feed (bad_delta 2 {|,"price_dollars":"0.4000","delta_fp":"1.00","side":"unknown"|})));
  assert_true "unknown side does not mutate YES book" (Poly.equal (W.Local_book.top a) top0);
  assert_true "missing side -> decode error"
    (is_decode_resync (feed (bad_delta 3 {|,"price_dollars":"0.4000","delta_fp":"1.00"|})));
  assert_true "missing price_dollars -> decode error (no exn)"
    (is_decode_resync (feed (bad_delta 4 {|,"delta_fp":"1.00","side":"yes"|})));
  assert_true "nonnumeric delta_fp -> decode error (no exn)"
    (is_decode_resync (feed (bad_delta 5 {|,"price_dollars":"0.4000","delta_fp":"abc","side":"yes"|})));
  assert_true "garbage price -> decode error"
    (is_decode_resync (feed (bad_delta 6 {|,"price_dollars":"0.5000garbage","delta_fp":"1","side":"yes"|})));
  assert_true "empty price -> decode error"
    (is_decode_resync (feed (bad_delta 7 {|,"price_dollars":"","delta_fp":"1","side":"yes"|})));
  assert_true "price '-' -> decode error"
    (is_decode_resync (feed (bad_delta 8 {|,"price_dollars":"-","delta_fp":"1","side":"yes"|})));
  assert_true "price > 1 -> decode error"
    (is_decode_resync (feed (bad_delta 9 {|,"price_dollars":"1.5000","delta_fp":"1","side":"yes"|})));
  assert_true "book unchanged after all bad deltas" (Poly.equal (W.Local_book.top a) top0);
  assert_true "bad snapshot level -> decode error, book kept"
    (is_decode_resync (feed {|{"type":"orderbook_snapshot","sid":1,"seq":10,"msg":{"market_ticker":"A","yes_dollars_fp":[["x","1"]],"no_dollars_fp":[]}}|})
     && Poly.equal (W.Local_book.top a) top0);
  assert_true "book msg without market_ticker -> decode error"
    (is_decode_resync (feed {|{"type":"orderbook_delta","sid":1,"seq":11,"msg":{"price_dollars":"0.4","delta_fp":"1","side":"yes"}}|}));
  assert_true "non-object JSON -> decode error" (is_decode_resync (feed {|[1,2]|}));
  assert_true "diff_with_rest bad REST -> reported, no exn"
    (match W.Local_book.diff_with_rest a (Yojson.Safe.from_string {|{"orderbook_fp":{"yes_dollars":[["zz","1"]]}}|}) with
     | [ m ] -> String.is_prefix m ~prefix:"rest decode error" | _ -> false);

  printf "\n--- WS interleaved markets / per-sid sequence ---\n";
  let books = String.Table.create () and seqs = W.Seq_tracker.create () in
  let feed s = W.handle ~books ~seqs (Yojson.Safe.from_string s) in
  let snapm sid seq m = sprintf {|{"type":"orderbook_snapshot","sid":%d,"seq":%d,"msg":{"market_ticker":"%s","yes_dollars_fp":[["0.1000","1.00"]],"no_dollars_fp":[]}}|} sid seq m in
  let deltam sid seq m = sprintf {|{"type":"orderbook_delta","sid":%d,"seq":%d,"msg":{"market_ticker":"%s","price_dollars":"0.1000","delta_fp":"1.00","side":"yes"}}|} sid seq m in
  let ok r = Poly.equal (snd r) `Continue in
  assert_true "interleaved: snap A seq1" (ok (feed (snapm 5 1 "A")));
  assert_true "interleaved: snap B seq2" (ok (feed (snapm 5 2 "B")));
  assert_true "interleaved: delta A seq3" (ok (feed (deltam 5 3 "A")));
  assert_true "interleaved: delta B seq4" (ok (feed (deltam 5 4 "B")));
  (* seq 5 (a delta for A) is lost; a snapshot for B arrives with seq 6 *)
  (match feed (snapm 5 6 "B") with
   | [ W.Gap { sid = 5; expected = 5; got = 6 } ], `Resync -> assert_true "lost A delta then B snapshot -> gap/resync" true
   | _ -> assert_true "lost A delta then B snapshot -> gap/resync" false);
  assert_true "B book not replaced by out-of-sequence snapshot"
    (Poly.equal (fst (W.Local_book.top (Hashtbl.find_exn books "B"))) (Some (1000, 2.)));
  assert_true "A book reflects only in-sequence deltas"
    (Poly.equal (fst (W.Local_book.top (Hashtbl.find_exn books "A"))) (Some (1000, 2.)));
  let s2 = W.Seq_tracker.create () in
  assert_true "first msg on new sid accepted (snapshot)" (Poly.equal (W.Seq_tracker.check s2 ~sid:1 ~seq:40 ~is_snapshot:true) `Ok);
  assert_true "in-sequence snapshot on known sid ok" (Poly.equal (W.Seq_tracker.check s2 ~sid:1 ~seq:41 ~is_snapshot:true) `Ok);
  assert_true "out-of-sequence snapshot on known sid -> gap" (Poly.equal (W.Seq_tracker.check s2 ~sid:1 ~seq:50 ~is_snapshot:true) (`Gap (42, 50)));
  assert_true "other sid unaffected" (Poly.equal (W.Seq_tracker.check s2 ~sid:2 ~seq:1 ~is_snapshot:false) `Ok);

  printf "\n--- WS run loop (fake transport) ---\n";
  let module Fake = struct
    type conn = { id : int; mutable inflight : bool; mutable closed : bool; inbox : string option Async.Ivar.t Queue.t }
    type t = conn
    let conns : conn list ref = ref []
    let violations = ref 0  (* close while a receive is in flight = would be use-after-free *)
    let connect_hook : (unit -> conn Or_error.t Async.Deferred.t) option ref = ref None
    let script : (int -> string option list) ref = ref (fun _ -> [])  (* per-connection messages; then block *)
    let next = ref 0
    let fresh () =
      incr next;
      let c = { id = !next; inflight = false; closed = false; inbox = Queue.create () } in
      List.iter (!script c.id) ~f:(fun m -> Queue.enqueue c.inbox (Async.Ivar.create_full m));
      conns := c :: !conns; c
    let connect ~url:_ ~headers:_ = match !connect_hook with Some f -> f () | None -> Async.return (Ok (fresh ()))
    let send _ _ = Async.return ()
    let receive c =
      let iv = match Queue.dequeue c.inbox with Some iv -> iv | None -> Async.Ivar.create () in
      c.inflight <- true;
      Async.Deferred.map (Async.Ivar.read iv) ~f:(fun m -> c.inflight <- false; m)
    let close c = if c.inflight then incr violations; c.closed <- true; Async.return ()
    (* deliver to the receive currently blocked on [c] *)
    let pending : (int, string option Async.Ivar.t) Hashtbl.t = Hashtbl.create (module Int)
    let reset () = conns := []; violations := 0; connect_hook := None; script := (fun _ -> []); next := 0; Hashtbl.clear pending
  end in
  let module Fake_t = struct
    type t = Fake.t
    let connect = Fake.connect
    let send = Fake.send
    let close = Fake.close
    let receive (c : Fake.conn) =
      if Queue.is_empty c.inbox then begin
        let iv = Async.Ivar.create () in
        Hashtbl.set Fake.pending ~key:c.id ~data:iv;
        Queue.enqueue c.inbox iv
      end;
      Fake.receive c
  end in
  let open Async in
  let pol = { W.base = 0.005; cap = 0.2; healthy_after = 60.; stop_grace = 0.05 } in
  let run_fake ?(policy = pol) ~stop ~on_event () =
    W.run_with (module Fake_t) ~headers:(fun () -> []) ~url:"wss://fake" ~tickers:[ "A" ] ~user:false ~stop ~policy
      ~rand:(fun () -> 1.0) ~on_event ()
  in
  let block f = Thread_safe.block_on_async_exn f in
  (* 1. stop while a receive is in flight: no close under the receive *)
  Fake.reset ();
  let r = block (fun () ->
    let stop = Ivar.create () in
    let finished = run_fake ~stop:(Ivar.read stop) ~on_event:(fun _ -> ()) () in
    let%bind () = Clock.after (Time_float.Span.of_ms 20.) in
    let c = List.hd_exn !Fake.conns in
    let inflight_before = c.inflight in
    Ivar.fill_exn stop ();
    let%bind () = finished in
    let closed_while_inflight = c.closed in
    (* the receive returns later (e.g. curl's 30 s poll timeout) *)
    Ivar.fill_exn (Hashtbl.find_exn Fake.pending c.id) None;
    let%bind () = Clock.after (Time_float.Span.of_ms 10.) in
    return (inflight_before, closed_while_inflight, c.closed, !Fake.violations))
  in
  let inflight_before, closed_early, closed_after, viol = r in
  assert_true "stop: receive was in flight" inflight_before;
  assert_true "stop: run returns within grace without closing under receive" (not closed_early);
  assert_true "stop: transport closed once receive returned" closed_after;
  assert_true "stop: no close-while-receiving (no use-after-free)" (viol = 0);
  (* 2. EOF closes before reconnecting; malformed data resyncs instead of escaping *)
  Fake.reset ();
  Fake.script := (fun id -> match id with
    | 1 -> [ None ]
    | 2 -> [ Some {|{"type":"orderbook_delta","sid":1,"seq":1,"msg":{"market_ticker":"A","price_dollars":"0.4","delta_fp":"x","side":"yes"}}|} ]
    | 3 -> [ Some "not json" ]
    | _ -> []);
  let evs = ref [] in
  let conns_closed = block (fun () ->
    let stop = Ivar.create () in
    let finished = run_fake ~stop:(Ivar.read stop) ~on_event:(fun e -> evs := e :: !evs) () in
    let%bind () = Clock.after (Time_float.Span.of_ms 300.) in
    Ivar.fill_exn stop ();
    let%bind () = finished in
    (match Hashtbl.find Fake.pending 4 with Some iv -> Ivar.fill_if_empty iv None | None -> ());
    let%bind () = Clock.after (Time_float.Span.of_ms 10.) in
    return (List.filter !Fake.conns ~f:(fun c -> c.id <= 3) |> List.for_all ~f:(fun c -> c.closed)))
  in
  assert_true "EOF / decode errors: reached 4th connection" (List.length !Fake.conns >= 4);
  assert_true "EOF / decode errors: every lost connection closed" conns_closed;
  assert_true "decode error surfaced as event (not exception)"
    (List.count !evs ~f:(function W.Decode_error _ -> true | _ -> false) = 2);
  assert_true "no close-while-receiving across reconnects" (!Fake.violations = 0);
  (* 3. short-lived established connections back off exponentially *)
  Fake.reset ();
  Fake.script := (fun _ -> [ None ]);
  let times = ref [] in
  block (fun () ->
    let stop = Ivar.create () in
    let finished = run_fake ~stop:(Ivar.read stop)
        ~on_event:(function W.Connected _ -> times := Time_float.now () :: !times | _ -> ()) () in
    let%bind () = Clock.after (Time_float.Span.of_ms 400.) in
    Ivar.fill_exn stop ();
    finished);
  let ts = List.rev !times in
  let gaps = List.map2_exn (List.drop_last_exn ts) (List.tl_exn ts) ~f:(fun a b -> Time_float.Span.to_sec (Time_float.diff b a)) in
  assert_true (sprintf "short-lived conns: bounded reconnects (%d in 400ms)" (List.length ts)) (List.length ts >= 4 && List.length ts <= 9);
  assert_true "short-lived conns: delay grows" (match gaps with g0 :: _ -> Float.(List.last_exn gaps > 3. *. g0) | [] -> false);
  (* 4. stop while the handshake is pending; late connection gets closed *)
  Fake.reset ();
  let hs = Ivar.create () in
  Fake.connect_hook := Some (fun () -> Ivar.read hs);
  let late = block (fun () ->
    let stop = Ivar.create () in
    let finished = run_fake ~stop:(Ivar.read stop) ~on_event:(fun _ -> ()) () in
    let%bind () = Clock.after (Time_float.Span.of_ms 10.) in
    Ivar.fill_exn stop ();
    let%bind () = Clock.with_timeout (Time_float.Span.of_ms 200.) finished >>| function `Result () -> () | `Timeout -> failwith "stop ignored during handshake" in
    Fake.connect_hook := None;
    let c = Fake.fresh () in
    Ivar.fill_exn hs (Ok c);
    let%bind () = Clock.after (Time_float.Span.of_ms 10.) in
    return c)
  in
  assert_true "stop during pending handshake returns promptly" true;
  assert_true "late handshake closed, never subscribed/read" (late.closed && not late.inflight);
  (* ---- second-round review fixes ---- *)
  Core.printf "\n--- Round-2 fixes ---\n";
  assert_true "parse_scaled overflow 922337203685477.5809 -> None"
    (Option.is_none (T.Fp.parse_scaled ~decimals:4 "922337203685477.5809"));
  assert_true "parse_scaled 999999999999999.9999 -> None"
    (Option.is_none (T.Fp.parse_scaled ~decimals:4 "999999999999999.9999"));
  assert_true "parse_scaled 461168601842738.7903 (= max_int/1) -> Some max_int"
    (Option.equal Int.equal (T.Fp.parse_scaled ~decimals:4 "461168601842738.7903") (Some Int.max_value));
  assert_true "parse_scaled 461168601842738.7904 (max_int+1) -> None"
    (Option.is_none (T.Fp.parse_scaled ~decimals:4 "461168601842738.7904"));
  assert_true "parse_scaled 0.5600 still Some 5600"
    (Option.equal Int.equal (T.Fp.parse_scaled ~decimals:4 "0.5600") (Some 5600));
  let ob s = T.Book.of_json (Yojson.Safe.from_string s) in
  (match ob {|{"orderbook_fp":{"yes_dollars":[["0.4000","10.00"],["0.4200","1.00"]],"no_dollars":[["0.5500","3.00"]]}}|} with
   | Ok b -> assert_true "REST book ok: sorted, best yes bid 0.42"
               (match T.Book.best_bid b T.Yes with Some l -> Float.equal l.price 0.42 | None -> false)
   | Error _ -> assert_true "REST book ok" false);
  assert_true "REST book: null/absent side -> empty"
    (match ob {|{"orderbook_fp":{"yes_dollars":null}}|} with Ok b -> List.is_empty b.yes_bids && List.is_empty b.no_bids | Error _ -> false);
  List.iter
    [ "NO bid 1.5000", {|{"orderbook_fp":{"no_dollars":[["1.5000","1.00"]]}}|}
    ; "price 0", {|{"orderbook_fp":{"yes_dollars":[["0.0000","1.00"]]}}|}
    ; "garbage price", {|{"orderbook_fp":{"yes_dollars":[["0.4x","1.00"]]}}|}
    ; "zero size", {|{"orderbook_fp":{"yes_dollars":[["0.40","0.00"]]}}|}
    ; "negative size", {|{"orderbook_fp":{"yes_dollars":[["0.40","-1.00"]]}}|}
    ; "numeric (non-string) level", {|{"orderbook_fp":{"yes_dollars":[[0.4,1]]}}|}
    ; "short level", {|{"orderbook_fp":{"yes_dollars":[["0.40"]]}}|}
    ; "side not a list", {|{"orderbook_fp":{"yes_dollars":"x"}}|}
    ; "missing orderbook_fp", {|{"orderbook":{}}|}
    ; "not an object", {|[1]|} ]
    ~f:(fun (name, s) -> assert_true ("REST book rejects " ^ name) (Result.is_error (ob s)));
  let pg s = T.page_of_json ~key:"fills" (Yojson.Safe.from_string s) in
  assert_true "page ok with cursor"
    (match pg {|{"fills":[1,2],"cursor":"abc"}|} with Ok (l, Some "abc") -> List.length l = 2 | _ -> false);
  assert_true "page ok, empty cursor = last"
    (match pg {|{"fills":[],"cursor":""}|} with Ok ([], None) -> true | _ -> false);
  assert_true "page null collection = empty"
    (match pg {|{"fills":null}|} with Ok ([], None) -> true | _ -> false);
  List.iter
    [ "wrongly typed collection", {|{"fills":"bad","cursor":""}|}
    ; "missing collection", {|{"cursor":""}|}
    ; "non-string cursor", {|{"fills":[],"cursor":5}|}
    ; "non-object page", {|[]|}
    ; "null page", {|null|} ]
    ~f:(fun (name, s) -> assert_true ("page rejects " ^ name) (Result.is_error (pg s)));
  let resp s = T.Order.response_of_yojson (Yojson.Safe.from_string s) in
  assert_true "create_order response: null body -> Error (no exn)" (Result.is_error (resp "null"));
  assert_true "create_order response: list -> Error" (Result.is_error (resp "[]"));
  assert_true "create_order response: missing order_id -> Error" (Result.is_error (resp {|{"x":1}|}));
  assert_true "create_order response: ok" (match resp {|{"order_id":"o1","fill_count":"0.00"}|} with Ok r -> String.equal r.order_id "o1" | Error _ -> false);
  let books = String.Table.create () and seqs = W.Seq_tracker.create () in
  let feed s = W.handle ~books ~seqs (Yojson.Safe.from_string s) in
  let is_resync_decode (evs, act) =
    (match act with `Resync -> true | `Continue -> false)
    && List.exists evs ~f:(function W.Decode_error _ -> true | _ -> false) in
  let snap = {|"msg":{"market_ticker":"A","yes_dollars_fp":[["0.4000","5.00"]],"no_dollars_fp":[]}|} in
  assert_true "snapshot without seq -> Decode_error + resync"
    (is_resync_decode (feed ({|{"type":"orderbook_snapshot","sid":1,|} ^ snap ^ "}")));
  assert_true "snapshot with string seq -> Decode_error + resync"
    (is_resync_decode (feed ({|{"type":"orderbook_snapshot","sid":1,"seq":"1",|} ^ snap ^ "}")));
  assert_true "delta without sid -> Decode_error + resync"
    (is_resync_decode (feed {|{"type":"orderbook_delta","seq":2,"msg":{"market_ticker":"A","price_dollars":"0.4000","delta_fp":"1.00","side":"yes"}}|}));
  assert_true "no book created by rejected messages" (Hashtbl.is_empty books);
  assert_true "delta before snapshot -> Decode_error + resync"
    (is_resync_decode (feed {|{"type":"orderbook_delta","sid":1,"seq":1,"msg":{"market_ticker":"A","price_dollars":"0.4000","delta_fp":"1.00","side":"yes"}}|}));
  assert_true "delta before snapshot leaves no book" (Hashtbl.is_empty books);
  W.Seq_tracker.reset seqs;
  let _, act = feed ({|{"type":"orderbook_snapshot","sid":1,"seq":1,|} ^ snap ^ "}") in
  assert_true "snapshot then registers book" ((match act with `Continue -> true | `Resync -> false) && Hashtbl.mem books "A");
  let _, act = feed {|{"type":"orderbook_delta","sid":1,"seq":2,"msg":{"market_ticker":"A","price_dollars":"0.4000","delta_fp":"1.00","side":"yes"}}|} in
  assert_true "delta after snapshot accepted" (match act with `Continue -> true | `Resync -> false);
  assert_true "ticker without seq still fine"
    (match snd (feed {|{"type":"ticker","sid":2,"msg":{"market_ticker":"A"}}|}) with `Continue -> true | `Resync -> false);
  (* ---- third-round review fixes ---- *)
  Core.printf "\n--- Round-3 fixes ---\n";
  assert_true "REST book rejects duplicate NO level"
    (Result.is_error (T.Book.of_json (Yojson.Safe.from_string {|{"orderbook_fp":{"no_dollars":[["0.5500","1.00"],["0.5500","1.00"]]}}|})));
  assert_true "REST book rejects duplicate after normalisation (0.55 vs 0.5500)"
    (Result.is_error (T.Book.of_json (Yojson.Safe.from_string {|{"orderbook_fp":{"yes_dollars":[["0.55","1.00"],["0.5500","2.00"]]}}|})));
  assert_true "REST book same price on opposite sides ok"
    (Result.is_ok (T.Book.of_json (Yojson.Safe.from_string {|{"orderbook_fp":{"yes_dollars":[["0.4000","1.00"]],"no_dollars":[["0.4000","1.00"]]}}|})));
  let sf s = T.series_fee_params_of_json (Yojson.Safe.from_string s) in
  assert_true "series ok" (match sf {|{"series":{"fee_type":"quadratic","fee_multiplier":1}}|} with Ok (_, m) -> Float.equal m 1. | Error _ -> false);
  assert_true "series string multiplier ok" (match sf {|{"series":{"fee_multiplier":"0.5"}}|} with Ok (_, m) -> Float.equal m 0.5 | Error _ -> false);
  assert_true "series defaults when fields absent" (match sf {|{"series":{}}|} with Ok (_, m) -> Float.equal m 1. | Error _ -> false);
  List.iter [ "{}", {|{}|}; "null", {|null|}; "non-object series", {|{"series":[]}|}
            ; "bad multiplier string", {|{"series":{"fee_multiplier":"abc"}}|}
            ; "negative multiplier", {|{"series":{"fee_multiplier":-1}}|}
            ; "non-string fee_type", {|{"series":{"fee_type":5}}|} ]
    ~f:(fun (n, s) -> assert_true ("series rejects " ^ n ^ " (no exn)") (Result.is_error (sf s)));
  let bb s = T.balance_breakdown (Yojson.Safe.from_string s) in
  assert_true "balance: string ints ok" (Poly.equal (bb {|{"balance_breakdown":[{"exchange_index":"1","balance":"250"}]}|}) (Ok [ (1, 250) ]));
  assert_true "balance: absent breakdown -> []" (Poly.equal (bb {|{"balance":5}|}) (Ok []));
  List.iter [ "invalid numeric string", {|{"balance_breakdown":[{"exchange_index":0,"balance":"12x"}]}|}
            ; "null entry", {|{"balance_breakdown":[null]}|}
            ; "float 1e300", {|{"balance_breakdown":[{"exchange_index":0,"balance":1e300}]}|}
            ; "huge int string", {|{"balance_breakdown":[{"exchange_index":0,"balance":"99999999999999999999999"}]}|}
            ; "missing balance", {|{"balance_breakdown":[{"exchange_index":0}]}|}
            ; "negative index", {|{"balance_breakdown":[{"exchange_index":-1,"balance":1}]}|}
            ; "duplicate index", {|{"balance_breakdown":[{"exchange_index":0,"balance":1},{"exchange_index":0,"balance":2}]}|}
            ; "breakdown not a list", {|{"balance_breakdown":{}}|}
            ; "non-object response", {|null|} ]
    ~f:(fun (n, s) -> assert_true ("balance rejects " ^ n ^ " (no exn)") (Result.is_error (bb s)));
  let books = String.Table.create () and seqs = W.Seq_tracker.create () in
  let feed s = W.handle ~books ~seqs (Yojson.Safe.from_string s) in
  ignore (feed {|{"type":"orderbook_snapshot","sid":1,"seq":1,"msg":{"market_ticker":"A","yes_dollars_fp":[["0.4000","5.00"]],"no_dollars_fp":[]}}|});
  let before = W.Local_book.to_levels (Hashtbl.find_exn books "A").yes in
  let evs, act = feed {|{"type":"orderbook_snapshot","sid":1,"seq":2,"msg":{"market_ticker":"A","yes_dollars_fp":[["0.3000","2.00"],["0.4000","-1.00"]],"no_dollars_fp":[]}}|} in
  assert_true "snapshot with negative size -> Decode_error + resync"
    ((match act with `Resync -> true | `Continue -> false) && List.exists evs ~f:(function W.Decode_error _ -> true | _ -> false));
  assert_true "negative snapshot leaves book unchanged"
    (Poly.equal before (W.Local_book.to_levels (Hashtbl.find_exn books "A").yes));
  let b = Hashtbl.find_exn books "A" in
  List.iter [ "null", "null"; "{}", "{}"; "orderbook_fp list", {|{"orderbook_fp":[]}|}; "list", "[1]"
            ; "level garbage", {|{"orderbook_fp":{"yes_dollars":[["x","1"]]}}|} ]
    ~f:(fun (n, s) ->
      let r = try `Ok (W.Local_book.diff_with_rest b (Yojson.Safe.from_string s)) with e -> `Exn e in
      assert_true ("diff_with_rest " ^ n ^ " -> decode discrepancy, no exn")
        (match r with `Ok [ m ] -> String.is_prefix m ~prefix:"rest decode error" | _ -> false));
  assert_true "diff_with_rest valid equal book -> []"
    (List.is_empty (W.Local_book.diff_with_rest b (Yojson.Safe.from_string {|{"orderbook_fp":{"yes_dollars":[["0.4000","5.00"]]}}|})));
  Core.printf "\n=== Results: %d passed, %d failed (%d total) ===\n"
    !passed !failed (!passed + !failed);
  Core.Out_channel.flush Core.stdout;
  Stdlib.exit (if !failed > 0 then 1 else 0)

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
    ~expected:"api.kalshi.com" (Kalshi.Cfg.host ~env:"production");
  assert_equal "demo host"
    ~expected:"demo-api.kalshi.co" (Kalshi.Cfg.host ~env:"demo");
  assert_equal "sandbox host"
    ~expected:"demo-api.kalshi.co" (Kalshi.Cfg.host ~env:"sandbox");

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

  printf "\n=== Results: %d passed, %d failed (%d total) ===\n"
    !passed !failed (!passed + !failed)

open Core
open Fluxum

let failf fmt = Printf.ksprintf failwith fmt

let check name condition =
  if not condition then failf "market-making test failed: %s" name

let approx ?(epsilon = 1e-9) a b = Float.(abs (a -. b) <= epsilon)

let ok_exn = function
  | Ok value -> value
  | Error error ->
    failf "unexpected market-making error: %s" (Sexp.to_string_hum (Market_making.sexp_of_error error))

let book =
  Market_making.Top_of_book.create
    ~bid_price:99.
    ~bid_qty:3.
    ~ask_price:101.
    ~ask_qty:1.

let balanced_config =
  Market_making.Config.
    { default with
      tick_size= 0.5
    ; lot_size= 0.1
    ; base_order_qty= 1.
    ; levels= 2
    ; base_half_spread_bps= 50.
    ; level_spacing_bps= 50.
    ; inventory_skew_bps= 100.
    ; max_position= 10.
    ; fair_value= `Midpoint }

let () =
  check "midpoint" (approx (Market_making.Top_of_book.midpoint book) 100.);
  check "microprice weights the bid queue"
    (approx (Market_making.Top_of_book.microprice book) 100.5);
  let large_book =
    Market_making.Top_of_book.create
      ~bid_price:1e300 ~bid_qty:1e308 ~ask_price:1.1e300 ~ask_qty:1e308
  in
  check "microprice avoids finite-input overflow"
    (Float.is_finite (Market_making.Top_of_book.microprice large_book));

  let generated =
    Market_making.generate ~config:balanced_config ~book ~inventory:0. () |> ok_exn
  in
  check "two bid levels" (List.length generated.bids = 2);
  check "two ask levels" (List.length generated.asks = 2);
  let bid0 = List.hd_exn generated.bids in
  let bid1 = List.nth_exn generated.bids 1 in
  let ask0 = List.hd_exn generated.asks in
  check "best bid rounds down" (approx bid0.price 99.5);
  check "second bid is farther away" (approx bid1.price 99.0);
  check "best ask rounds up" (approx ask0.price 100.5);
  check "quotes remain post-only"
    Float.(bid0.price < book.ask_price && ask0.price > book.bid_price);

  let long_inventory =
    Market_making.generate ~config:balanced_config ~book ~inventory:5. () |> ok_exn
  in
  check "long inventory lowers reservation price"
    Float.(long_inventory.reservation_price < generated.reservation_price);
  check "long inventory reduces bid size"
    Float.((List.hd_exn long_inventory.bids).qty < (List.hd_exn generated.bids).qty);
  check "long inventory increases ask size"
    Float.((List.hd_exn long_inventory.asks).qty > (List.hd_exn generated.asks).qty);

  let capacity_config =
    Market_making.Config.
      { balanced_config with base_order_qty= 1.; levels= 3; max_position= 1. }
  in
  let capacity_quotes =
    Market_making.generate ~config:capacity_config ~book ~inventory:0.9 () |> ok_exn
  in
  let total_qty quotes =
    List.sum (module Float) quotes ~f:(fun (q : Market_making.Quote.t) -> q.qty)
  in
  check "aggregate bids respect the long limit"
    Float.(total_qty capacity_quotes.bids <= 0.1 +. 1e-9);
  check "aggregate asks respect the short limit"
    Float.(total_qty capacity_quotes.asks <= 1.9 +. 1e-9);

  let fee_config =
    Market_making.Config.
      {balanced_config with base_half_spread_bps= 1.; maker_fee_bps= 3.; target_edge_bps= 4.}
  in
  let fee_quotes = Market_making.generate ~config:fee_config ~book ~inventory:0. () |> ok_exn in
  check "fees and target edge floor the spread" (approx fee_quotes.half_spread_bps 7.);

  let bounded_config =
    Market_making.Config.
      { balanced_config with
        price_bounds= Some (0.01, 1.)
      ; min_notional= 10. }
  in
  let bounded_book =
    Market_making.Top_of_book.create
      ~bid_price:0.45 ~bid_qty:10. ~ask_price:0.55 ~ask_qty:10.
  in
  let bounded =
    Market_making.generate ~config:bounded_config ~book:bounded_book ~inventory:0. () |> ok_exn
  in
  check "min notional filters undersized quotes" (List.is_empty (Market_making.quotes bounded));

  let desired : Market_making.Quote.t list =
    [ {side= Buy; level= 0; price= 100.; qty= 1.}
    ; {side= Buy; level= 1; price= 99.; qty= 1.}
    ; {side= Sell; level= 0; price= 101.; qty= 1.}
    ]
  in
  let existing : Market_making.Working_order.t list =
    [ {order_id= "keep"; side= Buy; level= 0; price= 99.5; qty= 1.}
    ; {order_id= "duplicate"; side= Buy; level= 0; price= 99.; qty= 1.}
    ; {order_id= "replace"; side= Sell; level= 0; price= 102.; qty= 1.}
    ; {order_id= "orphan"; side= Sell; level= 9; price= 110.; qty= 1.}
    ]
  in
  let plan =
    Market_making.reconcile
      ~tick_size:0.5
      ~price_tolerance_ticks:1
      ~existing
      ~desired
      ()
  in
  check "reconcile keeps a quote within tolerance" (List.length plan.keep = 1);
  check "reconcile cancels duplicate, replacement, and orphan" (List.length plan.cancel = 3);
  check "reconcile places missing and replacement quotes" (List.length plan.place = 2);

  let tolerance_plan =
    Market_making.reconcile
      ~tick_size:0.5
      ~price_tolerance_ticks:1
      ~qty_tolerance_ratio:0.1
      ~existing:
        [ {order_id= "wrong-qty"; side= Buy; level= 0; price= 100.; qty= 2.}
        ; {order_id= "keep-tolerant"; side= Buy; level= 0; price= 99.5; qty= 1.} ]
      ~desired:[{side= Buy; level= 0; price= 100.; qty= 1.}]
      ()
  in
  check "reconcile prefers any keepable duplicate"
    (match tolerance_plan.keep with
     | [working, _] -> String.equal working.order_id "keep-tolerant"
     | _ -> false);
  check "reconcile avoids churn when a duplicate is keepable"
    (List.length tolerance_plan.cancel = 1 && List.is_empty tolerance_plan.place);

  let overflow_config =
    Market_making.Config.
      { balanced_config with
        lot_size= 1e-308
      ; base_order_qty= 1e308
      ; max_position= 1e308 }
  in
  let overflow_quotes =
    Market_making.generate ~config:overflow_config ~book ~inventory:0. () |> ok_exn
  in
  check "non-finite rounded quantities are filtered"
    (List.is_empty (Market_making.quotes overflow_quotes));

  (match Market_making.Config.validate Market_making.Config.{default with tick_size= 0.} with
   | Error (Invalid_config _) -> ()
   | _ -> failwith "invalid tick size should be rejected");
  printf "market-making tests passed\n"

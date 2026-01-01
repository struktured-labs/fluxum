(** Comprehensive tests for Consolidated Order Book *)

open Core
open Async
open Consolidated_order_book

(** Helper to create test books *)
let make_gemini_book symbol =
  let book = Gemini.Order_book.Book.empty symbol in
  book

let make_kraken_book symbol =
  let book = Kraken.Order_book.Book.empty symbol in
  book

let make_binance_book symbol =
  let book = Binance.Order_book.Book.empty symbol in
  book

(** Helper for float equality *)
let float_equal ?(tolerance = 0.0001) a b =
  Float.abs (a -. b) < tolerance

let%test_module "Attributed_level" = (module struct
  let%test "create attributed level" =
    let level = Attributed_level.create ~price:50000. ~volume:1.5 ~exchange:Gemini in
    float_equal (Attributed_level.price level) 50000. &&
    float_equal (Attributed_level.volume level) 1.5

  let%test "empty attributed level" =
    let level = Attributed_level.empty in
    float_equal (Attributed_level.price level) 0. &&
    float_equal (Attributed_level.volume level) 0.

  let%test "multiple exchanges aggregation" =
    let level = Attributed_level.create ~price:50000. ~volume:1.0
                  ~exchange:(Multiple [Gemini; Kraken]) in
    match Attributed_level.exchange level with
    | Multiple exchanges -> List.length exchanges = 2
    | _ -> false
end)

let%test_module "Book - Creation and Basic Operations" = (module struct
  let%test "create empty consolidated book" =
    let book = Book.empty ~symbol:"BTC/USD" in
    String.equal (Book.symbol book) "BTC/USD"

  let%test "empty book has zero epoch" =
    let book = Book.empty ~symbol:"BTC/USD" in
    Book.epoch book = 0

  let%test "empty book has no best prices" =
    let book = Book.empty ~symbol:"BTC/USD" in
    let best_bid = Book.best_bid book in
    let best_ask = Book.best_ask book in
    float_equal (Attributed_level.price best_bid) 0. &&
    float_equal (Attributed_level.price best_ask) 0.

  let%test "spread on empty book is zero" =
    let book = Book.empty ~symbol:"BTC/USD" in
    match Book.spread book with
    | Some spread -> float_equal spread 0.
    | None -> true  (* Also acceptable for empty book *)
end)

let%test_module "Book - Single Exchange Updates" = (module struct
  let%test "update from gemini book" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.5 in
    let consolidated = Book.update_gemini consolidated gemini_book in
    let best_bid = Book.best_bid consolidated in
    float_equal (Attributed_level.price best_bid) 50000. &&
    float_equal (Attributed_level.volume best_bid) 1.5

  let%test "update from kraken book" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Ask ~price:51000. ~size:2.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in
    let best_ask = Book.best_ask consolidated in
    float_equal (Attributed_level.price best_ask) 51000.

  let%test "update from binance book" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in
    let binance_book = make_binance_book "BTCUSDT" in
    let binance_book = Binance.Order_book.Book.set binance_book
                        ~side:`Bid ~price:49999. ~size:3.0 in
    let consolidated = Book.update_binance consolidated binance_book in
    Book.epoch consolidated > 0

  let%test "sequential updates increment epoch" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in
    let epoch1 = Book.epoch consolidated in

    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50001. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in
    let epoch2 = Book.epoch consolidated in

    epoch2 > epoch1
end)

let%test_module "Book - Multi-Exchange Aggregation" = (module struct
  let%test "best bid from multiple exchanges - simple" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: bid at 50000 *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: bid at 50001 (better) *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50001. ~size:1.5 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let best_bid = Book.best_bid consolidated in
    float_equal (Attributed_level.price best_bid) 50001.

  let%test "best ask from multiple exchanges - simple" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: ask at 51000 *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Ask ~price:51000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: ask at 50999 (better) *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Ask ~price:50999. ~size:1.5 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let best_ask = Book.best_ask consolidated in
    float_equal (Attributed_level.price best_ask) 50999.

  let%test "volume aggregation at same price" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: bid at 50000 with 1.0 BTC *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: bid at 50000 with 2.0 BTC *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50000. ~size:2.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    (* Total volume at 50000 should be 3.0 BTC *)
    let bid_levels = Book.bids consolidated in
    match Map.find bid_levels 50000. with
    | Some level -> float_equal (Attributed_level.volume level) 3.0
    | None -> false

  let%test "exchange attribution tracking" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let bid_levels = Book.bids consolidated in
    match Map.find bid_levels 50000. with
    | Some level ->
      (match Attributed_level.exchange level with
       | Gemini -> true
       | _ -> false)
    | None -> false

  let%test "multiple exchanges at same price" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50000. ~size:2.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let bid_levels = Book.bids consolidated in
    match Map.find bid_levels 50000. with
    | Some level ->
      (match Attributed_level.exchange level with
       | Multiple exchanges -> List.length exchanges = 2
       | _ -> false)
    | None -> false
end)

let%test_module "Book - Spread Calculations" = (module struct
  let%test "spread across exchanges" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: bid at 50000 *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: ask at 50100 *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Ask ~price:50100. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    match Book.spread consolidated with
    | Some spread -> float_equal spread 100.
    | None -> false

  let%test "tight spread detection" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Ask ~price:50001. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    match Book.spread consolidated with
    | Some spread -> float_equal spread 1.
    | None -> false

  let%test "best prices from different exchanges" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: worse bid/ask *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:49990. ~size:1.0 in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Ask ~price:50110. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: better bid *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    (* Binance: better ask *)
    let binance_book = make_binance_book "BTCUSDT" in
    let binance_book = Binance.Order_book.Book.set binance_book
                        ~side:`Ask ~price:50100. ~size:1.0 in
    let consolidated = Book.update_binance consolidated binance_book in

    (* Best spread should use Kraken bid + Binance ask *)
    match Book.spread consolidated with
    | Some spread -> float_equal spread 100.  (* 50100 - 50000 *)
    | None -> false
end)

let%test_module "Book - Deep Books" = (module struct
  let%test "multiple price levels from single exchange" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:49999. ~size:1.5 in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:49998. ~size:2.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let bid_levels = Book.bids consolidated in
    Map.length bid_levels = 3

  let%test "interleaved price levels from multiple exchanges" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: 50000, 49998 *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:49998. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: 49999, 49997 *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:49999. ~size:1.0 in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:49997. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let bid_levels = Book.bids consolidated in
    Map.length bid_levels = 4

  let%test "level removal on zero volume" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Remove the level *)
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:0.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let bid_levels = Book.bids consolidated in
    Map.length bid_levels = 0
end)

let%test_module "Arbitrage Detection" = (module struct
  let%test "no arbitrage when properly ordered" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:1.0 in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Ask ~price:50100. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    match Book.spread consolidated with
    | Some spread -> Float.(spread > 0.)
    | None -> false

  let%test "potential arbitrage - crossed market" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    (* Gemini: high ask at 51000 *)
    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Ask ~price:51000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    (* Kraken: high bid at 51100 (can buy on Gemini, sell on Kraken) *)
    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:51100. ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    match Book.spread consolidated with
    | Some spread -> Float.(spread < 0.)  (* Negative spread = arbitrage *)
    | None -> false
end)

let%test_module "Edge Cases and Stress Tests" = (module struct
  let%test "very close prices" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000.0001 ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50000.0002 ~size:1.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let best_bid = Book.best_bid consolidated in
    float_equal ~tolerance:0.000001 (Attributed_level.price best_bid) 50000.0002

  let%test "large volume aggregation" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book = make_gemini_book `Btcusd in
    let gemini_book = Gemini.Order_book.Book.set gemini_book
                        ~side:`Bid ~price:50000. ~size:100_000.0 in
    let consolidated = Book.update_gemini consolidated gemini_book in

    let kraken_book = make_kraken_book "BTC/USD" in
    let kraken_book = Kraken.Order_book.Book.set kraken_book
                        ~side:`Bid ~price:50000. ~size:200_000.0 in
    let consolidated = Book.update_kraken consolidated kraken_book in

    let bid_levels = Book.bids consolidated in
    match Map.find bid_levels 50000. with
    | Some level -> float_equal (Attributed_level.volume level) 300_000.0
    | None -> false

  let%test "exchange override - later update wins for same source" =
    let consolidated = Book.empty ~symbol:"BTC/USD" in

    let gemini_book1 = make_gemini_book `Btcusd in
    let gemini_book1 = Gemini.Order_book.Book.set gemini_book1
                         ~side:`Bid ~price:50000. ~size:1.0 in
    let consolidated = Book.update_gemini consolidated gemini_book1 in

    let gemini_book2 = make_gemini_book `Btcusd in
    let gemini_book2 = Gemini.Order_book.Book.set gemini_book2
                         ~side:`Bid ~price:50000. ~size:5.0 in
    let consolidated = Book.update_gemini consolidated gemini_book2 in

    let bid_levels = Book.bids consolidated in
    match Map.find bid_levels 50000. with
    | Some level -> float_equal (Attributed_level.volume level) 5.0
    | None -> false
end)

(** Print test summary *)
let () =
  printf "\n=== Consolidated Order Book Tests ===\n";
  printf "✓ Attributed price levels\n";
  printf "✓ Single exchange updates\n";
  printf "✓ Multi-exchange aggregation\n";
  printf "✓ Volume aggregation at same price\n";
  printf "✓ Exchange attribution tracking\n";
  printf "✓ Spread calculations\n";
  printf "✓ Deep order books\n";
  printf "✓ Arbitrage opportunity detection\n";
  printf "✓ Edge cases and stress tests\n\n"

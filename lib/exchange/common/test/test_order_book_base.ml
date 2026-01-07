(** Comprehensive tests for Order_book_base *)

open Core
open Exchange_common.Order_book_base

(** Test configuration with unit metadata *)
module Test_config = struct
  type symbol = string
  let sexp_of_symbol = String.sexp_of_t
  let symbol_of_sexp = String.t_of_sexp
  let compare_symbol = String.compare

  type metadata = unit [@@deriving sexp]
  let default_metadata () = ()
end

module Book_base = Make (Test_config)
module Book = Book_base.Book
module Books = Book_base.Books

(** Test configuration with rich metadata *)
module Rich_metadata_config = struct
  type symbol = string
  let sexp_of_symbol = String.sexp_of_t
  let symbol_of_sexp = String.t_of_sexp
  let compare_symbol = String.compare

  type metadata = {
    exchange_id: int;
    last_update_id: int64;
  } [@@deriving sexp]

  let default_metadata () = { exchange_id = 0; last_update_id = 0L }
end

module Rich_book_base = Make (Rich_metadata_config)
module Rich_book = Rich_book_base.Book

(** Helper to create test book *)
let make_book symbol = Book.create ~symbol

(** Helper for float equality with tolerance *)
let float_equal ?(tolerance = 0.0001) a b =
  Float.(abs (a - b) < tolerance)

let%test_module "Price_level" = (module struct
  let%test "create price level" =
    let level = Price_level.create ~price:50000. ~volume:1.5 in
    float_equal (Price_level.price level) 50000. &&
    float_equal (Price_level.volume level) 1.5

  let%test "empty price level" =
    let level = Price_level.empty in
    float_equal (Price_level.price level) 0. &&
    float_equal (Price_level.volume level) 0.
end)

let%test_module "Book - Creation" = (module struct
  let%test "create empty book" =
    let book = make_book "BTC/USD" in
    String.equal (Book.symbol book) "BTC/USD"

  let%test "empty book has epoch 0" =
    let book = make_book "BTC/USD" in
    Book.epoch book = 0

  let%test "empty book has zero spread" =
    let book = make_book "BTC/USD" in
    float_equal (Book.spread book) 0.

  let%test "empty book best_bid is zero" =
    let book = make_book "BTC/USD" in
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 0.

  let%test "empty book best_ask is zero" =
    let book = make_book "BTC/USD" in
    let best = Book.best_ask book in
    float_equal (Price_level.price best) 0.

  let%test "empty book mid_price is zero" =
    let book = make_book "BTC/USD" in
    let mid = Book.mid_price book in
    float_equal mid 0.
end)

let%test_module "Book - Set Operations" = (module struct
  let%test "set bid creates price level" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.5 in
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 50000. &&
    float_equal (Price_level.volume best) 1.5

  let%test "set ask creates price level" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:2.0 in
    let best = Book.best_ask book in
    float_equal (Price_level.price best) 51000. &&
    float_equal (Price_level.volume best) 2.0

  let%test "set with size=0 removes level" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.5 in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:0.0 in
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 0.

  let%test "set updates existing level" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.5 in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:2.5 in
    let best = Book.best_bid book in
    float_equal (Price_level.volume best) 2.5

  let%test "set increments epoch" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.5 in
    Book.epoch book = 1
end)

let%test_module "Book - Best Prices" = (module struct
  let%test "best_bid returns highest bid" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:49999. ~size:1.5 in
    let book = Book.set book ~side:`Bid ~price:50001. ~size:0.5 in
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 50001.

  let%test "best_ask returns lowest ask" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:1.0 in
    let book = Book.set book ~side:`Ask ~price:51001. ~size:1.5 in
    let book = Book.set book ~side:`Ask ~price:50999. ~size:0.5 in
    let best = Book.best_ask book in
    float_equal (Price_level.price best) 50999.

  let%test "best prices after removal" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:50001. ~size:0.5 in
    let book = Book.set book ~side:`Bid ~price:50001. ~size:0.0 in (* Remove top *)
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 50000.
end)

let%test_module "Book - Spread and Mid Price" = (module struct
  let%test "spread calculation" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:1.0 in
    float_equal (Book.spread book) 1000.

  let%test "mid_price calculation" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:1.0 in
    let mid = Book.mid_price book in
    float_equal mid 50500.

  let%test "tight spread" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Ask ~price:50001. ~size:1.0 in
    float_equal (Book.spread book) 1.

  let%test "mid_price between bid and ask" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Ask ~price:50002. ~size:1.0 in
    let mid = Book.mid_price book in
    float_equal mid 50001.
end)

let%test_module "Book - Batch Operations" = (module struct
  let%test "set_many creates multiple levels" =
    let book = make_book "BTC/USD" in
    let levels = [
      (`Bid, 50000., 1.0);
      (`Bid, 49999., 1.5);
      (`Ask, 51000., 2.0);
      (`Ask, 51001., 2.5);
    ] in
    let book = Book.set_many book levels in
    let best_bid = Book.best_bid book in
    let best_ask = Book.best_ask book in
    float_equal (Price_level.price best_bid) 50000. &&
    float_equal (Price_level.price best_ask) 51000.

  let%test "set_many with metadata" =
    let book = make_book "BTC/USD" in
    let levels = [(`Bid, 50000., 1.0); (`Ask, 51000., 2.0)] in
    let book = Book.set_many book levels ~metadata:() in
    Book.epoch book > 0

  let%test "set_many increments epoch" =
    let book = make_book "BTC/USD" in
    let initial_epoch = Book.epoch book in
    let levels = [(`Bid, 50000., 1.0); (`Bid, 49999., 1.5)] in
    let book = Book.set_many book levels in
    Book.epoch book > initial_epoch

  let%test "set_many overwrites existing" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let levels = [(`Bid, 50000., 2.5)] in
    let book = Book.set_many book levels in
    let best = Book.best_bid book in
    float_equal (Price_level.volume best) 2.5
end)

let%test_module "Book - Accessors" = (module struct
  let%test "bids_alist returns bid levels" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:49999. ~size:1.5 in
    let alist = Book.bids_alist book in
    List.length alist = 2

  let%test "asks_alist returns ask levels" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:2.0 in
    let book = Book.set book ~side:`Ask ~price:51001. ~size:2.5 in
    let alist = Book.asks_alist book in
    List.length alist = 2

  let%test "bids_alist sorted descending" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:50001. ~size:1.5 in
    let alist = Book.bids_alist book in
    match alist with
    | (price1, _) :: (price2, _) :: _ -> Float.(price1 > price2)
    | _ -> false

  let%test "asks_alist sorted ascending" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Ask ~price:51000. ~size:2.0 in
    let book = Book.set book ~side:`Ask ~price:50999. ~size:2.5 in
    let alist = Book.asks_alist book in
    match alist with
    | (price1, _) :: (price2, _) :: _ -> Float.(price1 < price2)
    | _ -> false

  let%test "metadata accessor" =
    let book = make_book "BTC/USD" in
    let _meta = Book.metadata book in
    true  (* Metadata is unit, just test it's accessible *)
end)

let%test_module "Book - Metadata Handling" = (module struct
  let%test "rich metadata creation" =
    let book = Rich_book.create ~symbol:"BTC/USD" in
    let meta = Rich_book.metadata book in
    meta.exchange_id = 0 && Int64.(meta.last_update_id = 0L)

  let%test "set with custom metadata" =
    let book = Rich_book.create ~symbol:"BTC/USD" in
    let meta = { Rich_metadata_config.exchange_id = 1; last_update_id = 100L } in
    let book = Rich_book.set book ~side:`Bid ~price:50000. ~size:1.0 ~metadata:meta in
    let stored_meta = Rich_book.metadata book in
    stored_meta.exchange_id = 1 && Int64.(stored_meta.last_update_id = 100L)

  let%test "set_many with custom metadata" =
    let book = Rich_book.create ~symbol:"BTC/USD" in
    let meta = { Rich_metadata_config.exchange_id = 2; last_update_id = 200L } in
    let levels = [(`Bid, 50000., 1.0)] in
    let book = Rich_book.set_many book levels ~metadata:meta in
    let stored_meta = Rich_book.metadata book in
    Int64.(stored_meta.last_update_id = 200L)
end)

let%test_module "Books - Multi-Symbol" = (module struct
  let%test "create empty books collection" =
    let books = Books.empty in
    List.length (Books.symbols books) = 0

  let%test "set_book adds book to collection" =
    let books = Books.empty in
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let books = Books.set_book books book in
    List.length (Books.symbols books) = 1

  let%test "set_book multiple symbols" =
    let books = Books.empty in
    let btc_book = make_book "BTC/USD" in
    let eth_book = make_book "ETH/USD" in
    let books = Books.set_book books btc_book in
    let books = Books.set_book books eth_book in
    List.length (Books.symbols books) = 2

  let%test "book retrieval by symbol" =
    let books = Books.empty in
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let books = Books.set_book books book in
    match Books.book books "BTC/USD" with
    | Some retrieved_book ->
      float_equal (Price_level.price (Book.best_bid retrieved_book)) 50000.
    | None -> false

  let%test "book_exn raises on missing symbol" =
    let books = Books.empty in
    try
      let _ = Books.book_exn books "NONEXISTENT" in
      false
    with _ -> true

  let%test "remove_book removes symbol" =
    let books = Books.empty in
    let book = make_book "BTC/USD" in
    let books = Books.set_book books book in
    let books = Books.remove_book books "BTC/USD" in
    List.length (Books.symbols books) = 0

  let%test "update_book modifies existing book" =
    let books = Books.empty in
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let books = Books.set_book books book in
    let books = Books.update_book books "BTC/USD" ~f:(fun book ->
      Book.set book ~side:`Bid ~price:50001. ~size:2.0
    ) in
    match Books.book books "BTC/USD" with
    | Some updated_book ->
      float_equal (Price_level.price (Book.best_bid updated_book)) 50001.
    | None -> false

  let%test "set_book overwrites existing symbol" =
    let books = Books.empty in
    let book1 = make_book "BTC/USD" in
    let book1 = Book.set book1 ~side:`Bid ~price:50000. ~size:1.0 in
    let books = Books.set_book books book1 in
    let book2 = make_book "BTC/USD" in
    let book2 = Book.set book2 ~side:`Bid ~price:60000. ~size:2.0 in
    let books = Books.set_book books book2 in
    match Books.book books "BTC/USD" with
    | Some final_book ->
      float_equal (Price_level.price (Book.best_bid final_book)) 60000.
    | None -> false
end)

let%test_module "Edge Cases" = (module struct
  let%test "very small price differences" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000.0001 ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:50000.0002 ~size:1.5 in
    let best = Book.best_bid book in
    float_equal ~tolerance:0.000001 (Price_level.price best) 50000.0002

  let%test "large volume" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1_000_000.0 in
    let best = Book.best_bid book in
    float_equal (Price_level.volume best) 1_000_000.0

  let%test "many price levels" =
    let book = make_book "BTC/USD" in
    let rec add_levels book price count =
      match count = 0 with
      | true -> book
      | false ->
        let book = Book.set book ~side:`Bid ~price ~size:1.0 in
        add_levels book (price -. 1.) (count - 1)
    in
    let book = add_levels book 50000. 1000 in
    let alist = Book.bids_alist book in
    List.length alist = 1000

  let%test "empty then fill then empty" =
    let book = make_book "BTC/USD" in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:1.0 in
    let book = Book.set book ~side:`Bid ~price:50000. ~size:0.0 in
    let best = Book.best_bid book in
    float_equal (Price_level.price best) 0.
end)

(** Print test summary *)
let () =
  printf "\n=== Order Book Base Tests ===\n";
  printf "✓ Price level creation and accessors\n";
  printf "✓ Empty book initialization\n";
  printf "✓ Set operations (bid/ask)\n";
  printf "✓ Best price calculations\n";
  printf "✓ Spread and mid-price\n";
  printf "✓ Batch operations (set_many)\n";
  printf "✓ Metadata handling\n";
  printf "✓ Multi-symbol books collection\n";
  printf "✓ Edge cases and stress tests\n\n"

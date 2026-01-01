open Core
open Async

(** Helper: Parse exchange name from string *)
let parse_exchange_name name =
  match String.lowercase name with
  | "gemini" | "gem" -> Some `Gemini
  | "kraken" | "krk" -> Some `Kraken
  | "hyperliquid" | "hyp" -> Some `Hyperliquid
  | "bitrue" | "btr" -> Some `Bitrue
  | "binance" | "bin" -> Some `Binance
  | "coinbase" | "cbp" | "cb" -> Some `Coinbase
  | _ -> None

(** Helper: Validate and convert exchange names *)
let validate_exchanges names =
  List.filter_map names ~f:parse_exchange_name

(** Helper: Create a closed pipe for disabled exchanges *)
let create_closed_pipe () : 'a Pipe.Reader.t =
  let r, _w = Pipe.create () in
  Pipe.close_read r;
  r

(** Update event from any exchange *)
type book_update =
  | Gemini_update of [`Ok of Gemini.Order_book.Book.t | `Channel_parse_error of string | `Json_parse_error of string]
  | Kraken_update of (Kraken.Order_book.Book.t, string) Result.t
  | Hyperliquid_update of (Hyperliquid.Order_book.Book.t, string) Result.t
  | Bitrue_update of (Bitrue.Order_book.Book.t, string) Result.t
  | Binance_update of (Binance.Order_book.Book.t, string) Result.t
  | Coinbase_update of (Coinbase.Order_book.Book.t, string) Result.t

(** Print startup diagnostics *)
let print_diagnostics () =
  let fds = Core_unix.RLimit.get Core_unix.RLimit.num_file_descriptors in
  let threads = Scheduler.max_num_threads () in
  printf "=== Startup Diagnostics ===\n";
  printf "Thread pool size: %d\n" threads;
  printf "File descriptors: %s\n"
    (match fds.cur with
     | Limit n -> Int64.to_string n
     | Infinity -> "unlimited");
  printf "===========================\n\n%!"

let test ~exchanges ~depth ~max_display =
  let open Deferred.Let_syntax in

  (* Parse and validate exchanges *)
  let selected = match exchanges with
    | [] -> [`Gemini; `Kraken; `Hyperliquid; `Bitrue; `Binance; `Coinbase]  (* default: all *)
    | names -> validate_exchanges names
  in

  let exchange_names =
    List.map selected ~f:(function
      | `Gemini -> "Gemini"
      | `Kraken -> "Kraken"
      | `Hyperliquid -> "Hyperliquid"
      | `Bitrue -> "Bitrue"
      | `Binance -> "Binance"
      | `Coinbase -> "Coinbase")
    |> String.concat ~sep:", "
  in

  printf "Starting consolidated order book for BTC/USD(T)...\n";
  printf "Selected exchanges: %s\n" exchange_names;
  printf "Connecting to WebSockets...\n\n%!";

  (* Create consolidated book *)
  let consolidated = ref (Consolidated_order_book.Book.empty "BTC/USD") in
  let update_count = ref 0 in
  let last_print_time = ref (Time_float_unix.now ()) in

  (* Conditionally connect to Gemini *)
  let%bind gemini_pipe =
    if List.mem selected `Gemini ~equal:Poly.equal then (
      let module Gemini_cfg = Gemini.Cfg.Production () in
      let%bind pipe = Gemini.Order_book.Book.pipe (module Gemini_cfg) ~symbol:`Btcusd () in
      return (Pipe.map pipe ~f:(fun result -> Gemini_update result))
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Kraken *)
  let%bind kraken_pipe =
    if List.mem selected `Kraken ~equal:Poly.equal then (
      let%bind pipe = Kraken.Order_book.Book.pipe ~symbol:"XBT/USD" ~depth () in
      return (Pipe.map pipe ~f:(fun result -> Kraken_update result))
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Hyperliquid *)
  let%bind hyperliquid_pipe =
    if List.mem selected `Hyperliquid ~equal:Poly.equal then (
      let%bind pipe = Hyperliquid.Order_book.Book.pipe ~symbol:"BTC" () in
      return (Pipe.map pipe ~f:(fun result -> Hyperliquid_update result))
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Bitrue *)
  let%bind bitrue_pipe_result =
    if List.mem selected `Bitrue ~equal:Poly.equal then (
      let bitrue_symbol = Fluxum.Types.Symbol.of_string "BTCUSDT" in
      Bitrue.Order_book.Book.pipe ~symbol:bitrue_symbol ()
    ) else (
      return (Ok (create_closed_pipe ()))
    )
  in
  let bitrue_pipe = match bitrue_pipe_result with
    | Ok pipe -> Pipe.map pipe ~f:(fun result -> Bitrue_update result)
    | Error err ->
      eprintf "Bitrue connection failed: %s\n%!" (Error.to_string_hum err);
      create_closed_pipe ()
  in

  (* Conditionally connect to Binance *)
  let%bind binance_pipe =
    if List.mem selected `Binance ~equal:Poly.equal then (
      let binance_symbol = Fluxum.Types.Symbol.of_string "BTCUSDT" in
      let%bind pipe = Binance.Order_book.Book.pipe ~symbol:binance_symbol () in
      return (Pipe.map pipe ~f:(fun result -> Binance_update result))
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Coinbase *)
  let%bind coinbase_pipe =
    if List.mem selected `Coinbase ~equal:Poly.equal then (
      let coinbase_symbol = Fluxum.Types.Symbol.of_string "BTC-USD" in
      let%bind pipe = Coinbase.Order_book.Book.pipe ~symbol:coinbase_symbol () in
      return (Pipe.map pipe ~f:(fun result -> Coinbase_update result))
    ) else (
      return (create_closed_pipe ())
    )
  in

  printf "✓ Connected to selected exchanges\n\n%!";

  (* Merge all selected exchange pipes into unified stream *)
  let merged_pipe =
    Pipe.interleave [gemini_pipe; kraken_pipe; hyperliquid_pipe; bitrue_pipe; binance_pipe; coinbase_pipe]
  in

  (* Track first updates for connection diagnostics *)
  let gemini_first_update = ref true in
  let kraken_first_update = ref true in
  let hyperliquid_first_update = ref true in
  let bitrue_first_update = ref true in
  let binance_first_update = ref true in
  let coinbase_first_update = ref true in

  (* Process all updates in unified foreground loop *)
  let%bind () =
    Pipe.iter merged_pipe ~f:(fun update ->
      (* Process update based on source exchange *)
      let%bind () = match update with
        | Gemini_update book_result ->
          (match book_result with
           | `Ok gemini_book ->
             if !gemini_first_update then (
               printf "[INFO] Gemini data connected!\n%!";
               gemini_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_gemini !consolidated gemini_book;
             return ()
           | `Channel_parse_error err ->
             eprintf "[GEMINI] Channel parse error: %s\n%!" err;
             return ()
           | `Json_parse_error err ->
             eprintf "[GEMINI] JSON parse error: %s\n%!" err;
             return ())

        | Kraken_update book_result ->
          (match book_result with
           | Error err ->
             eprintf "Kraken error: %s\n%!" err;
             return ()
           | Ok kraken_book ->
             if !kraken_first_update then (
               printf "[INFO] Kraken data connected!\n%!";
               kraken_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_kraken !consolidated kraken_book;
             return ())

        | Hyperliquid_update book_result ->
          (match book_result with
           | Error err ->
             eprintf "Hyperliquid error: %s\n%!" err;
             return ()
           | Ok hyperliquid_book ->
             if !hyperliquid_first_update then (
               printf "[INFO] Hyperliquid data connected!\n%!";
               hyperliquid_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_hyperliquid !consolidated hyperliquid_book;
             return ())

        | Bitrue_update book_result ->
          (match book_result with
           | Error err ->
             eprintf "Bitrue error: %s\n%!" err;
             return ()
           | Ok bitrue_book ->
             if !bitrue_first_update then (
               printf "[INFO] Bitrue data connected!\n%!";
               bitrue_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_bitrue !consolidated bitrue_book;
             return ())

        | Binance_update book_result ->
          (match book_result with
           | Error err ->
             eprintf "Binance error: %s\n%!" err;
             return ()
           | Ok binance_book ->
             if !binance_first_update then (
               printf "[INFO] Binance data connected!\n%!";
               binance_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_binance !consolidated binance_book;
             return ())

        | Coinbase_update book_result ->
          (match book_result with
           | Error err ->
             eprintf "Coinbase error: %s\n%!" err;
             return ()
           | Ok coinbase_book ->
             if !coinbase_first_update then (
               printf "[INFO] Coinbase data connected!\n%!";
               coinbase_first_update := false
             );
             consolidated := Consolidated_order_book.Book.update_coinbase !consolidated coinbase_book;
             return ())
      in

      (* Update counter and trigger display if needed *)
      update_count := !update_count + 1;

      (* Print every 5 updates or every 2 seconds *)
      let now = Time_float_unix.now () in
      let time_since_print = Time_float_unix.diff now !last_print_time in
      let should_print =
        !update_count mod 5 = 0 ||
        Time_float_unix.Span.(time_since_print > of_sec 2.0)
      in

      if should_print then (
        last_print_time := now;
        Consolidated_order_book.Book.pretty_print ~max_depth:max_display !consolidated ();
      );

      return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  (* Note: Thread pool size is configured via ASYNC_CONFIG environment variable.
     Set before running: ASYNC_CONFIG='((max_num_threads 32))' ./consolidated_orderbook
     Default is typically 4 threads, which causes saturation with 4+ exchanges. *)

  Command.async_spec
    ~summary:"Consolidated order book from multiple exchanges"
    ~readme:(fun () ->
      "Real-time consolidated order book with selectable exchanges.\n\n\
       Usage:\n\
       \  # All exchanges (default) - requires ASYNC_CONFIG for 4+ exchanges\n\
       \  ASYNC_CONFIG='((max_num_threads 32))' ./consolidated_orderbook\n\n\
       \  # Select specific exchanges (works with default 4 threads)\n\
       \  ./consolidated_orderbook --exchanges kraken --exchanges hyperliquid\n\
       \  # Or use short form:\n\
       \  ./consolidated_orderbook -e kraken -e hyperliquid\n\n\
       \  # Single exchange\n\
       \  ./consolidated_orderbook -e gemini\n\n\
       Note: Default thread pool (4 threads) causes saturation with 4+ exchanges.\n\
       Use ASYNC_CONFIG='((max_num_threads 32))' for all 6 exchanges.\n\n\
       Available exchanges: gemini, kraken, hyperliquid, bitrue, binance, coinbase\n\
       Short flags: -e works as prefix for --exchanges (Core Command feature)")
    (Command.Spec.(
      empty
      +> flag "--exchanges" (listed string)
           ~doc:"EXCHANGE Enable exchange (gemini|kraken|hyperliquid|bitrue|binance|coinbase). \
                 Can be repeated. Short form: -e. Default: all"
      +> flag "-e" (listed string)
           ~doc:"" (* Hidden alias for --exchanges *)
      +> flag "--depth" (optional_with_default 10 int)
           ~doc:"INT Order book depth per exchange (default 10)"
      +> flag "--max-display" (optional_with_default 8 int)
           ~doc:"INT Max depth to display in output (default 8)"
    ))
    (fun exchanges_long exchanges_short depth max_display () ->
      (* Merge both flag values *)
      let exchanges = exchanges_long @ exchanges_short in
      (* Print diagnostics *)
      print_diagnostics ();

      (* Run the test *)
      test ~exchanges ~depth ~max_display
    )
  |> Command_unix.run

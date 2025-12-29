open Core
open Async

(** Helper: Parse exchange name from string *)
let parse_exchange_name name =
  match String.lowercase name with
  | "gemini" | "gem" -> Some `Gemini
  | "kraken" | "krk" -> Some `Kraken
  | "hyperliquid" | "hyp" -> Some `Hyperliquid
  | "bitrue" | "btr" -> Some `Bitrue
  | _ -> None

(** Helper: Validate and convert exchange names *)
let validate_exchanges names =
  List.filter_map names ~f:parse_exchange_name

(** Helper: Create a closed pipe for disabled exchanges *)
let create_closed_pipe () : 'a Pipe.Reader.t =
  let r, _w = Pipe.create () in
  Pipe.close_read r;
  r

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
    | [] -> [`Gemini; `Kraken; `Hyperliquid; `Bitrue]  (* default: all *)
    | names -> validate_exchanges names
  in

  let exchange_names =
    List.map selected ~f:(function
      | `Gemini -> "Gemini"
      | `Kraken -> "Kraken"
      | `Hyperliquid -> "Hyperliquid"
      | `Bitrue -> "Bitrue")
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
      Gemini.Order_book.Book.pipe_curl (module Gemini_cfg) ~symbol:`Btcusd ()
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Kraken *)
  let%bind kraken_pipe =
    if List.mem selected `Kraken ~equal:Poly.equal then (
      Kraken.Order_book.Book.pipe ~symbol:"XBT/USD" ~depth ()
    ) else (
      return (create_closed_pipe ())
    )
  in

  (* Conditionally connect to Hyperliquid *)
  let%bind hyperliquid_pipe =
    if List.mem selected `Hyperliquid ~equal:Poly.equal then (
      Hyperliquid.Order_book.Book.pipe ~symbol:"BTC" ()
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
    | Ok pipe -> pipe
    | Error err ->
      eprintf "Bitrue connection failed: %s\n%!" (Error.to_string_hum err);
      create_closed_pipe ()
  in

  printf "✓ Connected to selected exchanges\n\n%!";

  (* Process Gemini updates in background *)
  let gemini_first_update = ref true in
  don't_wait_for (
    Pipe.iter gemini_pipe ~f:(fun book_result ->
      match book_result with
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
        return ()
    )
  );

  (* Process Hyperliquid updates in background *)
  let hyperliquid_first_update = ref true in
  don't_wait_for (
    Pipe.iter hyperliquid_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Hyperliquid error: %s\n%!" err;
        return ()
      | Ok hyperliquid_book ->
        if !hyperliquid_first_update then (
          printf "[INFO] Hyperliquid data connected!\n%!";
          hyperliquid_first_update := false
        );
        consolidated := Consolidated_order_book.Book.update_hyperliquid !consolidated hyperliquid_book;
        return ()
    )
  );

  (* Process Bitrue updates in background *)
  let bitrue_first_update = ref true in
  don't_wait_for (
    Pipe.iter bitrue_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Bitrue error: %s\n%!" err;
        return ()
      | Ok bitrue_book ->
        if !bitrue_first_update then (
          printf "[INFO] Bitrue data connected!\n%!";
          bitrue_first_update := false
        );
        consolidated := Consolidated_order_book.Book.update_bitrue !consolidated bitrue_book;
        return ()
    )
  );

  (* Process Kraken updates in foreground *)
  let%bind () =
    Pipe.iter kraken_pipe ~f:(fun book_result ->
      match book_result with
      | Error err ->
        eprintf "Kraken error: %s\n%!" err;
        return ()
      | Ok kraken_book ->
        consolidated := Consolidated_order_book.Book.update_kraken !consolidated kraken_book;
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
       \  ./consolidated_orderbook --exchanges kraken --exchanges hyperliquid\n\n\
       \  # Single exchange\n\
       \  ./consolidated_orderbook --exchanges gemini\n\n\
       Note: Default thread pool (4 threads) causes saturation with 4+ exchanges.\n\
       Use ASYNC_CONFIG='((max_num_threads 32))' for all 4 exchanges.\n\n\
       Available exchanges: gemini, kraken, hyperliquid, bitrue")
    (Command.Spec.(
      empty
      +> flag "--exchanges" (listed string)
           ~doc:"EXCHANGE Enable exchange (gemini|kraken|hyperliquid|bitrue). \
                 Can be repeated. Default: all"
      +> flag "--depth" (optional_with_default 10 int)
           ~doc:"INT Order book depth per exchange (default 10)"
      +> flag "--max-display" (optional_with_default 8 int)
           ~doc:"INT Max depth to display in output (default 8)"
    ))
    (fun exchanges depth max_display () ->
      (* Print diagnostics *)
      print_diagnostics ();

      (* Run the test *)
      test ~exchanges ~depth ~max_display
    )
  |> Command_unix.run

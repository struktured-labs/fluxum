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

(** Exchange identifier for arbitrage *)
type exchange = Gemini | Kraken | Hyperliquid | Bitrue | Binance | Coinbase

let exchange_to_string = function
  | Gemini -> "Gemini"
  | Kraken -> "Kraken"
  | Hyperliquid -> "Hyperliquid"
  | Bitrue -> "Bitrue"
  | Binance -> "Binance"
  | Coinbase -> "Coinbase"

(** Trading fee configuration per exchange (as percentage) *)
type fee_config = {
  gemini_taker: float;
  kraken_taker: float;
  hyperliquid_taker: float;
  bitrue_taker: float;
  binance_taker: float;
  coinbase_taker: float;
}

(** Default fee configuration - typical taker fees *)
let default_fees = {
  gemini_taker = 0.35;       (* 0.35% *)
  kraken_taker = 0.26;       (* 0.26% *)
  hyperliquid_taker = 0.03;  (* 0.03% *)
  bitrue_taker = 0.10;       (* 0.10% *)
  binance_taker = 0.10;      (* 0.10% *)
  coinbase_taker = 0.60;     (* 0.60% - Advanced Trade *)
}

(** Get taker fee for an exchange *)
let get_taker_fee fees exchange =
  match exchange with
  | Gemini -> fees.gemini_taker
  | Kraken -> fees.kraken_taker
  | Hyperliquid -> fees.hyperliquid_taker
  | Bitrue -> fees.bitrue_taker
  | Binance -> fees.binance_taker
  | Coinbase -> fees.coinbase_taker

(** Arbitrage opportunity *)
type arb_opportunity = {
  buy_exchange: exchange;
  sell_exchange: exchange;
  buy_price: float;
  sell_price: float;
  profit: float;
  profit_pct: float;
  buy_volume: float;
  sell_volume: float;
  buy_fee_pct: float;
  sell_fee_pct: float;
  net_profit: float;
  net_profit_pct: float;
}

(** Get best bid/ask from individual exchange books *)
let get_exchange_best_bid_ask (book: Consolidated_order_book.Book.t) exchange =
  match exchange with
  | Gemini ->
    (match book.gemini_book with
     | None -> None
     | Some ex_book ->
       let bids = Gemini.Order_book.Book.best_n_bids ex_book ~n:1 () in
       let asks = Gemini.Order_book.Book.best_n_asks ex_book ~n:1 () in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)
  | Kraken ->
    (match book.kraken_book with
     | None -> None
     | Some ex_book ->
       let bids = Kraken.Order_book.Book.best_n_bids ex_book ~n:1 () in
       let asks = Kraken.Order_book.Book.best_n_asks ex_book ~n:1 () in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)
  | Hyperliquid ->
    (match book.hyperliquid_book with
     | None -> None
     | Some ex_book ->
       let bids = Map.to_alist ex_book.Hyperliquid.Order_book.Book.bids
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       let asks = Map.to_alist ex_book.Hyperliquid.Order_book.Book.asks
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)
  | Bitrue ->
    (match book.bitrue_book with
     | None -> None
     | Some ex_book ->
       let bids = Map.to_alist ex_book.Bitrue.Order_book.Book.bids
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       let asks = Map.to_alist ex_book.Bitrue.Order_book.Book.asks
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)
  | Binance ->
    (match book.binance_book with
     | None -> None
     | Some ex_book ->
       let bids = Map.to_alist ex_book.Binance.Order_book.Book.bids
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       let asks = Map.to_alist ex_book.Binance.Order_book.Book.asks
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)
  | Coinbase ->
    (match book.coinbase_book with
     | None -> None
     | Some ex_book ->
       let bids = Map.to_alist ex_book.Coinbase.Order_book.Book.bids
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       let asks = Map.to_alist ex_book.Coinbase.Order_book.Book.asks
         |> (fun list -> List.take list 1) |> List.map ~f:snd in
       match bids, asks with
       | bid :: _, ask :: _ -> Some (bid.price, bid.volume, ask.price, ask.volume)
       | _ -> None)

(** Detect arbitrage opportunities across all exchange pairs *)
let detect_arbitrage ?(fees=default_fees) (book: Consolidated_order_book.Book.t) : arb_opportunity list =
  let exchanges = [Gemini; Kraken; Hyperliquid; Bitrue; Binance; Coinbase] in
  let opportunities = ref [] in

  (* Check all exchange pairs *)
  List.iter exchanges ~f:(fun buy_ex ->
    List.iter exchanges ~f:(fun sell_ex ->
      if not (Poly.equal buy_ex sell_ex) then
        match get_exchange_best_bid_ask book buy_ex,
              get_exchange_best_bid_ask book sell_ex with
        | Some (_, _, buy_ask, buy_ask_vol), Some (sell_bid, sell_bid_vol, _, _) ->
          (* Arbitrage exists if we can buy cheaper on buy_ex than sell on sell_ex *)
          if Float.(sell_bid > buy_ask) then
            let profit = sell_bid -. buy_ask in
            let profit_pct = 100. *. profit /. buy_ask in

            (* Calculate fees *)
            let buy_fee_pct = get_taker_fee fees buy_ex in
            let sell_fee_pct = get_taker_fee fees sell_ex in
            let total_fee_pct = buy_fee_pct +. sell_fee_pct in

            (* Net profit after fees *)
            let net_profit_pct = profit_pct -. total_fee_pct in
            let net_profit = buy_ask *. net_profit_pct /. 100. in

            opportunities := {
              buy_exchange = buy_ex;
              sell_exchange = sell_ex;
              buy_price = buy_ask;
              sell_price = sell_bid;
              profit;
              profit_pct;
              buy_volume = buy_ask_vol;
              sell_volume = sell_bid_vol;
              buy_fee_pct;
              sell_fee_pct;
              net_profit;
              net_profit_pct;
            } :: !opportunities
        | _ -> ()
    )
  );

  (* Sort by net profit percentage (descending) *)
  List.sort !opportunities ~compare:(fun a b ->
    Float.compare b.net_profit_pct a.net_profit_pct)

(** Pretty print with arbitrage highlighting *)
let pretty_print_with_arb ?(max_depth = 10) (book: Consolidated_order_book.Book.t) () =
  let green = "\027[32m" in
  let red = "\027[31m" in
  let blue = "\027[34m" in
  let magenta = "\027[35m" in
  let cyan = "\027[36m" in
  let bold = "\027[1m" in
  let reset = "\027[0m" in
  let clear_screen = "\027[2J\027[H" in

  (* Bright colors for arbitrage *)
  let arb_bg = "\027[43m\027[30m" in (* Yellow background, black text *)
  let arb_highlight = "\027[1m\027[33m" in (* Bold yellow *)

  printf "%s" clear_screen;

  (* Detect arbitrage opportunities *)
  let arb_opps = detect_arbitrage book in

  (* Print arbitrage alerts at the top *)
  if not (List.is_empty arb_opps) then (
    printf "%s%s╔═══════════════════════════════════════════════════════════════════════╗%s\n"
      bold arb_bg reset;
    printf "%s%s║  🚨 ARBITRAGE OPPORTUNITIES DETECTED 🚨                                ║%s\n"
      bold arb_bg reset;
    printf "%s%s╚═══════════════════════════════════════════════════════════════════════╝%s\n\n"
      bold arb_bg reset;

    List.iteri arb_opps ~f:(fun i opp ->
      (* Color code based on net profitability *)
      let profit_color =
        if Float.(opp.net_profit_pct >= 0.5) then "\027[1m\027[32m"  (* Bold green: very profitable *)
        else if Float.(opp.net_profit_pct >= 0.2) then "\027[33m"    (* Yellow: marginally profitable *)
        else "\027[31m"                                                (* Red: unprofitable *)
      in

      printf "%s[ARB #%d]%s Buy on %s%-9s%s @ %s$%.2f%s → Sell on %s%-9s%s @ %s$%.2f%s\n"
        arb_highlight (i+1) reset
        cyan (exchange_to_string opp.buy_exchange) reset
        green opp.buy_price reset
        magenta (exchange_to_string opp.sell_exchange) reset
        green opp.sell_price reset;

      printf "       Gross: %s$%.2f (%.3f%%)%s | Fees: %.2f%% + %.2f%% = %.2f%%\n"
        blue opp.profit opp.profit_pct reset
        opp.buy_fee_pct opp.sell_fee_pct (opp.buy_fee_pct +. opp.sell_fee_pct);

      printf "       %sNet Profit: $%.2f (%.3f%%)%s"
        profit_color opp.net_profit opp.net_profit_pct reset;

      if Float.(opp.net_profit_pct >= 0.5) then
        printf " ✓ PROFITABLE"
      else if Float.(opp.net_profit_pct >= 0.2) then
        printf " ⚠ MARGINAL"
      else
        printf " ✗ UNPROFITABLE";

      printf "\n";
      printf "       Buy volume: %.4f BTC | Sell volume: %.4f BTC | Max size: %.4f BTC\n\n"
        opp.buy_volume opp.sell_volume (Float.min opp.buy_volume opp.sell_volume);
    );
    printf "%s%s%s\n\n" reset (String.make 75 '-') reset;
  ) else (
    printf "%s[No arbitrage opportunities detected]%s\n\n" blue reset;
  );

  (* Print consolidated order book *)
  printf "=== %s Consolidated Order Book (Epoch: %d) ===\n" book.symbol book.epoch;
  printf "Updated: %s\n" (Time_float_unix.to_string book.update_time);
  printf "Sources: ";
  if Option.is_some book.gemini_book then printf "[Gemini] ";
  if Option.is_some book.kraken_book then printf "[Kraken] ";
  if Option.is_some book.hyperliquid_book then printf "[Hyperliquid] ";
  if Option.is_some book.bitrue_book then printf "[Bitrue] ";
  printf "\n\n";

  (* Print asks (highest to lowest for display) *)
  let asks = Consolidated_order_book.Book.best_n_asks book ~n:max_depth () |> List.rev in
  printf "%sAsks (Sell Orders):%s\n" red reset;
  List.iter asks ~f:(fun level ->
    printf "  %s%-4s%s  %.8f @ %.2f\n"
      blue
      (Consolidated_order_book.Book.exchange_to_string level.Consolidated_order_book.Attributed_level.exchange)
      reset
      level.volume
      level.price
  );

  (* Print spread *)
  let best_bid = Consolidated_order_book.Book.best_bid book in
  let best_ask = Consolidated_order_book.Book.best_ask book in
  let spread = best_ask.price -. best_bid.price in
  let spread_pct = if Float.(best_bid.price > 0.) then
    100. *. spread /. best_bid.price
  else 0. in
  printf "\n--- Spread: $%.2f (%.4f%%) ---\n\n" spread spread_pct;

  (* Print bids *)
  let bids = Consolidated_order_book.Book.best_n_bids book ~n:max_depth () in
  printf "%sBids (Buy Orders):%s\n" green reset;
  List.iter bids ~f:(fun level ->
    printf "  %s%-4s%s  %.8f @ %.2f\n"
      blue
      (Consolidated_order_book.Book.exchange_to_string level.Consolidated_order_book.Attributed_level.exchange)
      reset
      level.volume
      level.price
  );

  printf "\n";
  Out_channel.flush stdout

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

  printf "Starting consolidated order book with ARBITRAGE DETECTION...\n";
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
      let%bind pipe = Gemini.Order_book.Book.pipe_curl (module Gemini_cfg) ~symbol:`Btcusd () in
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
        pretty_print_with_arb ~max_depth:max_display !consolidated ();
      );

      return ()
    )
  in

  printf "Order book stream ended\n%!";
  return ()

let () =
  Command.async_spec
    ~summary:"Consolidated order book with arbitrage detection"
    ~readme:(fun () ->
      "Real-time consolidated order book with ARBITRAGE DETECTION.\n\n\
       Highlights cross-exchange arbitrage opportunities in bright colors.\n\n\
       Usage:\n\
       \  # All exchanges (default) - requires ASYNC_CONFIG for 4+ exchanges\n\
       \  ASYNC_CONFIG='((max_num_threads 32))' ./consolidated_orderbook_arb\n\n\
       \  # Select specific exchanges\n\
       \  ./consolidated_orderbook_arb -e kraken -e hyperliquid\n\n\
       \  # Single exchange (no arbitrage detection)\n\
       \  ./consolidated_orderbook_arb -e gemini\n\n\
       Note: Arbitrage detection requires at least 2 exchanges.\n\
       Use ASYNC_CONFIG='((max_num_threads 32))' for 4+ exchanges.\n\n\
       Available exchanges: gemini, kraken, hyperliquid, bitrue, binance, coinbase\n\
       Exchange fees (taker): Gemini 0.35%, Kraken 0.26%, Hyperliquid 0.03%,\n\
       Bitrue 0.10%, Binance 0.10%, Coinbase 0.60%")
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

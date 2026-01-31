(** Bitrue Exchange Module *)

open Core
open Async

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Market_data_curl = Market_data_curl
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Unified_adapter = Unified_adapter
module Unified_cmd = Unified_cmd
module Fluxum_adapter = Fluxum_adapter

let cfg = (module Cfg.Production : Cfg.S)

let exchange_info_command : string * Command.t =
  ("exchange-info", Command.async
    ~summary:"Get exchange info (available symbols)"
    (Command.Param.return (fun () ->
      Rest.exchange_info cfg >>| function
      | Ok info ->
        printf "Symbols (%d):\n" (List.length info.symbols);
        List.iter info.symbols ~f:(fun s ->
          printf "  %s  base=%s quote=%s status=%s\n"
            s.symbol s.baseAsset s.quoteAsset s.status)
      | Error e ->
        eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e)))))

let depth_command : string * Command.t =
  ("depth", Command.async
    ~summary:"Get order book depth"
    (let%map_open.Command symbol = flag "--symbol" (required string)
        ~doc:"STRING Trading pair (e.g. BTCUSDT)"
     and limit = flag "--limit" (optional_with_default 20 int)
        ~doc:"INT Number of levels (default: 20)"
     in
     fun () ->
       Rest.depth cfg ~symbol ~limit () >>| function
       | Ok book ->
         printf "Order Book: %s (updateId: %Ld)\n" symbol book.lastUpdateId;
         printf "\n%-12s  %s\n" "BIDS" "ASKS";
         printf "%s\n" (String.make 40 '-');
         let max_len = max (List.length book.bids) (List.length book.asks) in
         let bids = Array.of_list book.bids in
         let asks = Array.of_list book.asks in
         for i = 0 to min max_len limit - 1 do
           let bid_str = match i < Array.length bids with
             | true -> let (p, q) = bids.(i) in sprintf "%s x %s" p q
             | false -> ""
           in
           let ask_str = match i < Array.length asks with
             | true -> let (p, q) = asks.(i) in sprintf "%s x %s" p q
             | false -> ""
           in
           printf "%-20s  %s\n" bid_str ask_str
         done
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e))))

let trades_command : string * Command.t =
  ("recent-trades", Command.async
    ~summary:"Get recent trades"
    (let%map_open.Command symbol = flag "--symbol" (required string)
        ~doc:"STRING Trading pair (e.g. BTCUSDT)"
     and limit = flag "--limit" (optional_with_default 20 int)
        ~doc:"INT Number of trades (default: 20)"
     in
     fun () ->
       Rest.trades cfg ~symbol ~limit () >>| function
       | Ok trades ->
         printf "Recent Trades: %s (%d)\n\n" symbol (List.length trades);
         List.iter trades ~f:(fun t ->
           let side = match t.isBuyerMaker with true -> "SELL" | false -> "BUY " in
           printf "  %s  price=%s  qty=%s  id=%Ld\n" side t.price t.qty t.id)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e))))

let ticker_command : string * Command.t =
  ("ticker", Command.async
    ~summary:"Get 24hr ticker"
    (let%map_open.Command symbol = flag "--symbol" (required string)
        ~doc:"STRING Trading pair (e.g. BTCUSDT)"
     in
     fun () ->
       Rest.ticker_24hr cfg ~symbol >>| function
       | Ok t ->
         printf "Ticker: %s\n" t.symbol;
         printf "  Last:    %s\n" t.lastPrice;
         printf "  Bid:     %s\n" (Option.value t.bidPrice ~default:"N/A");
         printf "  Ask:     %s\n" (Option.value t.askPrice ~default:"N/A");
         printf "  High:    %s\n" t.highPrice;
         printf "  Low:     %s\n" t.lowPrice;
         printf "  Volume:  %s\n" t.volume;
         printf "  Change:  %s (%s%%)\n" t.priceChange t.priceChangePercent
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e))))

let klines_command : string * Command.t =
  ("klines", Command.async
    ~summary:"Get candlestick data"
    (let%map_open.Command symbol = flag "--symbol" (required string)
        ~doc:"STRING Trading pair (e.g. BTCUSDT)"
     and interval = flag "--interval" (optional_with_default "1h" string)
        ~doc:"STRING Interval (1m, 5m, 15m, 1h, 4h, 1d; default: 1h)"
     and limit = flag "--limit" (optional_with_default 10 int)
        ~doc:"INT Number of candles (default: 10)"
     in
     fun () ->
       Rest.klines cfg ~symbol ~interval ~limit () >>| function
       | Ok klines ->
         printf "Klines: %s (%s, %d candles)\n\n" symbol interval (List.length klines);
         printf "  %-14s  %-10s  %-10s  %-10s  %-10s  %s\n"
           "TIME" "OPEN" "HIGH" "LOW" "CLOSE" "VOLUME";
         List.iter klines ~f:(fun k ->
           printf "  %-14Ld  %-10s  %-10s  %-10s  %-10s  %s\n"
             k.open_time k.open_ k.high k.low k.close k.volume)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e))))

let balances_command : string * Command.t =
  ("balances", Command.async
    ~summary:"Get account balances (authenticated)"
    (Command.Param.return (fun () ->
      Rest.account cfg >>| function
      | Ok resp ->
        let non_zero = List.filter resp.balances ~f:(fun b ->
          match Float.of_string_opt b.free, Float.of_string_opt b.locked with
          | Some free, Some locked -> Float.(free +. locked > 0.0)
          | _ -> false)
        in
        printf "Balances (%d non-zero):\n" (List.length non_zero);
        List.iter non_zero ~f:(fun b ->
          printf "  %-8s  free=%-14s  locked=%s\n" b.asset b.free b.locked)
      | Error e ->
        eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e)))))

let open_orders_command : string * Command.t =
  ("open-orders", Command.async
    ~summary:"Get open orders (authenticated)"
    (let%map_open.Command symbol = flag "--symbol" (required string)
        ~doc:"STRING Trading pair (e.g. BTCUSDT)"
     in
     fun () ->
       Rest.open_orders cfg ~symbol >>| function
       | Ok orders ->
         printf "Open Orders: %s (%d)\n\n" symbol (List.length orders);
         List.iter orders ~f:(fun o ->
           printf "  %s  %s  price=%s  qty=%s  filled=%s  status=%s\n"
             o.orderId o.side o.price o.origQty o.executedQty o.status)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.Error.sexp_of_t e))))

let command : Command.t =
  Command.group ~summary:"Bitrue Exchange Commands"
    [ exchange_info_command
    ; depth_command
    ; trades_command
    ; ticker_command
    ; klines_command
    ; balances_command
    ; open_orders_command
    ; Unified_cmd.command
    ]

open Core
open Async

(* Exchange-specific command groups *)
let gemini_command = Gemini.command
let kraken_command = Kraken.command
let binance_command = Binance.command
let mexc_command = Mexc.command
let hyperliquid_command = Hyperliquid.command
let coinbase_command = Coinbase.command
let bitrue_command = Bitrue.command

(* Gemini adapter helpers *)
let get_gemini_balances cfg =
  let module Cfg = (val cfg : Gemini.Cfg.S) in
  Gemini.Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
  Gemini.V1.Balances.post (module Cfg) nonce () >>= function
  | `Ok balances ->
    (* Convert native Gemini balances to normalized types *)
    let normalized = List.map balances ~f:(fun (b : Gemini.V1.Balances.balance) ->
      let open Fluxum.Types in
      (* Decimal_string.t is just string, so we can convert directly *)
      let total = Float.of_string b.amount in
      let available = Float.of_string b.available in
      (* Currency.Enum_or_string.t can be converted via sexp *)
      let currency = 
        Gemini.V1.Currency.Enum_or_string.sexp_of_t b.currency 
        |> Sexp.to_string 
      in
      { Balance.venue = Venue.Gemini
      ; currency
      ; total
      ; available
      ; locked = total -. available
      })
    in
    return (Ok normalized)
  | #Gemini.Rest.Error.post as err ->
    return (Error (Fluxum.Types.Error.Exchange_specific 
      { venue = Fluxum.Types.Venue.Gemini
      ; code = "rest_error"
      ; message = Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_post err)
      }))

(* Kraken adapter helpers *)
let get_kraken_balances cfg =
  let module Cfg = (val cfg : Kraken.Cfg.S) in
  Kraken.V1.balances (module Cfg) () >>= function
  | `Ok json ->
    (* Parse JSON response from Kraken - structure: { "error": [...], "result": { "CURRENCY": "amount", ... } } *)
    (try
      let open Yojson.Safe in
      let error_list = Util.member "error" json in
      (match error_list with
       | `List [] | `List _ ->
         (* Check for errors *)
         (match error_list with
          | `List (_ :: _ as errs) ->
            let error_msg = List.map errs ~f:(function
              | `String s -> s
              | _ -> "Unknown error")
              |> String.concat ~sep:"; " 
            in
            return (Error (Fluxum.Types.Error.Exchange_specific 
              { venue = Fluxum.Types.Venue.Kraken
              ; code = "api_error"
              ; message = error_msg
              }))
          | _ ->
            (* No errors, parse result *)
            (match Util.member "result" json with
            | `Assoc balances ->
              let normalized = List.filter_map balances ~f:(fun (currency, amount_json) ->
                match amount_json with
                | `String amount ->
                  (try
                    let total = Float.of_string amount in
                    Some (Fluxum.Types.{
                      Balance.venue = Venue.Kraken
                    ; currency = String.uppercase currency
                    ; total
                    ; available = total  (* Kraken returns total balance *)
                    ; locked = 0.0
                    })
                  with _ -> None)
                | _ -> None
              ) in
              return (Ok normalized)
            | _ ->
              return (Error (Fluxum.Types.Error.Exchange_specific 
                { venue = Fluxum.Types.Venue.Kraken
                ; code = "parse_error"
                ; message = "Could not find result field in Kraken response"
                }))))
       | _ ->
         return (Error (Fluxum.Types.Error.Exchange_specific 
           { venue = Fluxum.Types.Venue.Kraken
           ; code = "parse_error"
           ; message = "Invalid error field in Kraken response"
           })))
    with e ->
      return (Error (Fluxum.Types.Error.Exchange_specific 
        { venue = Fluxum.Types.Venue.Kraken
        ; code = "json_error"
        ; message = Exn.to_string e
        })))
  | #Kraken.Rest.Error.post as err ->
    return (Error (Fluxum.Types.Error.Exchange_specific
      { venue = Fluxum.Types.Venue.Kraken
      ; code = "rest_error"
      ; message = Sexp.to_string_hum (Kraken.Rest.Error.sexp_of_post err)
      }))

(* Helper to print Order.t *)
let print_order (order : Fluxum.Types.Order.t) =
  printf "  %s | %s | %s %s @ %s | %s/%s filled | %s\n"
    order.id
    order.symbol
    (Fluxum.Types.Side.to_string order.side)
    (match order.kind with
     | Fluxum.Types.Order_kind.Market -> "MARKET"
     | Fluxum.Types.Order_kind.Limit p -> sprintf "LIMIT %.2f" p
     | Fluxum.Types.Order_kind.Post_only_limit p -> sprintf "POST_ONLY %.2f" p)
    (Float.to_string order.qty)
    (Float.to_string order.filled)
    (Float.to_string order.qty)
    (Sexp.to_string_hum (Fluxum.Types.Order_status.sexp_of_t order.status))

(* Helper to print Trade.t *)
let print_trade (trade : Fluxum.Types.Trade.t) =
  printf "  %s | %s | %s %.8f @ %.2f | fee: %s\n"
    (Option.value trade.trade_id ~default:"-")
    trade.symbol
    (Fluxum.Types.Side.to_string trade.side)
    trade.qty
    trade.price
    (Option.value_map trade.fee ~default:"-" ~f:Float.to_string)

(* Helper to print Symbol_info.t *)
let print_symbol_info (info : Fluxum.Types.Symbol_info.t) =
  printf "  %s | %s/%s | status: %s | min: %.8f\n"
    info.symbol
    info.base_currency
    info.quote_currency
    info.status
    info.min_order_size

(* Helper to print Ticker.t *)
let print_ticker (t : Fluxum.Types.Ticker.t) =
  printf "  Symbol: %s\n" t.symbol;
  printf "  Last: %.8f | Bid: %.8f | Ask: %.8f\n" t.last_price t.bid_price t.ask_price;
  printf "  High 24h: %.8f | Low 24h: %.8f\n" t.high_24h t.low_24h;
  printf "  Volume 24h: %.8f\n" t.volume_24h;
  Option.iter t.price_change_pct ~f:(fun pct ->
    printf "  Change 24h: %.2f%%\n" pct)

(* Helper to print Order_book.t *)
let print_order_book (book : Fluxum.Types.Order_book.t) =
  printf "  Symbol: %s (epoch: %d)\n" book.symbol book.epoch;
  printf "  Asks:\n";
  List.take book.asks 5 |> List.iter ~f:(fun (level : Fluxum.Types.Order_book.Price_level.t) ->
    printf "    %.8f @ %.8f\n" level.volume level.price);
  let spread = Fluxum.Types.Order_book.spread book in
  printf "  --- Spread: %.8f ---\n" spread;
  printf "  Bids:\n";
  List.take book.bids 5 |> List.iter ~f:(fun (level : Fluxum.Types.Order_book.Price_level.t) ->
    printf "    %.8f @ %.8f\n" level.volume level.price)

(* Helper to print Public_trade.t *)
let print_public_trade (t : Fluxum.Types.Public_trade.t) =
  let side_str = match t.side with
    | Some s -> Fluxum.Types.Side.to_string s
    | None -> "?"
  in
  printf "  %s | %s %.8f @ %.8f\n"
    (Option.value t.trade_id ~default:"-")
    side_str
    t.qty
    t.price

(* Unified adapter helpers *)
module Unified = struct
  (* Create Gemini adapter *)
  let gemini_adapter cfg_env =
    let cfg = Gemini.Cfg.of_string cfg_env in
    Gemini.Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
    return (Gemini.Fluxum_adapter.Adapter.create ~cfg ~nonce ())

  (* Create Kraken adapter *)
  let kraken_adapter cfg_env ~symbols =
    let cfg = Kraken.Cfg.of_string cfg_env in
    Deferred.return (Kraken.Fluxum_adapter.Adapter.create ~cfg ~symbols ())

  (* Create MEXC adapter *)
  let mexc_adapter ~symbols =
    let cfg = Mexc.Cfg.or_default None in
    Deferred.return (Mexc.Fluxum_adapter.Adapter.create ~cfg ~symbols ())

  (* Get open orders *)
  let get_open_orders ~exchange ~cfg_env ?symbol () =
    match exchange with
    | "gemini" ->
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| Result.map ~f:(List.map ~f:Gemini.Fluxum_adapter.Adapter.Normalize.order_from_status)
      >>| Result.map_error ~f:Gemini.Fluxum_adapter.Adapter.Normalize.error
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.order_from_status)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      let symbols = match symbol with Some s -> [s] | None -> [] in
      mexc_adapter ~symbols >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.order_from_status)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get order history *)
  let get_order_history ~exchange ~cfg_env ?symbol ?limit () =
    match exchange with
    | "gemini" ->
      (* Gemini doesn't support order history *)
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = "Gemini does not support order history" }))
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_order_history adapter ?symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.order_from_status)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      let symbols = match symbol with Some s -> [s] | None -> [] in
      mexc_adapter ~symbols >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_order_history adapter ?symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.order_from_status)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get my trades *)
  let get_my_trades ~exchange ~cfg_env ~symbol ?limit () =
    match exchange with
    | "gemini" ->
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.get_my_trades adapter ~symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Gemini.Fluxum_adapter.Adapter.Normalize.trade)
      >>| Result.map_error ~f:Gemini.Fluxum_adapter.Adapter.Normalize.error
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_my_trades adapter ~symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.trade)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[symbol] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_my_trades adapter ~symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.trade)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get symbols *)
  let get_symbols ~exchange ~cfg_env () =
    match exchange with
    | "gemini" ->
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.get_symbols adapter ()
      >>| Result.map ~f:(List.map ~f:Gemini.Fluxum_adapter.Adapter.Normalize.symbol_info)
      >>| Result.map_error ~f:Gemini.Fluxum_adapter.Adapter.Normalize.error
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_symbols adapter ()
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.symbol_info)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_symbols adapter ()
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.symbol_info)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get balances (using new adapter) *)
  let get_balances ~exchange ~cfg_env () =
    match exchange with
    | "gemini" ->
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.balances adapter
      >>| Result.map ~f:(List.map ~f:Gemini.Fluxum_adapter.Adapter.Normalize.balance)
      >>| Result.map_error ~f:Gemini.Fluxum_adapter.Adapter.Normalize.error
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.balances adapter
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.balance)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.balances adapter
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.balance)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get ticker *)
  let get_ticker ~exchange ~cfg_env ~symbol () =
    match exchange with
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_ticker adapter ~symbol ()
      >>| Result.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.ticker
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[symbol] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_ticker adapter ~symbol ()
      >>| Result.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.ticker
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | "gemini" ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = "Gemini ticker endpoint not implemented" }))
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get order book *)
  let get_order_book ~exchange ~cfg_env ~symbol ?limit () =
    match exchange with
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_order_book adapter ~symbol ?limit ()
      >>| Result.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.order_book
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[symbol] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_order_book adapter ~symbol ?limit ()
      >>| Result.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.order_book
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | "gemini" ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = "Gemini order book endpoint not implemented" }))
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Get recent trades *)
  let get_recent_trades ~exchange ~cfg_env ~symbol ?limit () =
    match exchange with
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_recent_trades adapter ~symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Kraken.Fluxum_adapter.Adapter.Normalize.public_trade)
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      mexc_adapter ~symbols:[symbol] >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_recent_trades adapter ~symbol ?limit ()
      >>| Result.map ~f:(List.map ~f:Mexc.Fluxum_adapter.Adapter.Normalize.public_trade)
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | "gemini" ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = "Gemini recent trades endpoint not implemented" }))
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))

  (* Cancel all orders *)
  let cancel_all_orders ~exchange ~cfg_env ?symbol () =
    match exchange with
    | "gemini" ->
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.cancel_all_orders adapter ?symbol ()
      >>| Result.map_error ~f:Gemini.Fluxum_adapter.Adapter.Normalize.error
    | "kraken" ->
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.cancel_all_orders adapter ?symbol ()
      >>| Result.map_error ~f:Kraken.Fluxum_adapter.Adapter.Normalize.error
    | "mexc" ->
      let symbols = match symbol with Some s -> [s] | None -> [] in
      mexc_adapter ~symbols >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.cancel_all_orders adapter ?symbol ()
      >>| Result.map_error ~f:Mexc.Fluxum_adapter.Adapter.Normalize.error
    | _ ->
      Deferred.return (Error (Fluxum.Types.Error.Exchange_specific
        { venue = Fluxum.Types.Venue.Gemini; code = "unsupported"; message = sprintf "Exchange %s not supported" exchange }))
end

(* Generic API command that accepts --exchange flag *)
let api_command =
  Command.group ~summary:"Unified exchange API commands"
    [ ("open-orders", Command.async
        ~summary:"Get open orders from an exchange"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (gemini, kraken, mexc)"
          and symbol = flag "--symbol" (optional string)
              ~doc:"STRING filter by trading pair"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol cfg () ->
            Unified.get_open_orders ~exchange ~cfg_env:cfg ?symbol () >>= function
            | Ok orders ->
              printf "Open Orders (%s):\n" exchange;
              List.iter orders ~f:print_order;
              if List.is_empty orders then printf "  (none)\n";
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> cfg
        )))
    ; ("order-history", Command.async
        ~summary:"Get closed/historical orders"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (kraken, mexc)"
          and symbol = flag "--symbol" (optional string)
              ~doc:"STRING filter by trading pair"
          and limit = flag "--limit" (optional int)
              ~doc:"INT max number of orders"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol limit cfg () ->
            Unified.get_order_history ~exchange ~cfg_env:cfg ?symbol ?limit () >>= function
            | Ok orders ->
              printf "Order History (%s):\n" exchange;
              List.iter orders ~f:print_order;
              if List.is_empty orders then printf "  (none)\n";
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> limit <*> cfg
        )))
    ; ("my-trades", Command.async
        ~summary:"Get user's trade history"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (gemini, kraken, mexc)"
          and symbol = flag "--symbol" (required string)
              ~doc:"STRING trading pair (required)"
          and limit = flag "--limit" (optional int)
              ~doc:"INT max number of trades"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol limit cfg () ->
            Unified.get_my_trades ~exchange ~cfg_env:cfg ~symbol ?limit () >>= function
            | Ok trades ->
              printf "My Trades (%s - %s):\n" exchange symbol;
              List.iter trades ~f:print_trade;
              if List.is_empty trades then printf "  (none)\n";
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> limit <*> cfg
        )))
    ; ("symbols", Command.async
        ~summary:"Get available trading pairs"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (gemini, kraken, mexc)"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            Unified.get_symbols ~exchange ~cfg_env:cfg () >>= function
            | Ok symbols ->
              printf "Symbols (%s): %d pairs\n" exchange (List.length symbols);
              List.iter symbols ~f:print_symbol_info;
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> cfg
        )))
    ; ("balances", Command.async
        ~summary:"Get account balances (unified)"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "gemini" string)
              ~doc:"STRING exchange name (gemini, kraken, mexc)"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            Unified.get_balances ~exchange ~cfg_env:cfg () >>= function
            | Ok balances ->
              printf "Balances (%s):\n" exchange;
              List.iter balances ~f:(fun b ->
                if Float.(b.total > 0.) then
                  printf "  %s: %.8f (available: %.8f, locked: %.8f)\n"
                    b.currency b.total b.available b.locked);
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> cfg
        )))
    ; ("ticker", Command.async
        ~summary:"Get 24hr ticker for a symbol"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (kraken, mexc)"
          and symbol = flag "--symbol" (required string)
              ~doc:"STRING trading pair (e.g., XETHZUSD for Kraken, BTCUSDT for MEXC)"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol cfg () ->
            Unified.get_ticker ~exchange ~cfg_env:cfg ~symbol () >>= function
            | Ok ticker ->
              printf "Ticker (%s - %s):\n" exchange symbol;
              print_ticker ticker;
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> cfg
        )))
    ; ("order-book", Command.async
        ~summary:"Get order book depth for a symbol"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (kraken, mexc)"
          and symbol = flag "--symbol" (required string)
              ~doc:"STRING trading pair"
          and limit = flag "--limit" (optional int)
              ~doc:"INT number of levels"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol limit cfg () ->
            Unified.get_order_book ~exchange ~cfg_env:cfg ~symbol ?limit () >>= function
            | Ok book ->
              printf "Order Book (%s - %s):\n" exchange symbol;
              print_order_book book;
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> limit <*> cfg
        )))
    ; ("recent-trades", Command.async
        ~summary:"Get recent public trades for a symbol"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (kraken, mexc)"
          and symbol = flag "--symbol" (required string)
              ~doc:"STRING trading pair"
          and limit = flag "--limit" (optional int)
              ~doc:"INT number of trades"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol limit cfg () ->
            Unified.get_recent_trades ~exchange ~cfg_env:cfg ~symbol ?limit () >>= function
            | Ok trades ->
              printf "Recent Trades (%s - %s): %d trades\n" exchange symbol (List.length trades);
              List.take trades 10 |> List.iter ~f:print_public_trade;
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> limit <*> cfg
        )))
    ; ("cancel-all", Command.async
        ~summary:"Cancel all open orders"
        (Command.Param.(
          let exchange = flag "--exchange" (optional_with_default "kraken" string)
              ~doc:"STRING exchange name (gemini, kraken, mexc)"
          and symbol = flag "--symbol" (optional string)
              ~doc:"STRING filter by trading pair (required for MEXC)"
          and cfg = flag "-cfg" (optional_with_default "production" string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange symbol cfg () ->
            Unified.cancel_all_orders ~exchange ~cfg_env:cfg ?symbol () >>= function
            | Ok count ->
              printf "Cancelled %d orders on %s\n" count exchange;
              Deferred.unit
            | Error err ->
              eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
              Deferred.unit)
          <*> exchange <*> symbol <*> cfg
        )))
    (* Legacy commands for backwards compatibility *)
    ; ("order", Command.async
        ~summary:"[DEPRECATED] Use open-orders instead"
        (Command.Param.(
          let exchange = flag "--exchange" (optional string)
              ~doc:"STRING exchange name (gemini, kraken, binance, coinbase, mexc)"
          in
          let cfg = flag "-cfg" (optional string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            let cfg_env = match cfg with Some c -> c | None -> "production" in
            (match Option.value exchange ~default:"kraken" with
            | "kraken" ->
              let cfg = Kraken.Cfg.of_string cfg_env in
              let module Cfg = (val cfg : Kraken.Cfg.S) in
              Kraken.V1.open_orders (module Cfg) () >>= (function
                | `Ok json ->
                  printf "Open Orders:\n%s\n" (Yojson.Safe.pretty_to_string json);
                  Deferred.unit
                | #Kraken.Rest.Error.post as err ->
                  eprintf "Error: %s\n" (Sexp.to_string_hum (Kraken.Rest.Error.sexp_of_post err));
                  Deferred.unit)
            | "gemini" ->
              eprintf "Order queries not yet implemented for Gemini\n";
              Deferred.unit
            | exch ->
              eprintf "Exchange %s not yet implemented\n" exch;
              Deferred.unit))
          <*> exchange
          <*> cfg
        )))
    ; ("balance", Command.async
        ~summary:"Get balances across exchanges"
        (Command.Param.(
          let exchange = flag "--exchange" (optional string)
              ~doc:"STRING exchange name (gemini, kraken, binance, coinbase, mexc)"
          in
          let cfg = flag "-cfg" (optional string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            let cfg_env = match cfg with Some c -> c | None -> "production" in
            match Option.value exchange ~default:"gemini" with
            | "gemini" ->
              let cfg = Gemini.Cfg.of_string cfg_env in
              get_gemini_balances cfg >>= fun result ->
              (match result with
               | Ok balances ->
                 List.iter balances ~f:(fun balance ->
                   printf "%s\n" (Sexp.to_string_hum (Fluxum.Types.Balance.sexp_of_t balance)));
                 Deferred.unit
               | Error err ->
                 eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
                 Deferred.unit)
            | "kraken" ->
              let cfg = Kraken.Cfg.of_string cfg_env in
              get_kraken_balances cfg >>= fun result ->
              (match result with
               | Ok balances ->
                 List.iter balances ~f:(fun balance ->
                   printf "%s\n" (Sexp.to_string_hum (Fluxum.Types.Balance.sexp_of_t balance)));
                 Deferred.unit
               | Error err ->
                 eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
                 Deferred.unit)
            | exch ->
              eprintf "Exchange %s not yet implemented\n" exch;
              Deferred.unit)
          <*> exchange
          <*> cfg
        )))
    ]

(* Main command structure *)
let command =
  Command.group ~summary:"Fluxum - Multi-exchange trading API"
    [ ("gemini", gemini_command)
    ; ("kraken", kraken_command)
    ; ("binance", binance_command)
    ; ("mexc", mexc_command)
    ; ("hyperliquid", hyperliquid_command)
    ; ("coinbase", coinbase_command)
    ; ("bitrue", bitrue_command)
    ; ("api", api_command)
    ]

let () = Command_unix.run command

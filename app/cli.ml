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
let dydx_command = Dydx.command
let jupiter_command = Jupiter.command
let oneinch_command = Oneinch.command

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
      let result_transpose results =
        List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
          match res, acc with
          | Ok v, Ok vs -> Ok (v :: vs)
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      in
      gemini_adapter cfg_env >>= fun adapter ->
      Gemini.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| (function
        | Ok orders ->
          let normalized = List.map orders ~f:Gemini.Fluxum_adapter.Adapter.Normalize.order_from_status in
          (match result_transpose normalized with
           | Ok ords -> Ok ords
           | Error msg -> Error (Fluxum.Types.Error.Normalization_error msg))
        | Error e -> Error (Gemini.Fluxum_adapter.Adapter.Normalize.error e))
    | "kraken" ->
      let result_transpose results =
        List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
          match res, acc with
          | Ok v, Ok vs -> Ok (v :: vs)
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      in
      kraken_adapter cfg_env ~symbols:[] >>= fun adapter ->
      Kraken.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| (function
        | Ok orders ->
          let normalized = List.map orders ~f:Kraken.Fluxum_adapter.Adapter.Normalize.order_from_status in
          (match result_transpose normalized with
           | Ok ords -> Ok ords
           | Error msg -> Error (Fluxum.Types.Error.Normalization_error msg))
        | Error e -> Error (Kraken.Fluxum_adapter.Adapter.Normalize.error e))
    | "mexc" ->
      let symbols = match symbol with Some s -> [s] | None -> [] in
      mexc_adapter ~symbols >>= fun adapter ->
      Mexc.Fluxum_adapter.Adapter.get_open_orders adapter ?symbol ()
      >>| (function
        | Ok orders ->
          let normalized = List.map orders ~f:Mexc.Fluxum_adapter.Adapter.Normalize.order_from_status in
          let result_transpose results =
            List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
              match res, acc with
              | Ok v, Ok vs -> Ok (v :: vs)
              | Error e, _ -> Error e
              | _, Error e -> Error e)
          in
          (match result_transpose normalized with
           | Ok ords -> Ok ords
           | Error msg -> Error (Fluxum.Types.Error.Normalization_error msg))
        | Error e -> Error (Mexc.Fluxum_adapter.Adapter.Normalize.error e))
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
              (match List.is_empty orders with true -> printf "  (none)\n" | false -> ());
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
              (match List.is_empty orders with true -> printf "  (none)\n" | false -> ());
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
              (match List.is_empty trades with true -> printf "  (none)\n" | false -> ());
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
                match Float.(b.total > 0.) with
                | true ->
                  printf "  %s: %.8f (available: %.8f, locked: %.8f)\n"
                    b.currency b.total b.available b.locked
                | false -> ());
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

(* Backtest command group *)
let backtest_command =
  Command.group ~summary:"Backtesting commands"
    [ ("run", Command.async
        ~summary:"Run a backtest with a strategy"
        (Command.Param.(
          map
            (both
              (both
                (both
                  (flag "-strategy" (required string)
                     ~doc:"NAME Strategy to run (sma-crossover, momentum, buy-and-hold)")
                  (flag "-symbol" (optional_with_default "BTCUSD" string)
                     ~doc:"SYMBOL Trading symbol (default: BTCUSD)"))
                (both
                  (flag "-data" (optional string)
                     ~doc:"FILE CSV file with OHLCV data (or use -synthetic)")
                  (flag "-synthetic" no_arg
                     ~doc:" Use synthetic test data")))
              (both
                (both
                  (flag "-balance" (optional_with_default 10000. float)
                     ~doc:"AMOUNT Initial balance (default: 10000)")
                  (flag "-slippage" (optional_with_default 0.001 float)
                     ~doc:"PCT Slippage percentage (default: 0.001)"))
                (flag "-commission" (optional_with_default 0.001 float)
                   ~doc:"PCT Commission percentage (default: 0.001)")))
            ~f:(fun (((strategy, symbol), (data_file, use_synthetic)), ((initial_balance, slippage), commission)) () ->
              (* Get strategy by name *)
              let strategy_opt = Backtest_strategies.get strategy in
              match strategy_opt with
              | None ->
                eprintf "Unknown strategy: %s\n" strategy;
                eprintf "Available: sma-crossover, momentum, buy-and-hold\n";
                Deferred.return ()
              | Some strategy_module ->
                (* Load or generate candle data *)
                let%bind candles =
                  match data_file, use_synthetic with
                  | Some path, _ ->
                    Backtest.Data_source.Csv.load_from_file ~symbol ~path
                  | None, true ->
                    (* Generate synthetic data for testing *)
                    let start = Backtest.Data_source.parse_time "2024-01-01" in
                    let end_ = Backtest.Data_source.parse_time "2024-06-01" in
                    let interval = Backtest.Interval.hour_1 in
                    Deferred.return (Backtest.Data_source.Synthetic.trending
                      ~symbol ~start ~end_ ~interval
                      ~initial_price:45000. ~trend_pct:0.0003 ~volatility:0.015 ())
                  | None, false ->
                    eprintf "Must specify -data FILE or -synthetic\n";
                    Deferred.return []
                in
                match candles with
                | [] -> Deferred.return ()
                | _ ->
                  (* Configure and run backtest *)
                  let config = Backtest.Engine.Config.create
                      ~initial_balance
                      ~slippage_pct:slippage
                      ~commission_pct:commission
                      ()
                  in
                  let%bind result = Backtest.Engine.run_packed
                      ~strategy:strategy_module
                      ~config
                      ~candles
                      ()
                  in
                  (match result with
                   | Ok r ->
                     printf "\n";
                     Backtest.Result.print_summary r;
                     printf "\n"
                   | Error e ->
                     eprintf "Backtest error: %s\n" e);
                  Deferred.return ()))))
    ; ("list-strategies", Command.async
        ~summary:"List available strategies"
        (Command.Param.return (fun () ->
           printf "Available strategies:\n";
           List.iter (Backtest_strategies.list ()) ~f:(fun (name, desc) ->
             printf "  %-20s %s\n" name desc);
           Deferred.return ())))
    ; ("test", Command.async
        ~summary:"Run a quick test with synthetic data"
        (Command.Param.return (fun () ->
           let symbol = "BTCUSD" in
           let start = Backtest.Data_source.parse_time "2024-01-01" in
           let end_ = Backtest.Data_source.parse_time "2024-03-01" in
           let interval = Backtest.Interval.hour_1 in
           let candles = Backtest.Data_source.Synthetic.trending
               ~symbol ~start ~end_ ~interval
               ~initial_price:45000. ~trend_pct:0.0005 ~volatility:0.01 ()
           in
           printf "Generated %d candles\n\n" (List.length candles);

           let config = Backtest.Engine.Config.create
               ~initial_balance:10000.
               ~slippage_pct:0.001
               ~commission_pct:0.001
               ()
           in

           (* Run each strategy sequentially *)
           let rec run_strategies = function
             | [] -> Deferred.return ()
             | name :: rest ->
               match Backtest_strategies.get name with
               | None -> run_strategies rest
               | Some strategy ->
                 printf "=== %s ===\n" name;
                 let%bind result = Backtest.Engine.run_packed ~strategy ~config ~candles () in
                 (match result with
                  | Ok r ->
                    printf "Return: %.2f%%  Sharpe: %.2f  Trades: %d\n\n"
                      (r.metrics.total_return *. 100.)
                      r.metrics.sharpe_ratio
                      (List.length r.trades)
                  | Error e ->
                    eprintf "Error: %s\n\n" e);
                 run_strategies rest
           in
           run_strategies ["buy-and-hold"; "sma-crossover"; "momentum"])))
    ]

(* Bot command group *)
let bot_command =
  Command.group ~summary:"Trading bot framework commands"
    [ ("start", Command.async
        ~summary:"Start a trading bot with a strategy"
        (Command.Param.(
          let bot_id = flag "--bot-id" (optional_with_default "fluxum-bot" string)
              ~doc:"STRING Bot identifier"
          and symbols = flag "--symbols" (optional_with_default "BTCUSD" string)
              ~doc:"STRING Comma-separated symbols (default: BTCUSD)"
          and venue = flag "--venue" (optional_with_default "gemini" string)
              ~doc:"STRING Exchange venue (default: gemini)"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          and dashboard = flag "--dashboard" no_arg
              ~doc:" Enable TUI dashboard"
          in
          return (fun bot_id symbols venue event_path dashboard () ->
            let symbols = String.split symbols ~on:',' in
            let venue = match String.lowercase venue with
              | "gemini" -> Bot.Event.Venue.Gemini
              | "kraken" -> Bot.Event.Venue.Kraken
              | "mexc" -> Bot.Event.Venue.Mexc
              | "binance" -> Bot.Event.Venue.Binance
              | "coinbase" -> Bot.Event.Venue.Coinbase
              | "hyperliquid" -> Bot.Event.Venue.Hyperliquid
              | "bitrue" -> Bot.Event.Venue.Bitrue
              | "dydx" -> Bot.Event.Venue.Dydx
              | other -> Bot.Event.Venue.Other other
            in
            let config = Bot.Engine.Config.{
              default with
              bot_id;
              symbols;
              venues = [venue];
              event_store_path = event_path;
              enable_dashboard = dashboard;
            } in
            printf "Starting bot '%s' with symbols: %s\n" bot_id (String.concat ~sep:", " symbols);
            printf "Event store: %s\n" event_path;
            let%bind engine = Bot.Engine.create config in

            (* Set up noop strategy for now *)
            let strategy = Bot.Engine.Strategy_wrapper.wrap
              (module Bot.Strategy_intf.Noop) ()
            in
            Bot.Engine.set_strategy engine strategy;

            (* Start the engine *)
            let%bind result = Bot.Engine.start engine in
            (match result with
            | Ok () ->
              printf "Bot started successfully!\n";
              (match dashboard with
              | true ->
                let dash = Bot.Dashboard.create Bot.Dashboard.Config.default in
                Bot.Dashboard.run_with_engine dash engine
              | false ->
                (* Keep running until Ctrl-C *)
                printf "Press Ctrl+C to stop...\n";
                Deferred.never ())
            | Error e ->
              eprintf "Failed to start: %s\n" (Error.to_string_hum e);
              Deferred.unit))
          <*> bot_id <*> symbols <*> venue <*> event_path <*> dashboard
        )))
    ; ("status", Command.async
        ~summary:"Show status of a bot from event store"
        (Command.Param.(
          let bot_id = flag "--bot-id" (required string)
              ~doc:"STRING Bot identifier"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          in
          return (fun bot_id event_path () ->
            printf "Replaying events for bot '%s'...\n" bot_id;
            let%bind state = Bot.Engine.replay ~base_path:event_path ~bot_id in
            let summary = Bot.State.summary state in
            printf "%s\n" summary;
            printf "\n%s" (Bot.Dashboard.text_summary state);
            Deferred.unit)
          <*> bot_id <*> event_path
        )))
    ; ("replay", Command.async
        ~summary:"Replay events to reconstruct state"
        (Command.Param.(
          let bot_id = flag "--bot-id" (required string)
              ~doc:"STRING Bot identifier"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          and output = flag "--output" (optional string)
              ~doc:"FILE Output state to file"
          in
          return (fun bot_id event_path output () ->
            printf "Replaying events for bot '%s' from %s...\n" bot_id event_path;
            let%bind state = Bot.Engine.replay ~base_path:event_path ~bot_id in
            let summary = Bot.State.summary state in
            printf "\nFinal state:\n%s\n" summary;
            (match output with
            | Some path ->
              let contents = Bot.Dashboard.text_summary state in
              let%bind () = Writer.save path ~contents in
              printf "State saved to %s\n" path;
              Deferred.unit
            | None ->
              Deferred.unit))
          <*> bot_id <*> event_path <*> output
        )))
    ; ("export", Command.async
        ~summary:"Export events to CSV/JSON for Python analysis"
        (Command.Param.(
          let bot_id = flag "--bot-id" (required string)
              ~doc:"STRING Bot identifier"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          and output = flag "--output" (required string)
              ~doc:"FILE Output file path"
          and format = flag "--format" (optional_with_default "csv" string)
              ~doc:"FORMAT Output format: csv, jsonl (default: csv)"
          in
          return (fun bot_id event_path output format () ->
            printf "Exporting events for bot '%s'...\n" bot_id;
            let%bind files = Bot.Event_store.list_event_files ~base_path:event_path ~bot_id in
            (match files with
            | [] ->
              eprintf "No event files found for bot '%s' in %s\n" bot_id event_path;
              Deferred.unit
            | _ ->
              printf "Found %d event files\n" (List.length files);
              let%bind all_events =
                Deferred.List.concat_map files ~how:`Sequential ~f:(fun path ->
                  let%bind reader = Bot.Event_store.Reader.open_ ~path in
                  let%bind events = Bot.Event_store.Reader.read_all reader in
                  let%bind () = Bot.Event_store.Reader.close reader in
                  Deferred.return events)
              in
              printf "Total events: %d\n" (List.length all_events);
              let format = match String.lowercase format with
                | "jsonl" | "json" -> Bot.Parquet_export.Config.Json_lines
                | _ -> Bot.Parquet_export.Config.Csv
              in
              let config = Bot.Parquet_export.Config.{ default with output_path = output; format } in
              let%bind () = Bot.Parquet_export.export ~events:all_events ~config in
              printf "Exported to %s\n" output;
              (* Also export summary stats *)
              let stats = Bot.Parquet_export.Stats.of_events all_events in
              printf "\n%s" (Bot.Parquet_export.Stats.to_string stats);
              Deferred.unit))
          <*> bot_id <*> event_path <*> output <*> format
        )))
    ; ("stats", Command.async
        ~summary:"Show statistics for event store"
        (Command.Param.(
          let bot_id = flag "--bot-id" (required string)
              ~doc:"STRING Bot identifier"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          in
          return (fun bot_id event_path () ->
            printf "Getting stats for bot '%s'...\n\n" bot_id;
            let%bind stats = Bot.Event_store.Stats.for_bot ~base_path:event_path ~bot_id in
            printf "Bot ID: %s\n" stats.bot_id;
            printf "Total Events: %d\n" stats.total_events;
            printf "Total Size: %d bytes (%.2f MB)\n"
              stats.total_bytes
              (Float.of_int stats.total_bytes /. 1_000_000.);
            (match stats.time_range with
            | Some (first, last) ->
              printf "Time Range: %s to %s\n"
                (Bot.Event.Time.to_string first)
                (Bot.Event.Time.to_string last)
            | None -> ());
            printf "\nFiles:\n";
            List.iter stats.files ~f:(fun f ->
              printf "  %s: %d events, %d bytes\n" f.path f.event_count f.size_bytes);
            Deferred.unit)
          <*> bot_id <*> event_path
        )))
    ; ("dashboard", Command.async
        ~summary:"Show dashboard for a running or completed bot"
        (Command.Param.(
          let bot_id = flag "--bot-id" (required string)
              ~doc:"STRING Bot identifier"
          and event_path = flag "--events" (optional_with_default "./events" string)
              ~doc:"PATH Event store path (default: ./events)"
          in
          return (fun bot_id event_path () ->
            let%bind state = Bot.Engine.replay ~base_path:event_path ~bot_id in
            let dash = Bot.Dashboard.create Bot.Dashboard.Config.default in
            let output = Bot.Dashboard.render_once dash state in
            print_string output;
            Deferred.unit)
          <*> bot_id <*> event_path
        )))
    ; ("python-helper", Command.async
        ~summary:"Generate Python helper script for loading exported data"
        (Command.Param.(
          let output_dir = flag "--output" (optional_with_default "." string)
              ~doc:"DIR Output directory (default: .)"
          in
          return (fun output_dir () ->
            let%bind () = Bot.Parquet_export.write_python_helper ~output_dir in
            printf "Python helper script written to %s/load_events.py\n" output_dir;
            Deferred.unit)
          <*> output_dir
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
    ; ("dydx", dydx_command)
    ; ("jupiter", jupiter_command)
    ; ("1inch", oneinch_command)
    ; ("api", api_command)
    ; ("backtest", backtest_command)
    ; ("bot", bot_command)
    ]

let () = Command_unix.run command

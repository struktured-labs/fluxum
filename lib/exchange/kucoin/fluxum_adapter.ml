(** KuCoin Fluxum Adapter

    Normalizes KuCoin native types to Fluxum types and provides CLI commands.

    {b Features:}
    - ✅ REST market data (ticker, order book, trades, symbols)
    - ✅ REST trading (limit/market orders, cancel, order status)
    - ✅ Account balances

    {b Authentication:}
    - HMAC-SHA256 with passphrase via KUCOIN_API_KEY / KUCOIN_API_SECRET /
      KUCOIN_PASSPHRASE environment variables

    {b Symbol Format:}
    - Hyphen-separated uppercase: "BTC-USDT", "ETH-USDT" *)

open Core
open Async
module Native_types = Types
module Types = Fluxum.Types

module Adapter = struct
  type t =
    { cfg: Cfg.t
    ; rate_limiter: Exchange_common.Rate_limiter.t }

  let create ~cfg () =
    { cfg
    ; rate_limiter=
        Exchange_common.Rate_limiter.create
          ~config:Exchange_common.Rate_limiter.Configs.kucoin
          () }

  module Venue = struct
    let t = Types.Venue.Kucoin
  end

  module Normalize = struct
    let float_of_string_safe s =
      match Float.of_string_opt s with
      | Some f -> Ok f
      | None -> Error (sprintf "invalid numeric string: %S" s)

    let ticker (t : Native_types.ticker) : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind last_price = float_of_string_safe t.last in
      let%bind bid_price = float_of_string_safe t.buy in
      let%bind ask_price = float_of_string_safe t.sell in
      let%bind high_24h = float_of_string_safe t.high in
      let%bind low_24h = float_of_string_safe t.low in
      let%bind volume_24h = float_of_string_safe t.vol in
      let quote_volume = Float.of_string_opt t.volValue in
      let price_change = Float.of_string_opt t.changePrice in
      let price_change_pct =
        match Float.of_string_opt t.changeRate with
        | Some r -> Some (r *. 100.0)
        | None -> None
      in
      let ts =
        Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_int64 t.time))
      in
      Ok
        (Types.Ticker.create
           ~venue:Venue.t
           ~symbol:t.symbol
           ~last_price
           ~bid_price
           ~ask_price
           ~high_24h
           ~low_24h
           ~volume_24h
           ?quote_volume
           ?price_change
           ?price_change_pct
           ~ts
           ())

    let balance (b : Native_types.balance) : (Types.Balance.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind total = float_of_string_safe b.balance in
      let%bind available = float_of_string_safe b.available in
      let%bind holds = float_of_string_safe b.holds in
      Ok
        ({ venue= Venue.t
         ; currency= String.uppercase b.currency
         ; total
         ; available
         ; locked= holds }
         : Types.Balance.t)

    let public_trade (t : Native_types.trade) ~symbol
      : (Types.Public_trade.t, string) Result.t
      =
      let open Result.Let_syntax in
      let%bind price = float_of_string_safe t.price in
      let%bind qty = float_of_string_safe t.size in
      let side =
        match String.lowercase t.side with
        | "buy" -> Some Types.Side.Buy
        | "sell" -> Some Types.Side.Sell
        | _ -> None
      in
      let ts =
        Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ns (Float.of_int64 t.time))
      in
      Ok
        (Types.Public_trade.create
           ~venue:Venue.t
           ~symbol
           ~price
           ~qty
           ?side
           ~trade_id:t.sequence
           ~ts
           ())

    let order (o : Native_types.order) : (Types.Order.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind qty = float_of_string_safe o.size in
      let%bind filled = float_of_string_safe o.dealSize in
      let side =
        match String.lowercase o.side with
        | "buy" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let status =
        match o.isActive, o.cancelExist with
        | true, _ -> Types.Order_status.New
        | false, true -> Types.Order_status.Canceled
        | false, false ->
          (match Float.(filled >= qty) with
           | true -> Types.Order_status.Filled
           | false -> Types.Order_status.Partially_filled)
      in
      let%bind kind =
        match String.lowercase o.type_ with
        | "market" -> Ok (Types.Order_kind.Basic Market)
        | _ ->
          let%bind price = float_of_string_safe o.price in
          Ok (Types.Order_kind.Basic (Limit price))
      in
      let ts =
        Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_ms (Float.of_int64 o.createdAt))
      in
      Ok
        ({ venue= Venue.t
         ; id= o.id
         ; symbol= o.symbol
         ; side
         ; kind
         ; time_in_force= Types.Time_in_force.GTC
         ; qty
         ; filled
         ; status
         ; created_at= Some ts
         ; updated_at= None }
         : Types.Order.t)

    let error (e : Rest.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= Int.to_string code; message= msg }
      | `Json_parse msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "json"; message= msg }
      | `Api_error msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "api"; message= msg }
      | `Network msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "network"; message= msg }
  end

  (** {2 Market Data Operations} *)

  let ticker (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.ticker ~cfg:t.cfg ~symbol
      >>| function
      | Ok native ->
        (match Normalize.ticker native with
         | Ok n -> Ok n
         | Error err -> Error (`Json_parse err))
      | Error e -> Error e)

  let order_book (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.order_book ~cfg:t.cfg ~symbol
      >>| function
      | Ok book -> Ok book
      | Error e -> Error e)

  let trades (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.trades ~cfg:t.cfg ~symbol
      >>| function
      | Ok native_trades ->
        let normalized =
          List.filter_map native_trades ~f:(fun tr ->
            match Normalize.public_trade tr ~symbol with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error "kucoin trade normalize error: %s" err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let balances (t : t) =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.accounts ~cfg:t.cfg
      >>| function
      | Ok native_balances ->
        let normalized =
          List.filter_map native_balances ~f:(fun b ->
            match Normalize.balance b with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error "kucoin balance normalize error: %s" err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let place_limit_order (t : t) ~symbol ~side ~price ~size =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.place_limit_order ~cfg:t.cfg ~symbol ~side ~price ~size
      >>| function
      | Ok r -> Ok r
      | Error e -> Error e)

  let place_market_order (t : t) ~symbol ~side ~size =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.place_market_order ~cfg:t.cfg ~symbol ~side ~size
      >>| function
      | Ok r -> Ok r
      | Error e -> Error e)

  let cancel_order (t : t) ~order_id =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.cancel_order ~cfg:t.cfg ~order_id
      >>| function
      | Ok _ -> Ok ()
      | Error e -> Error e)

  let order_details (t : t) ~order_id =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.order_details ~cfg:t.cfg ~order_id
      >>| function
      | Ok native ->
        (match Normalize.order native with
         | Ok n -> Ok n
         | Error err -> Error (`Json_parse err))
      | Error e -> Error e)
end

(** {1 CLI Commands} *)

let cfg_param_env =
  Command.Param.(
    flag "-env" (optional_with_default "production" string)
      ~doc:"STRING environment (default: production)")

let cfg_of_env _env = Cfg.production

let cfg_param_auth =
  let open Command.Let_syntax in
  [%map_open
    let _env = cfg_param_env in
    let cfg = Cfg.production in
    match
      Unix.getenv "KUCOIN_API_KEY"
    , Unix.getenv "KUCOIN_API_SECRET"
    , Unix.getenv "KUCOIN_PASSPHRASE"
    with
    | Some key, Some secret, Some pass ->
      Cfg.with_auth ~api_key:key ~api_secret:secret ~passphrase:pass cfg
    | _ -> cfg]

let ticker_command =
  let open Command.Let_syntax in
  ( "ticker"
  , Command.async
      ~summary:"Get KuCoin 24hr ticker"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string)
            ~doc:"STRING trading pair (e.g., BTC-USDT)"
        in
        fun () ->
          let cfg = cfg_of_env "production" in
          let adapter = Adapter.create ~cfg () in
          Adapter.ticker adapter ~symbol
          >>= function
          | Ok t ->
            printf "%s  last=%.8g  bid=%.8g  ask=%.8g  vol=%.4f\n"
              t.symbol t.last_price t.bid_price t.ask_price t.volume_24h;
            (match t.price_change_pct with
             | Some pct -> printf "  24h change: %.2f%%\n" pct
             | None -> ());
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let book_command =
  let open Command.Let_syntax in
  ( "book"
  , Command.async
      ~summary:"Get KuCoin order book (top 20)"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string)
            ~doc:"STRING trading pair (e.g., BTC-USDT)"
        in
        fun () ->
          let cfg = cfg_of_env "production" in
          let adapter = Adapter.create ~cfg () in
          Adapter.order_book adapter ~symbol
          >>= function
          | Ok book ->
            printf "=== %s Order Book (seq=%Ld) ===\n" symbol book.sequence;
            printf "--- ASKS (%d levels) ---\n" (List.length book.asks);
            List.rev book.asks
            |> List.iter ~f:(fun (price, size) ->
              printf "  %s  qty=%s\n" price size);
            printf "--- BIDS (%d levels) ---\n" (List.length book.bids);
            List.iter book.bids ~f:(fun (price, size) ->
              printf "  %s  qty=%s\n" price size);
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let trades_command =
  let open Command.Let_syntax in
  ( "trades"
  , Command.async
      ~summary:"Get KuCoin recent trades"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string)
            ~doc:"STRING trading pair (e.g., BTC-USDT)"
        in
        fun () ->
          let cfg = cfg_of_env "production" in
          let adapter = Adapter.create ~cfg () in
          Adapter.trades adapter ~symbol
          >>= function
          | Ok trades ->
            List.iter trades ~f:(fun t ->
              let side_str =
                match t.side with
                | Some Buy -> "BUY "
                | Some Sell -> "SELL"
                | None -> "????"
              in
              printf "%s  %s  %.8g @ %.8g\n" side_str t.symbol t.qty t.price);
            printf "(%d trades)\n" (List.length trades);
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let balances_command =
  let open Command.Let_syntax in
  ( "balances"
  , Command.async
      ~summary:"Get KuCoin account balances (requires auth)"
      [%map_open
        let cfg = cfg_param_auth in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.balances adapter
          >>= function
          | Ok balances ->
            List.iter balances ~f:(fun b ->
              match Float.(b.total > 0.0) with
              | true ->
                printf "%s  total=%.8g  avail=%.8g  locked=%.8g\n"
                  b.currency b.total b.available b.locked
              | false -> ());
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let limit_order_command =
  let open Command.Let_syntax in
  ( "limit-order"
  , Command.async
      ~summary:"Place KuCoin spot limit order (requires auth)"
      [%map_open
        let cfg = cfg_param_auth
        and symbol =
          flag "-symbol" (required string)
            ~doc:"STRING trading pair (e.g., BTC-USDT)"
        and side = flag "-side" (required string) ~doc:"STRING buy or sell"
        and price = flag "-price" (required string) ~doc:"STRING limit price"
        and size = flag "-size" (required string) ~doc:"STRING order size"
        in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.place_limit_order adapter ~symbol ~side ~price ~size
          >>= function
          | Ok r ->
            printf "Order placed: %s\n" r.orderId;
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let market_order_command =
  let open Command.Let_syntax in
  ( "market-order"
  , Command.async
      ~summary:"Place KuCoin spot market order (requires auth)"
      [%map_open
        let cfg = cfg_param_auth
        and symbol =
          flag "-symbol" (required string)
            ~doc:"STRING trading pair (e.g., BTC-USDT)"
        and side = flag "-side" (required string) ~doc:"STRING buy or sell"
        and size = flag "-size" (required string) ~doc:"STRING order size"
        in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.place_market_order adapter ~symbol ~side ~size
          >>= function
          | Ok r ->
            printf "Market order placed: %s\n" r.orderId;
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let cancel_command =
  let open Command.Let_syntax in
  ( "cancel"
  , Command.async
      ~summary:"Cancel KuCoin order (requires auth)"
      [%map_open
        let cfg = cfg_param_auth
        and order_id =
          flag "-order-id" (required string) ~doc:"STRING order ID to cancel"
        in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.cancel_order adapter ~order_id
          >>= function
          | Ok () ->
            printf "Order %s cancelled\n" order_id;
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let order_status_command =
  let open Command.Let_syntax in
  ( "order-status"
  , Command.async
      ~summary:"Get KuCoin order details (requires auth)"
      [%map_open
        let cfg = cfg_param_auth
        and order_id =
          flag "-order-id" (required string) ~doc:"STRING order ID"
        in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.order_details adapter ~order_id
          >>= function
          | Ok o ->
            printf "%s  %s %s  qty=%.8g  filled=%.8g  [%s]\n"
              o.id
              (Types.Side.to_string o.side)
              o.symbol
              o.qty
              o.filled
              (Sexp.to_string (Types.Order_status.sexp_of_t o.status));
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let command : string * Command.t =
  ( "spot"
  , Command.group
      ~summary:"KuCoin Spot Trading Commands"
      [ ticker_command
      ; book_command
      ; trades_command
      ; balances_command
      ; limit_order_command
      ; market_order_command
      ; cancel_command
      ; order_status_command ] )

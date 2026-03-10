(** Gate.io Fluxum Adapter

    Normalizes Gate.io native types to Fluxum types and provides CLI commands.

    {b Features:}
    - ✅ REST market data (ticker, order book, trades)
    - ✅ REST trading (spot limit orders)
    - ✅ Account balances

    {b Authentication:}
    - HMAC-SHA512 via GATEIO_API_KEY / GATEIO_API_SECRET environment variables

    {b Symbol Format:}
    - Underscore-separated uppercase: "BTC_USDT", "ETH_USDT" *)

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
          ~config:Exchange_common.Rate_limiter.Configs.gateio
          () }

  module Venue = struct
    let t = Types.Venue.Gateio
  end

  module Normalize = struct
    let float_of_string_safe s =
      match Float.of_string_opt s with
      | Some f -> Ok f
      | None -> Error (sprintf "invalid numeric string: %S" s)

    let ticker (t : Native_types.ticker) ~symbol : (Types.Ticker.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind last_price = float_of_string_safe t.last in
      let%bind bid_price = float_of_string_safe t.highest_bid in
      let%bind ask_price = float_of_string_safe t.lowest_ask in
      let%bind high_24h = float_of_string_safe t.high_24h in
      let%bind low_24h = float_of_string_safe t.low_24h in
      let%bind volume_24h = float_of_string_safe t.base_volume in
      Ok
        (Types.Ticker.create
           ~venue:Venue.t
           ~symbol
           ~last_price
           ~bid_price
           ~ask_price
           ~high_24h
           ~low_24h
           ~volume_24h
           ?quote_volume:(Float.of_string_opt t.quote_volume)
           ())

    let balance (b : Native_types.balance) : (Types.Balance.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind available = float_of_string_safe b.available in
      let%bind locked = float_of_string_safe b.locked in
      Ok
        ({ venue= Venue.t
         ; currency= String.uppercase b.currency
         ; total= available +. locked
         ; available
         ; locked }
         : Types.Balance.t)

    let public_trade (t : Native_types.trade) ~symbol
      : (Types.Public_trade.t, string) Result.t
      =
      let open Result.Let_syntax in
      let%bind price = float_of_string_safe t.price in
      let%bind qty = float_of_string_safe t.amount in
      let side =
        match String.lowercase t.side with
        | "buy" -> Some Types.Side.Buy
        | "sell" -> Some Types.Side.Sell
        | _ -> None
      in
      let ts =
        Time_float_unix.of_span_since_epoch
          (Time_float_unix.Span.of_sec (Float.of_int64 t.create_time))
      in
      Ok
        (Types.Public_trade.create
           ~venue:Venue.t
           ~symbol
           ~price
           ~qty
           ?side
           ~trade_id:(Int64.to_string t.id)
           ~ts
           ())

    let order_response (o : Native_types.order_response)
      : (Types.Order.t, string) Result.t
      =
      let open Result.Let_syntax in
      let%bind price = float_of_string_safe o.price in
      let%bind qty = float_of_string_safe o.amount in
      let side =
        match String.lowercase o.side with
        | "buy" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let status =
        match String.lowercase o.status with
        | "open" -> Types.Order_status.New
        | "closed" -> Types.Order_status.Filled
        | "cancelled" -> Types.Order_status.Canceled
        | _ -> Types.Order_status.New
      in
      Ok
        ({ venue= Venue.t
         ; id= o.id
         ; symbol= o.currency_pair
         ; side
         ; kind= Types.Order_kind.Basic (Limit price)
         ; time_in_force= Types.Time_in_force.GTC
         ; qty
         ; filled= 0.0
         ; status
         ; created_at= None
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
      | `Network msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "network"; message= msg }
  end

  (** {2 Market Data Operations} *)

  let ticker (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.ticker ~cfg:t.cfg ~currency_pair:symbol
      >>| function
      | Ok native ->
        (match Normalize.ticker native ~symbol with
         | Ok n -> Ok n
         | Error err -> Error (`Json_parse err))
      | Error e -> Error e)

  let order_book (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.order_book ~cfg:t.cfg ~currency_pair:symbol
      >>| function
      | Ok book -> Ok book
      | Error e -> Error e)

  let trades (t : t) ~symbol =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.trades ~cfg:t.cfg ~currency_pair:symbol
      >>| function
      | Ok native_trades ->
        let normalized =
          List.filter_map native_trades ~f:(fun tr ->
            match Normalize.public_trade tr ~symbol with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error "gateio trade normalize error: %s" err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let balances (t : t) =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.balances ~cfg:t.cfg
      >>| function
      | Ok native_balances ->
        let normalized =
          List.filter_map native_balances ~f:(fun b ->
            match Normalize.balance b with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error "gateio balance normalize error: %s" err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let place_order (t : t) ~symbol ~side ~amount ~price =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Rest.place_limit_order ~cfg:t.cfg ~currency_pair:symbol ~side ~amount ~price
      >>| function
      | Ok native ->
        (match Normalize.order_response native with
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
    match Unix.getenv "GATEIO_API_KEY", Unix.getenv "GATEIO_API_SECRET" with
    | Some key, Some secret -> Cfg.with_auth ~api_key:key ~api_secret:secret cfg
    | _ -> cfg]

let ticker_command =
  let open Command.Let_syntax in
  ( "ticker"
  , Command.async
      ~summary:"Get Gate.io ticker"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string) ~doc:"STRING trading pair (e.g., BTC_USDT)"
        in
        fun () ->
          let cfg = cfg_of_env "production" in
          let adapter = Adapter.create ~cfg () in
          Adapter.ticker adapter ~symbol
          >>= function
          | Ok t ->
            printf "%s  last=%.8g  bid=%.8g  ask=%.8g  vol=%.4f\n"
              t.symbol t.last_price t.bid_price t.ask_price t.volume_24h;
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let book_command =
  let open Command.Let_syntax in
  ( "book"
  , Command.async
      ~summary:"Get Gate.io order book"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string) ~doc:"STRING trading pair (e.g., BTC_USDT)"
        in
        fun () ->
          let cfg = cfg_of_env "production" in
          let adapter = Adapter.create ~cfg () in
          Adapter.order_book adapter ~symbol
          >>= function
          | Ok book ->
            printf "=== %s Order Book ===\n" symbol;
            printf "--- ASKS (%d levels) ---\n" (List.length book.asks);
            List.rev book.asks
            |> List.iter ~f:(fun (price, qty) -> printf "  %s  qty=%s\n" price qty);
            printf "--- BIDS (%d levels) ---\n" (List.length book.bids);
            List.iter book.bids ~f:(fun (price, qty) ->
              printf "  %s  qty=%s\n" price qty);
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let trades_command =
  let open Command.Let_syntax in
  ( "trades"
  , Command.async
      ~summary:"Get Gate.io recent trades"
      [%map_open
        let _env = cfg_param_env
        and symbol =
          flag "-symbol" (required string) ~doc:"STRING trading pair (e.g., BTC_USDT)"
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
      ~summary:"Get Gate.io account balances (requires auth)"
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

let order_command =
  let open Command.Let_syntax in
  ( "order"
  , Command.async
      ~summary:"Place Gate.io spot limit order (requires auth)"
      [%map_open
        let cfg = cfg_param_auth
        and symbol =
          flag "-symbol" (required string) ~doc:"STRING trading pair (e.g., BTC_USDT)"
        and side = flag "-side" (required string) ~doc:"STRING buy or sell"
        and amount = flag "-amount" (required string) ~doc:"STRING quantity"
        and price = flag "-price" (required string) ~doc:"STRING limit price"
        in
        fun () ->
          let adapter = Adapter.create ~cfg () in
          Adapter.place_order adapter ~symbol ~side ~amount ~price
          >>= function
          | Ok order ->
            printf "Order %s placed: %s  qty=%.8g  [%s]\n"
              order.id
              (Types.Side.to_string order.side)
              order.qty
              (Sexp.to_string (Types.Order_status.sexp_of_t order.status));
            Deferred.unit
          | Error e ->
            printf "Error: %s\n" (Rest.Error.to_string e);
            Deferred.unit] )

let command : string * Command.t =
  ( "spot"
  , Command.group
      ~summary:"Gate.io Spot Trading Commands"
      [ ticker_command
      ; book_command
      ; trades_command
      ; balances_command
      ; order_command ] )

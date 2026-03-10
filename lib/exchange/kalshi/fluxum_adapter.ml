(** Kalshi Exchange Adapter

    Prediction market exchange adapter providing normalized types
    for Kalshi event contracts.

    {b Features:}
    - ✅ REST prediction market data (events, markets, orderbook, trades)
    - ✅ Order placement and cancellation
    - ✅ Position and balance tracking

    {b Authentication:}
    - RSA-PSS key signing via KALSHI-ACCESS-KEY/TIMESTAMP/SIGNATURE headers
    - Private key PEM file (default: ~/.kalshi/private_key.pem)

    {b Price Convention:}
    - Kalshi: cents (1-99), YES + NO = 100
    - Normalized: dollars (0.01-0.99), YES + NO = 1.00

    {b Symbol Format:}
    - Market tickers like "KXBTC-26MAR14-T67500" *)

open Core
open Async
module Types = Fluxum.Types

module Adapter = struct
  type t =
    { cfg: (module Cfg.S)
    ; rate_limiter: Exchange_common.Rate_limiter.t }

  let create ~cfg () =
    { cfg
    ; rate_limiter=
        Exchange_common.Rate_limiter.create
          ~config:Exchange_common.Rate_limiter.Configs.kalshi
          () }

  module Venue = struct
    let t = Types.Venue.Kalshi
  end

  (** {2 Normalization} *)

  module Normalize = struct
    let cents_to_dollars n = Float.of_int n /. 100.0

    let dollars_str_to_float s =
      match Float.of_string_opt s with
      | Some f -> Ok f
      | None -> Error (sprintf "invalid dollar amount: %S" s)

    let prediction_contract_of_market (m : Prediction_markets.Market.t)
      : (Types.Prediction_contract.t, string) Result.t =
      let last_price = Float.of_string_opt m.last_price in
      let best_bid = Float.of_string_opt m.yes_bid in
      let best_ask = Float.of_string_opt m.yes_ask in
      let total_shares =
        match Float.of_string_opt m.open_interest with
        | Some f -> f
        | None -> 0.0
      in
      Ok
        ({ instrument_symbol= m.ticker
         ; label= m.title
         ; ticker= m.ticker
         ; last_price
         ; best_bid
         ; best_ask
         ; total_shares
         ; status= m.status }
         : Types.Prediction_contract.t)

    let prediction_event_of_event (e : Prediction_markets.Event.t)
      : (Types.Prediction_event.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind contracts =
        List.map e.markets ~f:prediction_contract_of_market |> Result.all
      in
      let volume =
        List.fold e.markets ~init:0.0 ~f:(fun acc m ->
          acc +. Option.value ~default:0.0 (Float.of_string_opt m.volume))
      in
      let liquidity =
        List.fold e.markets ~init:0.0 ~f:(fun acc m ->
          acc +. Option.value ~default:0.0 (Float.of_string_opt m.open_interest))
      in
      let is_live =
        List.exists e.markets ~f:(fun m -> String.equal m.status "active")
      in
      Ok
        ({ venue= Venue.t
         ; id= e.event_ticker
         ; title= e.title
         ; description= e.subtitle
         ; category= e.category
         ; ticker= e.event_ticker
         ; status=
             (match is_live with
              | true -> "active"
              | false -> "closed")
         ; volume
         ; liquidity
         ; contracts
         ; is_live }
         : Types.Prediction_event.t)

    let prediction_event_of_market (m : Prediction_markets.Market.t)
      : (Types.Prediction_event.t, string) Result.t =
      let open Result.Let_syntax in
      let%bind contract = prediction_contract_of_market m in
      let volume = Option.value ~default:0.0 (Float.of_string_opt m.volume) in
      let liquidity = Option.value ~default:0.0 (Float.of_string_opt m.open_interest) in
      Ok
        ({ venue= Venue.t
         ; id= m.event_ticker
         ; title= m.title
         ; description= m.subtitle
         ; category= m.category
         ; ticker= m.event_ticker
         ; status= m.status
         ; volume
         ; liquidity
         ; contracts= [contract]
         ; is_live= String.equal m.status "active" }
         : Types.Prediction_event.t)

    let prediction_order (o : Prediction_markets.Order.t)
      : (Types.Prediction_order.t, string) Result.t =
      let side =
        match o.action with
        | "buy" -> Types.Side.Buy
        | _ -> Types.Side.Sell
      in
      let outcome =
        match o.side with
        | "yes" -> Types.Prediction_outcome.Yes
        | _ -> Types.Prediction_outcome.No
      in
      let price = cents_to_dollars o.yes_price in
      let parse_time s = try Some (Time_float_unix.of_string s) with _ -> None in
      Ok
        ({ venue= Venue.t
         ; id= o.order_id
         ; symbol= o.ticker
         ; side
         ; outcome
         ; qty= Float.of_int o.quantity
         ; filled= Float.of_int o.filled_count
         ; remaining= Float.of_int o.remaining_count
         ; price
         ; avg_execution_price= None
         ; status= o.status
         ; event_ticker= None
         ; contract_name= None
         ; contract_id= None
         ; created_at= Option.bind o.created_time ~f:parse_time
         ; updated_at= Option.bind o.updated_time ~f:parse_time }
         : Types.Prediction_order.t)

    let prediction_position (p : Prediction_markets.Position.t)
      : (Types.Prediction_position.t, string) Result.t =
      let outcome =
        match p.position > 0 with
        | true -> Types.Prediction_outcome.Yes
        | false -> Types.Prediction_outcome.No
      in
      let qty = Float.of_int (Int.abs p.position) in
      let avg_price = 0.0 in
      Ok
        ({ venue= Venue.t
         ; symbol= p.ticker
         ; outcome
         ; qty
         ; avg_price
         ; event_ticker= None
         ; contract_name= None
         ; contract_id= None }
         : Types.Prediction_position.t)

    let error (e : Prediction_markets.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= Int.to_string code; message= msg }
      | `Json_parse_error msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "json"; message= msg }
      | `Api_error msg ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "api"; message= msg }
      | `Not_found ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "404"; message= "not_found" }
      | `Unauthorized -> Types.Error.Auth_failed
      | `Rate_limited ->
        Types.Error.Exchange_specific
          { venue= Venue.t; code= "429"; message= "rate_limited" }
  end

  (** {2 Prediction Market Operations} *)

  let host (t : t) =
    let module Cfg = (val t.cfg : Cfg.S) in
    Cfg.api_host

  let prediction_list_events (t : t) ?status ?series_ticker ?limit () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.List_events.get ~host:(host t) ?status ?series_ticker ?limit ()
      >>| function
      | Ok events ->
        let normalized =
          List.filter_map events ~f:(fun e ->
            match Normalize.prediction_event_of_event e with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error
                "prediction event normalize error (ticker=%s): %s"
                e.event_ticker err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let prediction_list_markets (t : t) ?status ?event_ticker ?limit () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.List_markets.get ~host:(host t) ?status ?event_ticker ?limit ()
      >>| function
      | Ok markets ->
        let normalized =
          List.filter_map markets ~f:(fun m ->
            match Normalize.prediction_event_of_market m with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error
                "prediction market normalize error (ticker=%s): %s"
                m.ticker err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let prediction_get_event (t : t) ~event_ticker () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_event.get ~host:(host t) ~event_ticker ()
      >>| function
      | Ok event ->
        (match Normalize.prediction_event_of_event event with
         | Ok n -> Ok n
         | Error err -> Error (`Json_parse_error err))
      | Error e -> Error e)

  let prediction_get_market (t : t) ~ticker () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_market.get ~host:(host t) ~ticker ()
      >>| function
      | Ok market ->
        (match Normalize.prediction_event_of_market market with
         | Ok n -> Ok n
         | Error err -> Error (`Json_parse_error err))
      | Error e -> Error e)

  let prediction_place_order (t : t) (request : Prediction_markets.Place_order.request) =
    match request.yes_price with
    | Some p when p < 1 || p > 99 ->
      return (Error (`Api_error
        (sprintf "prediction price %d out of range [1, 99] cents" p)))
    | _ ->
      Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
        Prediction_markets.Place_order.post ~cfg:t.cfg request
        >>| function
        | Ok order ->
          (match Normalize.prediction_order order with
           | Ok n -> Ok n
           | Error err -> Error (`Json_parse_error err))
        | Error e -> Error e)

  let prediction_cancel_order (t : t) ~order_id =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Cancel_order.delete ~cfg:t.cfg ~order_id ()
      >>| function
      | Ok `Deleted -> Ok ()
      | Error e -> Error e)

  let prediction_active_orders (t : t) ?ticker ?limit () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_orders.get ~cfg:t.cfg ~status:"resting" ?ticker ?limit ()
      >>| function
      | Ok orders ->
        let normalized =
          List.filter_map orders ~f:(fun o ->
            match Normalize.prediction_order o with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error
                "prediction order normalize error (id=%s): %s"
                o.order_id err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let prediction_positions (t : t) ?ticker () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_positions.get ~cfg:t.cfg ?ticker ()
      >>| function
      | Ok positions ->
        let normalized =
          List.filter_map positions ~f:(fun p ->
            match Normalize.prediction_position p with
            | Ok n -> Some n
            | Error err ->
              Log.Global.error
                "prediction position normalize error (ticker=%s): %s"
                p.ticker err;
              None)
        in
        Ok normalized
      | Error e -> Error e)

  let prediction_balance (t : t) () =
    Exchange_common.Rate_limiter.with_rate_limit_retry t.rate_limiter ~f:(fun () ->
      Prediction_markets.Get_balance.get ~cfg:t.cfg ()
      >>| function
      | Ok b ->
        Ok
          ({ Types.Balance.venue= Venue.t
           ; currency= "USD"
           ; total= Float.of_int b.portfolio_value /. 100.0
           ; available= Float.of_int b.balance /. 100.0
           ; locked= Float.of_int (b.portfolio_value - b.balance) /. 100.0 }
           : Types.Balance.t)
      | Error e -> Error e)
end

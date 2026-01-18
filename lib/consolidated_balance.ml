(** Consolidated Multi-Exchange Balance Module

    This module provides a unified interface to query balances across
    multiple exchanges and aggregate them into a single view.
*)

open Core
open Async

module Types = Fluxum.Types

(** Result of querying a single exchange *)
module Exchange_result = struct
  type t =
    { venue : Types.Venue.t
    ; balances : Types.Balance.t list
    ; error : string option
    ; latency_ms : float
    }
  [@@deriving sexp]

  let success ~venue ~balances ~latency_ms =
    { venue; balances; error = None; latency_ms }

  let failure ~venue ~error ~latency_ms =
    { venue; balances = []; error = Some error; latency_ms }
end

(** Aggregated balance across exchanges *)
module Aggregated_balance = struct
  type exchange_amount =
    { venue : Types.Venue.t
    ; total : float
    ; available : float
    ; locked : float
    }
  [@@deriving sexp]

  type t =
    { currency : string
    ; total_across_exchanges : float
    ; available_across_exchanges : float
    ; locked_across_exchanges : float
    ; by_exchange : exchange_amount list
    }
  [@@deriving sexp]

  let empty currency =
    { currency
    ; total_across_exchanges = 0.0
    ; available_across_exchanges = 0.0
    ; locked_across_exchanges = 0.0
    ; by_exchange = []
    }

  let add_balance t (balance : Types.Balance.t) =
    let exchange_amt =
      { venue = balance.venue
      ; total = balance.total
      ; available = balance.available
      ; locked = balance.locked
      }
    in
    { t with
      total_across_exchanges = t.total_across_exchanges +. balance.total
    ; available_across_exchanges = t.available_across_exchanges +. balance.available
    ; locked_across_exchanges = t.locked_across_exchanges +. balance.locked
    ; by_exchange = exchange_amt :: t.by_exchange
    }
end

(** Consolidated view of all balances *)
module Consolidated_view = struct
  type t =
    { timestamp : Time_ns_unix.t
    ; exchange_results : Exchange_result.t list
    ; aggregated : Aggregated_balance.t String.Map.t  (* currency -> aggregated *)
    ; total_usd_value : float option  (* Optional USD valuation *)
    }
  [@@deriving sexp_of]

  let create ~exchange_results =
    let timestamp = Time_ns.now () in
    (* Aggregate balances by currency *)
    let aggregated =
      List.fold exchange_results ~init:String.Map.empty ~f:(fun acc result ->
        List.fold result.Exchange_result.balances ~init:acc ~f:(fun acc balance ->
          let currency = String.uppercase balance.currency in
          let agg =
            Map.find acc currency
            |> Option.value ~default:(Aggregated_balance.empty currency)
          in
          Map.set acc ~key:currency ~data:(Aggregated_balance.add_balance agg balance)
        )
      )
    in
    { timestamp; exchange_results; aggregated; total_usd_value = None }

  (** Get summary of successful/failed exchanges *)
  let summary t =
    let successful = List.count t.exchange_results ~f:(fun r -> Option.is_none r.error) in
    let failed = List.count t.exchange_results ~f:(fun r -> Option.is_some r.error) in
    let total_latency =
      List.sum (module Float) t.exchange_results ~f:(fun r -> r.latency_ms)
    in
    sprintf "Queried %d exchanges (%d ok, %d failed) in %.0fms"
      (successful + failed) successful failed total_latency

  (** Get non-zero balances sorted by total value *)
  let non_zero_balances ?(min_value = 0.0001) t =
    let threshold = min_value in
    Map.to_alist t.aggregated
    |> List.filter ~f:(fun (_, agg) ->
         Float.(agg.Aggregated_balance.total_across_exchanges > threshold))
    |> List.sort ~compare:(fun (_, a) (_, b) ->
         Float.compare
           b.Aggregated_balance.total_across_exchanges
           a.Aggregated_balance.total_across_exchanges)
end

(** Exchange query configuration *)
module Query_config = struct
  type exchange_config =
    | Gemini of (module Gemini.Cfg.S)
    | Kraken of (module Kraken.Cfg.S)
    | Binance of (module Binance.Cfg.S)
    | Mexc of (module Mexc.Cfg.S)
    | Coinbase of (module Coinbase.Cfg.S)
    | Bitrue of (module Bitrue.Cfg.S)

  type t =
    { exchanges : exchange_config list
    ; timeout : Time_ns.Span.t
    ; include_zero_balances : bool
    }

  let default =
    { exchanges = []
    ; timeout = Time_ns.Span.of_sec 30.0
    ; include_zero_balances = false
    }

  let with_gemini cfg t =
    { t with exchanges = Gemini cfg :: t.exchanges }

  let with_kraken cfg t =
    { t with exchanges = Kraken cfg :: t.exchanges }

  let with_binance cfg t =
    { t with exchanges = Binance cfg :: t.exchanges }

  let with_mexc cfg t =
    { t with exchanges = Mexc cfg :: t.exchanges }

  let with_coinbase cfg t =
    { t with exchanges = Coinbase cfg :: t.exchanges }

  let with_bitrue cfg t =
    { t with exchanges = Bitrue cfg :: t.exchanges }
end

(** Query a single exchange for balances *)
let query_exchange (config : Query_config.exchange_config) : Exchange_result.t Deferred.t =
  let start = Time_ns.now () in
  let calc_latency () =
    Time_ns.Span.to_ms (Time_ns.diff (Time_ns.now ()) start)
  in
  match config with
  | Query_config.Gemini cfg ->
    (* Gemini requires a nonce pipe for authentication *)
    Gemini.Nonce.Counter.pipe ~init:1 () >>= fun nonce ->
    let adapter = Gemini.Fluxum_adapter.Adapter.create ~cfg ~nonce () in
    Gemini.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.map balances ~f:Gemini.Fluxum_adapter.Adapter.Normalize.balance in
      let result_transpose results =
        List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
          match res, acc with
          | Ok v, Ok vs -> Ok (v :: vs)
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      in
      (match result_transpose normalized with
       | Ok bals -> Exchange_result.success ~venue:Types.Venue.Gemini ~balances:bals ~latency_ms:(calc_latency ())
       | Error _msg -> Exchange_result.failure ~venue:Types.Venue.Gemini ~error:"Normalization error" ~latency_ms:(calc_latency ()))
    | Error _ ->
      Exchange_result.failure ~venue:Types.Venue.Gemini ~error:"API error" ~latency_ms:(calc_latency ()))

  | Query_config.Kraken cfg ->
    let adapter = Kraken.Fluxum_adapter.Adapter.create ~cfg () in
    Kraken.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.map balances ~f:Kraken.Fluxum_adapter.Adapter.Normalize.balance in
      let result_transpose results =
        List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
          match res, acc with
          | Ok v, Ok vs -> Ok (v :: vs)
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      in
      (match result_transpose normalized with
       | Ok bals -> Exchange_result.success ~venue:Types.Venue.Kraken ~balances:bals ~latency_ms:(calc_latency ())
       | Error _msg -> Exchange_result.failure ~venue:Types.Venue.Kraken ~error:"Normalization error" ~latency_ms:(calc_latency ()))
    | Error _e ->
      Exchange_result.failure ~venue:Types.Venue.Kraken ~error:"API error" ~latency_ms:(calc_latency ()))

  | Query_config.Binance cfg ->
    let adapter = Binance.Fluxum_adapter.Adapter.create ~cfg () in
    Binance.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.filter_map balances ~f:(fun b ->
        match Binance.Fluxum_adapter.Adapter.Normalize.balance b with
        | Ok bal -> Some bal
        | Error _ -> None) in
      Exchange_result.success ~venue:Types.Venue.Binance ~balances:normalized ~latency_ms:(calc_latency ())
    | Error e ->
      let error = Sexp.to_string (Binance.Rest.Error.sexp_of_t e) in
      Exchange_result.failure ~venue:Types.Venue.Binance ~error ~latency_ms:(calc_latency ()))

  | Query_config.Mexc cfg ->
    let adapter = Mexc.Fluxum_adapter.Adapter.create ~cfg () in
    Mexc.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.map balances ~f:Mexc.Fluxum_adapter.Adapter.Normalize.balance in
      let result_transpose results =
        List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
          match res, acc with
          | Ok v, Ok vs -> Ok (v :: vs)
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      in
      (match result_transpose normalized with
       | Ok bals -> Exchange_result.success ~venue:Types.Venue.Mexc ~balances:bals ~latency_ms:(calc_latency ())
       | Error _msg -> Exchange_result.failure ~venue:Types.Venue.Mexc ~error:"Normalization error" ~latency_ms:(calc_latency ()))
    | Error e ->
      let error = Sexp.to_string (Mexc.Rest.Error.sexp_of_t e) in
      Exchange_result.failure ~venue:Types.Venue.Mexc ~error ~latency_ms:(calc_latency ()))

  | Query_config.Coinbase cfg ->
    let adapter = Coinbase.Fluxum_adapter.Adapter.create ~cfg () in
    Coinbase.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.filter_map balances ~f:(fun b ->
        match Coinbase.Fluxum_adapter.Adapter.Normalize.balance b with
        | Ok bal -> Some bal
        | Error _ -> None) in
      Exchange_result.success ~venue:Types.Venue.Coinbase ~balances:normalized ~latency_ms:(calc_latency ())
    | Error e ->
      let error = Sexp.to_string (Coinbase.Rest.Error.sexp_of_t e) in
      Exchange_result.failure ~venue:Types.Venue.Coinbase ~error ~latency_ms:(calc_latency ()))

  | Query_config.Bitrue cfg ->
    let adapter = Bitrue.Fluxum_adapter.Adapter.create ~cfg () in
    Bitrue.Fluxum_adapter.Adapter.balances adapter >>| (function
    | Ok balances ->
      let normalized = List.filter_map balances ~f:(fun b ->
        match Bitrue.Fluxum_adapter.Adapter.Normalize.balance b with
        | Ok bal -> Some bal
        | Error _ -> None) in
      Exchange_result.success ~venue:Types.Venue.Bitrue ~balances:normalized ~latency_ms:(calc_latency ())
    | Error e ->
      let error = Sexp.to_string (Bitrue.Rest.Error.sexp_of_t e) in
      Exchange_result.failure ~venue:Types.Venue.Bitrue ~error ~latency_ms:(calc_latency ()))

(** Query all configured exchanges in parallel *)
let query_all (config : Query_config.t) : Consolidated_view.t Deferred.t =
  Deferred.List.map config.exchanges ~how:`Parallel ~f:query_exchange
  >>| fun results ->
  let results =
    match config.include_zero_balances with
    | true -> results
    | false ->
      List.map results ~f:(fun r ->
        { r with Exchange_result.balances =
            List.filter r.balances ~f:(fun b -> Float.(b.total > 0.0001))
        })
  in
  Consolidated_view.create ~exchange_results:results

(** Pretty print consolidated view *)
let pp_consolidated (view : Consolidated_view.t) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "\n=== Consolidated Balance View ===\n";
  Buffer.add_string buf (sprintf "Timestamp: %s\n" (Time_ns_unix.to_string view.timestamp));
  Buffer.add_string buf (sprintf "%s\n\n" (Consolidated_view.summary view));

  (* Print exchange status *)
  Buffer.add_string buf "Exchange Status:\n";
  List.iter view.exchange_results ~f:(fun r ->
    let status = match r.error with
      | None -> sprintf "OK (%d balances)" (List.length r.balances)
      | Some e -> sprintf "FAILED: %s" e
    in
    Buffer.add_string buf (sprintf "  %-10s: %s (%.0fms)\n"
      (Types.Venue.to_string r.venue) status r.latency_ms)
  );

  (* Print aggregated balances *)
  Buffer.add_string buf "\nAggregated Balances:\n";
  let non_zero = Consolidated_view.non_zero_balances view in
  List.iter non_zero ~f:(fun (currency, agg) ->
    Buffer.add_string buf (sprintf "\n  %s:\n" currency);
    Buffer.add_string buf (sprintf "    Total:     %.8f\n" agg.total_across_exchanges);
    Buffer.add_string buf (sprintf "    Available: %.8f\n" agg.available_across_exchanges);
    Buffer.add_string buf (sprintf "    Locked:    %.8f\n" agg.locked_across_exchanges);
    Buffer.add_string buf "    By Exchange:\n";
    List.iter agg.by_exchange ~f:(fun ex ->
      Buffer.add_string buf (sprintf "      %-10s: %.8f (avail: %.8f)\n"
        (Types.Venue.to_string ex.venue) ex.total ex.available)
    )
  );

  Buffer.contents buf

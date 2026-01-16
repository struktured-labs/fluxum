(** Consolidated Multi-Exchange Balance Aggregation

    This module provides a unified interface to query and aggregate balances
    across multiple exchanges simultaneously.

    {b Key Features:}
    - Parallel querying of multiple exchanges
    - Automatic normalization and aggregation by currency
    - Latency tracking and error handling per exchange
    - USD valuation support (optional)
    - Zero balance filtering

    {b Use Cases:}
    - Portfolio overview across all exchanges
    - Risk management (total exposure)
    - Arbitrage opportunity detection
    - Tax reporting and accounting
    - Asset allocation analysis

    {b Example:}
    {[
      (* Configure exchanges to query *)
      let config =
        Query_config.default
        |> Query_config.with_gemini (module Gemini.Cfg : Gemini.Cfg.S)
        |> Query_config.with_kraken (module Kraken.Cfg : Kraken.Cfg.S)
        |> Query_config.with_mexc (module Mexc.Cfg : Mexc.Cfg.S)
      in

      (* Query all exchanges in parallel *)
      let%bind view = query_all config in

      (* Print summary *)
      printf "%s\n" (Consolidated_view.summary view);

      (* Iterate over non-zero balances *)
      let non_zero = Consolidated_view.non_zero_balances view in
      List.iter non_zero ~f:(fun (currency, agg) ->
        printf "%s: %.8f total across %d exchanges\n"
          currency
          agg.total_across_exchanges
          (List.length agg.by_exchange)
      )
    ]}

    @see <lib/consolidated_order_book.ml> for order book consolidation
*)

open Async

(** {1 Exchange Query Results} *)

module Exchange_result : sig
  (** Result of querying a single exchange

      Contains either successful balance data or error information,
      plus latency metrics for performance monitoring.
  *)

  type t = {
    venue : Fluxum.Types.Venue.t;
    (** Which exchange was queried *)

    balances : Fluxum.Types.Balance.t list;
    (** Normalized balances (empty on error) *)

    error : string option;
    (** Error message if query failed *)

    latency_ms : float;
    (** Round-trip query time in milliseconds *)
  }
  [@@deriving sexp]

  val success :
    venue:Fluxum.Types.Venue.t ->
    balances:Fluxum.Types.Balance.t list ->
    latency_ms:float ->
    t
  (** Create successful result *)

  val failure :
    venue:Fluxum.Types.Venue.t ->
    error:string ->
    latency_ms:float ->
    t
  (** Create failed result *)
end

(** {1 Aggregated Balances} *)

module Aggregated_balance : sig
  (** Balance for a single currency aggregated across exchanges

      Provides total, available, and locked amounts summed across
      all exchanges, plus per-exchange breakdown.
  *)

  type exchange_amount = {
    venue : Fluxum.Types.Venue.t;
    total : float;
    available : float;
    locked : float;
  }
  [@@deriving sexp]
  (** Balance on a single exchange for this currency *)

  type t = {
    currency : string;
    (** Currency code (uppercase, e.g., "BTC", "USD") *)

    total_across_exchanges : float;
    (** Sum of total across all exchanges *)

    available_across_exchanges : float;
    (** Sum of available across all exchanges *)

    locked_across_exchanges : float;
    (** Sum of locked across all exchanges *)

    by_exchange : exchange_amount list;
    (** Per-exchange breakdown *)
  }
  [@@deriving sexp]

  val empty : string -> t
  (** Create empty aggregated balance for currency

      {b Example:}
      {[
        let btc = Aggregated_balance.empty "BTC"
        (* btc.total_across_exchanges = 0.0 *)
      ]}
  *)

  val add_balance : t -> Fluxum.Types.Balance.t -> t
  (** Add a balance to aggregation

      Adds the balance's amounts to totals and appends to by_exchange list.

      {b Example:}
      {[
        let agg = Aggregated_balance.empty "BTC" in
        let gemini_bal = { currency = "BTC"; total = 1.5; available = 1.0; locked = 0.5; venue = Gemini } in
        let agg = Aggregated_balance.add_balance agg gemini_bal in
        (* agg.total_across_exchanges = 1.5 *)
        (* agg.by_exchange = [{ venue = Gemini; total = 1.5; ... }] *)

        let kraken_bal = { currency = "BTC"; total = 2.0; available = 2.0; locked = 0.0; venue = Kraken } in
        let agg = Aggregated_balance.add_balance agg kraken_bal in
        (* agg.total_across_exchanges = 3.5 *)
        (* agg.by_exchange = [... Kraken, Gemini ...] *)
      ]}
  *)
end

(** {1 Consolidated View} *)

module Consolidated_view : sig
  (** Complete view of all balances across all queried exchanges

      This is the main result type returned by [query_all].
      Contains raw exchange results, aggregated balances by currency,
      and optional USD valuation.
  *)

  type t = {
    timestamp : Time_ns_unix.t;
    (** When the query was completed *)

    exchange_results : Exchange_result.t list;
    (** Raw results from each exchange *)

    aggregated : Aggregated_balance.t Core.String.Map.t;
    (** Aggregated balances by currency (uppercase keys)

        {b Example access:}
        {[
          match Map.find view.aggregated "BTC" with
          | Some agg -> printf "Total BTC: %.8f\n" agg.total_across_exchanges
          | None -> printf "No BTC holdings\n"
        ]}
    *)

    total_usd_value : float option;
    (** Total portfolio value in USD (if price data available)

        Currently not implemented - returns None.
        Future: integrate with price feeds for automatic valuation.
    *)
  }
  [@@deriving sexp_of]

  val create : exchange_results:Exchange_result.t list -> t
  (** Create consolidated view from exchange results

      Automatically:
      - Aggregates balances by currency (case-insensitive)
      - Sets timestamp to now
      - Initializes USD value to None

      {b Example:}
      {[
        let results = [
          Exchange_result.success ~venue:Gemini ~balances:[...] ~latency_ms:150.0;
          Exchange_result.failure ~venue:Kraken ~error:"Timeout" ~latency_ms:5000.0;
        ] in
        let view = Consolidated_view.create ~exchange_results:results
      ]}
  *)

  val summary : t -> string
  (** Get human-readable summary

      Returns string like:
      "Queried 3 exchanges (2 ok, 1 failed) in 450ms"

      Useful for logging and status displays.
  *)

  val non_zero_balances :
    ?min_value:float ->
    t ->
    (string * Aggregated_balance.t) list
  (** Get non-zero balances sorted by total value

      @param min_value Minimum total to include (default: 0.0001)
      @return List of (currency, aggregated_balance) pairs, sorted descending by total

      Filters out:
      - Currencies with total <= min_value
      - Dust balances that are economically insignificant

      Sorted by total value (highest first) for prioritized display.

      {b Example:}
      {[
        let non_zero = Consolidated_view.non_zero_balances ~min_value:0.001 view in
        List.iter non_zero ~f:(fun (currency, agg) ->
          printf "%s: %.8f\n" currency agg.total_across_exchanges
        )
        (* Output (sorted by value):
           BTC: 3.45000000
           ETH: 12.00000000
           USDT: 5000.00000000
        *)
      ]}
  *)
end

(** {1 Query Configuration} *)

module Query_config : sig
  (** Configuration for multi-exchange balance queries

      Specifies which exchanges to query, timeout settings,
      and filtering options.
  *)

  type exchange_config =
    | Gemini of (module Gemini.Cfg.S)
    | Kraken of (module Kraken.Cfg.S)
    | Binance of (module Binance.Cfg.S)
    | Mexc of (module Mexc.Cfg.S)
    | Coinbase of (module Coinbase.Cfg.S)
    | Bitrue of (module Bitrue.Cfg.S)
  (** Exchange configuration with authentication credentials

      Each variant wraps a first-class module containing:
      - API key and secret
      - Optional passphrase (Coinbase)
      - Base URL overrides (for sandbox/testnet)

      {b Example:}
      {[
        module Gemini_cfg = struct
          let api_key = Sys.getenv_exn "GEMINI_API_KEY"
          let api_secret = Sys.getenv_exn "GEMINI_SECRET"
          let sandbox = false
        end
        let cfg = Gemini (module Gemini_cfg : Gemini.Cfg.S)
      ]}
  *)

  type t = {
    exchanges : exchange_config list;
    (** Which exchanges to query *)

    timeout : Time_ns.Span.t;
    (** Maximum time to wait for each exchange (default: 30s)

        Individual exchanges that exceed timeout will be marked as failed
        but won't block other exchanges (queries are parallel).
    *)

    include_zero_balances : bool;
    (** Whether to include zero/dust balances in results (default: false)

        If false, balances with total <= 0.0001 are filtered out.
        Reduces noise in portfolio views.
    *)
  }

  val default : t
  (** Default configuration

      {[
        { exchanges = [];
          timeout = 30s;
          include_zero_balances = false;
        }
      ]}

      Use builder functions to add exchanges.
  *)

  val with_gemini : (module Gemini.Cfg.S) -> t -> t
  (** Add Gemini exchange to configuration

      {b Example:}
      {[
        let config =
          Query_config.default
          |> Query_config.with_gemini (module My_gemini_cfg)
      ]}
  *)

  val with_kraken : (module Kraken.Cfg.S) -> t -> t
  (** Add Kraken exchange *)

  val with_binance : (module Binance.Cfg.S) -> t -> t
  (** Add Binance exchange *)

  val with_mexc : (module Mexc.Cfg.S) -> t -> t
  (** Add MEXC exchange *)

  val with_coinbase : (module Coinbase.Cfg.S) -> t -> t
  (** Add Coinbase exchange *)

  val with_bitrue : (module Bitrue.Cfg.S) -> t -> t
  (** Add Bitrue exchange *)
end

(** {1 Query Operations} *)

val query_exchange : Query_config.exchange_config -> Exchange_result.t Deferred.t
(** Query a single exchange for balances

    Internal function used by [query_all].
    Exposed for testing and advanced use cases.

    Handles:
    - Adapter creation with credentials
    - API call with timeout
    - Normalization of responses
    - Error capture with latency tracking

    @return Exchange_result.t (success or failure)
*)

val query_all : Query_config.t -> Consolidated_view.t Deferred.t
(** Query all configured exchanges in parallel

    This is the main entry point for multi-exchange balance queries.

    {b Process:}
    1. Query all exchanges concurrently (Deferred.List.map ~how:`Parallel)
    2. Normalize responses to Fluxum.Types.Balance.t
    3. Aggregate balances by currency
    4. Filter zero balances (if configured)
    5. Return consolidated view

    {b Error Handling:}
    - Individual exchange failures don't fail entire query
    - Failed exchanges return Exchange_result.t with error field set
    - Check Exchange_result.error to see which exchanges failed

    {b Performance:}
    - Queries are parallel (fastest exchange determines minimum latency)
    - Timeout applies per-exchange (one slow exchange won't block others)
    - Total time = max(individual latencies), not sum

    {b Example:}
    {[
      let config =
        Query_config.default
        |> Query_config.with_gemini (module Gemini_cfg)
        |> Query_config.with_kraken (module Kraken_cfg)
        |> Query_config.with_mexc (module Mexc_cfg)
      in

      let%bind view = query_all config in

      (* Check which exchanges succeeded *)
      List.iter view.exchange_results ~f:(fun result ->
        match result.error with
        | None ->
          printf "%s: %d balances (%.0fms)\n"
            (Fluxum.Types.Venue.to_string result.venue)
            (List.length result.balances)
            result.latency_ms
        | Some err ->
          printf "%s: FAILED - %s (%.0fms)\n"
            (Fluxum.Types.Venue.to_string result.venue)
            err
            result.latency_ms
      );

      (* Print aggregated balances *)
      let non_zero = Consolidated_view.non_zero_balances view in
      List.iter non_zero ~f:(fun (currency, agg) ->
        printf "\n%s: %.8f total\n" currency agg.total_across_exchanges;
        List.iter agg.by_exchange ~f:(fun ex ->
          printf "  %s: %.8f (avail: %.8f, locked: %.8f)\n"
            (Fluxum.Types.Venue.to_string ex.venue)
            ex.total
            ex.available
            ex.locked
        )
      )
    ]}
*)

(** {1 Display Utilities} *)

val pp_consolidated : Consolidated_view.t -> string
(** Pretty-print consolidated view

    Returns formatted multi-line string with:
    - Timestamp and summary
    - Exchange status (success/failure + latency)
    - Aggregated balances by currency
    - Per-exchange breakdown for each currency

    {b Example Output:}
    {v
      === Consolidated Balance View ===
      Timestamp: 2026-01-16 15:30:45.123456Z
      Queried 3 exchanges (2 ok, 1 failed) in 450ms

      Exchange Status:
        Gemini    : OK (3 balances) (150ms)
        Kraken    : OK (5 balances) (300ms)
        Mexc      : FAILED: Timeout (5000ms)

      Aggregated Balances:

        BTC:
          Total:     3.45000000
          Available: 3.20000000
          Locked:    0.25000000
          By Exchange:
            Gemini    : 1.50000000 (avail: 1.50000000)
            Kraken    : 1.95000000 (avail: 1.70000000)

        ETH:
          Total:     12.00000000
          Available: 11.50000000
          Locked:    0.50000000
          By Exchange:
            Gemini    : 5.00000000 (avail: 5.00000000)
            Kraken    : 7.00000000 (avail: 6.50000000)
    v}

    Use for:
    - CLI output
    - Logging
    - Reports
*)

(** Shared Normalization Utilities for Exchange Adapters

    This module provides safe, fallible conversion functions to eliminate
    unsafe operations (Float.of_string, etc.) across exchange adapters.

    {b Design Principle:} All conversion functions return Result.t to handle
    malformed data gracefully. This prevents crashes on unexpected API responses.

    {b Usage:} Exchange adapters should use these shared utilities instead of
    implementing their own conversion logic. This ensures:
    - Consistent behavior across all exchanges
    - Centralized bug fixes
    - Better test coverage (test once, applies everywhere)
    - Easier maintenance

    @see <https://github.com/struktured-labs/fluxum> for usage examples
*)

open Core

(** {1 Safe Float Conversions} *)

module Float_conv : sig
  (** Safe float conversion with validation

      All functions return Result.t to handle invalid input gracefully.
      Rejects: non-numeric strings, infinity, NaN.
  *)

  val of_string : string -> (float, string) Result.t
  (** Convert string to float with validation.

      @return Ok float if valid and finite
      @return Error message if invalid, non-finite, or parse error

      Examples:
      - "123.456" → Ok 123.456
      - "inf" → Error "Non-finite float: inf"
      - "abc" → Error "Invalid float 'abc': ..."
  *)

  val price_of_string : string -> (float, string) Result.t
  (** Convert string to price (must be positive).

      More strict than [of_string] - rejects zero and negative values.

      @return Ok float if > 0.0
      @return Error if ≤ 0.0 or invalid

      Examples:
      - "50000.25" → Ok 50000.25
      - "0.0" → Error "Price must be positive, got: 0.000000"
      - "-100" → Error "Price must be positive, got: -100.000000"
  *)

  val qty_of_string : string -> (float, string) Result.t
  (** Convert string to quantity (must be non-negative).

      Allows zero (for order cancellations) but rejects negative values.

      @return Ok float if ≥ 0.0
      @return Error if < 0.0 or invalid

      Examples:
      - "10.5" → Ok 10.5
      - "0.0" → Ok 0.0  (valid - represents no quantity)
      - "-5.0" → Error "Quantity cannot be negative, got: -5.000000"
  *)

  val amount_of_string : string -> (float, string) Result.t
  (** Convert string to amount (allows negative for balance differences).

      Same as [of_string] but with clearer intent for financial amounts.

      @return Ok float if finite
      @return Error if non-finite or invalid
  *)
end

(** {1 Result Utilities} *)

module Result_util : sig
  (** Utilities for working with Result.t lists *)

  val transpose : ('a, 'e) Result.t list -> ('a list, 'e) Result.t
  (** Transpose list of Results into Result of list.

      All elements must be Ok to get Ok list.
      First Error causes entire operation to fail.

      Examples:
      - [[Ok 1; Ok 2; Ok 3]] → [Ok [1; 2; 3]]
      - [[Ok 1; Error "e"; Ok 3]] → [Error "e"]  (first error wins)
      - [[]] → [Ok []]  (empty list succeeds)

      Use for: Converting multiple Result.t values in parallel operations.
  *)

  val map_transpose : f:('a -> ('b, 'e) Result.t) -> 'a list -> ('b list, 'e) Result.t
  (** Map function that returns Result, then transpose.

      Convenience function combining [List.map] and [transpose].
      Equivalent to: [List.map list ~f |> transpose]

      @param f Function that may fail (returns Result.t)
      @param list Input list
      @return Ok list if all succeed, Error if any fail
  *)
end

(** {1 Exchange Type Conversions} *)

module Side : sig
  (** Convert exchange-specific side strings to normalized Types.Side.t *)

  val of_string : string -> (Types.Side.t, string) Result.t
  (** Parse side from exchange string.

      Handles common variations (case-insensitive):
      - "buy", "b", "BID", "Buy", "BUY" → Ok Types.Side.Buy
      - "sell", "s", "ASK", "Sell", "SELL" → Ok Types.Side.Sell
      - Other → Error "Unrecognized side: ..."

      @return Ok side if recognized
      @return Error with details if unrecognized

      Prefer this over [of_string_exn] - explicit error handling is better.
  *)

  val of_string_exn : ?default:Types.Side.t -> string -> Types.Side.t
  (** Parse side with fallback default (for backwards compatibility).

      Warning: Using defaults can mask data quality issues.
      Only use when you have a reasonable default (e.g., in quotes, bid = buy).

      @param default Fallback value if string unrecognized (default: Buy)
      @return Parsed side or default if parsing fails

      Prefer [of_string] which returns Result.t for explicit error handling.
  *)
end

module Order_status : sig
  (** Convert exchange-specific status strings to normalized Types.Order_status.t *)

  val of_string : string -> (Types.Order_status.t, string) Result.t
  (** Parse order status from exchange string.

      Handles common variations across exchanges (case-insensitive):
      - "new", "pending", "open", "accepted" → Ok New
      - "filled", "closed", "executed" → Ok Filled
      - "partially_filled", "partial" → Ok Partially_filled
      - "canceled", "cancelled", "expired" → Ok Canceled
      - "rejected" → Ok (Rejected "Order rejected")
      - Other → Error "Unrecognized order status: ..."

      @return Ok status if recognized
      @return Error if unrecognized

      Note: "expired" maps to Canceled (common exchange behavior).
  *)

  val of_string_exn : ?default:Types.Order_status.t -> string -> Types.Order_status.t
  (** Parse status with fallback default (for backwards compatibility).

      Warning: Using defaults can mask data quality issues.
      Prefer [of_string] which returns Result.t.

      @param default Fallback value if unrecognized (default: New)
  *)
end

module Order_type : sig
  (** Convert exchange-specific order type strings to normalized Types.Order_kind.t *)

  val of_string : string -> (Types.Order_kind.t, string) Result.t
  (** Parse order type from exchange string.

      Handles common variations (case-insensitive):
      - "market", "MARKET" → Ok Market
      - "limit", "LIMIT" → Ok (Limit 0.0)  {i price placeholder}
      - "post_only", "maker_only", "limit_maker" → Ok (Post_only_limit 0.0)
      - Other → Error "Unrecognized order type: ..."

      Important: Returns placeholder price (0.0) for limit orders.
      Caller must fill in actual price from order data.

      @return Ok order_kind with placeholder prices
      @return Error if unrecognized

      Example:
      {[
        match Order_type.of_string "LIMIT" with
        | Ok (Types.Order_kind.Limit _) ->
          (* Replace placeholder with actual price *)
          Types.Order_kind.Limit (Float.of_string order.price)
        | Ok other -> other
        | Error msg -> failwith msg
      ]}
  *)

  val of_string_exn : ?default:Types.Order_kind.t -> string -> Types.Order_kind.t
  (** Parse order type with fallback default.

      Warning: Using defaults can mask data quality issues.
      Prefer [of_string] which returns Result.t.

      @param default Fallback value if unrecognized (default: Market)
  *)
end

(** Shared normalization utilities for exchange adapters

    This module provides safe, fallible conversion functions to eliminate
    unsafe Float.of_string and other crash-prone operations across exchange adapters.
*)

open Core

(** {1 Safe Float Conversions} *)

module Float_conv = struct
  (** Convert string to float with validation (no exception overhead) *)
  let of_string (s : string) : (float, string) Result.t =
    match Float.of_string_opt s with
    | Some f ->
      (match Float.is_finite f with
       | true -> Ok f
       | false -> Error (sprintf "Non-finite float: %s" s))
    | None -> Error (sprintf "Invalid float '%s'" s)

  (** Convert string to price (must be positive) *)
  let price_of_string (s : string) : (float, string) Result.t =
    match of_string s with
    | Ok f when Float.(f > 0.0) -> Ok f
    | Ok f -> Error (sprintf "Price must be positive, got: %f" f)
    | Error e -> Error e

  (** Convert string to quantity (must be non-negative) *)
  let qty_of_string (s : string) : (float, string) Result.t =
    match of_string s with
    | Ok f when Float.(f >= 0.0) -> Ok f
    | Ok f -> Error (sprintf "Quantity cannot be negative, got: %f" f)
    | Error e -> Error e

  (** Convert string to amount (allows negative for balance differences) *)
  let amount_of_string (s : string) : (float, string) Result.t =
    of_string s
end

(** {1 Result Utilities} *)

module Result_util = struct
  (** Transpose list of Results into Result of list

      [Ok 1; Ok 2; Ok 3] → Ok [1; 2; 3]
      [Ok 1; Error "e"; Ok 3] → Error "e" (first error wins)
  *)
  let transpose (results : ('a, 'e) Result.t list) : ('a list, 'e) Result.t =
    List.fold_right results ~init:(Ok []) ~f:(fun res acc ->
      match res, acc with
      | Ok v, Ok vs -> Ok (v :: vs)
      | Error e, _ -> Error e
      | _, Error e -> Error e)

  (** Map and transpose - apply function that returns Result, collect all successes or first error *)
  let map_transpose ~f list =
    List.map list ~f |> transpose
end

(** {1 Exchange Type Conversions} *)

module Side = struct
  (** Convert exchange-specific side strings to normalized Side.t

      Handles common variations:
      - "buy", "b", "BID", "Buy", "BUY" → Types.Side.Buy
      - "sell", "s", "ASK", "Sell", "SELL" → Types.Side.Sell

      Returns Error if side is unrecognized.
  *)
  let of_string (s : string) : (Types.Side.t, string) Result.t =
    match s with
    | "buy" | "Buy" | "BUY" | "b" | "B" | "bid" | "Bid" | "BID" -> Ok Types.Side.Buy
    | "sell" | "Sell" | "SELL" | "s" | "S" | "ask" | "Ask" | "ASK" -> Ok Types.Side.Sell
    | _ -> Error (sprintf "Unrecognized side: %s" s)

  (** Version with default fallback (for backwards compatibility).

      Warning: Using defaults can mask data quality issues.
      Prefer of_string which returns Result.t.
  *)
  let of_string_exn ?(default = Types.Side.Buy) (s : string) : Types.Side.t =
    match of_string s with
    | Ok side -> side
    | Error _ -> default
end

module Order_status = struct
  (** Convert exchange-specific status strings to normalized Order_status.t

      Handles common variations across exchanges:
      - "new", "pending", "open", "accepted", "NEW", "PENDING" → Order_status.New
      - "filled", "closed", "executed", "FILLED", "CLOSED" → Order_status.Filled
      - "partially_filled", "partial", "PARTIALLY_FILLED" → Order_status.Partially_filled
      - "canceled", "cancelled", "expired", "CANCELED", "CANCELLED" → Order_status.Canceled
      - "rejected", "REJECTED" → Order_status.Rejected

      Returns Error if status is unrecognized.
  *)
  let of_string (s : string) : (Types.Order_status.t, string) Result.t =
    match s with
    | "new" | "New" | "NEW" | "pending" | "Pending" | "PENDING"
    | "open" | "Open" | "OPEN" | "accepted" | "Accepted" | "ACCEPTED" ->
      Ok Types.Order_status.New
    | "filled" | "Filled" | "FILLED" | "closed" | "Closed" | "CLOSED"
    | "executed" | "Executed" | "EXECUTED" ->
      Ok Types.Order_status.Filled
    | "canceled" | "Canceled" | "CANCELED" | "cancelled" | "Cancelled" | "CANCELLED"
    | "expired" | "Expired" | "EXPIRED" ->
      Ok Types.Order_status.Canceled
    | "partially_filled" | "Partially_filled" | "PARTIALLY_FILLED"
    | "partial" | "Partial" | "PARTIAL" ->
      Ok Types.Order_status.Partially_filled
    | "rejected" | "Rejected" | "REJECTED" ->
      Ok (Types.Order_status.Rejected "Order rejected")
    | _ -> Error (sprintf "Unrecognized order status: %s" s)

  (** Version with default fallback (for backwards compatibility).

      Warning: Using defaults can mask data quality issues.
      Prefer of_string which returns Result.t.
  *)
  let of_string_exn ?(default = Types.Order_status.New) (s : string) : Types.Order_status.t =
    match of_string s with
    | Ok status -> status
    | Error _ -> default
end

module Order_type = struct
  (** Convert exchange-specific order type strings to normalized Order_kind.t

      Handles common variations:
      - "market", "MARKET" → Order_kind.Market
      - "limit", "LIMIT" → Order_kind.Limit (price must be provided separately)
      - "post_only", "maker_only", "LIMIT_MAKER" → Order_kind.Post_only_limit

      Note: This returns a placeholder price (0.0) for limit orders.
      Caller must fill in the actual price from the order data.

      Returns Error if order type is unrecognized.
  *)
  let of_string (s : string) : (Types.Order_kind.t, string) Result.t =
    match s with
    | "market" | "Market" | "MARKET" -> Ok Types.Order_kind.Market
    | "limit" | "Limit" | "LIMIT" -> Ok (Types.Order_kind.Limit 0.0)  (* Price filled by caller *)
    | "post_only" | "Post_only" | "POST_ONLY"
    | "maker_only" | "Maker_only" | "MAKER_ONLY"
    | "limit_maker" | "Limit_maker" | "LIMIT_MAKER" ->
      Ok (Types.Order_kind.Post_only_limit 0.0)
    | _ -> Error (sprintf "Unrecognized order type: %s" s)

  (** Version with default fallback (for backwards compatibility).

      Warning: Using defaults can mask data quality issues.
      Prefer of_string which returns Result.t.
  *)
  let of_string_exn ?(default = Types.Order_kind.Market) (s : string) : Types.Order_kind.t =
    match of_string s with
    | Ok kind -> kind
    | Error _ -> default
end

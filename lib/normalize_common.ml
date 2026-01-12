(** Shared normalization utilities for exchange adapters

    This module provides safe, fallible conversion functions to eliminate
    unsafe Float.of_string and other crash-prone operations across exchange adapters.
*)

open Core

(** {1 Safe Float Conversions} *)

module Float_conv = struct
  (** Convert string to float with validation *)
  let of_string (s : string) : (float, string) Result.t =
    try
      let f = Float.of_string s in
      match Float.is_finite f with
      | true -> Ok f
      | false -> Error (sprintf "Non-finite float: %s" s)
    with
    | Failure msg -> Error (sprintf "Invalid float '%s': %s" s msg)
    | exn -> Error (sprintf "Float conversion error '%s': %s" s (Exn.to_string exn))

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

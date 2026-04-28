open Core

module Near_rail = struct
  type t =
    { yes_bid : int
    ; yes_ask : int
    ; threshold : int
    }
  [@@deriving sexp]
end

module Regime_break = struct
  type t =
    { mid_start : float
    ; mid_end : float
    ; delta : float
    ; threshold : float
    }
  [@@deriving sexp]
end

module One_sided_book = struct
  type side = [ `Bid_empty | `Ask_empty ] [@@deriving sexp]

  type t =
    { side : side
    ; yes_bid_first : int
    ; yes_bid_last : int
    ; yes_ask_first : int
    ; yes_ask_last : int
    }
  [@@deriving sexp]
end

let check_near_rail ?(threshold = 5) ~max_yes_bid ~min_yes_ask_nonzero () =
  let near_top = max_yes_bid >= 100 - threshold in
  let near_bottom =
    (* min_yes_ask_nonzero <= threshold AND > 0 ; if 0 it means no nonzero ask
       observed — that's the One_sided_book case, not the rail case. *)
    min_yes_ask_nonzero > 0 && min_yes_ask_nonzero <= threshold
  in
    match near_top || near_bottom with
    | true ->
      Some
        { Near_rail.yes_bid = max_yes_bid
        ; yes_ask = min_yes_ask_nonzero
        ; threshold
        }
    | false -> None

let check_regime_break ?(threshold = 40.0) ~mid_start ~mid_end () =
  let delta = Float.abs (mid_end -. mid_start) in
    match Float.( > ) delta threshold with
    | true ->
      Some
        { Regime_break.mid_start
        ; mid_end
        ; delta
        ; threshold
        }
    | false -> None

let last_or_zero xs =
  match List.last xs with
  | Some x -> x
  | None -> 0

let first_or_zero xs =
  match List.hd xs with
  | Some x -> x
  | None -> 0

let all_zero xs = List.for_all xs ~f:(fun x -> x = 0)

let check_one_sided_book ~yes_bid_window ~yes_ask_window =
  let bid_empty = all_zero yes_bid_window in
  let ask_empty = all_zero yes_ask_window in
  let mk side =
    Some
      { One_sided_book.side
      ; yes_bid_first = first_or_zero yes_bid_window
      ; yes_bid_last = last_or_zero yes_bid_window
      ; yes_ask_first = first_or_zero yes_ask_window
      ; yes_ask_last = last_or_zero yes_ask_window
      }
  in
    match bid_empty, ask_empty with
    | true, _ -> mk `Bid_empty
    | _, true -> mk `Ask_empty
    | false, false -> None

type per_market_result =
  { near_rail : Near_rail.t option
  ; regime_break : Regime_break.t option
  ; one_sided_book : One_sided_book.t option
  }
[@@deriving sexp]

let is_pass r =
  Option.is_none r.near_rail
  && Option.is_none r.regime_break
  && Option.is_none r.one_sided_book

let check_all_per_market
    ?rail_threshold
    ?regime_threshold
    ~max_yes_bid
    ~min_yes_ask_nonzero
    ~mid_start
    ~mid_end
    ~yes_bid_window
    ~yes_ask_window
    ()
  =
  let near_rail =
    check_near_rail
      ?threshold:rail_threshold
      ~max_yes_bid
      ~min_yes_ask_nonzero
      ()
  in
  let regime_break =
    match mid_start, mid_end with
    | Some s, Some e ->
      check_regime_break ?threshold:regime_threshold ~mid_start:s ~mid_end:e ()
    | _ -> None
  in
  let one_sided_book = check_one_sided_book ~yes_bid_window ~yes_ask_window in
    { near_rail; regime_break; one_sided_book }

open Core

module Trade = struct
  type t =
    { tid : int64
    ; timestamp : Time_ns_unix.t
    ; price : float
    ; amount : float
    ; aggressor : [ `Buy | `Sell ]
    }
  [@@deriving sexp]
end

type ofi_value =
  | Ofi of float
  | Saturated_buy
  | Saturated_sell
  | No_data
  | Insufficient_trades of int
[@@deriving sexp]

module Tagged_trade = struct
  type t =
    { trade : Trade.t
    ; ofi : ofi_value
    ; window_buy_value : float
    ; window_sell_value : float
    ; window_trade_count : int
    }
  [@@deriving sexp]
end

let default_window = Time_ns.Span.of_min 60.

let default_min_window_trades = 5

let trade_value (t : Trade.t) = t.price *. t.amount

(** Walk backward from index [i-1], accumulating buy/sell value of trades
    whose timestamp is within [window] of trade [i]'s timestamp. Excludes
    trade at [i] itself — the look-ahead-bias guard. *)
let window_at_index trades i ~window =
  let n = Array.length trades in
    match i <= 0 || i >= n with
    | true -> (0., 0., 0)
    | false ->
      let t_now = trades.(i).Trade.timestamp in
      let cutoff =
        Time_ns_unix.sub t_now window
      in
      let buy_v = ref 0. in
      let sell_v = ref 0. in
      let count = ref 0 in
      let j = ref (i - 1) in
      let stop = ref false in
        while (not !stop) && !j >= 0 do
          let trade_j = trades.(!j) in
            match Time_ns_unix.( >= ) trade_j.timestamp cutoff with
            | false -> stop := true
            | true ->
              let v = trade_value trade_j in
                (match trade_j.aggressor with
                 | `Buy -> buy_v := !buy_v +. v
                 | `Sell -> sell_v := !sell_v +. v);
                incr count;
                decr j
        done;
        (!buy_v, !sell_v, !count)

let value_to_ofi ~buy_v ~sell_v ~count ~min_window_trades : ofi_value =
  match count < min_window_trades with
  | true -> Insufficient_trades count
  | false ->
    (match Float.( = ) buy_v 0., Float.( = ) sell_v 0. with
     | true, true -> No_data
     | false, true -> Saturated_buy
     | true, false -> Saturated_sell
     | false, false -> Ofi (buy_v /. sell_v))

let compute
    ~trades
    ?(window = default_window)
    ?(min_window_trades = default_min_window_trades)
    ()
  =
  let n = Array.length trades in
    Array.init n ~f:(fun i ->
      let buy_v, sell_v, count = window_at_index trades i ~window in
      let ofi =
        value_to_ofi
          ~buy_v
          ~sell_v
          ~count
          ~min_window_trades
      in
      { Tagged_trade.trade = trades.(i)
      ; ofi
      ; window_buy_value = buy_v
      ; window_sell_value = sell_v
      ; window_trade_count = count
      })

let default_bins =
  [ (0.0, 0.4, "<0.4 heavy SELL pressure")
  ; (0.4, 0.7, "0.4-0.7 moderate SELL pressure")
  ; (0.7, 1.4, "0.7-1.4 balanced")
  ; (1.4, 2.5, "1.4-2.5 moderate BUY pressure")
  ; (2.5, Float.infinity, ">2.5 heavy BUY pressure")
  ]

type bin = float * float * string [@@deriving sexp]

type bin_drift =
  { bin_label : string
  ; fwd_window : Time_ns.Span.t
  ; mean_drift_bps : float
  ; n : int
  }
[@@deriving sexp]

type shuffle_test =
  { fwd_window : Time_ns.Span.t
  ; actual_extreme_diff_bps : float
  ; n_shuffles : int
  ; p_value : float
  }
[@@deriving sexp]

type predictive_validity_result =
  { per_bin_drift : bin_drift array
  ; baseline_drift : (Time_ns.Span.t * float * int) array
  ; shuffle_tests : shuffle_test array
  }
[@@deriving sexp]

(** Find the most recent (timestamp <= t) entry. None if no such entry. *)
let mid_at_or_before mid_series t =
  Array.fold mid_series ~init:None ~f:(fun acc (ts, mid) ->
    match Time_ns_unix.( <= ) ts t with
    | true ->
      (match acc with
       | None -> Some mid
       | Some _ -> Some mid (* keep walking forward; later wins *))
    | false -> acc)

(** Find the first (timestamp >= t) entry. None if no such entry. *)
let mid_at_or_after mid_series t =
  Array.find_map mid_series ~f:(fun (ts, mid) ->
    match Time_ns_unix.( >= ) ts t with
    | true -> Some mid
    | false -> None)

let drift_bps_for_trade
    (tagged : Tagged_trade.t)
    ~mid_series
    ~fwd_window
  : float option
  =
  let t = tagged.trade.timestamp in
  let t_fwd = Time_ns_unix.add t fwd_window in
    match mid_at_or_before mid_series t, mid_at_or_after mid_series t_fwd with
    | Some m_at, Some m_fwd when Float.( > ) m_at 0. ->
      Some (10_000. *. ((m_fwd /. m_at) -. 1.))
    | _ -> None

(** Find the bin that contains an OFI value. Returns the label, or None
    if the value is non-numeric or outside all bins. *)
let bin_for_value ~bins (v : ofi_value) =
  match v with
  | Ofi x ->
    List.find_map bins ~f:(fun (lo, hi, label) ->
      match Float.( >= ) x lo && Float.( < ) x hi with
      | true -> Some label
      | false -> None)
  | Saturated_buy ->
    (* Treat as the top bin *)
    (match List.last bins with
     | Some (_, _, label) -> Some label
     | None -> None)
  | Saturated_sell ->
    (match List.hd bins with
     | Some (_, _, label) -> Some label
     | None -> None)
  | No_data | Insufficient_trades _ -> None

let mean_of_array xs =
  match Array.length xs with
  | 0 -> 0.
  | n -> Array.fold xs ~init:0. ~f:( +. ) /. Float.of_int n

(** Random shuffle in place, Fisher-Yates. Takes an explicit
    [Random.State.t] so callers can choose between bit-reproducible
    (seeded) or entropy-seeded behavior. *)
let shuffle_in_place ~rng_state arr =
  let n = Array.length arr in
    for i = n - 1 downto 1 do
      let j = Random.State.int rng_state (i + 1) in
      let tmp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- tmp
    done

let predictive_validity
    ~tagged_trades
    ~mid_series
    ~fwd_windows
    ?(bins = default_bins)
    ?(n_shuffles = 1000)
    ?seed
    ()
  =
  let rng_state =
    match seed with
    | Some s -> Random.State.make [| s |]
    | None -> Random.State.make_self_init ()
  in
  (* For each fwd_window, build (bin_label, drift_bps) pairs for trades
     whose mid anchors are available. *)
  let per_window_data =
    List.map fwd_windows ~f:(fun fwd ->
      let pairs =
        Array.filter_map tagged_trades ~f:(fun tagged ->
          match drift_bps_for_trade tagged ~mid_series ~fwd_window:fwd with
          | None -> None
          | Some drift ->
            (match bin_for_value ~bins tagged.ofi with
             | None -> None
             | Some label -> Some (label, drift)))
      in
        (fwd, pairs))
  in
  (* Per-bin drift across all (label, fwd) combinations. *)
  let per_bin_drift =
    List.concat_map per_window_data ~f:(fun (fwd, pairs) ->
      List.map bins ~f:(fun (_, _, label) ->
        let drifts =
          Array.filter_map pairs ~f:(fun (l, d) ->
            match String.equal l label with
            | true -> Some d
            | false -> None)
        in
          { bin_label = label
          ; fwd_window = fwd
          ; mean_drift_bps = mean_of_array drifts
          ; n = Array.length drifts
          }))
    |> Array.of_list
  in
  let baseline_drift =
    List.map per_window_data ~f:(fun (fwd, pairs) ->
      let all_drifts = Array.map pairs ~f:snd in
        (fwd, mean_of_array all_drifts, Array.length all_drifts))
    |> Array.of_list
  in
  (* Shuffle test: for each fwd_window, take top-bin minus bottom-bin
     drift difference, then shuffle bin labels n_shuffles times and
     compute distribution of |diff|. *)
  let bottom_label =
    match List.hd bins with
    | Some (_, _, l) -> l
    | None -> ""
  in
  let top_label =
    match List.last bins with
    | Some (_, _, l) -> l
    | None -> ""
  in
  let shuffle_tests =
    List.map per_window_data ~f:(fun (fwd, pairs) ->
      let actual_top =
        Array.filter_map pairs ~f:(fun (l, d) ->
          match String.equal l top_label with
          | true -> Some d
          | false -> None)
        |> mean_of_array
      in
      let actual_bottom =
        Array.filter_map pairs ~f:(fun (l, d) ->
          match String.equal l bottom_label with
          | true -> Some d
          | false -> None)
        |> mean_of_array
      in
      let actual_abs_diff = Float.abs (actual_top -. actual_bottom) in
      (* Shuffle the labels and recompute *)
      let labels = Array.map pairs ~f:fst in
      let drifts = Array.map pairs ~f:snd in
      let extreme_count = ref 0 in
        for _ = 1 to n_shuffles do
          let shuffled = Array.copy labels in
            shuffle_in_place ~rng_state shuffled;
            let top_drifts =
              Array.filter_mapi shuffled ~f:(fun i l ->
                match String.equal l top_label with
                | true -> Some drifts.(i)
                | false -> None)
            in
            let bot_drifts =
              Array.filter_mapi shuffled ~f:(fun i l ->
                match String.equal l bottom_label with
                | true -> Some drifts.(i)
                | false -> None)
            in
            let shuffled_diff =
              Float.abs (mean_of_array top_drifts -. mean_of_array bot_drifts)
            in
              match Float.( >= ) shuffled_diff actual_abs_diff with
              | true -> incr extreme_count
              | false -> ()
        done;
        let p_value = Float.of_int !extreme_count /. Float.of_int n_shuffles in
        { fwd_window = fwd
        ; actual_extreme_diff_bps = actual_top -. actual_bottom
        ; n_shuffles
        ; p_value
        })
    |> Array.of_list
  in
    { per_bin_drift; baseline_drift; shuffle_tests }

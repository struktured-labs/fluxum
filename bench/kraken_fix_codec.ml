open Core
module Fix = Kraken.Fix

let or_fail = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (Fix.sexp_of_error error))

let codec_or_fail = function
  | Ok value -> value
  | Error error -> failwith (Sexp.to_string_hum (Fix.Codec.sexp_of_error error))

let decimal value =
  match Fix.Decimal.of_string value with
  | Ok value -> value
  | Error error -> failwith error

let allocated_words () =
  let stats = Gc.quick_stat () in
  stats.minor_words +. stats.major_words

let measure ~iterations name operation =
  Gc.compact ();
  let words_before = allocated_words () in
  let started = Time_ns.now () in
  let rec loop remaining checksum =
    match remaining with
    | 0 -> checksum
    | _ -> loop (remaining - 1) (checksum + operation ())
  in
  let checksum = loop iterations 0 in
  let elapsed = Time_ns.diff (Time_ns.now ()) started |> Time_ns.Span.to_ns in
  let words = allocated_words () -. words_before in
  printf "%-24s %10.1f ns/op %10.1f words/op (guard=%d)\n%!" name
    (elapsed /. Float.of_int iterations)
    (words /. Float.of_int iterations)
    checksum

let () =
  let iterations =
    match Array.to_list (Sys.get_argv ()) with
    | [ _ ] -> 200_000
    | [ _; value ] -> Int.of_string value
    | _ -> failwith "usage: kraken_fix_codec.exe [iterations]"
  in
  let header =
    Fix.Header.create ~sender_comp_id:"CLIENT" ~msg_seq_num:42
      ~sending_time:"20260824-12:34:56.123"
    |> or_fail
  in
  let order =
    Fix.Order.
      {
        client_order_id =
          Fix.Client_order_id.create "1744036325000000" |> or_fail;
        kind = Limit { price = decimal "84000.00"; post_only = true };
        quantity = decimal "0.00100000";
        side = Buy;
        symbol = "BTC/USD";
        time_in_force = Gtc;
        self_trade_prevention = Some Cancel_newest;
      }
  in
  let encoded = Fix.Order.new_single ~header order |> or_fail in
  let decoded = Fix.Codec.Frame.decode encoded |> codec_or_fail in
  let market_data =
    Fix.Codec.Encoder.message ~sender_comp_id:"KRAKEN-MD"
      ~target_comp_id:"CLIENT" ~msg_type:"X" ~msg_seq_num:43
      ~sending_time:"20260824-12:34:56.124"
      ~body_fields:
        [
          (55, "BTC/USD");
          (268, "2");
          (279, "1");
          (269, "0");
          (278, "B84000.0");
          (270, "84000.0");
          (271, "0.12500000");
          (273, "12:34:56.124");
          (279, "1");
          (269, "1");
          (278, "O84000.1");
          (270, "84000.1");
          (271, "0.25000000");
          (273, "12:34:56.124");
        ]
    |> codec_or_fail |> Fix.Codec.Frame.decode |> codec_or_fail
  in
  printf "Kraken FIX codec microbenchmark (%d iterations)\n" iterations;
  measure ~iterations "encode limit order" (fun () ->
      Fix.Order.new_single ~header order |> or_fail |> String.length);
  measure ~iterations "decode limit order" (fun () ->
      Fix.Codec.Frame.decode encoded
      |> codec_or_fail |> Fix.Codec.Frame.sequence_number);
  let framer = Fix.Codec.Framer.create () in
  measure ~iterations "frame TCP message" (fun () ->
      match Fix.Codec.Framer.feed framer encoded with
      | Ok [ frame ] -> Fix.Codec.Frame.sequence_number frame
      | Ok _ -> failwith "unexpected frame count"
      | Error error ->
          failwith (Sexp.to_string_hum (Fix.Codec.sexp_of_error error)));
  measure ~iterations "fold L2 entries" (fun () ->
      Fix.Market_data.fold_entries market_data ~init:0 ~f:(fun checksum entry ->
          let price = Fix.Market_data.Entry.price entry |> or_fail in
          checksum + Int64.to_int_exn (Fix.Decimal.mantissa price))
      |> or_fail);
  ignore (Sys.opaque_identity decoded : Fix.Codec.Frame.t)

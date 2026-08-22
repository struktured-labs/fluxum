open Core
open Fluxum.Order_book_incremental

module Simple_book = struct
  let apply book levels = book + List.length levels
end

let check name condition =
  if not condition then failwithf "incremental-book test failed: %s" name ()

let apply_ok manager update =
  match Manager.apply manager ~update ~apply_fn:Simple_book.apply with
  | Ok manager -> manager
  | Error _ -> failwith "unexpected sequence error"

let () =
  let sequence = Sequence.update Sequence.empty ~seq:10L in
  check "expected next is valid" (Sequence.is_valid sequence ~seq:11L);
  check "forward gap is invalid" (not (Sequence.is_valid sequence ~seq:12L));
  check "duplicate is invalid" (not (Sequence.is_valid sequence ~seq:10L));
  let duplicate = Sequence.update sequence ~seq:10L in
  check "duplicate does not regress expected sequence"
    (Option.equal Int64.equal (Sequence.expected_next duplicate) (Some 11L));

  let level = Level_update.create ~price:100. ~size:1. ~side:`Bid in
  let manager = Manager.create 0 in
  let manager = apply_ok manager (Update.snapshot ~sequence:100L ~levels:[level] ()) in
  check "snapshot applied" (Manager.book manager = 1);

  let stale = apply_ok manager (Update.delta ~sequence:100L ~levels:[level] ()) in
  check "stale delta ignored" (Manager.book stale = 1);
  check "stale delta not counted" (Manager.updates_processed stale = 1);

  (match
     Manager.apply
       manager
       ~update:(Update.delta ~sequence:102L ~levels:[level] ())
       ~apply_fn:Simple_book.apply
   with
   | Error (`Sequence_gap (sequence, Some 102L)) ->
     check "gap count reported" (Sequence.gaps_detected sequence = 1)
   | _ -> failwith "forward gap should be rejected");
  check "rejected gap leaves original book unchanged" (Manager.book manager = 1);

  (match
     Manager.apply
       manager
       ~update:(Update.delta ~sequence_range:(105L, 104L) ~levels:[level] ())
       ~apply_fn:Simple_book.apply
   with
   | Error (`Sequence_gap _) -> ()
   | _ -> failwith "inverted sequence range should be rejected");

  let overlap =
    apply_ok manager (Update.delta ~sequence_range:(99L, 101L) ~levels:[level] ())
  in
  check "overlapping range applies" (Manager.book overlap = 2);
  check "range advances to final sequence"
    (Option.equal Int64.equal (Sequence.expected_next (Manager.sequence overlap)) (Some 102L));

  let reset =
    apply_ok overlap (Update.snapshot ~sequence:50L ~levels:[level] ())
  in
  check "snapshot can reset to an earlier baseline"
    (Option.equal Int64.equal (Sequence.expected_next (Manager.sequence reset)) (Some 51L));

  let batch = Batch.create manager in
  ignore (Batch.enqueue batch (Update.delta ~sequence:103L ~levels:[level] ()) : bool);
  (match Batch.flush batch ~apply_fn:Simple_book.apply with
   | Error _ -> ()
   | Ok _ -> failwith "batch gap should fail");
  (match Batch.flush batch ~apply_fn:Simple_book.apply with
   | Error _ -> ()
   | Ok _ -> failwith "failed batch must retain queued updates");
  printf "incremental order-book tests passed\n"

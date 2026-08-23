(** Incremental Order Book - Handles snapshots, deltas, and sequence tracking *)

open Core
open Async

(** Update type - snapshot or delta *)
module Update_type = struct
  type t =
    | Snapshot (** Full order book snapshot *)
    | Delta (** Incremental update (add/remove/modify levels) *)
  [@@deriving sexp, compare, equal]
end

(** Sequence tracking for gap detection *)
module Sequence = struct
  type t =
    { last_seen: int64 option (** Last sequence number we processed *)
    ; expected_next: int64 option (** Expected next sequence number *)
    ; gaps_detected: int (** Counter for gap detection *) }
  [@@deriving sexp, fields]

  let empty = {last_seen= None; expected_next= None; gaps_detected= 0}

  (** Check if sequence number is valid (no gaps) *)
  let is_valid t ~seq =
    match t.expected_next with
    | None -> true (* First message, always valid *)
    | Some expected -> Int64.equal seq expected

  (** Update sequence tracker with new sequence number *)
  let update t ~seq =
    match t.expected_next with
    | Some expected when Int64.(seq < expected) ->
      (* Never let a duplicate regress the tracker. *)
      t
    | expected_next ->
      let gaps_detected =
        match expected_next with
        | Some expected when Int64.(seq > expected) -> t.gaps_detected + 1
        | None | Some _ -> t.gaps_detected
      in
      {last_seen= Some seq; expected_next= Some Int64.(seq + 1L); gaps_detected}

  (** Reset sequence tracker (after snapshot) *)
  let reset _t ~seq =
    {last_seen= Some seq; expected_next= Some Int64.(seq + 1L); gaps_detected= 0}

  (** Check if we need to re-sync (too many gaps) *)
  let needs_resync t ~max_gaps = t.gaps_detected > max_gaps

  let record_gap t = {t with gaps_detected= t.gaps_detected + 1}
end

(** Price level update operation *)
module Level_update = struct
  type t =
    { price: float
    ; size: float
    ; side: [`Bid | `Ask] }
  [@@deriving sexp, fields]

  let create ~price ~size ~side = {price; size; side}
end

(** Order book update with sequence tracking *)
module Update = struct
  type t =
    { update_type: Update_type.t
    ; sequence: int64 option (** Sequence number if provided *)
    ; sequence_range: (int64 * int64) option (** Range for delta updates (first, final) *)
    ; levels: Level_update.t list (** Price level updates *)
    ; checksum: string option (** Checksum for validation if provided *)
    ; timestamp: Time_float_unix.t (** Update timestamp *) }
  [@@deriving sexp, fields]

  let create
        ?(update_type = Update_type.Delta)
        ?sequence
        ?sequence_range
        ?checksum
        ?(timestamp = Time_float_unix.now ())
        ~levels
        ()
    =
    {update_type; sequence; sequence_range; levels; checksum; timestamp}

  (** Create snapshot update *)
  let snapshot ?sequence ?checksum ~levels () =
    create ~update_type:Update_type.Snapshot ?sequence ?checksum ~levels ()

  (** Create delta update *)
  let delta ?sequence ?sequence_range ?checksum ~levels () =
    create ~update_type:Update_type.Delta ?sequence ?sequence_range ?checksum ~levels ()

  (** Get effective sequence number (use range end if available) *)
  let effective_sequence t =
    match t.sequence_range with
    | Some (_, final) -> Some final
    | None -> t.sequence

  (** Accessor functions *)
  let update_type t = t.update_type

  let levels t = t.levels
  let timestamp t = t.timestamp
end

(** Incremental order book manager *)
module Manager = struct
  type 'book t =
    { book: 'book (** Current order book state *)
    ; sequence: Sequence.t (** Sequence tracker *)
    ; last_update: Time_float_unix.t (** Last update timestamp *)
    ; updates_processed: int (** Counter for updates *)
    ; snapshots_received: int (** Counter for snapshots *)
    ; max_gaps: int (** Max gaps before re-sync *) }
  [@@deriving sexp, fields]

  let create ?(max_gaps = 5) book =
    { book
    ; sequence= Sequence.empty
    ; last_update= Time_float_unix.epoch
    ; updates_processed= 0
    ; snapshots_received= 0
    ; max_gaps }

  (** Check if sequence is valid *)
  let is_valid_sequence t ~seq = Sequence.is_valid t.sequence ~seq

  (** Check if we need to re-sync *)
  let needs_resync t = Sequence.needs_resync t.sequence ~max_gaps:t.max_gaps

  (** Apply update to book using provided apply function *)
  let apply t ~update ~apply_fn =
    let effective_seq = Update.effective_sequence update in
    let update_type = Update.update_type update in
    (* Sequence ranges may overlap the expected ID (as Binance ranges do).
       A fully old range is a harmless duplicate; a range beginning after
       the expected ID is a real gap and must not be applied. Snapshots
       always establish a new sequence baseline. *)
    let sequence_status =
      match update_type, t.sequence.expected_next, update.sequence_range, effective_seq with
      | Update_type.Delta, _, Some (first, final), _ when Int64.(first > final) -> `Gap
      | Update_type.Snapshot, _, _, _ | Update_type.Delta, None, _, _ -> `Accept
      | Update_type.Delta, Some _, _, None -> `Accept
      | Update_type.Delta, Some expected, Some (first, final), Some _ ->
        if Int64.(final < expected)
        then `Stale
        else if Int64.(first > expected)
        then `Gap
        else `Accept
      | Update_type.Delta, Some expected, None, Some seq ->
        if Int64.(seq < expected)
        then `Stale
        else if Int64.(seq > expected)
        then `Gap
        else `Accept
    in
      match sequence_status with
      | `Stale -> Ok t
      | `Gap ->
        (* Sequence gap detected *)
        let sequence = Sequence.record_gap t.sequence in
          Log.Global.info_s
            [%message
              "Order book sequence gap detected"
                ~gaps:(sequence.gaps_detected : int)
                ~expected:(t.sequence.expected_next : int64 option)
                ~received:(effective_seq : int64 option)];
          Error (`Sequence_gap (sequence, effective_seq))
      | `Accept ->
        (* Apply update *)
        let book' = apply_fn t.book (Update.levels update) in
        let sequence' =
          match (update_type, effective_seq) with
          | Update_type.Snapshot, Some seq -> Sequence.reset t.sequence ~seq
          | Update_type.Snapshot, None -> Sequence.empty
          | Update_type.Delta, Some seq -> Sequence.update t.sequence ~seq
          | Update_type.Delta, None -> t.sequence
        in
        let updates_processed = t.updates_processed + 1 in
        let snapshots_received =
          match update_type with
          | Update_type.Snapshot -> t.snapshots_received + 1
          | Update_type.Delta -> t.snapshots_received
        in
          Ok
            { book= book'
            ; sequence= sequence'
            ; last_update= Update.timestamp update
            ; updates_processed
            ; snapshots_received
            ; max_gaps= t.max_gaps }

  (** Force snapshot update (reset sequence tracking) *)
  let force_snapshot t ~update ~apply_fn =
    let book' = apply_fn t.book (Update.levels update) in
    let sequence' =
      match Update.effective_sequence update with
      | Some seq -> Sequence.reset t.sequence ~seq
      | None -> Sequence.empty
    in
      { book= book'
      ; sequence= sequence'
      ; last_update= Update.timestamp update
      ; updates_processed= t.updates_processed + 1
      ; snapshots_received= t.snapshots_received + 1
      ; max_gaps= t.max_gaps }

  (** Get statistics *)
  let stats t =
    sprintf
      {|updates_processed: %d
snapshots_received: %d
gaps_detected: %d
last_update: %s
last_sequence: %s|}
      t.updates_processed
      t.snapshots_received
      t.sequence.gaps_detected
      (Time_float_unix.to_string t.last_update)
      (Option.value_map t.sequence.last_seen ~default:"None" ~f:Int64.to_string)
end

(** Batch update processor for efficiency *)
module Batch = struct
  type 'book t =
    { manager: 'book Manager.t
    ; pending: Update.t Queue.t
    ; max_batch_size: int }

  let create ?(max_batch_size = 100) manager =
    {manager; pending= Queue.create (); max_batch_size}

  (** Add update to batch *)
  let enqueue t update =
    Queue.enqueue t.pending update;
    Queue.length t.pending >= t.max_batch_size

  (** Process all pending updates *)
  let flush t ~apply_fn =
    let rec process manager = function
      | [] -> Ok manager
      | update :: rest ->
        (match Manager.apply manager ~update ~apply_fn with
         | Ok manager' -> process manager' rest
         | Error e -> Error e)
    in
    let updates = Queue.to_list t.pending in
      match process t.manager updates with
      | Ok manager' ->
        Queue.clear t.pending;
        Ok {t with manager= manager'}
      (* Retain every update on failure. The returned error carries no updated
         batch value, so clearing here would silently lose market data. *)
      | Error e -> Error e

  (** Process updates with automatic flushing *)
  let process t ~update ~apply_fn =
    let should_flush = enqueue t update in
      match should_flush with
      | true -> flush t ~apply_fn
      | false -> Ok t
end

(** Checksum validation (exchange-specific implementations) *)
module Checksum = struct
  type validator = bids:(float * float) list -> asks:(float * float) list -> string

  (** CRC32 checksum placeholder (used by Kraken, OKX, etc.)

      Note: Uses String.hash as a stand-in. For production validation against
      exchange checksums, add 'checkseum' or 'crc' opam package dependency
      and implement proper CRC32 algorithm. The current implementation is
      useful for detecting data changes but won't match exchange checksums. *)
  let crc32 ~bids ~asks =
    let levels_to_string levels =
      List.map levels ~f:(fun (price, size) -> sprintf "%.8f:%.8f" price size)
      |> String.concat ~sep:":"
    in
    let data = sprintf "%s:%s" (levels_to_string bids) (levels_to_string asks) in
    (* Placeholder: uses String.hash. For exchange-compatible CRC32,
       add checkseum library: opam install checkseum *)
    let hash = String.hash data in
      sprintf "%u" (abs hash)

  (** Validate checksum *)
  let validate ~validator ~expected ~bids ~asks =
    let computed = validator ~bids ~asks in
      String.equal computed expected
end

(** Incremental Order Book - Handles snapshots, deltas, and sequence tracking

    This module provides infrastructure for maintaining order books with:
    - Snapshot vs delta update handling
    - Sequence number tracking and gap detection
    - Automatic re-sync triggering
    - Batch update processing
    - Checksum validation (exchange-specific)
*)

open Core

(** Update type - snapshot or delta *)
module Update_type : sig
  type t =
    | Snapshot  (** Full order book snapshot *)
    | Delta     (** Incremental update (add/remove/modify levels) *)
  [@@deriving sexp, compare, equal]
end

(** Sequence tracking for gap detection *)
module Sequence : sig
  type t [@@deriving sexp]

  val empty : t
  val is_valid : t -> seq:int64 -> bool
  val update : t -> seq:int64 -> t
  val reset : t -> seq:int64 -> t
  val needs_resync : t -> max_gaps:int -> bool

  val last_seen : t -> int64 option
  val expected_next : t -> int64 option
  val gaps_detected : t -> int
end

(** Price level update operation *)
module Level_update : sig
  type t = {
    price: float;
    size: float;
    side: [`Bid | `Ask];
  } [@@deriving sexp]

  val create : price:float -> size:float -> side:[`Bid | `Ask] -> t
end

(** Order book update with sequence tracking *)
module Update : sig
  type t [@@deriving sexp]

  val create :
    ?update_type:Update_type.t ->
    ?sequence:int64 ->
    ?sequence_range:(int64 * int64) ->
    ?checksum:string ->
    ?timestamp:Time_float_unix.t ->
    levels:Level_update.t list ->
    unit ->
    t

  val snapshot :
    ?sequence:int64 ->
    ?checksum:string ->
    levels:Level_update.t list ->
    unit ->
    t

  val delta :
    ?sequence:int64 ->
    ?sequence_range:(int64 * int64) ->
    ?checksum:string ->
    levels:Level_update.t list ->
    unit ->
    t

  val effective_sequence : t -> int64 option
  val update_type : t -> Update_type.t
  val levels : t -> Level_update.t list
  val timestamp : t -> Time_float_unix.t
end

(** Incremental order book manager *)
module Manager : sig
  type 'book t [@@deriving sexp]

  val create : ?max_gaps:int -> 'book -> 'book t

  val is_valid_sequence : 'book t -> seq:int64 -> bool
  val needs_resync : 'book t -> bool

  (** Apply update to book using provided apply function
      Returns Error if sequence gap detected *)
  val apply :
    'book t ->
    update:Update.t ->
    apply_fn:('book -> Level_update.t list -> 'book) ->
    ('book t, [`Sequence_gap of Sequence.t * int64 option]) Result.t

  (** Force snapshot update (reset sequence tracking) *)
  val force_snapshot :
    'book t ->
    update:Update.t ->
    apply_fn:('book -> Level_update.t list -> 'book) ->
    'book t

  val stats : 'book t -> string

  val book : 'book t -> 'book
  val sequence : 'book t -> Sequence.t
  val last_update : 'book t -> Time_float_unix.t
  val updates_processed : 'book t -> int
  val snapshots_received : 'book t -> int
end

(** Batch update processor for efficiency *)
module Batch : sig
  type 'book t

  val create : ?max_batch_size:int -> 'book Manager.t -> 'book t

  val enqueue : 'book t -> Update.t -> bool
  val flush :
    'book t ->
    apply_fn:('book -> Level_update.t list -> 'book) ->
    ('book t, [`Sequence_gap of Sequence.t * int64 option]) Result.t

  val process :
    'book t ->
    update:Update.t ->
    apply_fn:('book -> Level_update.t list -> 'book) ->
    ('book t, [`Sequence_gap of Sequence.t * int64 option]) Result.t
end

(** Checksum validation (exchange-specific implementations) *)
module Checksum : sig
  type validator = bids:(float * float) list -> asks:(float * float) list -> string

  val crc32 : validator
  val validate : validator:validator -> expected:string -> bids:(float * float) list -> asks:(float * float) list -> bool
end

(** Allocation-conscious FIX 4.4 framing and field access.

    The decoder validates framing, BodyLength, and CheckSum before exposing any
    fields. Field values remain slices of the original message and are only
    copied when [Field.value] is called. *)

val soh : char

type error =
  [ `Body_length_mismatch of int * int
  | `Checksum_mismatch of int * int
  | `Frame_too_large of int * int
  | `Invalid_body_length of string
  | `Invalid_checksum of string
  | `Invalid_tag of string
  | `Invalid_value of int * string
  | `Malformed_field of int
  | `Missing_field of int
  | `Trailing_data of int
  | `Unexpected_field of int * int
  | `Unsupported_begin_string of string ]
[@@deriving sexp]

module Field : sig
  type t

  val tag : t -> int
  val value_length : t -> int
  val value : message:string -> t -> string
  val value_equal : message:string -> t -> string -> bool
  val int_value : message:string -> t -> (int, error) Result.t
end

module Decimal : sig
  type t [@@deriving sexp, equal]
  (** Exact fixed-point decimal. [scale] is the number of fractional digits. *)

  val of_string : string -> (t, string) Result.t
  val of_field : message:string -> Field.t -> (t, string) Result.t
  val to_string : t -> string
  val mantissa : t -> int64
  val scale : t -> int
end

module Frame : sig
  type t

  val decode : string -> (t, error) Result.t
  val raw : t -> string
  val fields : t -> Field.t array
  val msg_type : t -> string
  val sequence_number : t -> int
  val find : t -> int -> Field.t option
  val find_all : t -> int -> Field.t list
  val value : t -> int -> string option
  val value_exn : t -> int -> string
  val int_value : t -> int -> (int, error) Result.t
end

module Encoder : sig
  val message :
    sender_comp_id:string ->
    target_comp_id:string ->
    msg_type:string ->
    msg_seq_num:int ->
    sending_time:string ->
    body_fields:(int * string) list ->
    (string, error) Result.t
end

module Framer : sig
  type t
  (** Incremental TCP framer. It fails closed on malformed input and never scans
      ahead for a plausible message boundary. *)

  val create : ?max_frame_length:int -> unit -> t
  val pending_bytes : t -> int
  val feed : t -> string -> (Frame.t list, error) Result.t
end

module Sequence : sig
  type t [@@deriving sexp, equal]
  type incoming = [ `Accept | `Possible_duplicate ] [@@deriving sexp, equal]

  type sequence_error =
    [ `Duplicate_without_poss_dup of int * int
    | `Gap of int * int
    | `Possible_duplicate_without_orig_sending_time of int ]
  [@@deriving sexp, equal]

  val create : ?outgoing:int -> ?incoming:int -> unit -> t
  val next_outgoing : t -> int * t
  val accept_incoming : t -> Frame.t -> (t * incoming, sequence_error) Result.t
  val reset : t -> next_outgoing:int -> next_incoming:int -> t
end

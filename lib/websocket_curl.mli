open Async

(** WebSocket client using libcurl for TLS (bypasses Cloudflare fingerprinting) *)

type t

(** Connect to a WebSocket URL.
    @param headers Optional HTTP headers as (name, value) pairs to include in the upgrade request *)
val connect : url:string -> ?headers:(string * string) list -> unit -> t Deferred.Or_error.t

(** Send a text message *)
val send : t -> string -> unit Deferred.t

(** Receive a message (None on close/error).
    Automatically responds to ping frames with pong (RFC 6455). *)
val receive : t -> string option Deferred.t

(** Close the connection *)
val close : t -> unit Deferred.t

(** {1 Frame Encoding (exposed for testing)} *)

(** Encode a masked WebSocket frame with given opcode.
    @param opcode WebSocket opcode (1=text, 2=binary, 8=close, 9=ping, 10=pong) *)
val encode_frame : opcode:int -> string -> string

(** Encode a masked text frame (opcode 1) *)
val encode_text_frame : string -> string

(** Encode a masked pong frame (opcode 10) *)
val encode_pong_frame : string -> string

(** {1 Handshake Utilities (exposed for testing)} *)

(** Compute the Sec-WebSocket-Accept value per RFC 6455 section 4.2.2 *)
val compute_accept_key : string -> string

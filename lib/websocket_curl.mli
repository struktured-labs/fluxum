open Async

(** WebSocket client using libcurl for TLS (bypasses Cloudflare fingerprinting) *)

type t

(** Connect to a WebSocket URL.
    @param headers Optional HTTP headers as (name, value) pairs to include in the upgrade request *)
val connect : url:string -> ?headers:(string * string) list -> unit -> t Deferred.Or_error.t

(** Send a text message *)
val send : t -> string -> unit Deferred.t

(** Receive a message (None on close/error) *)
val receive : t -> string option Deferred.t

(** Close the connection *)
val close : t -> unit Deferred.t

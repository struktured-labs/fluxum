(** Extended curl functions not exposed in ocurl *)

val set_nonblocking : Curl.t -> unit
(** [set_nonblocking conn] sets the underlying socket to non-blocking mode.
    Must be called after connection is established (CONNECT_ONLY mode).
    @raise Failure on error *)

val send : Curl.t -> bytes -> int -> int -> int
(** [send conn buf offset len] sends data through an established curl connection.
    Returns the number of bytes actually sent.
    @raise Failure on error *)

val recv : Curl.t -> bytes -> int -> int -> int
(** [recv conn buf offset len] receives data through an established curl connection.
    Returns the number of bytes actually received.
    @raise Failure on error *)

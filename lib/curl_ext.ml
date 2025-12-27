(* Extended curl functions not exposed in ocurl *)

external send : Curl.t -> bytes -> int -> int -> int = "caml_curl_easy_send"
(** [send conn buf offset len] sends data through an established curl connection.
    Returns the number of bytes actually sent.
    @raise Failure on error *)

external recv : Curl.t -> bytes -> int -> int -> int = "caml_curl_easy_recv"
(** [recv conn buf offset len] receives data through an established curl connection.
    Returns the number of bytes actually received.
    @raise Failure on error *)

type reader =
  { pipe : int Inf_pipe.Reader.t
  ; sequencer : unit Throttle.Sequencer.t
  }

(** Serialize an operation through this nonce's sequencer.
    Ensures nonce-read + HTTP-call are atomic — prevents out-of-order delivery. *)
let enqueue t f = Throttle.enqueue t.sequencer (fun () -> f ())

module type S = sig
  type t [@@deriving sexp]

  val pipe : init:t -> unit -> reader Deferred.t
end

module Counter : S with type t = int = struct
  type t = int [@@deriving sexp]

  let pipe ~init () =
    let pipe =
      Inf_pipe.unfold ~init ~f:(fun s ->
        let s' = s + 1 in
          (s, s') |> return)
    in
    return { pipe; sequencer = Throttle.Sequencer.create ~continue_on_error:true () }
end

module File = struct
  let create_nonce_file ?(default = 0) filename =
    Unix.mkdir ~p:() (Filename.dirname filename) ~perm:0o700
    >>= fun () ->
    try_with ~extract_exn:true (fun () ->
      Unix.with_file ~mode:[`Rdonly] filename ~f:(fun _fd -> Deferred.unit))
    >>= function
    | Result.Ok _ -> Deferred.unit
    | Result.Error _ -> Writer.save ~contents:(sprintf "%d\n" default) filename

  type t = string [@@deriving sexp]

  (** Get current millisecond timestamp as nonce base.
      Thread-safe — uses gettimeofday which works in any context. *)
  let now_ms () =
    Time_float_unix.now ()
    |> Time_float_unix.to_span_since_epoch
    |> Time_float.Span.to_ms
    |> Int64.of_float
    |> Int64.to_int_exn

  (** Atomically read-increment-write a nonce file using flock.
      Safe for multiple processes sharing one Gemini API key.

      Each call:
      1. Opens the shared nonce file
      2. Acquires an exclusive flock (blocks until available)
      3. Reads the current nonce value
      4. Computes next = max(current + 1, now_ms())
      5. Writes back, truncates, unlocks

      This guarantees globally-increasing nonces across all processes.
      The flock serializes access — no two processes can increment
      simultaneously, eliminating InvalidNonce errors permanently.

      Runs in In_thread to avoid blocking the Async scheduler. *)
  let locked_increment filename =
    In_thread.run (fun () ->
      let fd = Core_unix.openfile filename
        ~mode:[Core_unix.O_RDWR; Core_unix.O_CREAT]
        ~perm:0o600 in
      let cleanup () =
        (try Core_unix.flock_blocking fd Core_unix.Flock_command.unlock with _ -> ());
        Core_unix.close fd
      in
      match
        Core_unix.flock_blocking fd Core_unix.Flock_command.lock_exclusive;
        let buf = Bytes.create 64 in
        let n = Core_unix.read fd ~buf ~pos:0 ~len:64 in
        let contents = Bytes.To_string.sub buf ~pos:0 ~len:n in
        let current =
          match Int.of_string (String.strip contents) with
          | n -> n
          | exception _ -> now_ms ()
        in
        let next = Int.max (current + 1) (now_ms ()) in
        let _pos = Core_unix.lseek fd 0L ~mode:Core_unix.SEEK_SET in
        let data = sprintf "%d\n" next in
        let _written = Core_unix.write_substring fd ~buf:data ~pos:0
          ~len:(String.length data) in
        Core_unix.ftruncate fd ~len:(Int64.of_int (String.length data));
        next
      with
      | nonce -> cleanup (); nonce
      | exception exn -> cleanup (); raise exn)

  (** Shared nonce file pipe using flock for cross-process safety.

      Unlike per-bot nonce files, ALL bots using the same API key share
      a single nonce file. The flock ensures only one process increments
      at a time, preventing InvalidNonce errors that occur when multiple
      processes race to use the same Gemini API key.

      The init parameter is the path to the shared nonce file.
      On first use, the file is created with a nonce based on now_ms(). *)
  let pipe ~init:filename () =
    Cfg.create_config_dir ()
    >>= fun () ->
    create_nonce_file ?default:None filename
    >>= fun () ->
    let pipe =
      Inf_pipe.unfold ~init:() ~f:(fun () ->
        let%bind nonce = locked_increment filename in
          return (nonce, ()))
    in
    return { pipe; sequencer = Throttle.Sequencer.create ~continue_on_error:true () }

  let default_filename =
    let root_path =
      match Unix.getenv "HOME" with
      | Some h -> h
      | None ->
        (match Unix.getenv "XDG_CONFIG_HOME" with
         | Some h -> h
         | None -> "/tmp")
    in
      sprintf "%s/.gemini/nonce.txt" root_path

  let default = pipe ~init:default_filename
end

let _assert_module_file_is_nonce =
  let module F : S with type t = string = File in
  ()

module Request = struct
  type request_nonce =
    { request: string
    ; nonce: int }
  [@@deriving sexp, yojson]

  type t =
    { request: string
    ; nonce: int
    ; payload: Yojson.Safe.t option [@default None] }

  let make ~request ~nonce ?payload () =
    Inf_pipe.read nonce.pipe >>= fun n -> return {request; nonce = n; payload}

  let to_yojson {request; nonce; payload} : Yojson.Safe.t =
    match request_nonce_to_yojson {request; nonce} with
    | `Assoc assoc as a ->
      (match Option.value ~default:`Null payload with
       | `Null -> a
       | `Assoc assoc' -> `Assoc (assoc @ assoc')
       | #Yojson.Safe.t as unsupported_yojson ->
         failwithf
           "expected json association for request payload but got %S"
           (Yojson.Safe.to_string unsupported_yojson)
           ())
    | #Yojson.Safe.t as unsupported_yojson ->
      failwithf
        "expected json association for type request_nonce but got %S"
        (Yojson.Safe.to_string unsupported_yojson)
        ()
end

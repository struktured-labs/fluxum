type reader = int Inf_pipe.Reader.t

module type S = sig
  type t [@@deriving sexp]

  val pipe : init:t -> unit -> int Inf_pipe.Reader.t Deferred.t
end

module Counter : S with type t = int = struct
  type t = int [@@deriving sexp]

  let pipe ~init () =
    Inf_pipe.unfold ~init ~f:(fun s ->
        let s' = s + 1 in
        (s, s') |> return )
    |> return
end

module File = struct
  let create_nonce_file ?(default = 0) filename =
    Unix.mkdir ~p:() (Filename.dirname filename) ~perm:0o700 >>= fun () ->
    try_with ~extract_exn:true (fun () ->
        Unix.with_file ~mode:[ `Rdonly ] filename ~f:(fun _fd -> Deferred.unit) )
    >>= function
    | Result.Ok _ -> Deferred.unit
    | Result.Error _ -> Writer.save ~contents:(sprintf "%d\n" default) filename

  type t = string [@@deriving sexp]

  (** Get current millisecond timestamp as nonce base *)
  let now_ms () =
    Time_float_unix.now ()
    |> Time_float_unix.to_span_since_epoch
    |> Time_float.Span.to_ms
    |> Int64.of_float
    |> Int64.to_int_exn

  (** Use timestamp-based nonces: every nonce = max(prev+1, now_ms).
      This is self-healing across process restarts and competing processes:
      - Each nonce uses the current timestamp (always increasing over time)
      - If multiple calls happen within 1ms, falls back to prev+1
      - On restart, the new timestamp is always higher than old nonces
      - No dependency on file state for correctness (file is advisory only)
      Persists periodically for compatibility with other tools. *)
  let pipe ~init:filename () =
    Cfg.create_config_dir () >>= fun () ->
    create_nonce_file ?default:None filename >>= fun () ->
    Reader.with_file filename ~f:(fun reader ->
        Reader.really_read_line reader
          ~wait_time:(Time_float.Span.of_ms 1.0))
    >>= fun line ->
    let file_nonce =
      match line with
      | None -> 0
      | Some nonce_str ->
        (try Int.of_string (String.strip nonce_str) with _ -> 0)
    in
    (* Bootstrap: use max of current timestamp and file nonce + safety margin *)
    let initial_nonce = Int.max (now_ms ()) (file_nonce + 100000) in
    (* Persist immediately *)
    Writer.save filename ~contents:(sprintf "%d\n" initial_nonce)
    >>= fun () ->
    Inf_pipe.unfold ~init:initial_nonce ~f:(fun prev_nonce ->
        (* KEY FIX: Use current timestamp for EVERY nonce, not just the first.
           This guarantees self-healing: even if a competing process used a
           higher nonce, the next call uses current time which is always
           moving forward. With 300ms+ delays between calls, each nonce
           gets a unique millisecond timestamp. *)
        let nonce = Int.max (prev_nonce + 1) (now_ms ()) in
        (* Persist every 100 nonces for recovery *)
        (match nonce mod 100 with
         | 0 -> don't_wait_for (Writer.save filename ~contents:(sprintf "%d\n" nonce))
         | _ -> ());
        return (nonce, nonce) )
    |> return

  let default_filename =
    let root_path = match Unix.getenv "HOME" with
      | Some h -> h
      | None -> match Unix.getenv "XDG_CONFIG_HOME" with
        | Some h -> h
        | None -> "/tmp"
    in
    sprintf "%s/.gemini/nonce.txt" root_path

  let default = pipe ~init:default_filename
end

let _assert_module_file_is_nonce =
  let module F : S with type t = string = File in
  ()

module Request = struct
  type request_nonce =
    { request : string;
      nonce : int
    }
  [@@deriving sexp, yojson]

  type t =
    { request : string;
      nonce : int;
      payload : Yojson.Safe.t option [@default None]
    }

  let make ~request ~nonce ?payload () =
    Inf_pipe.read nonce >>= fun nonce -> return { request; nonce; payload }

  let to_yojson { request; nonce; payload } : Yojson.Safe.t =
    match request_nonce_to_yojson { request; nonce } with
    | `Assoc assoc as a -> (
      match Option.value ~default:`Null payload with
      | `Null -> a
      | `Assoc assoc' -> `Assoc (assoc @ assoc')
      | #Yojson.Safe.t as unsupported_yojson ->
        failwithf "expected json association for request payload but got %S"
          (Yojson.Safe.to_string unsupported_yojson)
          () )
    | #Yojson.Safe.t as unsupported_yojson ->
      failwithf "expected json association for type request_nonce but got %S"
        (Yojson.Safe.to_string unsupported_yojson)
        ()
end

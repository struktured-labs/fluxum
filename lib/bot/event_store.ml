(** Event Store - Persistent Event Log with Streaming

    Provides:
    - Streaming writes via bin_prot
    - File rotation by size
    - Read/replay capabilities
    - Pipe-based interface for async consumption
*)

open Core
open Async

module Config = struct
  type t =
    { base_path : string
    ; max_file_size : int            (** Rotate after this many bytes *)
    ; sync_interval : Event.Time_span.t (** fsync interval *)
    ; buffer_size : int              (** Write buffer size *)
    }
  [@@deriving sexp]

  let default =
    { base_path = "./events"
    ; max_file_size = 100 * 1024 * 1024  (** 100 MB *)
    ; sync_interval = Event.Time_span.of_sec 1.0
    ; buffer_size = 64 * 1024  (** 64 KB *)
    }
end

(** Writer for streaming events to disk *)
module Writer = struct
  type t =
    { config : Config.t
    ; bot_id : string
    ; mutable current_file : string
    ; mutable writer : Writer.t option
    ; mutable file_size : int
    ; mutable sequence : int64
    ; mutable last_sync : Event.Time.t
    ; pipe_writer : Event.envelope Pipe.Writer.t
    ; pipe_reader : Event.envelope Pipe.Reader.t
    }

  let file_pattern ~base_path ~bot_id ~index =
    sprintf "%s/%s_%06d.events" base_path bot_id index

  let ensure_dir path =
    let dir = Filename.dirname path in
    Core_unix.mkdir_p dir

  let next_file_index ~base_path ~bot_id =
    let%bind files = Sys.ls_dir base_path in
    let indices =
      List.filter_map files ~f:(fun f ->
        let prefix = sprintf "%s_" bot_id in
        match String.chop_prefix f ~prefix with
        | None -> None
        | Some rest ->
          match String.chop_suffix rest ~suffix:".events" with
          | None -> None
          | Some idx_str ->
            try Some (Int.of_string idx_str)
            with _ -> None
      )
    in
    let max_idx = List.fold indices ~init:0 ~f:Int.max in
    return (max_idx + 1)

  let open_file t =
    let%bind () = ensure_dir t.current_file |> return in
    let%bind writer = Writer.open_file t.current_file in
    t.writer <- Some writer;
    t.file_size <- 0;
    return ()

  let rotate t =
    match t.writer with
    | None -> return ()
    | Some writer ->
      let%bind () = Writer.close writer in
      let%bind next_idx = next_file_index ~base_path:t.config.base_path ~bot_id:t.bot_id in
      t.current_file <- file_pattern ~base_path:t.config.base_path ~bot_id:t.bot_id ~index:next_idx;
      open_file t

  let maybe_sync t =
    let now = Event.Time.now () in
    let elapsed = Event.Time.diff now t.last_sync in
    match Event.Time_span.(elapsed >= t.config.sync_interval) with
    | false -> return ()
    | true ->
      t.last_sync <- now;
      match t.writer with
      | None -> return ()
      | Some writer -> Writer.flushed writer

  let rec write_envelope t envelope =
    let data = Event.serialize envelope in
    let len = String.length data in
    (* Write length prefix (4 bytes, little endian) then data *)
    let len_buf = Bytes.create 4 in
    (* Write 4 bytes little-endian *)
    Bytes.set len_buf 0 (Char.of_int_exn (len land 0xff));
    Bytes.set len_buf 1 (Char.of_int_exn ((len lsr 8) land 0xff));
    Bytes.set len_buf 2 (Char.of_int_exn ((len lsr 16) land 0xff));
    Bytes.set len_buf 3 (Char.of_int_exn ((len lsr 24) land 0xff));
    match t.writer with
    | None ->
      let%bind () = open_file t in
      write_envelope t envelope
    | Some writer ->
      Writer.write_bytes writer len_buf;
      Writer.write writer data;
      t.file_size <- t.file_size + 4 + len;
      let%bind () =
        match t.file_size >= t.config.max_file_size with
        | true -> rotate t
        | false -> return ()
      in
      maybe_sync t

  let create config ~bot_id =
    let pipe_reader, pipe_writer = Pipe.create () in
    let base_path = config.Config.base_path in
    let%bind () = return (Core_unix.mkdir_p base_path) in
    let%bind next_idx = next_file_index ~base_path ~bot_id in
    let current_file = file_pattern ~base_path ~bot_id ~index:next_idx in
    let t =
      { config
      ; bot_id
      ; current_file
      ; writer = None
      ; file_size = 0
      ; sequence = 0L
      ; last_sync = Event.Time.now ()
      ; pipe_writer
      ; pipe_reader
      }
    in
    (* Start background writer that consumes from pipe *)
    don't_wait_for (
      Pipe.iter t.pipe_reader ~f:(fun envelope ->
        write_envelope t envelope
      )
    );
    return t

  let write t event =
    t.sequence <- Int64.(t.sequence + 1L);
    let envelope = { event with Event.sequence = t.sequence } in
    match Pipe.is_closed t.pipe_writer with
    | true -> return ()
    | false ->
      Pipe.write_without_pushback t.pipe_writer envelope;
      return ()

  let write_batch t events =
    Deferred.List.iter events ~f:(write t)

  let sync t =
    match t.writer with
    | None -> return ()
    | Some writer ->
      let%bind () = Writer.flushed writer in
      t.last_sync <- Event.Time.now ();
      return ()

  let close t =
    Pipe.close t.pipe_writer;
    let%bind () = Pipe.closed t.pipe_reader in
    match t.writer with
    | None -> return ()
    | Some writer -> Writer.close writer

  let pipe t = t.pipe_writer
  let current_sequence t = t.sequence
end

(** Reader for loading events from disk *)
module Reader = struct
  type t =
    { path : string
    ; mutable reader : Reader.t option
    }

  let open_ ~path =
    return { path; reader = None }

  let read_envelope_from_reader reader =
    (* Read 4-byte length prefix *)
    let len_buf = Bytes.create 4 in
    match%bind Reader.really_read reader ~len:4 len_buf with
    | `Eof _ -> return None
    | `Ok ->
      (* Read 4 bytes little-endian *)
      let len =
        Char.to_int (Bytes.get len_buf 0)
        lor (Char.to_int (Bytes.get len_buf 1) lsl 8)
        lor (Char.to_int (Bytes.get len_buf 2) lsl 16)
        lor (Char.to_int (Bytes.get len_buf 3) lsl 24)
      in
      let data_buf = Bytes.create len in
      match%bind Reader.really_read reader ~len data_buf with
      | `Eof _ -> return None
      | `Ok ->
        let data = Bytes.to_string data_buf in
        match Event.deserialize data with
        | Ok envelope -> return (Some envelope)
        | Error _ -> return None

  let read_all t =
    let%bind reader = Reader.open_file t.path in
    t.reader <- Some reader;
    let rec loop acc =
      match%bind read_envelope_from_reader reader with
      | None -> return (List.rev acc)
      | Some envelope -> loop (envelope :: acc)
    in
    let%bind envelopes = loop [] in
    let%bind () = Reader.close reader in
    t.reader <- None;
    return envelopes

  let stream t =
    let%bind reader = Reader.open_file t.path in
    t.reader <- Some reader;
    let pipe_reader, pipe_writer = Pipe.create () in
    don't_wait_for (
      let rec loop () =
        match%bind read_envelope_from_reader reader with
        | None ->
          Pipe.close pipe_writer;
          Reader.close reader
        | Some envelope ->
          match Pipe.is_closed pipe_writer with
          | true -> Reader.close reader
          | false ->
            Pipe.write_without_pushback pipe_writer envelope;
            loop ()
      in
      loop ()
    );
    return pipe_reader

  let read_range t ~start ~end_ =
    let%bind all = read_all t in
    let filtered =
      List.filter all ~f:(fun env ->
        Time_ns.(env.Event.timestamp >= start && env.Event.timestamp <= end_)
      )
    in
    return filtered

  let tail t =
    (* For tailing, we'd need inotify - simplified version just reads existing *)
    stream t

  let close t =
    match t.reader with
    | None -> return ()
    | Some reader ->
      t.reader <- None;
      Reader.close reader
end

(** Replay events to reconstruct state *)
let replay ~path ~f ~init =
  let%bind reader = Reader.open_ ~path in
  let%bind envelopes = Reader.read_all reader in
  let%bind () = Reader.close reader in
  let final_state = List.fold envelopes ~init ~f:(fun state env -> f env state) in
  return final_state

(** Replay from directory (all event files for a bot) *)
let replay_all ~base_path ~bot_id ~f ~init =
  let%bind files = Sys.ls_dir base_path in
  let event_files =
    List.filter files ~f:(fun f ->
      String.is_prefix f ~prefix:(bot_id ^ "_") &&
      String.is_suffix f ~suffix:".events"
    )
    |> List.sort ~compare:String.compare  (* Ensures chronological order *)
    |> List.map ~f:(fun f -> base_path ^/ f)
  in
  Deferred.List.fold event_files ~init ~f:(fun state path ->
    replay ~path ~f ~init:state
  )

(** Get all event files for a bot *)
let list_event_files ~base_path ~bot_id =
  let%bind files = Sys.ls_dir base_path in
  let event_files =
    List.filter files ~f:(fun f ->
      String.is_prefix f ~prefix:(bot_id ^ "_") &&
      String.is_suffix f ~suffix:".events"
    )
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun f -> base_path ^/ f)
  in
  return event_files

(** Get stats for event files *)
module Stats = struct
  type file_stats =
    { path : string
    ; size_bytes : int
    ; event_count : int
    ; first_event : Event.Time.t option
    ; last_event : Event.Time.t option
    }
  [@@deriving sexp_of]

  type t =
    { bot_id : string
    ; total_events : int
    ; total_bytes : int
    ; files : file_stats list
    ; time_range : (Event.Time.t * Event.Time.t) option
    }
  [@@deriving sexp_of]

  let for_file path =
    let%bind stat = return (Core_unix.stat path) in
    let size_bytes = Int64.to_int_exn stat.st_size in
    let%bind reader = Reader.open_ ~path in
    let%bind envelopes = Reader.read_all reader in
    let%bind () = Reader.close reader in
    let event_count = List.length envelopes in
    let first_event = List.hd envelopes |> Option.map ~f:(fun e -> e.Event.timestamp) in
    let last_event = List.last envelopes |> Option.map ~f:(fun e -> e.Event.timestamp) in
    return { path; size_bytes; event_count; first_event; last_event }

  let for_bot ~base_path ~bot_id =
    let%bind files = list_event_files ~base_path ~bot_id in
    let%bind file_stats = Deferred.List.map files ~how:`Sequential ~f:for_file in
    let total_events = List.sum (module Int) file_stats ~f:(fun s -> s.event_count) in
    let total_bytes = List.sum (module Int) file_stats ~f:(fun s -> s.size_bytes) in
    let all_first = List.filter_map file_stats ~f:(fun s -> s.first_event) in
    let all_last = List.filter_map file_stats ~f:(fun s -> s.last_event) in
    let time_range =
      match List.min_elt all_first ~compare:Event.Time.compare,
            List.max_elt all_last ~compare:Event.Time.compare with
      | Some first, Some last -> Some (first, last)
      | _ -> None
    in
    return { bot_id; total_events; total_bytes; files = file_stats; time_range }
end

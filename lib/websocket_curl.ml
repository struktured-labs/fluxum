open Core
open Async

(* Suppress unused value warnings for helper functions *)
[@@@warning "-32"]

(* WebSocket client using libcurl's send/recv (TLS-aware) *)

let random_key () =
  let bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set bytes i (Char.of_int_exn (Random.int 256))
  done;
  Base64.encode_exn (Bytes.to_string bytes)

let compute_accept_key key =
  let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let combined = key ^ magic in
  let hash = Digestif.SHA1.digest_string combined in
  Base64.encode_exn (Digestif.SHA1.to_raw_string hash)

type t =
  { curl : Curl.t
  ; mutable closed : bool
  ; leftover : bytes  (* Bytes read during HTTP upgrade that belong to the first WS frame *)
  ; mutable leftover_len : int
  ; fragment_buf : Buffer.t  (* Buffer for reassembling fragmented WebSocket messages *)
  }

let connect ~url ?headers () =
  let open Deferred.Or_error.Let_syntax in

  (* Parse URL for building WebSocket upgrade request *)
  let uri = Uri.of_string url in
  let%bind host = Deferred.return (
    Result.of_option (Uri.host uri) ~error:(Error.of_string "URL must have host")
  ) in
  let path = match Uri.path uri with "" | "/" -> "/" | p -> p in

  (* Use curl to establish TLS connection *)
  let%bind curl =
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        let conn = Curl.init () in
        (* Convert wss:// to https:// for CONNECT_ONLY mode *)
        let connect_url =
          match String.is_prefix url ~prefix:"wss://" with
          | true -> "https://" ^ String.drop_prefix url 6
          | false ->
            match String.is_prefix url ~prefix:"ws://" with
            | true -> "http://" ^ String.drop_prefix url 5
            | false -> url
        in
        Curl.set_url conn connect_url;
        Curl.set_connectonly conn true;
        Curl.set_httpversion conn Curl.HTTP_VERSION_1_1;  (* Force HTTP/1.1, not HTTP/2 *)
        Curl.set_nosignal conn true;  (* Prevent SIGPIPE signals *)
        Curl.set_tcpnodelay conn true;  (* Disable Nagle's algorithm for low-latency sends *)
        Curl.perform conn;
        (* Set socket to non-blocking mode for fast send/recv operations *)
        Curl_ext.set_nonblocking conn;
        conn
      )
    )
  in

  (* Build and send WebSocket upgrade request *)
  let ws_key = random_key () in
  let extra_headers = match headers with
    | None -> []
    | Some h ->
      List.map h ~f:(fun (name, value) -> sprintf "%s: %s" name value)
  in
  let request = String.concat ~sep:"\r\n"
    ([ sprintf "GET %s HTTP/1.1" path
     ; sprintf "Host: %s" host
     ; "Upgrade: websocket"
     ; "Connection: Upgrade"
     ; sprintf "Sec-WebSocket-Key: %s" ws_key
     ; "Sec-WebSocket-Version: 13"
     ; sprintf "Origin: https://%s" host
     ] @ extra_headers @ [ ""; "" ]) in

  (* Send using curl's send (goes through TLS) *)
  let%bind () =
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        let bytes = Bytes.of_string request in
        let rec send_all offset =
          match offset >= Bytes.length bytes with
          | true -> ()
          | false ->
            let sent = Curl_ext.send curl bytes offset (Bytes.length bytes - offset) in
            match sent = 0 with
            | true -> failwith "Connection closed while sending"
            | false -> send_all (offset + sent)
        in
        send_all 0
      )
    )
  in

  (* Read HTTP response using curl's recv, preserving any leftover bytes *)
  let%bind (response_lines, leftover_bytes, leftover_length) =
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        let buffer = Buffer.create 4096 in
        let recv_buf = Bytes.create 1024 in
        let rec read_until_headers () =
          let received = Curl_ext.recv curl recv_buf 0 (Bytes.length recv_buf) in
          match received = 0 with
          | true -> failwith "Connection closed during upgrade"
          | false ->
            Buffer.add_subbytes buffer recv_buf ~pos:0 ~len:received;
            let content = Buffer.contents buffer in
            (* Check if we have complete headers (ends with \r\n\r\n) *)
            match String.is_substring content ~substring:"\r\n\r\n" with
            | true -> content
            | false -> read_until_headers ()
        in
        let response = read_until_headers () in
        (* Split at \r\n\r\n boundary - save any leftover bytes for WS frames *)
        let header_end =
          match String.substr_index response ~pattern:"\r\n\r\n" with
          | Some idx -> idx + 4
          | None -> String.length response
        in
        let leftover_str = String.drop_prefix response header_end in
        let leftover_bytes = Bytes.of_string leftover_str in
        let leftover_length = String.length leftover_str in
        (* Parse only the header portion *)
        let header_portion = String.prefix response header_end in
        let lines = String.split header_portion ~on:'\n' in
        let parsed_lines =
          List.map lines ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s))
        in
        (parsed_lines, leftover_bytes, leftover_length)
      )
    )
  in

  (* Parse status code *)
  let status_code =
    match response_lines with
    | [] -> 0
    | status_line :: _ ->
      match String.split status_line ~on:' ' with
      | _ :: code_str :: _ ->
        (match Int.of_string code_str with
        | code -> code
        | exception _ -> 0)
      | _ -> 0
  in

  match status_code <> 101 with
  | true ->
    Deferred.Or_error.error_string (sprintf "WebSocket upgrade failed: HTTP %d" status_code)
  | false ->
    (* Parse headers *)
    let headers = String.Table.create () in
    List.iter (List.tl_exn response_lines) ~f:(fun line ->
      match String.lsplit2 line ~on:':' with
      | Some (name, value) ->
        Hashtbl.set headers
          ~key:(String.lowercase (String.strip name))
          ~data:(String.strip value)
      | None -> ()
    );

    (* Verify Sec-WebSocket-Accept *)
    let expected_accept = compute_accept_key ws_key in
    match Hashtbl.find headers "sec-websocket-accept" with
    | Some actual when String.equal actual expected_accept ->
      return { curl; closed = false; leftover = leftover_bytes; leftover_len = leftover_length
             ; fragment_buf = Buffer.create 4096 }
    | _ ->
      Deferred.Or_error.error_string "Invalid Sec-WebSocket-Accept"

(* Encode a masked WebSocket frame with given opcode *)
let encode_frame ~opcode payload =
  let len = String.length payload in
  let header = Buffer.create 14 in

  (* FIN + opcode *)
  Buffer.add_char header (Char.of_int_exn (0x80 lor opcode));

  (* Mask bit + length *)
  (match len < 126 with
   | true ->
     Buffer.add_char header (Char.of_int_exn (0x80 lor len))
   | false ->
     match len < 65536 with
     | true ->
       Buffer.add_char header (Char.of_int_exn (0x80 lor 126));
       Buffer.add_char header (Char.of_int_exn (len lsr 8));
       Buffer.add_char header (Char.of_int_exn (len land 0xFF))
     | false ->
       Buffer.add_char header (Char.of_int_exn (0x80 lor 127));
       for _ = 0 to 3 do Buffer.add_char header '\x00' done;
       Buffer.add_char header (Char.of_int_exn ((len lsr 24) land 0xFF));
       Buffer.add_char header (Char.of_int_exn ((len lsr 16) land 0xFF));
       Buffer.add_char header (Char.of_int_exn ((len lsr 8) land 0xFF));
       Buffer.add_char header (Char.of_int_exn (len land 0xFF)));

  (* Masking key *)
  let mask = Array.init 4 ~f:(fun _ -> Random.int 256) in
  Array.iter mask ~f:(fun b -> Buffer.add_char header (Char.of_int_exn b));

  (* Mask payload *)
  for i = 0 to len - 1 do
    let byte = Char.to_int payload.[i] in
    let masked = byte lxor mask.(i mod 4) in
    Buffer.add_char header (Char.of_int_exn masked)
  done;

  Buffer.contents header

(* Encode WebSocket text frame (opcode 1) *)
let encode_text_frame payload = encode_frame ~opcode:1 payload

(* Encode WebSocket pong frame (opcode 10) — echo ping payload per RFC 6455 *)
let encode_pong_frame payload = encode_frame ~opcode:10 payload

(* Encode WebSocket ping frame (opcode 9) — for proactive health monitoring *)
let encode_ping_frame payload = encode_frame ~opcode:9 payload

(* Blocking pong send — called from within In_thread.run receive context *)
let send_pong_blocking t payload =
  let frame = encode_pong_frame payload in
  let bytes = Bytes.of_string frame in
  let rec send_all offset =
    match offset >= Bytes.length bytes with
    | true -> ()
    | false ->
      let sent = Curl_ext.send t.curl bytes offset (Bytes.length bytes - offset) in
      match sent = 0 with
      | true -> ()  (* connection closing, recv loop will handle *)
      | false -> send_all (offset + sent)
  in
  send_all 0

(* Send a ping frame asynchronously *)
let send_ping t ?(payload = "") () =
  match t.closed with
  | true -> Deferred.Or_error.error_string "WebSocket connection closed"
  | false ->
    let send_with_timeout () =
      Deferred.any
        [ (Deferred.Or_error.try_with (fun () ->
             In_thread.run (fun () ->
               let frame = encode_ping_frame payload in
               let bytes = Bytes.of_string frame in
               let rec send_all offset =
                 match offset >= Bytes.length bytes with
                 | true -> ()
                 | false ->
                   let sent = Curl_ext.send t.curl bytes offset (Bytes.length bytes - offset) in
                   match sent = 0 with
                   | true -> failwith "Connection closed"
                   | false -> send_all (offset + sent)
               in
               send_all 0
             )
           ))
        ; (let%map () = after (Time_float.Span.of_sec 1.5) in
           Error (Error.of_string "WebSocket ping send timeout after 1.5s"))
        ]
    in
    send_with_timeout ()

let send t payload =
  match t.closed with
  | true -> Deferred.unit
  | false ->
    let send_with_timeout () =
      Deferred.any
        [ (Deferred.Or_error.try_with (fun () ->
             In_thread.run (fun () ->
               let frame = encode_text_frame payload in
               let bytes = Bytes.of_string frame in
               let rec send_all offset =
                 match offset >= Bytes.length bytes with
                 | true -> ()
                 | false ->
                   let sent = Curl_ext.send t.curl bytes offset (Bytes.length bytes - offset) in
                   match sent = 0 with
                   | true -> failwith "Connection closed"
                   | false -> send_all (offset + sent)
               in
               send_all 0
             )
           ))
        ; (let%map () = after (Time_float.Span.of_sec 1.5) in
           Error (Error.of_string "WebSocket send timeout after 1.5s"))
        ]
    in
    send_with_timeout ()
    >>| function
    | Ok () -> ()
    | Error _ -> ()

(** Result type for receive operations that distinguishes message types *)
type receive_result =
  | Message of string        (** Application data (text/binary frame) *)
  | Pong_received            (** Pong frame was received (for health monitoring) *)
  | Connection_closed        (** Connection was closed *)

(** Internal receive implementation that returns detailed frame info.
    @param on_pong Optional callback invoked when pong frames are received *)
let receive_internal t ~on_pong =
  match t.closed with
  | true -> return Connection_closed
  | false ->
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        let header_buf = Bytes.create 14 in
        (* recv_exact: reads exactly len bytes, using leftover buffer first *)
        let rec recv_exact buf offset len =
          match len = 0 with
          | true -> ()
          | false ->
            match t.leftover_len > 0 with
            | true ->
              let to_copy = min len t.leftover_len in
              Bytes.blit ~src:t.leftover ~src_pos:(Bytes.length t.leftover - t.leftover_len)
                ~dst:buf ~dst_pos:offset ~len:to_copy;
              t.leftover_len <- t.leftover_len - to_copy;
              recv_exact buf (offset + to_copy) (len - to_copy)
            | false ->
              let received = Curl_ext.recv t.curl buf offset len in
              match received = 0 with
              | true -> failwith "Connection closed"
              | false -> recv_exact buf (offset + received) (len - received)
        in

        (* Read one WebSocket frame and return (opcode, fin, payload) *)
        let read_frame () =
          recv_exact header_buf 0 2;
          let byte0 = Bytes.get header_buf 0 |> Char.to_int in
          let byte1 = Bytes.get header_buf 1 |> Char.to_int in
          let opcode = byte0 land 0x0F in
          let fin = (byte0 land 0x80) <> 0 in
          let masked = (byte1 land 0x80) <> 0 in
          let payload_len_initial = byte1 land 0x7F in
          let payload_len =
            match payload_len_initial < 126 with
            | true -> payload_len_initial
            | false ->
              match payload_len_initial = 126 with
              | true ->
                recv_exact header_buf 2 2;
                (Bytes.get header_buf 2 |> Char.to_int) lsl 8
                lor (Bytes.get header_buf 3 |> Char.to_int)
              | false ->
                recv_exact header_buf 2 8;
                (Bytes.get header_buf 6 |> Char.to_int) lsl 24
                lor (Bytes.get header_buf 7 |> Char.to_int) lsl 16
                lor (Bytes.get header_buf 8 |> Char.to_int) lsl 8
                lor (Bytes.get header_buf 9 |> Char.to_int)
          in
          match payload_len = 0 with
          | true -> (opcode, fin, "")
          | false ->
            (match masked with true -> recv_exact header_buf 0 4 | false -> ());
            let payload_buf = Bytes.create payload_len in
            recv_exact payload_buf 0 payload_len;
            (opcode, fin, Bytes.to_string payload_buf)
        in

        (* Handle WebSocket frame fragmentation:
           - Continuation frames (opcode=0) are parts of a fragmented message
           - Non-continuation frames (opcode=1/2) with FIN=true are complete messages
           - Non-continuation frames with FIN=false start a new fragmented message
           - During fragmentation, complete non-continuation frames (like heartbeats)
             are returned immediately; continuation fragments are buffered
           - fragment_buf persists across receive calls for multi-call reassembly *)
        let rec read_message () =
          let (opcode, fin, payload) = read_frame () in
          match opcode with
          | 0 ->
            (* Continuation frame - part of a fragmented message *)
            Buffer.add_string t.fragment_buf payload;
            (match fin with
             | true ->
               (* Final fragment - return assembled message *)
               let result = Buffer.contents t.fragment_buf in
               Buffer.clear t.fragment_buf;
               `Message result
             | false ->
               (* More fragments expected - keep reading *)
               read_message ())
          | 8 ->
            (* Close frame *)
            `Closed
          | 9 ->
            (* Ping frame - respond with pong per RFC 6455 *)
            send_pong_blocking t payload;
            read_message ()
          | 10 ->
            (* Pong frame - notify callback and continue reading *)
            Option.iter on_pong ~f:(fun f -> f ());
            `Pong
          | _ ->
            (* Text (1) or Binary (2) frame *)
            (match fin with
             | true ->
               (* Complete message in a single frame *)
               `Message payload
             | false ->
               (* Start of a new fragmented message *)
               Buffer.clear t.fragment_buf;
               Buffer.add_string t.fragment_buf payload;
               read_message ())
        in
        read_message ()
      )
    )
    >>| function
    | Ok (`Message payload) -> Message payload
    | Ok `Pong -> Pong_received
    | Ok `Closed -> Connection_closed
    | Error _ -> Connection_closed

(** Receive a message, with optional callback when pong frames are received.
    This is the recommended API for health-monitored connections.

    @param on_pong Callback invoked when a pong frame is received (for health tracking)
    @return The next application message, or None if connection closed
*)
let receive_with_pong_callback t ~on_pong =
  let rec loop () =
    let%bind result = receive_internal t ~on_pong:(Some on_pong) in
    match result with
    | Message payload -> return (Some payload)
    | Pong_received -> loop ()  (* Continue reading after pong *)
    | Connection_closed -> return None
  in
  loop ()

(** Receive a message (original API, ignores pong frames).
    For backward compatibility with existing code. *)
let receive t =
  let rec loop () =
    let%bind result = receive_internal t ~on_pong:None in
    match result with
    | Message payload -> return (Some payload)
    | Pong_received -> loop ()  (* Continue reading after pong *)
    | Connection_closed -> return None
  in
  loop ()

let close t =
  match t.closed with
  | true -> Deferred.unit
  | false ->
    t.closed <- true;
    Curl.cleanup t.curl;
    Deferred.unit

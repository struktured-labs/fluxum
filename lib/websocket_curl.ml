open Core
open Async

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
  }

let connect ~url =
  let open Deferred.Or_error.Let_syntax in

  (* Parse URL for building WebSocket upgrade request *)
  let uri = Uri.of_string url in
  let host = Option.value_exn (Uri.host uri) ~message:"URL must have host" in
  let path = match Uri.path uri with "" | "/" -> "/" | p -> p in

  (* Use curl to establish TLS connection *)
  let%bind curl =
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        let conn = Curl.init () in
        (* Convert wss:// to https:// for CONNECT_ONLY mode *)
        let connect_url =
          if String.is_prefix url ~prefix:"wss://" then
            "https://" ^ String.drop_prefix url 6
          else if String.is_prefix url ~prefix:"ws://" then
            "http://" ^ String.drop_prefix url 5
          else
            url
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
  let request = String.concat ~sep:"\r\n"
    [ sprintf "GET %s HTTP/1.1" path
    ; sprintf "Host: %s" host
    ; "Upgrade: websocket"
    ; "Connection: Upgrade"
    ; sprintf "Sec-WebSocket-Key: %s" ws_key
    ; "Sec-WebSocket-Version: 13"
    ; sprintf "Origin: https://%s" host
    ; ""
    ; ""
    ] in

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

  (* Read HTTP response using curl's recv *)
  let%bind response_lines =
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
        (* Parse headers *)
        let lines = String.split response ~on:'\n' in
        List.map lines ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s))
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
      return { curl; closed = false }
    | _ ->
      Deferred.Or_error.error_string "Invalid Sec-WebSocket-Accept"

(* Encode WebSocket text frame *)
let encode_text_frame payload =
  let len = String.length payload in
  let header = Buffer.create 14 in

  (* FIN + Text opcode *)
  Buffer.add_char header (Char.of_int_exn 0x81);

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

let receive t =
  match t.closed with
  | true -> return None
  | false ->
    Deferred.Or_error.try_with (fun () ->
      In_thread.run (fun () ->
        (* Read frame header (at least 2 bytes) *)
        let header_buf = Bytes.create 14 in
        let rec recv_exact buf offset len =
          match len = 0 with
          | true -> ()
          | false ->
            let received = Curl_ext.recv t.curl buf offset len in
            match received = 0 with
            | true -> failwith "Connection closed"
            | false -> recv_exact buf (offset + received) (len - received)
        in
        recv_exact header_buf 0 2;

        let byte0 = Bytes.get header_buf 0 |> Char.to_int in
        let byte1 = Bytes.get header_buf 1 |> Char.to_int in

        let _opcode = byte0 land 0x0F in
        let masked = (byte1 land 0x80) <> 0 in
        let payload_len_initial = byte1 land 0x7F in

        (* Read extended payload length if needed *)
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
              (* Use lower 32 bits *)
              (Bytes.get header_buf 6 |> Char.to_int) lsl 24
              lor (Bytes.get header_buf 7 |> Char.to_int) lsl 16
              lor (Bytes.get header_buf 8 |> Char.to_int) lsl 8
              lor (Bytes.get header_buf 9 |> Char.to_int)
        in

        match payload_len = 0 with
        | true -> ""
        | false ->
          (* Skip mask if present (server shouldn't mask) *)
          (match masked with true -> recv_exact header_buf 0 4 | false -> ());

          (* Read payload *)
          let payload_buf = Bytes.create payload_len in
          recv_exact payload_buf 0 payload_len;
          Bytes.to_string payload_buf
      )
    )
    >>| function
    | Ok payload -> Some payload
    | Error _ -> None

let close t =
  match t.closed with
  | true -> Deferred.unit
  | false ->
    t.closed <- true;
    Curl.cleanup t.curl;
    Deferred.unit

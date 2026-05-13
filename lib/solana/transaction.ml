open Core

type error =
  [ `Base64_decode of string
  | `Truncated of string
  | `Unsupported_sig_count of int ]

let sexp_of_error = function
  | `Base64_decode s -> Sexp.List [ Sexp.Atom "Base64_decode"; Sexp.Atom s ]
  | `Truncated s -> Sexp.List [ Sexp.Atom "Truncated"; Sexp.Atom s ]
  | `Unsupported_sig_count n ->
    Sexp.List [ Sexp.Atom "Unsupported_sig_count"; Sexp.Atom (Int.to_string n) ]

(* Solana compact-u16 (a.k.a. ShortVec): 1-3 byte varint.
   Returns (value, bytes_read). *)
let read_compact_u16 (buf : bytes) ~(pos : int) : (int * int, error) result =
  let n = Bytes.length buf in
  match pos >= n with
  | true -> Error (`Truncated "compact-u16 at EOF")
  | false ->
    let b0 = Char.to_int (Bytes.get buf pos) in
    match b0 < 0x80 with
    | true -> Ok (b0, 1)
    | false ->
      (match pos + 1 >= n with
       | true -> Error (`Truncated "compact-u16 truncated after byte 0")
       | false ->
         let b1 = Char.to_int (Bytes.get buf (pos + 1)) in
         let v = (b0 land 0x7f) lor ((b1 land 0x7f) lsl 7) in
         (match b1 < 0x80 with
          | true -> Ok (v, 2)
          | false ->
            (match pos + 2 >= n with
             | true -> Error (`Truncated "compact-u16 truncated after byte 1")
             | false ->
               let b2 = Char.to_int (Bytes.get buf (pos + 2)) in
               let v = v lor ((b2 land 0x03) lsl 14) in
               Ok (v, 3))))

let sign_jupiter_swap_b64 ~keypair (b64 : string) : (string, error) result =
  (* Decode base64. *)
  match Base64.decode b64 with
  | Error (`Msg m) -> Error (`Base64_decode m)
  | Ok decoded ->
    let buf = Bytes.of_string decoded in
    let total = Bytes.length buf in
    (* Read signature count (compact-u16). *)
    (match read_compact_u16 buf ~pos:0 with
     | Error _ as err -> err
     | Ok (sig_count, sig_count_len) ->
       (match sig_count with
        | 0 | 1 ->
          let sig_offset = sig_count_len in
          let message_offset = sig_count_len + (sig_count * 64) in
          (match message_offset > total with
           | true -> Error (`Truncated "Signature region overruns buffer")
           | false ->
             let message =
               Bytes.sub buf ~pos:message_offset ~len:(total - message_offset)
             in
             let signature = Keypair.sign keypair ~message in
             (* Write the signature into slot 0 (only meaningful if sig_count > 0). *)
             (match sig_count > 0 with
              | true -> Bytes.blit ~src:signature ~src_pos:0 ~dst:buf ~dst_pos:sig_offset ~len:64
              | false -> ());
             Ok (Base64.encode_exn (Bytes.to_string buf)))
        | n -> Error (`Unsupported_sig_count n)))

let signature_of_signed_b64 (b64 : string) : (string, error) result =
  match Base64.decode b64 with
  | Error (`Msg m) -> Error (`Base64_decode m)
  | Ok decoded ->
    let buf = Bytes.of_string decoded in
    let total = Bytes.length buf in
    (match read_compact_u16 buf ~pos:0 with
     | Error _ as err -> err
     | Ok (sig_count, sig_count_len) ->
       (match sig_count >= 1 && sig_count_len + 64 <= total with
        | false -> Error (`Truncated "No signature present")
        | true ->
          let sig_bytes = Bytes.sub buf ~pos:sig_count_len ~len:64 in
          Ok (Base58.encode sig_bytes)))

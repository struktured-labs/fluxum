(* Base58 encode/decode for Solana pubkeys, signatures, and tx ids.

   Pure-OCaml implementation: ~50 lines, no external dependency.
   Uses the Bitcoin alphabet (same as Solana). *)

let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

(* Reverse lookup table for decoding. -1 means invalid char. *)
let alphabet_map =
  let tbl = Array.create ~len:128 (-1) in
  String.iteri alphabet ~f:(fun i c -> tbl.(Char.to_int c) <- i);
  tbl

let encode (input : bytes) : string =
  let n = Bytes.length input in
  match n = 0 with
  | true -> ""
  | false ->
    (* Count leading zero bytes — each becomes a leading '1'. *)
    let zeros = ref 0 in
    let rec count i =
      match i < n && Bytes.get input i |> Char.equal '\x00' with
      | true ->
        incr zeros;
        count (i + 1)
      | false -> ()
    in
    count 0;
    (* Convert big-endian input bytes to a list of base-58 digits.
       Repeatedly divide the big integer (stored as an int array of digits) by 58. *)
    let digits = Array.create ~len:(n * 138 / 100 + 1) 0 in
    let digit_len = ref 0 in
    for i = 0 to n - 1 do
      let carry = ref (Bytes.get input i |> Char.to_int) in
      for j = 0 to !digit_len - 1 do
        let v = digits.(j) * 256 + !carry in
        digits.(j) <- v mod 58;
        carry := v / 58
      done;
      while !carry > 0 do
        digits.(!digit_len) <- !carry mod 58;
        carry := !carry / 58;
        incr digit_len
      done
    done;
    (* Output: leading '1's for zero bytes, then digits in reverse order. *)
    let out = Buffer.create (!zeros + !digit_len) in
    for _ = 1 to !zeros do Buffer.add_char out '1' done;
    for i = !digit_len - 1 downto 0 do
      Buffer.add_char out alphabet.[digits.(i)]
    done;
    Buffer.contents out

let decode (s : string) : (bytes, [> `Invalid_char of char ]) result =
  let n = String.length s in
  match n = 0 with
  | true -> Ok (Bytes.create 0)
  | false ->
    (* Count leading '1's — each becomes a leading zero byte. *)
    let zeros = ref 0 in
    let rec count i =
      match i < n && Char.equal s.[i] '1' with
      | true ->
        incr zeros;
        count (i + 1)
      | false -> ()
    in
    count 0;
    let bytes_buf = Array.create ~len:(n * 733 / 1000 + 1) 0 in
    let byte_len = ref 0 in
    let exception Bad_char of char in
    try
      for i = 0 to n - 1 do
        let c = s.[i] in
        let v = match Char.to_int c < 128 with
          | true -> alphabet_map.(Char.to_int c)
          | false -> -1
        in
        match v < 0 with
        | true -> raise (Bad_char c)
        | false ->
          let carry = ref v in
          for j = 0 to !byte_len - 1 do
            let w = bytes_buf.(j) * 58 + !carry in
            bytes_buf.(j) <- w land 0xff;
            carry := w lsr 8
          done;
          while !carry > 0 do
            bytes_buf.(!byte_len) <- !carry land 0xff;
            carry := !carry lsr 8;
            incr byte_len
          done
      done;
      let out = Bytes.create (!zeros + !byte_len) in
      Bytes.fill out ~pos:0 ~len:!zeros '\x00';
      for i = 0 to !byte_len - 1 do
        Bytes.set out (!zeros + i) (Char.of_int_exn bytes_buf.(!byte_len - 1 - i))
      done;
      Ok out
    with
    | Bad_char c -> Error (`Invalid_char c)

let decode_exn s =
  match decode s with
  | Ok b -> b
  | Error (`Invalid_char c) ->
    failwithf "Base58.decode_exn: invalid char %C in %S" c s ()

let decode_pubkey s =
  match decode s with
  | Error e -> Error e
  | Ok b ->
    let len = Bytes.length b in
    match len = 32 with
    | true -> Ok b
    | false -> Error (`Wrong_length len)

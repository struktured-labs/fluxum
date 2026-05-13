open Core

type t =
  { public : bytes  (* 32-byte derived public key *)
  ; priv : Mirage_crypto_ec.Ed25519.priv
  }

(* Load from Solana CLI JSON format: a single array of 64 integers (0-255).
   First 32 = secret seed, last 32 = pubkey. *)
let of_file path =
  match Sys_unix.file_exists path with
  | `No | `Unknown -> Error (`Io_error (sprintf "Keypair file not found: %s" path))
  | `Yes ->
    (match In_channel.read_all path with
     | exception Sys_error msg -> Error (`Io_error msg)
     | content ->
       (match Yojson.Safe.from_string content with
        | exception Yojson.Json_error msg ->
          Error (`Parse_error (sprintf "JSON parse: %s" msg))
        | `List ints ->
          let len = List.length ints in
          (match len = 64 with
           | false ->
             Error (`Parse_error (sprintf "Expected 64 bytes in keypair, got %d" len))
           | true ->
             let bytes_arr = Bytes.create 64 in
             let bad = ref None in
             List.iteri ints ~f:(fun i v ->
               match v with
               | `Int b when b >= 0 && b <= 255 ->
                 Bytes.set bytes_arr i (Char.of_int_exn b)
               | _ -> bad := Some i);
             (match !bad with
              | Some i ->
                Error (`Parse_error (sprintf "Non-byte value at index %d" i))
              | None ->
                let secret = Bytes.sub bytes_arr ~pos:0 ~len:32 in
                let public = Bytes.sub bytes_arr ~pos:32 ~len:32 in
                (* Recover priv from seed; double-check derived pubkey matches stored. *)
                let priv_result =
                  Mirage_crypto_ec.Ed25519.priv_of_octets (Bytes.to_string secret)
                in
                (match priv_result with
                 | Error _ ->
                   Error (`Parse_error "Ed25519.priv_of_octets failed")
                 | Ok priv ->
                   let derived_pub = Mirage_crypto_ec.Ed25519.pub_of_priv priv in
                   let derived_pub_bytes =
                     Mirage_crypto_ec.Ed25519.pub_to_octets derived_pub |> Bytes.of_string
                   in
                   (match Bytes.equal derived_pub_bytes public with
                    | true -> Ok { public; priv }
                    | false ->
                      Error (`Parse_error
                        "Stored pubkey does not match derived pubkey — file corrupt?")))))
        | _ ->
          Error (`Parse_error "Keypair JSON must be a top-level array of 64 ints")))

let of_file_exn path =
  match of_file path with
  | Ok t -> t
  | Error (`Io_error msg) -> failwithf "Keypair.of_file_exn IO: %s" msg ()
  | Error (`Parse_error msg) -> failwithf "Keypair.of_file_exn parse: %s" msg ()

let generate () =
  Mirage_crypto_rng_unix.use_default ();
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets pub |> Bytes.of_string in
  { public; priv }

let pubkey_bytes t = t.public
let pubkey_base58 t = Base58.encode t.public

let sign t ~message =
  let sig_str = Mirage_crypto_ec.Ed25519.sign ~key:t.priv (Bytes.to_string message) in
  Bytes.of_string sig_str

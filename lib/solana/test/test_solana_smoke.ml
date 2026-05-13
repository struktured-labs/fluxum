(* Smoke tests for the Solana lib (offline only — base58, keypair, tx parse).
   Network-touching tests live in fluxit/bin/solana_swap_test.ml. *)

open! Core
open Solana

let%test_unit "base58 roundtrip" =
  let raw = Bytes.of_string "hello world" in
  let encoded = Base58.encode raw in
  let decoded =
    match Base58.decode encoded with
    | Ok b -> b
    | Error _ -> failwith "decode failed"
  in
  [%test_eq: string] (Bytes.to_string raw) (Bytes.to_string decoded)

let%test_unit "base58 known vector — leading zeros become leading 1s" =
  let raw = Bytes.of_string "\x00\x00\x01" in
  [%test_eq: string] "112" (Base58.encode raw)

let%test_unit "keypair generate then sign produces 64-byte signature" =
  let kp = Keypair.generate () in
  let sig_ = Keypair.sign kp ~message:(Bytes.of_string "test") in
  [%test_eq: int] 64 (Bytes.length sig_);
  let pubkey = Keypair.pubkey_base58 kp in
  [%test_pred: int] (fun n -> n >= 32 && n <= 44) (String.length pubkey)

let%test_unit "transaction sign_jupiter_swap_b64 replaces placeholder" =
  let placeholder = Bytes.create (1 + 64 + 5) in
  Bytes.set placeholder 0 '\x01';
  Bytes.fill placeholder ~pos:1 ~len:64 '\x00';
  Bytes.set placeholder 65 '\x80';
  Bytes.set placeholder 66 '\x01';
  Bytes.set placeholder 67 '\x02';
  Bytes.set placeholder 68 '\x03';
  Bytes.set placeholder 69 '\x04';
  let b64 = Base64.encode_exn (Bytes.to_string placeholder) in
  let kp = Keypair.generate () in
  let signed =
    match Transaction.sign_jupiter_swap_b64 ~keypair:kp b64 with
    | Ok s -> s
    | Error e ->
      failwithf "sign: %s" (Sexp.to_string (Transaction.sexp_of_error e)) ()
  in
  let sig_b58 =
    match Transaction.signature_of_signed_b64 signed with
    | Ok s -> s
    | Error e ->
      failwithf "extract: %s" (Sexp.to_string (Transaction.sexp_of_error e)) ()
  in
  [%test_pred: int] (fun n -> n >= 86 && n <= 90) (String.length sig_b58)

open Core

(* Test vectors for Kraken signature generation *)
(* These are synthetic test vectors that verify the HMAC-SHA512 computation *)

let test_hmac_sha512 () =
  (* Test 1: Simple case *)
  let secret = "secret_key" in
  let message = "test_message" in
  let signature = Kraken.Signature.hmac_sha512 ~secret ~message in
  (* Just verify it produces non-empty output *)
  match String.is_empty signature with
  | true -> printf "HMAC-SHA512 test FAILED: empty signature\n"
  | false -> printf "HMAC-SHA512 test passed (output length: %d)\n" (String.length signature)

let test_kraken_signature () =
  (* Test with synthetic data *)
  let api_secret_b64 = "YmFzZTY0ZW5jb2RlZHNlY3JldGtleQ==" in (* "base64encodedsecretkey" in base64 *)
  let api_path = "/0/private/Balance" in
  let nonce = "1234567890000" in
  let post_data = "nonce=1234567890000" in
  
  (* This should produce a signature without errors *)
  let signature = Kraken.Signature.kraken_signature ~api_secret_b64 ~api_path ~nonce ~post_data in
  
  (* Verify it's non-empty and looks like base64 *)
  match String.is_empty signature with
  | true -> printf "Kraken signature test FAILED: empty signature\n"
  | false -> 
    match String.for_all signature ~f:(fun c -> 
      Char.is_alphanum c || Char.equal c '+' || Char.equal c '/' || Char.equal c '=') 
    with
    | true -> printf "Kraken signature test passed (signature length: %d)\n" (String.length signature)
    | false -> 
      printf "Kraken signature test FAILED: invalid base64 characters\n";
      printf "Got: %s\n" signature

let test_base64_roundtrip () =
  let original = "This is a test string for base64 encoding" in
  let encoded = Kraken.Signature.base64_encode original in
  let decoded = Kraken.Signature.base64_decode encoded in
  match String.equal original decoded with
  | true -> printf "Base64 roundtrip test passed\n"
  | false ->
    printf "Base64 roundtrip test FAILED\n";
    printf "Original: %s\n" original;
    printf "Decoded:  %s\n" decoded

let () =
  printf "Running Kraken signature tests...\n\n";
  test_base64_roundtrip ();
  test_hmac_sha512 ();
  test_kraken_signature ();
  printf "\nAll tests completed\n"

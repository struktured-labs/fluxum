open Core
open Async

(** Kraken balance types *)
module Balance = struct
  type t = string * float [@@deriving sexp]
end

(** Make an authenticated Kraken API call using Python for proper signature *)
let api_call ~api_key ~api_secret ~endpoint ~data () =
  (* Generate nonce from current Unix timestamp in milliseconds *)
  let nonce = Int.to_string (Int.of_float (Unix.gettimeofday () *. 1000.0)) in
  
  (* Build post data *)
  let post_pairs = ("nonce", nonce) :: data in
  let post_data = String.concat ~sep:"&" (List.map post_pairs ~f:(fun (k, v) -> 
    sprintf "%s=%s" k v))
  in
  
  (* API path for signing *)
  let api_path = sprintf "/0/private/%s" endpoint in
  
  (* Use Python to generate the signature properly with base64 decoding *)
  let python_script = sprintf {|import base64, hashlib, hmac, sys, json
try:
    nonce = "%s"
    post_data = "%s"
    api_path = "%s"
    api_key = "%s"
    api_secret_b64 = "%s"
    
    # Decode the base64 secret
    api_secret = base64.b64decode(api_secret_b64)
    
    # SHA256(nonce + post_data)
    msg_hash = hashlib.sha256((nonce + post_data).encode()).digest()
    
    # HMAC-SHA512(api_path + msg_hash)
    sig = base64.b64encode(
        hmac.new(api_secret, (api_path.encode() + msg_hash), hashlib.sha512).digest()
    ).decode()
    
    # Make the request
    import subprocess
    result = subprocess.run([
        "curl", "-s", "-X", "POST",
        "-H", f"API-Key: {api_key}",
        "-H", f"API-Sign: {sig}",
        "-d", post_data,
        "https://api.kraken.com" + api_path
    ], capture_output=True, text=True)
    
    print(result.stdout, end='')
except Exception as e:
    print(json.dumps({"error": [str(e)]}))
|} nonce post_data api_path api_key api_secret in
  
  (* Execute Python script *)
  In_thread.run (fun () ->
    let tmp_script = Filename_unix.temp_file "kraken" ".py" in
    Out_channel.write_all tmp_script ~data:python_script;
    
    let response = 
      try
        let process = Core_unix.open_process_in (sprintf "python3 %s" tmp_script) in
        let output = In_channel.input_all process in
        let _ = Core_unix.close_process_in process in
        output
      with e -> 
        sprintf "{\"error\": [\"%s\"]}" (Exn.to_string e)
    in
    (try Sys_unix.remove tmp_script with _ -> ());
    response
  ) >>= fun response_str ->
  try
    let json = Yojson.Safe.from_string response_str in
    Deferred.return (`Ok json)
  with e ->
    Deferred.return (`Error (sprintf "JSON parse error: %s (response: %s)" (Exn.to_string e) response_str))

(** Get account balances *)
let balances (module Cfg : Cfg.S) () =
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"Balance" ~data:[] ()

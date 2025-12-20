open Core
open Async

(** Kraken balance types *)
module Balance = struct
  type t = string * float [@@deriving sexp]
end

(** Make an authenticated Kraken API call using pure OCaml signature generation *)
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
  
  (* Generate Kraken signature using pure OCaml *)
  let signature = Signature.kraken_signature ~api_secret_b64:api_secret ~api_path ~nonce ~post_data in
  
  (* Make HTTP request via curl *)
  In_thread.run (fun () ->
    let tmp_data = Filename_unix.temp_file "kraken" ".data" in
    Out_channel.write_all tmp_data ~data:post_data;
    
    let response =
      try
        let curl_cmd = sprintf {|curl -s -X POST \
          -H "API-Key: %s" \
          -H "API-Sign: %s" \
          -d @%s \
          https://api.kraken.com%s|} api_key signature tmp_data api_path in
        let process = Core_unix.open_process_in curl_cmd in
        let output = In_channel.input_all process in
        let _ = Core_unix.close_process_in process in
        output
      with e ->
        sprintf "{\"error\": [\"%s\"]}" (Exn.to_string e)
    in
    (try Sys_unix.remove tmp_data with _ -> ());
    response
  ) >>= fun response_str ->
  try
    let json = Yojson.Safe.from_string response_str in
    Deferred.return (`Ok json)
  with e ->
    Deferred.return (`Error (sprintf "JSON parse error: %s" (Exn.to_string e)))

(** Get account balances *)
let balances (module Cfg : Cfg.S) () =
  api_call ~api_key:Cfg.api_key ~api_secret:Cfg.api_secret ~endpoint:"Balance" ~data:[] ()

let param ?default ~name ~env () =
  let name = sprintf "OKX_%s_%s" (String.uppercase env) name in
  match Unix.getenv name with
  | Some param -> param
  | None -> (
    match default with
    | None -> failwithf "Environment variable \"%s\" must be specified" name ()
    | Some default -> default )

let api_key ?default = param ?default ~name:"API_KEY"

let api_secret ?default = param ?default ~name:"API_SECRET"

let api_passphrase ?default = param ?default ~name:"API_PASSPHRASE"

(** OKX configuration module signature *)
module type S = sig
  val api_key : string
  val api_secret : string
  val api_passphrase : string
  val base_url : string
end

(** Create OKX configuration from environment *)
let make env ~base_url =
  let module M = struct
    let api_key = api_key ~env ()
    let api_secret = api_secret ~env ()
    let api_passphrase = api_passphrase ~env ()
    let base_url = base_url
  end in
  (module M : S)

(** Production configuration *)
module Production () = struct
  include (val make "production" ~base_url:"www.okx.com" : S)
end

(** AWS configuration (alternative endpoint) *)
module Aws () = struct
  include (val make "aws" ~base_url:"aws.okx.com" : S)
end

(** Testnet configuration (demo trading) *)
module Testnet () = struct
  include (val make "testnet" ~base_url:"www.okx.com" : S)
end

(** Produce configuration from string *)
let of_string s =
  match String.lowercase s with
  | "production" ->
    let module Cfg : S = Production () in
    (module Cfg : S)
  | "aws" ->
    let module Cfg : S = Aws () in
    (module Cfg : S)
  | "testnet" ->
    let module Cfg : S = Testnet () in
    (module Cfg : S)
  | unsupported_env ->
    failwithf "okx environment %s not supported" unsupported_env ()

let arg_type = Command.Arg_type.create of_string

let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:"STRING okx environment (production|aws|testnet)")

let or_default cfg =
  match cfg with
  | Some cfg -> cfg
  | None -> of_string "production"

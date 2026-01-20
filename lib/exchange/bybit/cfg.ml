let param ?default ~name ~env () =
  let name = sprintf "BYBIT_%s_%s" (String.uppercase env) name in
  match Unix.getenv name with
  | Some param -> param
  | None -> (
    match default with
    | None -> failwithf "Environment variable \"%s\" must be specified" name ()
    | Some default -> default )

let api_key ?default = param ?default ~name:"API_KEY"

let api_secret ?default = param ?default ~name:"API_SECRET"

(** Bybit configuration module signature *)
module type S = sig
  val api_key : string
  val api_secret : string
end

(** Create Bybit configuration from environment *)
let make env =
  let module M = struct
    let api_key = api_key ~env ()
    let api_secret = api_secret ~env ()
  end in
  (module M : S)

(** Production configuration *)
module Production () = struct
  include (val make "production" : S)
end

(** Testnet configuration *)
module Testnet () = struct
  include (val make "testnet" : S)
end

(** Produce configuration from string *)
let of_string s =
  match String.lowercase s with
  | "production" ->
    let module Cfg : S = Production () in
    (module Cfg : S)
  | "testnet" ->
    let module Cfg : S = Testnet () in
    (module Cfg : S)
  | unsupported_env ->
    failwithf "bybit environment %s not supported" unsupported_env ()

let arg_type = Command.Arg_type.create of_string

let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:"STRING bybit environment (production|testnet)")

let or_default cfg =
  match cfg with
  | Some cfg -> cfg
  | None -> of_string "production"

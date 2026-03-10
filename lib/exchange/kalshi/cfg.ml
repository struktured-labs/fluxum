(** Kalshi API Configuration

    Authentication uses RSA-PSS key signing.
    API base: https://api.kalshi.com/trade-api/v2/
    Demo:     https://demo-api.kalshi.co/trade-api/v2/ *)

let api_version = "v2"

module type S = sig
  val api_host : string
  val api_key : string
  val private_key_path : string
end

let home_dir () =
  match Unix.getenv "HOME" with
  | Some h -> h
  | None -> "/tmp"

let param ?default ~name () =
  let env_name = sprintf "KALSHI_%s" (String.uppercase name) in
  match Unix.getenv env_name with
  | Some v -> v
  | None ->
    (match default with
     | None -> failwithf "Environment variable %S must be specified" env_name ()
     | Some d -> d)

let host ~env =
  match String.lowercase env with
  | "production" | "prod" -> "api.kalshi.com"
  | "demo" | "sandbox" -> "demo-api.kalshi.co"
  | _ -> failwithf "Kalshi environment must be 'production' or 'demo'" ()

let make env =
  let module M = struct
    let api_host = host ~env
    let api_key = param ~name:"API_KEY" ()

    let private_key_path =
      param ~name:"PRIVATE_KEY_PATH"
        ~default:(sprintf "%s/.kalshi/private_key.pem" (home_dir ()))
        ()
  end
  in
  (module M : S)

module Production () = struct
  include (val make "production" : S)
end

module Demo () = struct
  include (val make "demo" : S)
end

let of_string s =
  match String.lowercase s with
  | "production" | "prod" -> make "production"
  | "demo" | "sandbox" -> make "demo"
  | env -> failwithf "Kalshi environment %S not supported" env ()

let arg_type = Command.Arg_type.create of_string

let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:"STRING Kalshi environment (production or demo, defaults to demo)")

let or_default param =
  match param with
  | None ->
    (match Unix.getenv "KALSHI_ENV" with
     | Some env -> of_string env
     | None -> make "demo")
  | Some p -> p

let cfg_param_public =
  Command.Param.(
    flag "-env" (optional_with_default "production" string)
      ~doc:"STRING environment (production or demo, default: production)")

let public_config_of_env env =
  let module M = struct
    let api_host = host ~env
    let api_key = ""
    let private_key_path = ""
  end
  in
  (module M : S)

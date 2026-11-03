(** Kalshi API Configuration

    Authentication uses RSA-PSS key signing.
    API base: https://external-api.kalshi.com/trade-api/v2/
    Demo:     https://external-api.demo.kalshi.co/trade-api/v2/ *)

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

(** Look up a Kalshi env var. Prefers env-specific form
    (KALSHI_PRODUCTION_API_KEY, KALSHI_DEMO_API_KEY) when [env] is given;
    falls back to the generic form (KALSHI_API_KEY). This matches the
    KALSHI_PRODUCTION_* / KALSHI_DEMO_* convention used by fluxum's other
    exchange adapters (GEMINI_PRODUCTION_*, KRAKEN_PRODUCTION_*, etc). *)
let param ?default ?env ~name () =
  let name_u = String.uppercase name in
  let env_candidates =
    match env with
    | Some e ->
      let env_u = String.uppercase e in
      (* Normalize "prod" → "PRODUCTION", "sandbox" → "DEMO" for variable naming *)
      let canon =
        match String.lowercase e with
        | "prod" -> "PRODUCTION"
        | "sandbox" -> "DEMO"
        | _ -> env_u
      in
      [ sprintf "KALSHI_%s_%s" canon name_u; sprintf "KALSHI_%s" name_u ]
    | None -> [ sprintf "KALSHI_%s" name_u ]
  in
  let found =
    List.find_map env_candidates ~f:(fun v -> Unix.getenv v)
  in
  match found with
  | Some v -> v
  | None ->
    (match default with
     | None ->
       let primary = List.hd_exn env_candidates in
       failwithf "Environment variable %S must be specified" primary ()
     | Some d -> d)

let host ~env =
  match String.lowercase env with
  | "production" | "prod" -> "external-api.kalshi.com"
  | "demo" | "sandbox" -> "external-api.demo.kalshi.co"
  | _ -> failwithf "Kalshi environment must be 'production' or 'demo'" ()

let production_allowed () =
  List.mem [ "1"; "yes"; "true" ]
    (String.lowercase (String.strip (Option.value (Unix.getenv "KALSHI_ALLOW_PRODUCTION") ~default:"")))
    ~equal:String.equal

let is_production env =
  match String.lowercase (String.strip env) with
  | "production" | "prod" -> true
  | _ -> false

(** Every way of building an authenticated config ([make], [of_string],
    [Production ()], [resolve], [or_default]) goes through here, so the
    real-money guard can't be bypassed by using the library directly:
    production raises unless KALSHI_ALLOW_PRODUCTION=1 (or yes/true). *)
let make env =
  if is_production env && not (production_allowed ()) then
    failwith "Kalshi production config refused: real-money production requires KALSHI_ALLOW_PRODUCTION=1";
  let module M = struct
    let api_host = host ~env
    let api_key = param ~env ~name:"API_KEY" ()

    let private_key_path =
      param ~env ~name:"PRIVATE_KEY_PATH"
        ~default:(sprintf "%s/.kalshi/private_key.pem" (home_dir ()))
        ()
  end
  in
  (module M : S)

(** Raises unless KALSHI_ALLOW_PRODUCTION=1 (see [make]). *)
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

(** Resolve the authenticated config from [-cfg] (if given) or [KALSHI_ENV]
    (default demo). Production, from either source, additionally requires
    KALSHI_ALLOW_PRODUCTION=1. Never raises: missing credentials or an
    unknown environment are returned as [Error]. *)
let resolve (choice : string option) : (module S) Or_error.t =
  let source, env =
    match choice with
    | Some e -> ("-cfg " ^ e, e)
    | None ->
      (match Unix.getenv "KALSHI_ENV" with
       | Some e -> ("KALSHI_ENV=" ^ e, e)
       | None -> ("default", "demo"))
  in
  if is_production env && not (production_allowed ()) then
    Or_error.errorf "Kalshi: %s refused: real-money production also requires KALSHI_ALLOW_PRODUCTION=1" source
  else
    Or_error.try_with (fun () -> of_string env)
    |> Result.map_error ~f:(fun e -> Error.tag e ~tag:"Kalshi")

(** Kept for compatibility; [-cfg] values are resolved (and the production
    guard applied) by [resolve]. *)
let arg_type = Command.Arg_type.create Fn.id

let param =
  Command.Param.(
    flag "-cfg" (optional string)
      ~doc:"STRING Kalshi environment (production or demo, defaults to demo; production also \
            needs KALSHI_ALLOW_PRODUCTION=1)")

(** Like [resolve] but raises. Prefer [resolve]. *)
let or_default choice = Or_error.ok_exn (resolve choice)

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

(** MEXC Configuration *)

open Core

module type S = sig
  val api_key : string
  val api_secret : string
  val base_url : string
end

let production : (module S) =
  (module struct
    let api_key = Sys.getenv "MEXC_API_KEY" |> Option.value ~default:""
    let api_secret = Sys.getenv "MEXC_API_SECRET" |> Option.value ~default:""
    let base_url = "https://api.mexc.com"
  end)

let of_string = function
  | "production" -> production
  | env -> failwith (sprintf "Unknown MEXC environment: %s" env)

let arg_type = Command.Arg_type.create of_string

let param =
  Command.Param.(
    flag "-cfg" (optional arg_type)
      ~doc:"STRING MEXC environment (production)")

let or_default cfg =
  match cfg with
  | Some cfg -> cfg
  | None -> of_string "production"

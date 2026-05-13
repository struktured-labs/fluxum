open Core
open Async

let default_rpc_url = "https://api.mainnet-beta.solana.com"

type error =
  [ `Network of string
  | `Http of int * string
  | `Json_parse of string
  | `Rpc of int * string ]

let sexp_of_error = function
  | `Network s -> Sexp.List [ Sexp.Atom "Network"; Sexp.Atom s ]
  | `Http (c, s) ->
    Sexp.List [ Sexp.Atom "Http"; Sexp.Atom (Int.to_string c); Sexp.Atom s ]
  | `Json_parse s -> Sexp.List [ Sexp.Atom "Json_parse"; Sexp.Atom s ]
  | `Rpc (c, s) ->
    Sexp.List [ Sexp.Atom "Rpc"; Sexp.Atom (Int.to_string c); Sexp.Atom s ]

let next_id = ref 1

let request ~rpc_url ~method_ ~params =
  let id = !next_id in
  incr next_id;
  let body_json =
    `Assoc
      [ ("jsonrpc", `String "2.0")
      ; ("id", `Int id)
      ; ("method", `String method_)
      ; ("params", params)
      ]
  in
  let body_str = Yojson.Safe.to_string body_json in
  let uri = Uri.of_string rpc_url in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post ~body:(Cohttp_async.Body.of_string body_str) ~headers uri)
    |> Deferred.map
         ~f:(Result.map_error ~f:(fun err -> `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    (match code = 200 with
     | false -> return (Error (`Http (code, body_str)))
     | true ->
       (match Yojson.Safe.from_string body_str with
        | exception Yojson.Json_error m -> return (Error (`Json_parse m))
        | json ->
          let open Yojson.Safe.Util in
          (match member "error" json with
           | `Null ->
             (match member "result" json with
              | `Null -> return (Error (`Rpc (0, "missing result")))
              | result -> return (Ok result))
           | err_json ->
             let code = err_json |> member "code" |> to_int_option |> Option.value ~default:0 in
             let msg = err_json |> member "message" |> to_string_option
                       |> Option.value ~default:(Yojson.Safe.to_string err_json) in
             return (Error (`Rpc (code, msg))))))

let get_balance ?(rpc_url = default_rpc_url) ~address () =
  let params = `List [ `String address ] in
  match%bind request ~rpc_url ~method_:"getBalance" ~params with
  | Error _ as err -> return err
  | Ok json ->
    let open Yojson.Safe.Util in
    (match member "value" json |> to_int_option with
     | Some n -> return (Ok n)
     | None -> return (Error (`Json_parse "getBalance: no .value")))

let send_transaction
    ?(rpc_url = default_rpc_url)
    ?(skip_preflight = false)
    ?(max_retries = 0)
    ~tx_base64
    ()
  =
  let opts =
    `Assoc
      [ ("encoding", `String "base64")
      ; ("skipPreflight", `Bool skip_preflight)
      ; ("maxRetries", `Int max_retries)
      ]
  in
  let params = `List [ `String tx_base64; opts ] in
  match%bind request ~rpc_url ~method_:"sendTransaction" ~params with
  | Error _ as err -> return err
  | Ok (`String s) -> return (Ok s)
  | Ok other ->
    return (Error (`Json_parse
      (sprintf "sendTransaction: expected string, got %s"
         (Yojson.Safe.to_string other))))

type confirmation =
  [ `Processed
  | `Confirmed
  | `Finalized ]

type signature_status =
  { slot : int option
  ; confirmation : confirmation option
  ; err : string option
  }

let parse_confirmation = function
  | "processed" -> Some `Processed
  | "confirmed" -> Some `Confirmed
  | "finalized" -> Some `Finalized
  | _ -> None

let get_signature_status ?(rpc_url = default_rpc_url) ~signature_base58 () =
  let opts = `Assoc [ ("searchTransactionHistory", `Bool true) ] in
  let params = `List [ `List [ `String signature_base58 ]; opts ] in
  match%bind request ~rpc_url ~method_:"getSignatureStatuses" ~params with
  | Error _ as err -> return err
  | Ok json ->
    let open Yojson.Safe.Util in
    (match member "value" json with
     | `List (first :: _) ->
       (match first with
        | `Null -> return (Ok None)
        | obj ->
          let slot = obj |> member "slot" |> to_int_option in
          let confirmation =
            obj |> member "confirmationStatus" |> to_string_option
            |> Option.bind ~f:parse_confirmation
          in
          let err = obj |> member "err" |> function
            | `Null -> None
            | v -> Some (Yojson.Safe.to_string v)
          in
          return (Ok (Some { slot; confirmation; err })))
     | _ -> return (Error (`Json_parse "getSignatureStatuses: bad .value")))

let confirmation_at_least cur target =
  let rank = function
    | `Processed -> 0
    | `Confirmed -> 1
    | `Finalized -> 2
  in
  rank cur >= rank target

let confirm
    ?(rpc_url = default_rpc_url)
    ?(until = `Confirmed)
    ?(timeout = Time_float.Span.of_sec 30.0)
    ?(poll = Time_float.Span.of_sec 1.0)
    ~signature_base58
    ()
  =
  let deadline = Time_float.add (Time_float.now ()) timeout in
  let rec loop () =
    match%bind get_signature_status ~rpc_url ~signature_base58 () with
    | Error _ as err -> return err
    | Ok None ->
      (match Time_float.(now () >= deadline) with
       | true -> return (Ok `Timeout)
       | false ->
         let%bind () = Clock.after poll in
         loop ())
    | Ok (Some status) ->
      (match status.err with
       | Some e -> return (Ok (`Tx_error e))
       | None ->
         (match status.confirmation with
          | Some c when confirmation_at_least c until ->
            return (Ok (`Confirmed status))
          | _ ->
            (match Time_float.(now () >= deadline) with
             | true -> return (Ok `Timeout)
             | false ->
               let%bind () = Clock.after poll in
               loop ())))
  in
  loop ()

(** Ethereum JSON-RPC Client

    Provides typed access to standard Ethereum JSON-RPC methods.
    All methods use polymorphic variants for errors.

    @see <https://ethereum.org/en/developers/docs/apis/json-rpc/>
*)

open Core
open Async

type error = [
  | `Rpc of string
  | `Network of string
  | `Json_parse of string
] [@@deriving sexp]

let next_id = ref 1

(** Make a JSON-RPC request to an Ethereum node *)
let make_request ~rpc_url ~method_ ~params =
  let id = !next_id in
  incr next_id;
  let body_json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
    ("id", `Int id);
  ] in
  let body_str = Yojson.Safe.to_string body_json in
  let uri = Uri.of_string rpc_url in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post
        ~body:(Cohttp_async.Body.of_string body_str)
        ~headers
        uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err ->
      `Network (Core.Error.to_string_hum err)))
  in
  match result with
  | Error e -> return (Error e)
  | Ok (response, body) ->
    let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
    let%bind body_str = Cohttp_async.Body.to_string body in
    match code with
    | 200 ->
      (try
        let json = Yojson.Safe.from_string body_str in
        match json with
        | `Assoc fields ->
          (match List.Assoc.find fields ~equal:String.equal "error" with
           | Some (`Assoc err_fields) ->
             let msg = match List.Assoc.find err_fields ~equal:String.equal "message" with
               | Some (`String s) -> s
               | _ -> "Unknown RPC error"
             in
             return (Error (`Rpc msg))
           | _ ->
             (match List.Assoc.find fields ~equal:String.equal "result" with
              | Some result -> return (Ok result)
              | None -> return (Error (`Json_parse "No result field in RPC response"))))
        | _ -> return (Error (`Json_parse "Expected object at root"))
      with ex -> return (Error (`Json_parse (Exn.to_string ex))))
    | _ -> return (Error (`Network (sprintf "HTTP %d: %s" code body_str)))

(** Parse hex string to int *)
let hex_to_int (hex : string) : int =
  let hex = match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  Int.of_string ("0x" ^ hex)

(** Parse hex string to int64 *)
let hex_to_int64 (hex : string) : int64 =
  let hex = match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  Int64.of_string ("0x" ^ hex)

(** eth_call - Execute a message call without creating a transaction *)
let eth_call ~rpc_url ~to_ ~data =
  let params = `List [
    `Assoc [("to", `String to_); ("data", `String data)];
    `String "latest";
  ] in
  make_request ~rpc_url ~method_:"eth_call" ~params >>| (function
  | Ok (`String result) -> Ok result
  | Ok _ -> Error (`Json_parse "Expected string result from eth_call")
  | Error e -> Error e)

(** eth_sendRawTransaction - Submit a signed transaction *)
let eth_send_raw_transaction ~rpc_url ~raw_tx =
  let params = `List [`String raw_tx] in
  make_request ~rpc_url ~method_:"eth_sendRawTransaction" ~params >>| (function
  | Ok (`String tx_hash) -> Ok tx_hash
  | Ok _ -> Error (`Json_parse "Expected string result from eth_sendRawTransaction")
  | Error e -> Error e)

(** eth_getTransactionCount - Get nonce for address *)
let eth_get_transaction_count ~rpc_url ~address =
  let params = `List [`String address; `String "latest"] in
  make_request ~rpc_url ~method_:"eth_getTransactionCount" ~params >>| (function
  | Ok (`String hex) -> Ok (hex_to_int hex)
  | Ok _ -> Error (`Json_parse "Expected string result from eth_getTransactionCount")
  | Error e -> Error e)

(** eth_gasPrice - Get current gas price *)
let eth_gas_price ~rpc_url =
  let params = `List [] in
  make_request ~rpc_url ~method_:"eth_gasPrice" ~params >>| (function
  | Ok (`String hex) -> Ok hex
  | Ok _ -> Error (`Json_parse "Expected string result from eth_gasPrice")
  | Error e -> Error e)

(** eth_estimateGas - Estimate gas for a call *)
let eth_estimate_gas ~rpc_url ~to_ ~data ~from =
  let params = `List [
    `Assoc [
      ("from", `String from);
      ("to", `String to_);
      ("data", `String data);
    ];
  ] in
  make_request ~rpc_url ~method_:"eth_estimateGas" ~params >>| (function
  | Ok (`String hex) -> Ok hex
  | Ok _ -> Error (`Json_parse "Expected string result from eth_estimateGas")
  | Error e -> Error e)

(** eth_chainId - Get chain ID *)
let eth_chain_id ~rpc_url =
  let params = `List [] in
  make_request ~rpc_url ~method_:"eth_chainId" ~params >>| (function
  | Ok (`String hex) -> Ok (hex_to_int hex)
  | Ok _ -> Error (`Json_parse "Expected string result from eth_chainId")
  | Error e -> Error e)

(** eth_getBalance - Get ETH balance of address *)
let eth_get_balance ~rpc_url ~address =
  let params = `List [`String address; `String "latest"] in
  make_request ~rpc_url ~method_:"eth_getBalance" ~params >>| (function
  | Ok (`String hex) -> Ok hex
  | Ok _ -> Error (`Json_parse "Expected string result from eth_getBalance")
  | Error e -> Error e)

(** eth_getTransactionReceipt - Get receipt for a mined transaction *)
let eth_get_transaction_receipt ~rpc_url ~tx_hash =
  let params = `List [`String tx_hash] in
  make_request ~rpc_url ~method_:"eth_getTransactionReceipt" ~params >>| (function
  | Ok (`Null) -> Ok None
  | Ok json -> Ok (Some json)
  | Error e -> Error e)

let _hex_to_int64 = hex_to_int64

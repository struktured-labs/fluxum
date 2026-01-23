(** GMX DEX Subgraph API Client

    @see <https://gmxio.gitbook.io/gmx/api>
*)

open Core
open Async

type error = [
  | `Network of string
  | `Json_parse of string
  | `Http of int * string
  | `GraphQL of string
] [@@deriving sexp]

let make_graphql_request ~cfg ~query =
  let uri = Uri.of_string cfg.Cfg.subgraph_url in
  let headers = Cohttp.Header.init () in
  let headers = Cohttp.Header.add headers "Content-Type" "application/json" in
  let body = `Assoc [("query", `String query)] |> Yojson.Safe.to_string in
  let%bind result =
    Deferred.Or_error.try_with (fun () ->
      Cohttp_async.Client.post ~body:(Cohttp_async.Body.of_string body) ~headers uri)
    |> Deferred.map ~f:(Result.map_error ~f:(fun err -> `Network (Core.Error.to_string_hum err)))
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
          (match List.Assoc.find fields ~equal:String.equal "errors" with
           | Some errors -> return (Error (`GraphQL (Yojson.Safe.to_string errors)))
           | None ->
             (match List.Assoc.find fields ~equal:String.equal "data" with
              | Some data -> return (Ok data)
              | None -> return (Error (`Json_parse "No data field"))))
        | _ -> return (Error (`Json_parse "Expected object"))
      with ex -> return (Error (`Json_parse (Exn.to_string ex))))
    | _ -> return (Error (`Http (code, body_str)))

let get_trades ~cfg ?(first = 100) () =
  let query = sprintf {|
    {
      trades(first: %d, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        account
        sizeDelta
        collateralDelta
        isLong
        price
        fee
      }
    }
  |} first in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "trades" with
         | Some (`List trades_json) ->
           let trade_results = List.map trades_json ~f:Types.trade_of_yojson in
           let rec collect acc = function
             | [] -> Ok (List.rev acc)
             | Ok t :: rest -> collect (t :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect [] trade_results)
         | _ -> return (Error (`Json_parse "Expected trades array")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let get_positions ~cfg ~account =
  let query = sprintf {|
    {
      positions(where: { account: "%s" }) {
        id
        account
        collateral
        size
        averagePrice
        entryFundingRate
        realisedPnl
      }
    }
  |} account in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok json ->
    (try
      match json with
      | `Assoc fields ->
        (match List.Assoc.find fields ~equal:String.equal "positions" with
         | Some (`List positions_json) ->
           let position_results = List.map positions_json ~f:Types.position_of_yojson in
           let rec collect acc = function
             | [] -> Ok (List.rev acc)
             | Ok p :: rest -> collect (p :: acc) rest
             | Error msg :: _ -> Error (`Json_parse msg)
           in
           return (collect [] position_results)
         | _ -> return (Error (`Json_parse "Expected positions array")))
      | _ -> return (Error (`Json_parse "Expected object"))
    with ex -> return (Error (`Json_parse (Exn.to_string ex))))

let get_volume_stats ~cfg =
  let query = {|
    {
      volumeStats(first: 30, orderBy: timestamp, orderDirection: desc) {
        id
        timestamp
        margin
        liquidation
        swap
        mint
        burn
      }
    }
  |} in
  let%bind data = make_graphql_request ~cfg ~query in
  match data with
  | Error e -> return (Error e)
  | Ok _ -> return (Ok ())

(** Kraken order management commands *)

let name = "order"

module New = struct
  let name = "new"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Place a new Kraken order"
      (Command.Param.(
        let pair = flag "--pair" (required string) ~doc:"STRING trading pair (e.g., XETHZUSD)"
        and side = flag "--side" (required string) ~doc:"STRING buy or sell"
        and ordertype = flag "--type" (required string) ~doc:"STRING market, limit, etc."
        and volume = flag "--volume" (required float) ~doc:"FLOAT order volume"
        and price = flag "--price" (optional string) ~doc:"STRING order price (for limit orders)"
        and cfg = flag "-cfg" (optional string) ~doc:"STRING environment (production)"
        in
        return (fun pair side ordertype volume price cfg () ->
          let cfg_env = match cfg with Some c -> c | None -> "production" in
          let cfg = Cfg.of_string cfg_env in
          let module Cfg = (val cfg : Cfg.S) in
          let price_opt = match price with Some p -> p | None -> "" in
          V1.add_order (module Cfg) ~pair ~type_:side ~ordertype ~volume ~price:price_opt () >>= function
          | `Ok json ->
            printf "Order placed:\n%s\n" (Yojson.Safe.pretty_to_string json);
            Deferred.unit
          | `Api_error { errors } ->
            eprintf "API Error: %s\n" (String.concat ~sep:", " errors);
            Deferred.unit
          | `Json_parse_error { message; body } ->
            eprintf "JSON Parse Error: %s\nBody: %s\n" message body;
            Deferred.unit
          | `Bad_request err ->
            eprintf "Bad Request: %s\n" err;
            Deferred.unit
          | `Not_found ->
            eprintf "Not found\n";
            Deferred.unit
          | `Service_unavailable err ->
            eprintf "Service Unavailable: %s\n" err;
            Deferred.unit
          | `Not_acceptable err ->
            eprintf "Not Acceptable: %s\n" err;
            Deferred.unit
          | `Unauthorized err ->
            eprintf "Unauthorized: %s\n" err;
            Deferred.unit
          | `Too_many_requests err ->
            eprintf "Too Many Requests: %s\n" err;
            Deferred.unit)
        <*> pair
        <*> side
        <*> ordertype
        <*> volume
        <*> price
        <*> cfg
      )))
end

module Cancel = struct
  let name = "cancel"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Cancel a Kraken order by ID"
      (Command.Param.(
        let txid = flag "--txid" (required string) ~doc:"STRING order transaction ID"
        and cfg = flag "-cfg" (optional string) ~doc:"STRING environment (production)"
        in
        return (fun txid cfg () ->
          let cfg_env = match cfg with Some c -> c | None -> "production" in
          let cfg = Cfg.of_string cfg_env in
          let module Cfg = (val cfg : Cfg.S) in
          V1.cancel_order (module Cfg) ~txid () >>= function
          | `Ok json ->
            printf "Order canceled:\n%s\n" (Yojson.Safe.pretty_to_string json);
            Deferred.unit
          | #Rest.Error.post as err ->
            eprintf "Error: %s\n" (Rest.Error.sexp_of_post err |> Sexp.to_string_hum);
            Deferred.unit)
        <*> txid
        <*> cfg
      )))
end

module Status = struct
  let name = "status"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Get open orders from Kraken"
      (Command.Param.(
        let trades = flag "--trades" no_arg ~doc:"Include trade details"
        and cfg = flag "-cfg" (optional string) ~doc:"STRING environment (production)"
        in
        return (fun trades cfg () ->
          let cfg_env = match cfg with Some c -> c | None -> "production" in
          let cfg = Cfg.of_string cfg_env in
          let module Cfg = (val cfg : Cfg.S) in
          V1.open_orders ~trades (module Cfg) () >>= function
          | `Ok json ->
            printf "Open Orders:\n%s\n" (Yojson.Safe.pretty_to_string json);
            Deferred.unit
          | #Rest.Error.post as err ->
            eprintf "Error: %s\n" (Rest.Error.sexp_of_post err |> Sexp.to_string_hum);
            Deferred.unit)
        <*> trades
        <*> cfg
      )))
end

let command : string * Command.t =
  (name, Command.group
    ~summary:"Kraken order management commands"
    [ New.command; Cancel.command; Status.command ])

open Core
open Async

(* Exchange-specific command groups *)
let gemini_command = Gemini.command
let kraken_command = Kraken.command
let binance_command = Binance.command
let mexc_command = Mexc.command
let hyperliquid_command = Hyperliquid.command
let coinbase_command = Coinbase.command
let bitrue_command = Bitrue.command

(* Gemini adapter helpers *)
let get_gemini_balances cfg =
  let module Cfg = (val cfg : Gemini.Cfg.S) in
  Gemini.Nonce.File.(pipe ~init:default_filename) () >>= fun nonce ->
  Gemini.V1.Balances.post (module Cfg) nonce () >>= function
  | `Ok balances ->
    (* Convert native Gemini balances to normalized types *)
    let normalized = List.map balances ~f:(fun (b : Gemini.V1.Balances.balance) ->
      let open Fluxum.Types in
      (* Decimal_string.t is just string, so we can convert directly *)
      let total = Float.of_string b.amount in
      let available = Float.of_string b.available in
      (* Currency.Enum_or_string.t can be converted via sexp *)
      let currency = 
        Gemini.V1.Currency.Enum_or_string.sexp_of_t b.currency 
        |> Sexp.to_string 
      in
      { Balance.venue = Venue.Gemini
      ; currency
      ; total
      ; available
      ; locked = total -. available
      })
    in
    return (Ok normalized)
  | #Gemini.Rest.Error.post as err ->
    return (Error (Fluxum.Types.Error.Exchange_specific 
      { venue = Fluxum.Types.Venue.Gemini
      ; code = "rest_error"
      ; message = Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_post err)
      }))

(* Kraken adapter helpers *)
let get_kraken_balances cfg =
  let module Cfg = (val cfg : Kraken.Cfg.S) in
  Kraken.V1.balances (module Cfg) () >>= function
  | `Ok json ->
    (* Parse JSON response from Kraken - structure: { "error": [...], "result": { "CURRENCY": "amount", ... } } *)
    (try
      let open Yojson.Safe in
      let error_list = Util.member "error" json in
      (match error_list with
       | `List [] | `List _ ->
         (* Check for errors *)
         (match error_list with
          | `List (_ :: _ as errs) ->
            let error_msg = List.map errs ~f:(function
              | `String s -> s
              | _ -> "Unknown error")
              |> String.concat ~sep:"; " 
            in
            return (Error (Fluxum.Types.Error.Exchange_specific 
              { venue = Fluxum.Types.Venue.Kraken
              ; code = "api_error"
              ; message = error_msg
              }))
          | _ ->
            (* No errors, parse result *)
            (match Util.member "result" json with
            | `Assoc balances ->
              let normalized = List.filter_map balances ~f:(fun (currency, amount_json) ->
                match amount_json with
                | `String amount ->
                  (try
                    let total = Float.of_string amount in
                    Some (Fluxum.Types.{
                      Balance.venue = Venue.Kraken
                    ; currency = String.uppercase currency
                    ; total
                    ; available = total  (* Kraken returns total balance *)
                    ; locked = 0.0
                    })
                  with _ -> None)
                | _ -> None
              ) in
              return (Ok normalized)
            | _ ->
              return (Error (Fluxum.Types.Error.Exchange_specific 
                { venue = Fluxum.Types.Venue.Kraken
                ; code = "parse_error"
                ; message = "Could not find result field in Kraken response"
                }))))
       | _ ->
         return (Error (Fluxum.Types.Error.Exchange_specific 
           { venue = Fluxum.Types.Venue.Kraken
           ; code = "parse_error"
           ; message = "Invalid error field in Kraken response"
           })))
    with e ->
      return (Error (Fluxum.Types.Error.Exchange_specific 
        { venue = Fluxum.Types.Venue.Kraken
        ; code = "json_error"
        ; message = Exn.to_string e
        })))
  | #Kraken.Rest.Error.post as err ->
    return (Error (Fluxum.Types.Error.Exchange_specific
      { venue = Fluxum.Types.Venue.Kraken
      ; code = "rest_error"
      ; message = Sexp.to_string_hum (Kraken.Rest.Error.sexp_of_post err)
      }))

(* Generic API command that accepts --exchange flag *)
let api_command =
  Command.group ~summary:"Generic exchange API commands"
    [ ("order", Command.async
        ~summary:"Get open orders from an exchange"
        (Command.Param.(
          let exchange = flag "--exchange" (optional string)
              ~doc:"STRING exchange name (gemini, kraken, binance, coinbase, mexc)"
          in
          let cfg = flag "-cfg" (optional string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            let cfg_env = match cfg with Some c -> c | None -> "production" in
            (match Option.value exchange ~default:"kraken" with
            | "kraken" ->
              let cfg = Kraken.Cfg.of_string cfg_env in
              let module Cfg = (val cfg : Kraken.Cfg.S) in
              Kraken.V1.open_orders (module Cfg) () >>= (function
                | `Ok json ->
                  printf "Open Orders:\n%s\n" (Yojson.Safe.pretty_to_string json);
                  Deferred.unit
                | #Kraken.Rest.Error.post as err ->
                  eprintf "Error: %s\n" (Sexp.to_string_hum (Kraken.Rest.Error.sexp_of_post err));
                  Deferred.unit)
            | "gemini" ->
              eprintf "Order queries not yet implemented for Gemini\n";
              Deferred.unit
            | exch ->
              eprintf "Exchange %s not yet implemented\n" exch;
              Deferred.unit))
          <*> exchange
          <*> cfg
        )))
    ; ("balance", Command.async
        ~summary:"Get balances across exchanges"
        (Command.Param.(
          let exchange = flag "--exchange" (optional string)
              ~doc:"STRING exchange name (gemini, kraken, binance, coinbase, mexc)"
          in
          let cfg = flag "-cfg" (optional string)
              ~doc:"STRING environment (production)"
          in
          return (fun exchange cfg () ->
            let cfg_env = match cfg with Some c -> c | None -> "production" in
            match Option.value exchange ~default:"gemini" with
            | "gemini" ->
              let cfg = Gemini.Cfg.of_string cfg_env in
              get_gemini_balances cfg >>= fun result ->
              (match result with
               | Ok balances ->
                 List.iter balances ~f:(fun balance ->
                   printf "%s\n" (Sexp.to_string_hum (Fluxum.Types.Balance.sexp_of_t balance)));
                 Deferred.unit
               | Error err ->
                 eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
                 Deferred.unit)
            | "kraken" ->
              let cfg = Kraken.Cfg.of_string cfg_env in
              get_kraken_balances cfg >>= fun result ->
              (match result with
               | Ok balances ->
                 List.iter balances ~f:(fun balance ->
                   printf "%s\n" (Sexp.to_string_hum (Fluxum.Types.Balance.sexp_of_t balance)));
                 Deferred.unit
               | Error err ->
                 eprintf "Error: %s\n" (Sexp.to_string_hum (Fluxum.Types.Error.sexp_of_t err));
                 Deferred.unit)
            | exch ->
              eprintf "Exchange %s not yet implemented\n" exch;
              Deferred.unit)
          <*> exchange
          <*> cfg
        )))
    ]

(* Main command structure *)
let command =
  Command.group ~summary:"Fluxum - Multi-exchange trading API"
    [ ("gemini", gemini_command)
    ; ("kraken", kraken_command)
    ; ("binance", binance_command)
    ; ("mexc", mexc_command)
    ; ("hyperliquid", hyperliquid_command)
    ; ("coinbase", coinbase_command)
    ; ("bitrue", bitrue_command)
    ; ("api", api_command)
    ]

let () = Command_unix.run command

(** OKX Ledger - Placeholder

    TODO: Implement Ledger_intf.ENTRY for OKX
*)

open Core

let command =
  Command.group ~summary:"OKX Ledger Commands"
    [ ("placeholder", Command.basic ~summary:"Ledger not yet implemented"
        (Command.Param.return (fun () -> print_endline "OKX ledger TODO")))
    ]

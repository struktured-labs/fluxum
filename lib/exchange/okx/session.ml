(** OKX Session - Placeholder

    TODO: Implement Session_intf.S for OKX
*)

open Core
open Async

type t = unit

let create ~symbol =
  ignore symbol;
  return ()

let start _t = return ()
let stop _t = return ()

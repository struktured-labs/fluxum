(** Bitrue Exchange Module *)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Order_book = Order_book

let command : Command.t =
  Command.group ~summary:"Bitrue Exchange Commands"
    []

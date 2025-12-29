(** Bitrue Exchange Module *)

open Core

module Cfg = Cfg
module Rest = Rest
module Ws = Ws
module Market_data_curl = Market_data_curl
module Order_book = Order_book

let command : Command.t =
  Command.group ~summary:"Bitrue Exchange Commands"
    []

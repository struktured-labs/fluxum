(** KuCoin Exchange Module

    KuCoin is a global cryptocurrency exchange founded in 2017,
    known for wide token selection and competitive fees.

    This module provides REST API access for spot trading.

    @see <https://www.kucoin.com/docs> *)

module Cfg = Cfg
module Types = Types
module Rest = Rest
module Pool_adapter = Pool_adapter
module Fluxum_adapter = Fluxum_adapter

let command : Command.t =
  Command.group
    ~summary:"KuCoin Command System"
    [ Fluxum_adapter.command ]
